(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ty
open Typedtree

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

type occurs_check_mode =
  | Enable
  | Disable

let mode = ref Disable

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Stdlib.Format.fprintf ppf "Typechecker error: occurs check failed"
  | `No_variable s ->
    Stdlib.Format.fprintf ppf "Typechecker error: undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Stdlib.Format.fprintf
      ppf
      "Typechecker error: unification failed on %a and %a"
      Pprintty.pp_ty
      l
      Pprintty.pp_ty
      r
;;

module R : sig
  include Monad.Infix

  (* val bind : 'a t -> f:('a -> 'b t) -> 'b t *)
  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Stdlib.Set.Make (Int)

  (* let pp ppf s =
     Stdlib.Format.fprintf ppf "[ ";
    iter (Stdlib.Format.fprintf ppf "%d; ") s;
    Stdlib.Format.fprintf ppf "]"
     ;; *)

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | Tyvar b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | Prim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Tyvar b -> VarSet.add b acc
      | Arrow (l, r) -> helper (helper acc l) r
      | Prim _ -> acc
    in
    helper VarSet.empty
  ;;
end

let fold_left map ~init ~f =
  Map.Poly.fold map ~init ~f:(fun ~key ~data acc ->
    let open R.Syntax in
    let* acc = acc in
    f key data acc)
;;

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
  (* val pp : Format.formatter -> t -> unit *)
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty) Map.Poly.t

  (* let pp ppf subst =
     let open Format in
     Map.Poly.iteri subst ~f:(fun ~key ~data ->
     fprintf ppf "'_%d -> %a@\n" key (Ty.pp_ty_with_subs None) data)
     ;; *)

  let empty = Map.Poly.empty

  let mapping k v =
    match !mode with
    | Enable -> if Type.occurs_in k v then fail `Occurs_check else return (k, v)
    | Disable -> return (k, v)
  ;;

  let singleton k v =
    let* f, t = mapping k v in
    return @@ Map.Poly.singleton f t
  ;;

  let find_exn f m = Map.Poly.find_exn m f
  let remove m f = Map.Poly.remove m f

  let apply subst =
    let rec helper = function
      | Tyvar b as ty ->
        (match find_exn b subst with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Prim l, Prim r when equal_prim l r -> return empty
    | Prim _, Prim _ -> fail (`Unification_failed (l, r))
    | Tyvar a, Tyvar b when Int.equal a b -> return empty
    | Tyvar b, t | t, Tyvar b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail (`Unification_failed (l, r))

  and extend key value extensible_subst =
    match Map.Poly.find extensible_subst key with
    | None ->
      let v = apply extensible_subst value in
      let* s2 = singleton key v in
      fold_left extensible_subst ~init:(return s2) ~f:(fun key value acc ->
        let v = apply s2 value in
        let* mapk, mapv = mapping key v in
        return @@ Map.Poly.set acc ~key:mapk ~data:mapv)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose extensible_subst s2

  and compose s1 s2 = fold_left s2 ~init:(return s1) ~f:extend

  let compose_all s1 =
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    in
    fold_left s1 ~init:(return empty) ~f:compose
  ;;
end

type binder_set = VarSet.t
type scheme = S of binder_set * ty

module Scheme = struct
  type t = scheme

  let free_vars = function
    | S (names, ty) -> VarSet.diff (Type.free_vars ty) names
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  (* let pp =
     let helper ppf =
     let open Stdlib.Format in
     let open Ty in
     function
     | S (set, ty) -> fprintf ppf "S(%s, %a)" (show_binder_set set) pp_ty ty
     in
     helper
     ;; *)
end

type environment = (string * scheme) list

module TypeEnv = struct
  type t = environment

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, scheme) ->
      VarSet.union acc (Scheme.free_vars scheme))
  ;;

  let apply subst env = List.Assoc.map env ~f:(Scheme.apply subst)

  (* let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;; *)

  (* let available_bindings env = List.Assoc.map env ~f:(fun (S (_, ty)) -> ty) *)
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> var_typ n

let instantiate : scheme -> ty R.t =
  fun (S (names, ty)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    names
    (return ty)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env var env =
  match List.Assoc.find_exn env ~equal:String.equal var with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_variable var)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

(* let pp_env subst ppf env =
   let env : TypeEnv.t =
   List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
   in
   TypeEnv.pp ppf env
   ;; *)

let infer_expr =
  let open Ast in
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty * texpr) R.t) =
    fun env -> function
    | EVar x ->
      let* s, t = lookup_env x env in
      return (s, t, tvar x t)
    | EConst const ->
      (match const with
       | CBool _ -> return (Subst.empty, tybool, tconst const tybool)
       | CInt _ -> return (Subst.empty, tyint, tconst const tyint))
    | EBinop (op, e1, e2) ->
      let* s1, t1, te1 = helper env e1 in
      let* s2, t2, te2 = helper env e2 in
      (match op with
       | Add | Sub | Div | Mul ->
         let* s3 = unify t1 tyint in
         let* s4 = unify t2 tyint in
         let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
         return (final_subst, tyint, tbinop op te1 te2 (arrow tyint (arrow tyint tyint)))
       | Xor | And | Or ->
         let* s3 = unify t1 tybool in
         let* s4 = unify t2 tybool in
         let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
         return
           (final_subst, tybool, tbinop op te1 te2 (arrow tybool (arrow tybool tybool)))
       | Eq | Neq | Gt | Lt | Gte | Lte ->
         (match t1, t2 with
          | _ ->
            let* s3 = unify t1 t2 in
            let final_typ = Subst.apply s3 t1 in
            let* final_subst = Subst.compose_all [ s1; s2; s3 ] in
            return
              ( final_subst
              , tybool
              , tbinop op te1 te2 (arrow final_typ (arrow final_typ tybool)) )))
    | EApp (e1, e2) ->
      let* s1, t1, te1 = helper env e1 in
      let* s2, t2, te2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      let typedres = Subst.apply final_subst tv in
      return (final_subst, typedres, tapp te1 te2 typedres)
    | EIfThenElse (c, th, el) ->
      let* s1, t1, te1 = helper env c in
      let* s2, t2, te2 = helper env th in
      let* s3, t3, te3 = helper env el in
      let* s4 = unify t1 tybool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return
        ( final_subst
        , Subst.apply final_subst t2
        , tifthenelse te1 te2 te3 (Subst.apply final_subst t2) )
    | ELetIn (name, e1, e2) ->
      let* s1, t1_typ, te1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t1 = generalize env2 t1_typ in
      let* s2, t3, te2 = helper (TypeEnv.extend env2 (name, t1)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3, tletin name te1 te2 t1_typ)
    | ELetRecIn (name, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
      let* s1, t1, te1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s1 s2 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2, te2 = helper TypeEnv.(extend (apply s env) (name, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2, tletrecin name te1 te2 t1)
    | EFun (arg, e) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (arg, S (VarSet.empty, tv)) in
      let* s1, t1, te1 = helper env e in
      let typedres = Arrow (Subst.apply s1 tv, t1) in
      return (s1, typedres, tfun arg tv te1 typedres)
  in
  helper
;;

let infer_binding env =
  let open Ast in
  let open Typedtree in
  function
  | ELet (name, e) ->
    let* s, t, te = infer_expr env e in
    return (s, t, tlet name te t)
  | ELetRec (name, e) ->
    let* tv = fresh_var in
    let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
    let* s1, t, te = infer_expr env e in
    let* s2 = unify (Subst.apply s1 tv) t in
    let* funal_subst = Subst.compose s1 s2 in
    return (funal_subst, t, tletrec name te t)
;;

let fix_typedtree subst =
  let apply_subst = Subst.apply subst in
  let rec helper = function
    | TVar (name, typ) -> tvar name (apply_subst typ)
    | TBinop (op, e1, e2, typ) -> tbinop op (helper e1) (helper e2) (apply_subst typ)
    | TApp (e1, e2, typ) -> tapp (helper e1) (helper e2) (apply_subst typ)
    | TLetIn (name, e1, e2, typ) -> tletin name (helper e1) (helper e2) (apply_subst typ)
    | TLetRecIn (name, e1, e2, typ) ->
      tletrecin name (helper e1) (helper e2) (apply_subst typ)
    | TFun (Arg (name, arg_typ), e, typ) ->
      tfun name (apply_subst arg_typ) (helper e) (apply_subst typ)
    | TIfThenElse (i, t, e, typ) ->
      tifthenelse (helper i) (helper t) (helper e) (apply_subst typ)
    | other -> other
  in
  helper
;;

let empty : environment = TypeEnv.empty

let infer_expr e =
  let* subst, ty, te = infer_expr empty e in
  let typed_tree = fix_typedtree subst te in
  return (ty, typed_tree)
;;

let fix_typedtree subst =
  let apply_subst = Subst.apply subst in
  function
  | TLet (name, e, typ) -> tlet name (fix_typedtree subst e) (apply_subst typ)
  | TLetRec (name, e, typ) -> tletrec name (fix_typedtree subst e) (apply_subst typ)
;;

let infer_statements (bindings : Ast.statements) : tbinding list t =
  let open Ast in
  let* _, tbindings =
    List.fold
      ~init:(return (empty, []))
      ~f:(fun env_binding ->
        function
        | (ELet (name, _) | ELetRec (name, _)) as new_binding ->
          let* env, tbindings = env_binding in
          let* subst, ty, tbinding = infer_binding env new_binding in
          return
            ( TypeEnv.extend env (name, S (VarSet.empty, ty))
            , fix_typedtree subst tbinding :: tbindings ))
      bindings
  in
  return @@ List.rev tbindings
;;

let infer_type infer_fun s = Result.map ~f:Stdlib.Fun.id (run (infer_fun s))
let infer_expr = infer_type infer_expr
let infer_statements = infer_type infer_statements

let infer (stms : Ast.statements) (check_mode : occurs_check_mode)
  : (Typedtree.tbinding list, error) result
  =
  mode := check_mode;
  infer_statements stms
;;
