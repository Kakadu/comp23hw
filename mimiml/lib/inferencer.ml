(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* http://dev.stephendiehl.com/fun/006_hindley_milner.html *)

open Base
open Ast
open Typing
module Format = Stdlib.Format

(* Original code was taken from
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml,
   (with small updates for printf supporting) *)

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable %S" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "Unification failed: type of the expression is '%a'" Pp.pp_ty l;
    Format.fprintf ppf " but expected type was '%a'" Pp.pp_ty r
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
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
    | (_, Result.Error _) as er -> er
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
    let ( let+ ) x f = bind x ~f:(fun x -> return (f x))
  end

  module RList = struct
    let fold_left xs ~init ~f =
      let open Syntax in
      List.fold_left xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | VarTy name -> name = v
    | ArrowTy (l, r) -> occurs_in v l || occurs_in v r
    | PrimTy _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | VarTy name -> VarSet.add name acc
      | ArrowTy (l, r) -> helper (helper acc l) r
      | PrimTy _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty * ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh * ty) list

  let pp ppf =
    let open Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k Pp.pp_ty v))
  ;;

  let empty = []
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)
  let singleton k v = mapping k v >>| fun mapping -> [ mapping ]
  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper = function
      | VarTy b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | ArrowTy (l, r) -> arrowty (helper l) (helper r)
      | other -> other
    in
    helper
  ;;

  let rec unify =
    let open Typing in
    function
    | PrimTy l, PrimTy r when prim_ty_eq l r -> return empty
    | (PrimTy _, PrimTy _) as x -> fail (`Unification_failed x)
    | VarTy a, VarTy b when Int.equal a b -> return empty
    | VarTy b, t | t, VarTy b -> singleton b t
    | ArrowTy (l1, r1), ArrowTy (l2, r2) ->
      let* subs1 = unify (l1, l2) in
      let* subs2 = unify (apply subs1 r1, apply subs1 r2) in
      compose subs1 subs2
    | l, r -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let* s2 = singleton k (apply s v) in
      RList.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        mapping k (apply s2 v) >>| (Fn.flip List.cons) acc)
    | Some v2 -> unify (v, v2) >>= compose s

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let fold_left_m f =
    fold (fun x acc ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  let pp = Pp.pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend = Fn.flip List.cons
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n Pp.pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| varty

let instantiate =
  VarSet.fold_left_m (fun typ name ->
    fresh_var >>= Subst.singleton name >>| (Fn.flip Subst.apply) typ)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_variable e)
  | S (bs, t) -> instantiate bs (return t) >>| fun ty -> Subst.empty, ty
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer =
  let rec helper env = function
    | EVar "*" | EVar "-" | EVar "+" | EVar "/" ->
      return (Subst.empty, arrowty int_typ (arrowty int_typ int_typ))
    | EVar "^" -> return (Subst.empty, arrowty string_typ (arrowty string_typ string_typ))
    | EVar "=" -> fresh_var >>| fun tv -> Subst.empty, arrowty tv (arrowty tv bool_typ)
    | EVar x -> lookup_env x env
    | ELam (PatVar x, e1) ->
      let* tv = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let+ s, ty = helper env2 e1 in
      s, arrowty (Subst.apply s tv) ty
    | EApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1, arrowty t2 tv) in
      let+ final_subst = Subst.compose_all [ s3; s2; s1 ] in
      final_subst, Subst.apply s3 tv
    | EConst lit ->
      let typ =
        match lit with
        | CInt _ -> int_typ
        | CString _ -> string_typ
        | CBool _ -> bool_typ
        | CUnit -> unit_typ
      in
      return (Subst.empty, typ)
    | EIfElse (c, e1, e2) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env e1 in
      let* s3, t3 = helper env e2 in
      let* s4 = unify (t1, bool_typ) in
      let* s5 = unify (t2, t3) in
      let+ final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      final_subst, Subst.apply s5 t2
    | ELet ((NRecF, PatVar x, e1), e2) ->
      let* s1, t1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t2 = generalize env2 t1 in
      let* s2, t3 = helper (TypeEnv.extend env2 (x, t2)) e2 in
      let+ final_subst = Subst.compose s1 s2 in
      final_subst, t3
    | ELet ((RecF, PatVar x, e1), e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s1, t1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv, t1) in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2 = helper TypeEnv.(extend (apply s env) (x, t2)) e2 in
      let+ final_subst = Subst.compose s s2 in
      final_subst, t2
  in
  helper
;;

let w e = Result.map (run (infer TypeEnv.empty e)) ~f:snd

(** {3} Tests *)

let run_subst subst =
  match run subst with
  | Result.Error _ -> Format.printf "Error%!"
  | Ok subst -> Format.printf "%a%!" Subst.pp subst
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 1, int_typ @-> v 2) |> run_subst in
  [%expect {| [ 1 -> int, 2 -> int ] |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> bool_typ, int_typ @-> v 3) |> run_subst in
  [%expect {| [ 1 -> int, 3 -> bool ] |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> bool_typ, v 2 @-> string_typ) |> run_subst in
  [%expect {| Error |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 1, (v 2 @-> int_typ) @-> int_typ @-> int_typ) |> run_subst in
  [%expect {| [ 1 -> (int -> int), 2 -> int ] |}]
;;

let%expect_test _ =
  let _ = unify (v 1 @-> v 2, v 2 @-> v 3) |> run_subst in
  [%expect {| [ 1 -> '_3, 2 -> '_3 ] |}]
;;

let%expect_test "Getting free variables from type scheme" =
  let _ =
    Format.printf
      "%a%!"
      VarSet.pp
      (Scheme.free_vars (S (VarSet.singleton 1, v 1 @-> v 2)))
  in
  [%expect {| [ 2; ] |}]
;;

let pp_err fmt =
  let open Format in
  function
  | `Occurs_check -> fprintf fmt "Occurs check failed.\n"
  | `Unification_failed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is '%a'" Pp.pp_ty t1;
    fprintf fmt " but expected type was '%a'" Pp.pp_ty t2
  | `No_variable s -> fprintf fmt "No variable %S" s
;;

(** run inference and print ty *)
let run_inference e =
  match w e with
  | Ok typ -> Pp.pp_ty Format.std_formatter typ
  | Error err -> pp_error Format.std_formatter err
;;

(** parse input and run 'run_inference' *)
let parse_and_inference input =
  match Parser.parse input with
  | Ok ast -> run_inference ast
  | Error e -> Stdio.print_endline e
;;

let%expect_test "int inference" =
  parse_and_inference "1";
  [%expect {| int |}]
;;

let%expect_test "bool inference" =
  parse_and_inference "true";
  [%expect {| bool |}]
;;

let%expect_test "string inference" =
  parse_and_inference {|"str"|};
  [%expect {| string |}]
;;

let%expect_test "unit inference" =
  parse_and_inference {|()|};
  [%expect {| unit |}]
;;

let%expect_test "arithmetic" =
  parse_and_inference {|1+2/8|};
  [%expect {| int |}]
;;

let%expect_test "arithmetic" =
  parse_and_inference {|(+) 1 2|};
  [%expect {| int |}]
;;

let%expect_test "arithmetic (fail)" =
  parse_and_inference {|(+) 1 true|};
  [%expect
    {| Unification failed: type of the expression is 'int' but expected type was 'bool' |}]
;;

let%expect_test "conditional" =
  parse_and_inference {|if true then 1 else 5|};
  [%expect {| int |}]
;;

let%expect_test "conditional (fail)" =
  parse_and_inference {|if 1 then 1 else 5|};
  [%expect
    {| Unification failed: type of the expression is 'int' but expected type was 'bool' |}]
;;

let%expect_test "conditional (fail)" =
  parse_and_inference {|if true then 1 else ()|};
  [%expect
    {| Unification failed: type of the expression is 'int' but expected type was 'unit' |}]
;;

let%expect_test "conditional (fail)" =
  parse_and_inference {|fun y -> let x=10 in x|};
  [%expect {| ('_0 -> int) |}]
;;

let%expect_test "let" =
  parse_and_inference {|let x = 10=20 in x|};
  [%expect {| bool |}]
;;

let%expect_test "let rec" =
  parse_and_inference {|let rec fact n = if n = 0 then 1 else n * (fact (n - 1)) in fact 4|};
  [%expect {| int |}]
;;
