(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Typing

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Int.comparator_witness) Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok x -> f x last
  ;;

  let fail err state = state, Result.fail err
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f state ->
    match x state with
    | state, Ok x -> state, Ok (f x)
    | state, Error e -> state, Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | _ -> false
  ;;

  let unpack_vars =
    let empty = Set.empty (module Int) in
    let rec ehelper acc = function
      | TVar n -> Set.add acc n
      | TArr (left, right) -> ehelper (ehelper acc left) right
      | TGround _ -> acc
    in
    ehelper empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t
  val find_exn : fresh -> t -> typ
  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val add : t -> fresh -> typ -> t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping key value =
    if Type.occurs_in key value then fail `OccursCheck else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Map.find_exn subst key
  let find key subst = Map.find subst key
  let remove subst key = Map.remove subst key
  let add subst key value = Map.update subst key ~f:(fun _ -> value)

  let apply s =
    let rec ehelper = function
      | TVar n ->
        (match find_exn n s with
         | exception Not_found_s _ -> TVar n
         | x -> x)
      | TArr (left, right) -> arrow_t (ehelper left) (ehelper right)
      | ground -> ground
    in
    ehelper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when phys_equal l r -> return empty
    | TGround _, TGround _ -> fail @@ `UnificationFailed (l, r)
    | TVar a, TVar b when a = b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose_all [ subs1; subs2 ]
    | _ -> fail @@ `UnificationFailed (l, r)

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return @@ Base.Map.update acc k ~f:(fun _ -> v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold f ini set =
    Set.fold set ~init:ini ~f:(fun acc x ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | s, t -> (not (Set.mem s v)) && Type.occurs_in v t
  ;;

  let unpack_vars = function
    | s, t -> Set.diff (Type.unpack_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, String.comparator_witness) Map.t

  let extend env id scheme = Map.update env id ~f:(fun _ -> scheme)
  let empty = Map.empty (module String)

  let unpack_vars : t -> (type_variable_number, Int.comparator_witness) Set.t =
    Map.fold
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.unpack_vars data))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| var_t

let instantiate : scheme -> typ R.t =
  fun (set, t) ->
  VarSet.fold
    (fun typ name ->
      let* f = fresh_var in
      let* s = Subst.singleton name f in
      return (Subst.apply s typ))
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env typ ->
  let free = Set.diff (Type.unpack_vars typ) (TypeEnv.unpack_vars env) in
  free, typ
;;

let lookup_env e map =
  match Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans, map)
;;

let rec get_name = function
  | EApp (l, _) -> get_name l
  | EVar name -> return name
  | _ -> fail `Not_function
;;

let infer =
  let rec ehelper : TypeEnv.t -> exp -> (Subst.t * typ * TypeEnv.t) R.t =
    fun env -> function
    | EConst x ->
      (match x with
       | CInt _ -> return (Subst.empty, int_typ, env)
       | CBool _ -> return (Subst.empty, bool_typ, env))
    | EVar identifier -> lookup_env identifier env
    | EUnOp (op, expr) ->
      let operand_type =
        match op with
        | Minus -> int_typ
        | Not -> bool_typ
      in
      let* subst, t, _ = ehelper env expr in
      let* subst' = unify t operand_type in
      let* final_subst = Subst.compose subst' subst in
      return (final_subst, operand_type, env)
    | EBinOp (op, left, right) ->
      let* left_subst, left_type, _ = ehelper env left in
      let* right_subst, right_type, _ = ehelper env right in
      (match op with
       | Add | Sub | Mul | Div ->
         let* subst' = unify left_type int_typ in
         let* subst'' = unify right_type int_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         return (final_subst, int_typ, env)
       | Gre | Geq | Less | Leq ->
         let* subst' = unify left_type int_typ in
         let* subst'' = unify right_type int_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         return (final_subst, bool_typ, env)
       | Eq | Neq ->
         let* fresh_eq_var = fresh_var in
         let* subst' = unify left_type fresh_eq_var in
         let* subst'' = unify right_type fresh_eq_var in
         let* final_subst =
           Subst.compose_all [ left_subst; right_subst; subst'; subst'' ]
         in
         return (final_subst, bool_typ, env)
       | And | Or ->
         let* subst1 = unify left_type bool_typ in
         let* subst2 = unify right_type bool_typ in
         let* final_subst =
           Subst.compose_all [ subst1; subst2; left_subst; right_subst ]
         in
         return (final_subst, bool_typ, env))
    | ELet (bindings_list, expression) ->
      let rec process_list subst env = function
        | [] -> return (subst, env)
        | elem :: tail ->
          let* identifier, exp =
            match elem with
            | _, PtVar id, exp -> return (id, exp)
            | _ -> fail `Unreachable
          in
          let* fresh_var = fresh_var in
          let env' =
            TypeEnv.extend env identifier (Base.Set.empty (module Base.Int), fresh_var)
          in
          let* elem_subst, elem_typ, _ = ehelper env' exp in
          let env'' = TypeEnv.apply elem_subst env' in
          let generalized_type = generalize env'' elem_typ in
          let* subst'' = Subst.compose subst elem_subst in
          process_list subst'' (TypeEnv.extend env'' identifier generalized_type) tail
      in
      let* subst', env' = process_list Subst.empty env bindings_list in
      let* subst_expr, typ_expr, _ = ehelper env' expression in
      let* final_subst = Subst.compose subst' subst_expr in
      return (final_subst, typ_expr, env)
    | EApp (left, right) ->
      let* subst_left, typ_left, _ = ehelper env left in
      let* subst_right, typ_right, _ = ehelper (TypeEnv.apply subst_left env) right in
      let* type_variable = fresh_var in
      let* subst' =
        unify (arrow_t typ_right type_variable) (Subst.apply subst_right typ_left)
      in
      let result_type = Subst.apply subst' type_variable in
      let* final_subst = Subst.compose_all [ subst_left; subst_right; subst' ] in
      let new_env = env in
      return (final_subst, result_type, new_env)
    | EFun (PtVar head, body) ->
      let* type_variable = fresh_var in
      let env' = TypeEnv.extend env head (Set.empty (module Int), type_variable) in
      let* subst, typ, _ = ehelper env' body in
      let result_type = arrow_t (Subst.apply subst type_variable) typ in
      return (subst, result_type, env)
    | EFun (PtWild, body) ->
      let* type_variable = fresh_var in
      let* subst, typ, _ = ehelper env body in
      return (subst, arrow_t type_variable typ, env)
    | EFun (_, _) -> fail `Unreachable
    | EIf (condition, true_branch, false_branch) ->
      let* condition_subst, condition_type, _ = ehelper env condition in
      let* true_branch_subst, true_branch_type, _ = ehelper env true_branch in
      let* false_branch_subst, false_branch_type, _ = ehelper env false_branch in
      let* subst' = unify condition_type bool_typ in
      let* subst'' = unify false_branch_type true_branch_type in
      let* final_subst =
        Subst.compose_all
          [ condition_subst; subst'; subst''; true_branch_subst; false_branch_subst ]
      in
      return (final_subst, Subst.apply final_subst false_branch_type, env)
  in
  ehelper
;;

let check_types environment (dec : decl) =
  match dec with
  | DLet (false, PtVar name, exp) ->
    let* subst, function_type, environment' = infer environment exp in
    let res_typ = function_type in
    let generalized_type = generalize environment' (Subst.apply subst res_typ) in
    return (TypeEnv.extend environment' name generalized_type, res_typ)
  | DLet (true, PtVar name, exp) ->
    let* type_variable = fresh_var in
    let env =
      TypeEnv.extend environment name (Base.Set.empty (module Base.Int), type_variable)
    in
    let* subst, typ', environment' = infer env exp in
    let typ = typ' in
    let* subst' = unify (Subst.apply subst type_variable) typ in
    let* final_subst = Subst.compose subst' subst in
    let env = TypeEnv.apply final_subst env in
    let generalized_type = generalize env (Subst.apply final_subst type_variable) in
    return (TypeEnv.extend environment' name generalized_type, typ)
  | _ -> fail `Unreachable
;;

let run_inference expression env = run (check_types env expression)
