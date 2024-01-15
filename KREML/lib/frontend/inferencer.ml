(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Typing

type error = Typing.error

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

  module RMap : sig
    val fold_left
      :  (int, 'a, Int.comparator_witness) Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
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
    let ( let+ ) x f = bind x ~f:(fun x -> return (f x))
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
    | TVar id | TEqualityVar id -> id = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | _ -> false
  ;;

  let free_vars =
    let empty_set = Set.empty (module Int) in
    let rec helper acc = function
      | TVar n -> Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
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
    let+ key, value = mapping key value in
    Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Map.find_exn subst key
  let find key subst = Map.find subst key
  let remove subst key = Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find_exn n s with
         | exception Not_found_s _ -> var_t n
         | x -> x)
      | TEqualityVar n ->
        (match find_exn n s with
         | exception Not_found_s _ -> var_eq_t n
         | x -> x)
      | TArr (left, right) -> arrow_t (helper left) (helper right)
      | ground -> ground
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when l == r -> return empty
    | TGround _, TGround _ -> fail @@ `UnificationFailed (l, r)
    | (TVar a, TVar b | TEqualityVar a, TEqualityVar b) when a = b -> return empty
    | TEqualityVar _, TArr _ | TArr _, TEqualityVar _ -> fail @@ `UnificationFailed (l, r)
    | TVar b, t | t, TVar b | TEqualityVar b, t | t, TEqualityVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail @@ `UnificationFailed (l, r)

  and extend k v s =
    match find k s with
    | None ->
      let* s2 = singleton k (apply s v) in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let+ k, v = mapping k (apply s2 v) in
        Map.update acc k ~f:(fun _ -> v))
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
  let fold_right f ini set =
    Set.fold_right set ~init:ini ~f:(fun x acc ->
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

  let free_vars = function
    | s, t -> Set.diff (Type.free_vars t) s
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

  let free_vars : t -> (int, Int.comparator_witness) Set.t =
    Map.fold
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| var_t
let fresh_eq_var = fresh >>| var_eq_t

let instantiate : scheme -> typ R.t =
  fun (set, t) ->
  VarSet.fold_right
    (fun typ name ->
      let* f = fresh_var in
      let+ s = Subst.singleton name f in
      Subst.apply s typ)
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env typ ->
  let free = Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env e map =
  match Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let+ ans = instantiate scheme in
    Subst.empty, ans
;;

let rec find_identifiers = function
  | EBinaryOp (_, left, right) ->
    Set.union (find_identifiers left) (find_identifiers right)
  | EUnaryOp (_, operand) -> find_identifiers operand
  | EApp (expr1, expr2) -> Set.union (find_identifiers expr1) (find_identifiers expr2)
  | EIdentifier id -> Set.singleton (module String) id
  | _ -> Set.empty (module String)
;;

let is_syntactically_value = function
  | EAbs _ | EIdentifier _ | ELiteral _ -> true
  | _ -> false
;;

let infer =
  let rec infer_binding : TypeEnv.t -> binding -> (Subst.t * typ) R.t =
    fun env -> function
    | BVal (_, body) -> infer_expr env body
    | BFun (_, args, body) ->
      let rec curry = function
        | [] -> body
        | hd :: tl -> EAbs (hd, curry tl)
      in
      infer_expr env @@ curry args
  and infer_expr : TypeEnv.t -> expr -> (Subst.t * typ) R.t =
    fun env -> function
    | ELiteral literal ->
      (match literal with
       | LInt _ -> return (Subst.empty, int_t)
       | LBool _ -> return (Subst.empty, bool_t))
    | EIdentifier identifier ->
      (match identifier with
       | "_" ->
         let+ fresh_var = fresh_var in
         Subst.empty, fresh_var
       | _ -> lookup_env identifier env)
    | EUnaryOp (op, expr) ->
      let operand_type =
        match op with
        | Neg -> int_t
        | Not -> bool_t
      in
      let* subst, t = infer_expr env expr in
      let* subst' = unify t operand_type in
      let+ final_subst = Subst.compose subst' subst in
      final_subst, operand_type
    | EBinaryOp (op, left, right) ->
      let* left_subst, left_type = infer_expr env left in
      let* right_subst, right_type = infer_expr env right in
      (match op with
       | Add | Sub | Mult | Div ->
         let* subst' = unify left_type int_t in
         let* subst'' = unify right_type int_t in
         let+ final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         final_subst, int_t
       | Eq | Lt | LtOrEq | Gt | GtOrEq ->
         let* fresh_eq_var = fresh_eq_var in
         let* subst' = unify left_type fresh_eq_var in
         let* subst'' = unify right_type fresh_eq_var in
         let+ final_subst =
           Subst.compose_all [ left_subst; right_subst; subst'; subst'' ]
         in
         final_subst, bool_t
       | And | Or ->
         let* subst' = unify left_type bool_t in
         let* subst'' = unify right_type bool_t in
         let+ final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         final_subst, bool_t)
    | ELetIn (bindings_list, expression) ->
      let rec process_list subst env = function
        | [] -> return (subst, env)
        | elem :: tail ->
          let* identifier, binding_body, env' =
            match elem with
            | BVal (id, body) -> return (id, body, env)
            | BFun (id, _, body) ->
              let* fresh_var = fresh_var in
              let env' = TypeEnv.extend env id (Set.empty (module Int), fresh_var) in
              return (id, body, env')
          in
          let* elem_subst, elem_type = infer_binding env' elem in
          let env'' = TypeEnv.apply elem_subst env' in
          let generalized_type =
            if is_syntactically_value binding_body
            then generalize env'' elem_type
            else TypeEnv.free_vars env'', elem_type
          in
          let* subst' = Subst.compose subst elem_subst in
          let env''' = TypeEnv.extend env'' identifier generalized_type in
          process_list subst' env''' tail
      in
      let* subst, env = process_list Subst.empty env bindings_list in
      let* subst_expr, typ_expr = infer_expr env expression in
      let+ final_subst = Subst.compose subst subst_expr in
      final_subst, typ_expr
    | EApp (left, right) ->
      let* left_subst, left_type = infer_expr env left in
      let* right_subst, right_type = infer_expr (TypeEnv.apply left_subst env) right in
      let* type_variable = fresh_var in
      let* subst' =
        unify (arrow_t right_type type_variable) (Subst.apply right_subst left_type)
      in
      let result_type = Subst.apply subst' type_variable in
      let+ final_subst = Subst.compose_all [ left_subst; right_subst; subst' ] in
      final_subst, result_type
    | EAbs (arg, body) ->
      let* type_variable = fresh_var in
      let env' = TypeEnv.extend env arg (Set.empty (module Int), type_variable) in
      let+ subst, typ = infer_expr env' body in
      let result_type = arrow_t (Subst.apply subst type_variable) typ in
      subst, result_type
    | EIfThenElse (condition, true_branch, false_branch) ->
      let* condition_subst, condition_type = infer_expr env condition in
      let* true_branch_subst, true_branch_type = infer_expr env true_branch in
      let* false_branch_subst, false_branch_type = infer_expr env false_branch in
      let* subst' = unify condition_type bool_t in
      let* subst'' = unify true_branch_type false_branch_type in
      let+ final_subst =
        Subst.compose_all
          [ condition_subst; true_branch_subst; false_branch_subst; subst'; subst'' ]
      in
      final_subst, Subst.apply final_subst true_branch_type
  in
  let infer_program env bindings =
    let rec program_to_expr = function
      | [ BVal (id, _) ] | [ BFun (id, _, _) ] ->
        infer_expr env @@ e_let_in bindings (e_identifier id)
      | _ :: tl -> program_to_expr tl
      | [] -> return (Subst.empty, unit_t)
    in
    program_to_expr bindings
  in
  infer_program
;;

let run_inference program =
  let stdlib =
    List.fold_left
      [ "print_int", (Set.empty (module Int), arrow_t int_t unit_t)
      ; "print_bool", (Set.empty (module Int), arrow_t bool_t unit_t)
      ]
      ~init:TypeEnv.empty
      ~f:(fun acc (id, scheme) -> TypeEnv.extend acc id scheme)
  in
  Result.map (run (infer stdlib program)) ~f:snd
;;

let infer ast =
  match run_inference ast with
  | Ok typ -> print_type typ
  | Error e -> print_type_error e
;;

let parse_and_infer input = infer (Parser.parse_optimistically input)

(* tests *)

let%expect_test "Empty input" =
  parse_and_infer "";
  [%expect {|
    ()
  |}]
;;

let%expect_test "Square function" =
  parse_and_infer "val x = fn x => x * x";
  [%expect {|
    int -> int
  |}]
;;

let%expect_test "Sum function" =
  parse_and_infer "fun sum x = fn x => x * x";
  [%expect {|
    'd -> int -> int
  |}]
;;

let%expect_test "Boolean not function" =
  parse_and_infer "val r = fn x => not x";
  [%expect {|
    bool -> bool
  |}]
;;

let%expect_test "Identity function" =
  parse_and_infer "val id = fn x => x";
  [%expect {|
    'b -> 'b
  |}]
;;

let%expect_test "Multi parameter logical function" =
  parse_and_infer "val x = fn x => fn y => fn z => fn w => x < y orelse z > w";
  [%expect {|
    ''e -> ''e -> ''f -> ''f -> bool
  |}]
;;

let%expect_test "Function definition within let expression" =
  parse_and_infer "val y = let fun f x = x * 2 in (f 5) end";
  [%expect {|
    int
  |}]
;;

let%expect_test "Value bindings in let expression" =
  parse_and_infer "val y = let val x = 5 in x * 2 end";
  [%expect {|
    int
  |}]
;;

let%expect_test "Recursion and factorial calculation" =
  parse_and_infer
    "fun fact n = if n <= 1 then 1 else n * fact (n - 1) \n val fact3 = fact 3";
  [%expect {|
    int
  |}]
;;

let%expect_test "Cubing function" =
  parse_and_infer "val cube = fn x => x * x * x";
  [%expect {|
    int -> int
  |}]
;;

let%expect_test "Two argument comparison function" =
  parse_and_infer "val compare = fn x => fn y => x * y > 10";
  [%expect {|
    int -> int -> bool
  |}]
;;

let%expect_test "Identity function parsing" =
  parse_and_infer "val identity = fn x => x";
  [%expect {|
    'b -> 'b
  |}]
;;

let%expect_test "Four variables" =
  parse_and_infer "val complex_cond = fn x => fn y => fn z => fn w => x < y andalso z > w";
  [%expect {|
    ''e -> ''e -> ''f -> ''f -> bool
  |}]
;;

let%expect_test "Multiplication function within let expression" =
  parse_and_infer "val result = let fun multiply3 x = x * 3 in (multiply3 5) end";
  [%expect {|
    int
  |}]
;;

let%expect_test "Multiplication in let expression" =
  parse_and_infer "val result = let val x = 7 in x * 2 end";
  [%expect {|
  int
  |}]
;;

let%expect_test "Recursive factorial function" =
  parse_and_infer "fun fact n = if n <= 1 then 1 else n * fact (n - 1)";
  [%expect {|
  int -> int
  |}]
;;

let%expect_test "Factorial calculation" =
  parse_and_infer
    "fun factorial n = if n <= 1 then 1 else n * factorial (n - 1) val result =\n\
    \  factorial 3";
  [%expect {|
  int
  |}]
;;

let%expect_test "Function double" =
  parse_and_infer "fun double x = x + x";
  [%expect {|
  int -> int
  |}]
;;

let%expect_test "Function sum" =
  parse_and_infer "fun sum x y = x + y";
  [%expect {|
  int -> int -> int
  |}]
;;

let%expect_test "Let expression with condition and multiplication" =
  parse_and_infer "val r = let val n = (if n <= 1 then 1 else n * (n - 1)) in n * 2 end";
  [%expect {|
  No such variable: n
  |}]
;;

let%expect_test "Equality and sum function" =
  parse_and_infer "val z = fn x => fn y => x = y orelse y + x";
  [%expect
    {| Unification failed: type of the expression is int but expected type was bool |}]
;;
