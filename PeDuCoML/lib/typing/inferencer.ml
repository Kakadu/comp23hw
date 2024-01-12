(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing

(* Ground types *)
let int_typ = TGround Int
let bool_typ = TGround Bool
let string_typ = TGround String
let char_typ = TGround Char
(* ------------ *)

(* Smart constructors for types *)
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ
let tvar n = TVar n
(* ---------------------------- *)

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
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
    | Ok value -> f value last
  ;;

  let fail error state = state, Base.Result.fail error
  let return value last = last, Base.Result.return value
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
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run monad = snd (monad 0)
end

type fresh = int

module Type = struct
  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> Base.List.exists typ_list ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TGround _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper acc = function
      | TVar n -> Base.Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple typ_list ->
        Base.List.fold_right
          typ_list
          ~f:(fun t s -> Base.Set.union s (helper empty_set t))
          ~init:acc
      | TList typ -> helper acc typ
      | TGround _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution *)
  val apply : t -> typ -> typ

  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* An association list. In real world replace it by Map *)
  type t = (fresh, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping key value =
    if Type.occurs_in key value then fail `Occurs_check else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.set empty ~key ~data:value
  ;;

  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find n s with
         | None -> tvar n
         | Some x -> x)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TTuple typ_list -> ttuple @@ Base.List.map typ_list ~f:helper
      | TList typ -> tlist @@ helper typ
      | ground -> ground
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when l = r -> return empty
    | TGround _, TGround _ -> fail (`UnificationFailed (l, r))
    | TVar a, TVar b when a = b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match Base.List.zip typ_list_l typ_list_r with
       | Base.List.Or_unequal_lengths.Unequal_lengths -> fail (`UnificationFailed (l, r))
       | Base.List.Or_unequal_lengths.Ok zipped_list ->
         Base.List.fold_right
           zipped_list
           ~f:(fun (x, y) subst ->
             let* head_sub = unify x y in
             let* subst = subst in
             compose head_sub subst)
           ~init:(return empty))
    | TList typ1, TList typ2 -> unify typ1 typ2
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
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold_right f ini set =
    Base.Set.fold_right set ~init:ini ~f:(fun x acc ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  let free_vars = function
    | s, t -> Base.Set.diff (Type.free_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Base.Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.set env ~key:id ~data:scheme
  let empty = Base.Map.empty (module Base.String)

  let free_vars ~name ~is_rec
    : t -> (type_variable_number, Base.Int.comparator_witness) Base.Set.t
    =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key ~data acc ->
        if key <> name && is_rec then Base.Set.union acc (Scheme.free_vars data) else acc)
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> tvar n

let instantiate : scheme -> typ R.t =
  fun (set, t) ->
  VarSet.fold_right
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return @@ Subst.apply s typ)
    (return t)
    set
;;

let generalize name is_rec env typ =
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env ~name ~is_rec) in
  free, typ
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer =
  let rec pattern_helper : TypeEnv.t -> pattern -> (Subst.t * typ) R.t =
    fun env -> function
    | PLiteral literal ->
      (match literal with
       | LInt _ -> return (Subst.empty, int_typ)
       | LString _ -> return (Subst.empty, string_typ)
       | LChar _ -> return (Subst.empty, char_typ)
       | LBool _ -> return (Subst.empty, bool_typ))
    | PIdentifier identifier -> lookup_env identifier env
    | PWildcard ->
      let* fresh = fresh_var in
      return (Subst.empty, fresh)
    | PTuple (first_pattern, second_pattern, other_patterns) ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_typ = pattern_helper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_typ = subst_tuple subst' tail in
          return (final_subst, head_typ :: tail_typ)
      in
      let* final_subst, typ_list =
        subst_tuple Subst.empty (first_pattern :: second_pattern :: other_patterns)
      in
      return (final_subst, ttuple @@ List.map (Subst.apply final_subst) typ_list)
    | PList pattern_list ->
      (match pattern_list with
       | [] ->
         let* fresh_var = fresh_var in
         return (Subst.empty, TList fresh_var)
       | head :: tail ->
         let* head_subst, head_typ = pattern_helper env head in
         let rec substlist subst = function
           | [] -> return subst
           | elem :: tail ->
             let* elem_subst, elem_typ = pattern_helper env elem in
             let* subst' = unify elem_typ head_typ in
             let* subst'' = Subst.compose_all [ subst; elem_subst; subst' ] in
             substlist subst'' tail
         in
         let* final_subst = substlist head_subst tail in
         return (final_subst, tlist @@ Subst.apply final_subst head_typ))
    | PConstructList (operand, list) ->
      let* operand_subst, operand_typ = pattern_helper env operand in
      let* list_subst, list_typ = pattern_helper env list in
      let* subst' = unify (tlist operand_typ) list_typ in
      let* final_subst = Subst.compose_all [ operand_subst; list_subst; subst' ] in
      return (final_subst, Subst.apply subst' list_typ)
  and declaration_helper : TypeEnv.t -> declaration -> (Subst.t * typ) R.t =
    fun env -> function
    | DDeclaration (_, arguments_list, function_body)
    | DRecursiveDeclaration (_, arguments_list, function_body) ->
      (match arguments_list with
       | head :: tail -> helper env @@ efun head tail function_body
       | _ -> helper env function_body)
  and helper : TypeEnv.t -> expression -> (Subst.t * typ) R.t =
    fun env -> function
    | ELiteral literal ->
      (match literal with
       | LInt _ -> return (Subst.empty, int_typ)
       | LString _ -> return (Subst.empty, string_typ)
       | LChar _ -> return (Subst.empty, char_typ)
       | LBool _ -> return (Subst.empty, bool_typ))
    | EIdentifier identifier -> lookup_env identifier env
    | EFun (first_arg, other_args, body) ->
      let* env =
        Base.Set.fold
          (Util.find_identifiers_pattern first_arg)
          ~f:(fun acc id ->
            let* type_variable = fresh_var in
            let* acc = acc in
            return
            @@ TypeEnv.extend acc id (Base.Set.empty (module Base.Int), type_variable))
          ~init:(return env)
      in
      let next_fun =
        match other_args with
        | [] -> body
        | head :: tail -> efun head tail body
      in
      let* head_subst, head_typ = pattern_helper env first_arg in
      let env' = TypeEnv.apply head_subst env in
      let* subst, typ = helper env' next_fun in
      let result_type = tarrow (Subst.apply subst head_typ) typ in
      let* result_subst = Subst.compose head_subst subst in
      return (result_subst, result_type)
    | EUnaryOperation (unary_operator, expression) ->
      (match unary_operator with
       | Minus ->
         let* subst, typ = helper env expression in
         let* subst' = unify typ int_typ in
         let* final_subst = Subst.compose subst' subst in
         return (final_subst, int_typ)
       | Not ->
         let* subst, typ = helper env expression in
         let* subst' = unify typ bool_typ in
         let* final_subst = Subst.compose subst' subst in
         return (final_subst, bool_typ))
    | EBinaryOperation (binary_operator, left_operand, right_operand) ->
      let* subst_left, typ_left = helper env left_operand in
      let* subst_right, typ_right = helper env right_operand in
      (match binary_operator with
       | Add | Sub | Mul | Div ->
         let* subst' = unify typ_left int_typ in
         let* subst'' = unify typ_right int_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; subst_left; subst_right ]
         in
         return (final_subst, int_typ)
       | Eq | NEq | GT | GTE | LT | LTE ->
         let* subst' = unify typ_left typ_right in
         let* final_subst = Subst.compose_all [ subst'; subst_left; subst_right ] in
         return (final_subst, bool_typ)
       | AND | OR ->
         let* subst' = unify typ_left bool_typ in
         let* subst'' = unify typ_right bool_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; subst_left; subst_right ]
         in
         return (final_subst, bool_typ))
    | EApplication (left_operand, right_operand) ->
      let* subst_left, typ_left = helper env left_operand in
      let* subst_right, typ_right = helper (TypeEnv.apply subst_left env) right_operand in
      let* type_variable = fresh_var in
      let* subst' =
        unify (tarrow typ_right type_variable) (Subst.apply subst_right typ_left)
      in
      let result_type = Subst.apply subst' type_variable in
      let* final_subst = Subst.compose_all [ subst_left; subst_right; subst' ] in
      return (final_subst, result_type)
    | EIf (condition, true_branch, false_branch) ->
      let* subst_condition, typ_condition = helper env condition in
      let* subst_true_branch, typ_true_branch = helper env true_branch in
      let* subst_false_branch, typ_false_branch = helper env false_branch in
      let* subst' = unify typ_condition bool_typ in
      let* subst'' = unify typ_false_branch typ_true_branch in
      let* final_subst =
        Subst.compose_all
          [ subst_condition; subst_true_branch; subst_false_branch; subst'; subst'' ]
      in
      return (final_subst, Subst.apply final_subst typ_true_branch)
    | EList list ->
      (match list with
       | [] ->
         let* fresh_var = fresh_var in
         return (Subst.empty, TList fresh_var)
       | head :: tail ->
         let* head_subst, head_typ = helper env head in
         let rec substlist subst = function
           | [] -> return subst
           | elem :: tail ->
             let* elem_subst, elem_typ = helper env elem in
             let* subst' = unify elem_typ head_typ in
             let* subst'' = Subst.compose_all [ subst; elem_subst; subst' ] in
             substlist subst'' tail
         in
         let* final_subst = substlist head_subst tail in
         return (final_subst, tlist @@ Subst.apply final_subst head_typ))
    | ETuple (first_elem, second_elem, other_elems) ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_typ = helper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_typ = subst_tuple subst' tail in
          return (final_subst, head_typ :: tail_typ)
      in
      let* final_subst, typ_list =
        subst_tuple Subst.empty (first_elem :: second_elem :: other_elems)
      in
      return (final_subst, ttuple @@ List.map (Subst.apply final_subst) typ_list)
    | EConstructList (operand, list) ->
      let* operand_subst, operand_typ = helper env operand in
      let* list_subst, list_typ = helper env list in
      let* subst' = unify (tlist operand_typ) list_typ in
      let* final_subst = Subst.compose_all [ operand_subst; list_subst; subst' ] in
      return (final_subst, Subst.apply subst' list_typ)
    | ELetIn (first_declaration, other_declarations, body) ->
      let rec process_list subst env = function
        | [] -> return (subst, env)
        | elem :: tail ->
          let* identifier, is_rec =
            match elem with
            | DDeclaration (id, _, _) -> return (id, false)
            | DRecursiveDeclaration (id, _, _) -> return (id, true)
          in
          let* fresh_var = fresh_var in
          let env' =
            TypeEnv.extend env identifier (Base.Set.empty (module Base.Int), fresh_var)
          in
          let* elem_subst, elem_typ = declaration_helper env' elem in
          let env'' = TypeEnv.apply elem_subst env' in
          let generalized_type = generalize identifier is_rec env'' elem_typ in
          let* subst'' = Subst.compose subst elem_subst in
          process_list subst'' (TypeEnv.extend env'' identifier generalized_type) tail
      in
      let* subst', env' =
        process_list Subst.empty env (first_declaration :: other_declarations)
      in
      let* subst_expr, typ_expr = helper env' body in
      let* final_subst = Subst.compose subst' subst_expr in
      return (final_subst, typ_expr)
    | EMatchWith (matched_expression, first_case, other_cases) ->
      let case_list = first_case :: other_cases in
      let* matched_subst, matched_type = helper env matched_expression in
      let head = Base.List.hd_exn case_list in
      let bootstrap_pattern env case =
        let identifiers = Util.find_identifiers_pattern case in
        Base.Set.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
          let* fresh_var = fresh_var in
          let* acc = acc in
          return @@ TypeEnv.extend acc id (Base.Set.empty (module Base.Int), fresh_var))
      in
      let* env' = bootstrap_pattern env (fst head) in
      let* _, head_expression_type = helper env' (snd head) in
      let* subst' =
        Base.List.fold_right case_list ~init:(return Subst.empty) ~f:(fun case subst ->
          let* env'' = bootstrap_pattern env (fst case) in
          let* case_subst, case_type = pattern_helper env'' (fst case) in
          let* subst'' = unify case_type matched_type in
          let* computation_subst, computation_type = helper env'' (snd case) in
          let* subst''' = unify computation_type head_expression_type in
          let* subst = subst in
          Subst.compose_all [ subst'''; subst''; subst; case_subst; computation_subst ])
      in
      let* final_subst = Subst.compose subst' matched_subst in
      return (subst', Subst.apply final_subst head_expression_type)
  in
  declaration_helper
;;

open Peducoml_stdlib

let check_types (program : declaration list) =
  let rec helper environment = function
    | head :: tail ->
      (match head with
       | DDeclaration (name, _, _) ->
         let* _, function_type = infer environment head in
         let generalized_type = generalize name false environment function_type in
         let* tail = helper (TypeEnv.extend environment name generalized_type) tail in
         return @@ ((name, generalized_type) :: tail)
       | DRecursiveDeclaration (name, _, _) ->
         let* type_variable = fresh_var in
         let env =
           TypeEnv.extend
             environment
             name
             (Base.Set.empty (module Base.Int), type_variable)
         in
         let* subst, typ = infer env head in
         let* subst' = unify (Subst.apply subst type_variable) typ in
         let* final_subst = Subst.compose subst' subst in
         let env = TypeEnv.apply final_subst env in
         let generalized_type =
           generalize name true env (Subst.apply final_subst type_variable)
         in
         let* tail = helper (TypeEnv.extend environment name generalized_type) tail in
         return @@ ((name, generalized_type) :: tail))
    | _ -> return []
  in
  let env =
    Base.List.fold stdlib ~init:TypeEnv.empty ~f:(fun acc (id, scheme) ->
      TypeEnv.extend acc id scheme)
  in
  helper env program
;;

let run_inference expression = Result.map snd (run (infer TypeEnv.empty expression))

let print_result expression =
  match run_inference expression with
  | Ok typ -> print_typ typ
  | Error x -> print_type_error x
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , []
           , ETuple
               ( ELiteral (LString "amount")
               , EIdentifier "x"
               , [ EBinaryOperation (Mul, EIdentifier "x", ELiteral (LInt 3)) ] ) ) ));
  [%expect {|
    int -> string * int * int
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , []
           , EList
               [ EIdentifier "x"
               ; ELiteral (LInt 5)
               ; EBinaryOperation (Mul, EIdentifier "x", ELiteral (LInt 3))
               ] ) ));
  [%expect {|
    int -> int list
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , []
           , EList
               [ EBinaryOperation (Add, EIdentifier "x", EIdentifier "x")
               ; EBinaryOperation (Mul, EIdentifier "x", ELiteral (LInt 3))
               ] ) ));
  [%expect {|
    int -> int list 
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , [ PIdentifier "y"; PIdentifier "z" ]
           , EList [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ] ) ));
  [%expect {|
    'a -> 'a -> 'a -> 'a list
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           (PIdentifier "x", [], EIf (EIdentifier "x", EIdentifier "x", EIdentifier "x"))
       ));
  [%expect {|
    bool -> bool
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , ETuple
           ( EBinaryOperation (Add, ELiteral (LInt 3), ELiteral (LInt 2))
           , EUnaryOperation (Minus, ELiteral (LInt 7))
           , [] ) ));
  [%expect {|
    int * int
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ("main", [], EFun (PIdentifier "x", [], EUnaryOperation (Not, EIdentifier "x"))));
  [%expect {|
    bool -> bool
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ("main", [], EFun (PWildcard, [], EUnaryOperation (Not, ELiteral (LInt 1)))));
  [%expect
    {|
  Unification failed: type of the expression is int but expected type was bool
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , []
           , EBinaryOperation (Add, ELiteral (LInt 2), EIdentifier "x") ) ));
  [%expect {|
  int -> int
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , [ PIdentifier "y" ]
           , EBinaryOperation (Div, EIdentifier "y", EIdentifier "x") ) ));
  [%expect {|
  int -> int -> int
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , [ PIdentifier "y" ]
           , EBinaryOperation (LT, EIdentifier "y", EIdentifier "x") ) ));
  [%expect {|
  'a -> 'a -> bool
  |}]
;;

let%expect_test _ =
  print_result
    (DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , []
           , EBinaryOperation (LT, ELiteral (LString "asdf"), EIdentifier "x") ) ));
  [%expect {|
  string -> bool
  |}]
;;

let%expect_test _ =
  print_result
  @@ DDeclaration
       ( "main"
       , []
       , EApplication
           ( EFun
               ( PIdentifier "x"
               , []
               , EBinaryOperation (LT, ELiteral (LString "asdf"), EIdentifier "x") )
           , ELiteral (LString "asdfg") ) );
  [%expect {|
  bool
  |}]
;;

let%expect_test _ =
  print_result
  @@ DDeclaration
       ( "main"
       , []
       , EApplication
           ( EFun
               ( PIdentifier "x"
               , []
               , EBinaryOperation (LT, ELiteral (LString "asdf"), EIdentifier "x") )
           , ELiteral (LBool true) ) );
  [%expect
    {|
  Unification failed: type of the expression is bool but expected type was string
  |}]
;;

let%expect_test _ =
  print_result
  @@ DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "line"
           , [ PIdentifier "number"; PIdentifier "line_mult_number" ]
           , EMatchWith
               ( EIdentifier "line"
               , ( PConstructList (PIdentifier "head", PIdentifier "tail")
                 , EConstructList
                     ( EBinaryOperation (Mul, EIdentifier "head", EIdentifier "number")
                     , EApplication (EIdentifier "line_mult_number", EIdentifier "tail")
                     ) )
               , [ PWildcard, EList [] ] ) ) );
  [%expect {|
  int list -> int -> (int list -> int list) -> int list
  |}]
;;

let%expect_test _ =
  print_result
  @@ DDeclaration
       ( "main"
       , []
       , EFun
           ( PIdentifier "x"
           , [ PIdentifier "y"; PIdentifier "z" ]
           , EMatchWith
               ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
               , ( PTuple
                     ( PLiteral (LBool true)
                     , PLiteral (LBool true)
                     , [ PLiteral (LBool false) ] )
                 , ELiteral (LBool true) )
               , [ ( PTuple
                       ( PLiteral (LBool true)
                       , PLiteral (LBool false)
                       , [ PLiteral (LBool true) ] )
                   , ELiteral (LBool true) )
                 ; ( PTuple
                       ( PLiteral (LBool false)
                       , PLiteral (LBool true)
                       , [ PLiteral (LBool true) ] )
                   , ELiteral (LBool true) )
                 ; PWildcard, ELiteral (LBool false)
                 ] ) ) );
  [%expect {|
  bool -> bool -> bool -> bool
  |}]
;;

let%expect_test _ =
  print_result
  @@ DDeclaration
       ( "main"
       , []
       , ELetIn
           ( DRecursiveDeclaration
               ( "factorial"
               , [ PIdentifier "n"; PIdentifier "acc" ]
               , EIf
                   ( EBinaryOperation (LTE, EIdentifier "n", ELiteral (LInt 1))
                   , EIdentifier "acc"
                   , EApplication
                       ( EApplication
                           ( EIdentifier "factorial"
                           , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                       , EBinaryOperation (Mul, EIdentifier "acc", EIdentifier "n") ) ) )
           , []
           , EIdentifier "factorial" ) );
  [%expect {|
  int -> int -> int
  |}]
;;
