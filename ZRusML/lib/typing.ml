(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type type_variable_number = int
type identifier = string

type ground_type =
  | Int
  | Bool

type typ =
  | TVar of type_variable_number (** 'a *)
  | TArr of typ * typ (** string -> int *)
  | TGround of ground_type (** int *)

(* Ground types *)
let int_typ = TGround Int
let bool_typ = TGround Bool
let var_t n = TVar n
let arrow_t left_type right_type = TArr (left_type, right_type)

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of identifier (** Using undefined variable *)
  | `UnificationFailed of typ * typ (** Unify of typs failed *)
  | `Unreachable (** Bug in parser *)
  | `Not_function
  | `Matching_failed
  ]

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | Bool -> fprintf fmt "bool")
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Stdlib.Char.chr (var + 97))
;;

let print_typ typ = Format.printf "%a\n" pp_type typ

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmt "Occurs check failed."
  | `NoVariable identifier ->
    fprintf fmt "Elaboration failed: Unbound value identifier %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf
      fmt
      "Elaboration failed: Rules disagree on type: Cannot merge %a and %a"
      pp_type
      t1
      pp_type
      t2
  | `Unreachable -> fprintf fmt "Not reachable."
  | `Not_function -> fprintf fmt "Applying not a function"
  | `Matching_failed -> fprintf fmt "Non-exhaustive matching"
;;

let print_type_error error = Format.printf "%a\n" pp_error error
