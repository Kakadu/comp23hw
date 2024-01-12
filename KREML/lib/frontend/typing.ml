(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ground_type =
  | Int
  | Bool
[@@deriving show { with_path = false }]

type typ =
  | TVar of int (** 'a *)
  | TEqualityVar of int (** ''a *)
  | TArr of typ * typ (** string -> int *)
  | TGround of ground_type (** int *)
  | TUnit
[@@deriving show { with_path = false }]

(* Smart constructors for types *)
let int_t = TGround Int
let bool_t = TGround Bool
let unit_t = TUnit
let var_t n = TVar n
let var_eq_t n = TEqualityVar n
let arrow_t left_type right_type = TArr (left_type, right_type)

type scheme = (int, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of string (** Use of undefined variable *)
  | `UnificationFailed of typ * typ (** Expression of a different type was expected *)
  ]

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TUnit -> fprintf fmt "()"
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | Bool -> fprintf fmt "bool")
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Stdlib.Char.chr (var + 97))
  | TEqualityVar var ->
    fprintf fmt "%s" @@ "''" ^ Char.escaped (Stdlib.Char.chr (var + 97))
;;

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "No such variable: %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is ";
    pp_type fmt t1;
    fprintf fmt " but expected type was ";
    pp_type fmt t2
;;

let print_type typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;
