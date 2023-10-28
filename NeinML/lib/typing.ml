(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Original code was taken from
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ground =
  | TInt
  | TBool
[@@deriving show { with_path = false }]

type ty =
  | Prim of ground
  | Ty_var of binder
  | Arrow of ty * ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = Arrow (l, r)
let v x = Ty_var x
let ( @-> ) = arrow
let int_typ = Prim TInt
let bool_typ = Prim TBool

let rec pp_type fmt typ =
  let open Format in
  match typ with
  | Prim TInt -> fprintf fmt "int"
  | Prim TBool -> fprintf fmt "bool"
  | Ty_var var_num -> fprintf fmt "%s" ("'var" ^ string_of_int var_num)
  | Arrow (left, right) ->
    let format_for_arg =
      match left with
      | Arrow _ -> format_of_string "(%a) -> %a"
      | _ -> format_of_string "%a -> %a"
    in
    fprintf fmt format_for_arg pp_type left pp_type right
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_type l pp_type r
;;

let print_typ_error err =
  let s = Format.asprintf "%a" pp_error err in
  Format.printf "%s\n" s
;;
