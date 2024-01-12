(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

(* The code is a modified version of the inferencer taken from here
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TBool
  | TInt
  | TUnit
  | TVar of binder
  | TArrow of ty * ty
[@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

type scheme = S of binder_set * ty

let arrow l r = TArrow (l, r)
let int_typ = TInt
let bool_typ = TBool
let unit_typ = TUnit
let v x = TVar x

let rec pp_typ ppf = function
  | TVar n -> fprintf ppf "%s" @@ "'" ^ Char.escaped (Char.chr (n + 97))
  | TInt -> fprintf ppf "int"
  | TBool -> fprintf ppf "bool"
  | TArrow (l, r) ->
    (match l with
     | TArrow (_, _) -> fprintf ppf "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf ppf "%a -> %a" pp_typ l pp_typ r)
  | TUnit -> fprintf ppf "()"
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_typ typ in
  Format.printf "%s\n" s
;;

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_typ l pp_typ r
;;

let print_typ_err e =
  let s = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" s
;;
