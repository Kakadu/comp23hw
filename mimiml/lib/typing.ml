(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type prim_ty =
  | BoolTy
  | IntTy
  | UnitTy
  | StringTy
[@@deriving variants, show { with_path = false }]

type ty =
  | PrimTy of prim_ty
  | VarTy of int
  | ArrowTy of ty * ty
[@@deriving variants, show { with_path = false }]

let prim_ty_eq l r =
  match l, r with
  | BoolTy, BoolTy | IntTy, IntTy | UnitTy, UnitTy | StringTy, StringTy -> true
  | _ -> false
;;

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

(* primitive (base) types *)

let int_typ = primty intty
let bool_typ = primty boolty
let string_typ = primty stringty
let unit_typ = primty unitty
let ( @-> ) = arrowty
let v = varty
