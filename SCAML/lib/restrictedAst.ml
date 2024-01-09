(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

type immexpr =
  | ImmNum of int (** 777 *)
  | ImmId of id (** x *)
  | ImmBool of bool (** true *)
  | ImmUnit (** () *)

type pexpr =
  | PImmExpr of immexpr (** 1 *)
  | PImmWild (** _ *)

type cexpr =
  | CBinOp of bin_op * immexpr * immexpr (** 1 + 1 *)
  | CApp of immexpr * immexpr (** f x *)
  | CImmExpr of immexpr (** 1 *)
  | CIf of immexpr * aexpr * aexpr (** if true then (then_expr) else (else_expr) *)

and aexpr =
  | ALetIn of id * cexpr * aexpr (** let x = 5 in x *)
  | ACExpr of cexpr (** 1 + 1 *)

type bexpr = ALet of bool * id * pexpr list * aexpr (** let [rec] f x = ae *)

(** be1 ;; be2 ;; ... ;; ben;; *)
type prexpr = bexpr list
