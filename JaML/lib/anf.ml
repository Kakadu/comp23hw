(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmNum of int (** ..., -1, 0, 1, ... *)
  | ImmBool of bool (** true, false *)
  | ImmId of string (** identifiers or variables *)
[@@deriving show { with_path = false }]

type cexpr =
  | CPlus of immexpr * immexpr (** a + b *)
  | CMinus of immexpr * immexpr (** a - b *)
  | CDivide of immexpr * immexpr (** a / b *)
  | CMultiply of immexpr * immexpr (** a * b *)
  | CXor of immexpr * immexpr (** a ^ b *)
  | CAnd of immexpr * immexpr (** a && b *)
  | COr of immexpr * immexpr (** a || b *)
  | CEq of immexpr * immexpr (** a = b *)
  | CNeq of immexpr * immexpr (** a <> b *)
  | CGt of immexpr * immexpr (** a > b *)
  | CLt of immexpr * immexpr (** a < b *)
  | CGte of immexpr * immexpr (** a >= b *)
  | CLte of immexpr * immexpr (** a <= b *)
  | CApp of immexpr * immexpr (** function arg *)
  | CImmExpr of immexpr (** immexpr *)
[@@deriving show { with_path = false }]

type aexpr =
  | ALet of string * cexpr * aexpr (** let name = cexpr in aexpr *)
  | ALetRec of string * cexpr * aexpr (** let rec name = cexpr in aexpr *)
  | AIfThenElse of aexpr * aexpr * aexpr (** if aexpr1 then aexpr2 else aexpe3 *)
  | ACEexpr of cexpr (** cexpr *)
[@@deriving show { with_path = false }]

(** Anf binding type (top level declarations) *)
type anfexpr =
  | AnfLet of string * string list * aexpr (** let name [arg1, arg2, ... arg3] = aexpr *)
  | AnfLetRec of string * string list * aexpr
  (** let rec name [arg1, arg2, ... arg3] = aexpr *)
[@@deriving show { with_path = false }]

(** Statements type (list of top level declarations) *)
type anfstatements = anfexpr list [@@deriving show { with_path = false }]
