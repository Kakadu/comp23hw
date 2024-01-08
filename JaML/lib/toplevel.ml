(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ty
open Ast

(** Almost the same ast as Typedtree.
    Changes: Arguments in let and let rec are represented by a list of arguments.
    There are no TFuns, so anonymous functions are not expressible. *)

type arg = Arg of string * ty (** Typed function argument *)
[@@deriving show { with_path = false }]

(** Typed expession type *)
type llexpr =
  | LConst of const * ty (** Typed expression for the constant *)
  | LVar of string * ty (** Typed expression for the variables *)
  | LBinop of bin_op * llexpr * llexpr * ty
  (** Typed expression for the binary operations *)
  | LApp of llexpr * llexpr * ty
  (** Typed expression for the function application to the arguments *)
  | LIfThenElse of llexpr * llexpr * llexpr * ty
  (** Typed expression for condition statement *)
  | LLetIn of string * llexpr * llexpr * ty (** Typed expression for let in declaration *)
  | LLetRecIn of string * llexpr * llexpr * ty
  (** Typed expression for let rec in declaration *)
[@@deriving show { with_path = false }]

(** Typed binding type *)
type llbinding =
  | LLet of string * arg list * llexpr * ty (** Typed expression for let declaration *)
  | LLetRec of string * arg list * llexpr * ty
  (** Typed expression for let rec declaration *)
[@@deriving show { with_path = false }]

(** Typed statements type *)
type llstatements = llbinding list [@@deriving show { with_path = false }]
