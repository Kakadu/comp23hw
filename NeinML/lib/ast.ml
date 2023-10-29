(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int
  | VBool of bool
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type expression =
  | Add of expression * expression
  | Sub of expression * expression
  (* | UnaryMin of expression
  | UnaryPlus of expression *)
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Less of expression * expression
  | LessOrEq of expression * expression
  | More of expression * expression
  | MoreOrEq of expression * expression
  | LetIn of name * expression * expression
  | RecLetIn of name * expression * expression
  | IfThenElse of expression * expression * expression
  | Func of name * expression
  | Apply of expression * expression
  | Variable of name
  | Value of const
[@@deriving show { with_path = false }]

type statement =
  | Define of name * expression (** let var (arg1 arg2...) = <expression> *)
  | RecDefine of name * expression (** let rec var (arg1 arg2...) = <expression> *)

and statements_list = statement list [@@deriving show { with_path = false }]
