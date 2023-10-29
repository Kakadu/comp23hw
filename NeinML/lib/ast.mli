(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | VInt of int (** int number *)
  | VBool of bool (** bool value *)
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type expression =
  | Add of expression * expression (** a + b *)
  | Sub of expression * expression (** a - b *)
  | Mul of expression * expression (** a * b *)
  | Div of expression * expression (** a / b *)
  | Mod of expression * expression (** a % b *)
  | And of expression * expression (** a && b *)
  | Or of expression * expression (** a || b *)
  | Equal of expression * expression (** a = b *)
  | NotEqual of expression * expression (** a <> b *)
  | Less of expression * expression (** a < b *)
  | LessOrEq of expression * expression (** a <= b *)
  | More of expression * expression (** a > b *)
  | MoreOrEq of expression * expression (** a >= b *)
  | LetIn of name * expression * expression (** let func = ... in ... *)
  | RecLetIn of name * expression * expression (** let rec func = ... in ... *)
  | IfThenElse of expression * expression * expression
      (** if condition then expr1 else expr2 *)
  | Func of name * expression (** ast expression for defining functions with currying *)
  | Apply of expression * expression (** func arg1 arg2 ... *)
  | Variable of name (** var *)
  | Value of const (** value (const) *)
[@@deriving show { with_path = false }]

type statement =
  | Define of name * expression (** let var (arg1 arg2...) = <expression> *)
  | RecDefine of name * expression (** let rec var (arg1 arg2...) = <expression> *)

and statements_list = statement list [@@deriving show { with_path = false }]
