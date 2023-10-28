(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type mltype =
  | TBool (** bool type *)
  | TInt (** int_32t or int type in C *)
[@@deriving show { with_path = false }]

type const =
  | VInt of int (** int number *)
  | VBool of bool (** bool value *)
[@@deriving show { with_path = false }]

type varname_part = VarPart of name [@@deriving show { with_path = false }]

type expression =
  | Add of expression * expression (** a + b *)
  | Sub of expression * expression (** a - b *)
  (* | UnaryMin of expression (** -(expr) or -var *)
  | UnaryPlus of expression (** +(expr) or +var *) *)
  | Mul of expression * expression (** a * b *)
  | Div of expression * expression (** a / b *)
  | Mod of expression * expression (** a % b *)
  | And of expression * expression (** a && b *)
  | Or of expression * expression (** a || b *)
  | Equal of expression * expression (** a == b *)
  | NotEqual of expression * expression (** a != b *)
  | Less of expression * expression (** a < b *)
  | LessOrEq of expression * expression (** a <= b *)
  | More of expression * expression (** a > b *)
  | MoreOrEq of expression * expression (** a >= b *)
  | LetIn of name * expression * expression
  | RecLetIn of name * expression * expression
  | IfThenElse of expression * expression * expression
  | Func of name * expression
  | Apply of expression * expression
  | Variable of name (** var *)
  | Value of const (** value (const) *)
[@@deriving show { with_path = false }]

type statement =
  | Define of name * expression (** let var (arg1 arg2...) = <expression> *)
  | RecDefine of name * expression (** let rec var (arg1 arg2...) = <expression> *)

and statements_list = statement list [@@deriving show { with_path = false }]
