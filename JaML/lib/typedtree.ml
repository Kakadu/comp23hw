(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Ty

type arg = Arg of string * ty (** Typed function argument *)

(** Typed expession type *)
type texpr =
  | TConst of const * ty (** Typed expression for the constant *)
  | TVar of string * ty (** Typed expression for the variables *)
  | TBinop of bin_op * texpr * texpr * ty
  (** Typed expression for the binary operations *)
  | TApp of texpr * texpr * ty
  (** Typed expression for the function application to the arguments *)
  | TIfThenElse of texpr * texpr * texpr * ty
  (** Typed expression for condition statement *)
  | TLetIn of string * texpr * texpr * ty (** Typed expression for let in declaration *)
  | TLetRecIn of string * texpr * texpr * ty
  (** Typed expression for let rec in declaration *)
  | TFun of arg * texpr * ty (** Typed expression for function *)

(** Typed binding type *)
type tbinding =
  | TLet of string * texpr * ty (** Typed expression for let declaration *)
  | TLetRec of string * texpr * ty (** Typed expression for let rec declaration *)

(** Typed statements type *)
type tstatements = tbinding list

(** Typed texpr constructors *)

let tconst c t = TConst (c, t)
let tvar s t = TVar (s, t)
let tbinop b e1 e2 t = TBinop (b, e1, e2, t)
let tapp f a t = TApp (f, a, t)
let tifthenelse i t e ty = TIfThenElse (i, t, e, ty)
let tletin s a b t = TLetIn (s, a, b, t)
let tletrecin s a b t = TLetRecIn (s, a, b, t)
let tfun arg_name arg_type a t = TFun (Arg (arg_name, arg_type), a, t)

(** tbinding constructors *)

let tlet n e t = TLet (n, e, t)
let tletrec n e t = TLetRec (n, e, t)
