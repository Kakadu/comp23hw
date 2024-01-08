(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typedtree
open Toplevel

(** Lambda lifting of statements *)
val lambda_lift : tstatements -> llstatements
