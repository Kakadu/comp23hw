(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ty

let stdlib_functions =
  [ "print_int", arrow tyint tyint, 1; "print_bool", arrow tybool tyint, 1 ]
;;
