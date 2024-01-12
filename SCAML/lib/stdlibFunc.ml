(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ty

let print_int_type = TArrow (TInt, TUnit)
let print_bool_type = TArrow (TBool, TUnit)
let stdlib = [ "print_int", print_int_type; "print_bool", print_bool_type ]
