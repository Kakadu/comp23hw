(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Performs conversion to anf *)
val anf : Toplevel.llstatements -> Anf.anfstatements
