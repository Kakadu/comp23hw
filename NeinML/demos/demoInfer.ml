(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  let s = Stdio.In_channel.input_all Caml.stdin in
  Neinml_lib.Inferencer.parse_and_infer s
;;
