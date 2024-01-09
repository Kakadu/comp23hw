(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let cnt = ref 0

let cnt_next () =
  cnt := !cnt + 1;
  !cnt
