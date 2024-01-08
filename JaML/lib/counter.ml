(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module Counter = struct
  let count = ref 0
  let reset n = count := n

  let genid x =
    incr count;
    Format.sprintf "%s%d" x !count
  ;;
end
