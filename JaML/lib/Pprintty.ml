(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ty
open Base

(** Convert number to letter *)
let rec transform_binder binder =
  if 0 <= binder && binder < 26
  then (
    let letter = Stdlib.char_of_int (97 + binder) in
    Stdlib.Format.sprintf "'%c" letter)
  else (
    let div = binder / 26 in
    let remainder = binder % 26 in
    let letter = transform_binder remainder in
    Stdlib.Format.sprintf "%s%d" letter div)
;;

let add map k v = if Map.mem map k then map else Map.add_exn map ~key:k ~data:v

let get_ty_subs =
  let rec helper subs index = function
    | Tyvar n -> add subs n index, index + 1
    | Prim _ -> subs, index
    | Arrow (l, r) ->
      let subs, index = helper subs index l in
      helper subs index r
  in
  helper
;;

(** Print type *)
let pp_ty_with_subs subs =
  let open Stdlib.Format in
  let rec helper ppf = function
    | Tyvar n ->
      (match subs with
       | Some subs ->
         (match Map.find subs n with
          | Some v ->
            let letter = transform_binder v in
            fprintf ppf "%s" letter
          | None -> fprintf ppf "'_%d" n)
       | None -> fprintf ppf "'_%d" n)
    | Prim s -> pp_print_string ppf @@ show_prim s
    | Arrow (l, r) -> fprintf ppf "(%a -> %a)" helper l helper r
  in
  helper
;;

let pp_ty ppf ty =
  let subs, _ = get_ty_subs (Base.Map.empty (module Base.Int)) 0 ty in
  pp_ty_with_subs (Some subs) ppf ty
;;
