(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Get subs from type. Using for beautiful printing. *)
val get_ty_subs
  :  (int, int, 'a) Base.Map.t
  -> int
  -> Ty.ty
  -> (int, int, 'a) Base.Map.t * int

(** Print type using some substitution for types *)
val pp_ty_with_subs
  :  (int, int, 'a) Base.Map.t option
  -> Format.formatter
  -> Ty.ty
  -> unit

(** Print type *)
val pp_ty : Format.formatter -> Ty.ty -> unit
