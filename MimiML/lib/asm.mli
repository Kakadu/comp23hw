(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf

type loc =
  | StackLocation of int
  | Register of string
  | IntConstant of int
  | Displacement of string * int
  | SubDefenition of string

module AsmBuilderTools : sig
  val displacement : string -> int -> loc
  val rbx : loc
  val rci : loc
  val r10 : loc
  val r11 : loc
  val r12 : loc
  val r13 : loc
  val r14 : loc
  val r15 : loc
  val movzx : loc -> loc -> unit
  val imul : loc -> loc -> unit
  val idiv : loc -> unit
  val setle : loc -> unit
  val setl : loc -> unit
  val setge : loc -> unit
  val setg : loc -> unit
  val sete : loc -> unit
end

module SMap : Map.S with type key = string

module AsmMonad : sig
  type context =
    { variable_bindings : loc SMap.t
    ; function_defenitions : string list
    ; function_arities : int SMap.t
    ; rsp_offset : int
    ; counter : int
    }

  type 'a t = context -> 'a * context

  val rsp_offset : context -> int * context
  val add_to_rsp : int -> context -> unit * context
  val sub_to_rsp : int -> context -> unit * context
end

val zip : string list -> string list -> (string * string) list
val compile : (string * string list * aprogram) list -> aprogram -> unit
