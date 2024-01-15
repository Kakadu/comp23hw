module Asm : sig
  open Anf.AnfTypeAst
  open Monad.Result

  type reg = string

  type storage =
    | Stack of int
    | Reg of reg
    | IntConst of int
    | Displacement of string * int
    | Fndef of string

  type instruction =
    | Mov of storage * storage
    | Movzx of storage * storage
    | Add of storage * storage
    | Sub of storage * storage
    | Imul of storage * storage
    | Idiv of storage
    | Call of string
    | Push of storage
    | Pop of storage
    | Int of int
    | Ret
    | Jmp of string
    | Je of string
    | Jne of string
    | Cmp of storage * storage
    | Setge of storage
    | Setg of storage
    | Setle of storage
    | Setl of storage
    | Sete of storage
    | Label of string (* bad *)
    | Cqo

  type fn = { name : string; body : instruction list }

  val compile : anf_program -> fn list t
end

module AsmRenderer : sig
  open Asm

  val render : fn list -> string
end

module AsmOptimizer : sig
  open Asm

  val optimize : fn list -> fn list
end

module Build : sig
  open Asm

  val make_exe : string -> string -> fn list -> unit
end
