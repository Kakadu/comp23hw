(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VariableNameGeneratorMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : string -> string t
  val run : 'a t -> 'a
  val monad_fold : init:'a -> f:('a -> 'b -> 'a t) -> 'b list -> 'a t
end

(** The monad is actually a large result-state monad, which encapsulates some of
    complexities of generating an assembler for an x86-64 machine. The monad works
    accoring to SYSTEM V ABI calling conventions.

    See https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf 3.2 Function Calling Sequence
    for reference.

    What parts of calling convenction are handeled:

    + First six arguments are passed via registers and others arguments are pushed on stack.
    + Monad do care about stack alignment by 16 before making function call.
    + Monad do save caller save registers except RAX. Monad user has to save it excplicetly himself.

    What parts of calling convenciton are not handled:

    + Callee save registers do not save.

    What benefits are offered by the monad:

    + Monad encapsulate working with stack. User does not have access to RBS, RSP registers.
      Also he does not have access to push and pop commands. All stack work is done by monad.
    + Monad introduce user abstraction -- variable. It can be saved using the command: [let* cmd = save_return_value my_var]
    + Monad takes care of the saved variables and prevents the user from accidentally overwriting them.
    + Monad prohibits the user from exiting a function twice and creating a function twice in a row without
      using doing function exit. *)
module CompilerMonad : sig
  open Format

  (** The type represents registers: rax, rbx, rcx, ... *)
  type register

  (** The type represents assembly labels *)
  type label

  (** The type represent all values from assembly: labels, memory, registers *)
  type asm_value

  (** Represent assembly mnemonic with operands *)
  type command

  (** Returned from monad error *)
  type error

  type ('a, 'error) t

  (** Print error *)
  val pp_error : formatter -> error -> unit

  (* Standard monad functions *)

  val return : 'a -> ('a, 'error) t
  val fail : 'error -> ('a, 'error) t
  val bind : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val ( let* ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val fresh : string -> (string, 'error) t
  val monad_map : 'a list -> f:('a -> ('b, 'c) t) -> ('b list, 'c) t
  val run : ('a, 'error) t -> ('a, 'error) result

  (* Registers *)

  (** Registers available for monad user *)
  module R : sig
    val rax : register
    val rbx : register
    val rcx : register
    val rdx : register
    val rsi : register
    val rdi : register
    val r8 : register
    val r9 : register
    val r10 : register
    val r11 : register
    val r12 : register
    val r13 : register
    val r14 : register
    val r15 : register
  end

  (** Just empty command which does nothing.*)
  val empty_cmd : command

  (** Create mov mnemonic with two operands *)
  val mov_cmd
    :  ?comment:string option
    -> register
    -> asm_value
    -> (command list, 'error) t

  (** Create add mnemonic with two operands *)
  val add_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create sum mnemonic with two operands *)
  val sub_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create idiv mnemonic with two operands *)
  val idiv_cmd : ?comment:string option -> asm_value -> (command list, error) t

  (** Create imul mnemonic with two operands *)
  val imul_cmd
    :  ?comment:string option
    -> register
    -> asm_value
    -> (command list, error) t

  (** Create xor mnemonic with two operands *)
  val xor_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create and mnemonic with two operands *)
  val and_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create or mnemonic with two operands *)
  val or_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create cmp mnemonic with two operands *)
  val cmp_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (** Create setemnemonic with one operand *)
  val sete_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create setne mnemonic with one operand *)
  val setne_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create setg mnemonic with one operand *)
  val setg_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create setl mnemonic with one operand *)
  val setl_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create setge mnemonic with one operand *)
  val setge_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create setle mnemonic with one operand *)
  val setle_cmd : ?comment:string option -> register -> (command list, error) t

  (** Create jmp mnemonic with one operand *)
  val jmp_cmd : ?comment:string option -> label -> (command, error) t

  (** Create jne mnemonic with one operand *)
  val jne_cmd : ?comment:string option -> label -> (command, error) t

  (** Create je mnemonic with one operand *)
  val je_cmd : ?comment:string option -> label -> (command, error) t

  (* Command without arguments *)
  val cqo_cmd : ?comment:string option -> (command, error) t

  (** Create assembler comment *)
  val make_comment : string -> (command, 'error) t

  (** Create global declaration *)
  val add_global : label -> (command, error) t

  (** Create extern declaration. All extern functions must be declared using THIS command. *)
  val add_extern : label -> (command, error) t

  (** Convert string to label type. And add the label to the environment *)
  val make_label : string -> (label, 'error) t

  (** Convert bool value to asm_value *)
  val make_bool : bool -> (asm_value, error) t

  (** Convert integer value to asm_value *)
  val make_integer : int -> (asm_value, error) t

  (** Convert label to asm_value *)
  val label_to_asmvalue : label -> asm_value

  (** Convert register to asm_value *)
  val reg_to_asmvalue : register -> asm_value

  (** Used to get variable or saved label *)
  val get_data_from_environment : string -> (command * asm_value, error) t

  (** Create command form label.
      For example main label can be declared using the command. *)
  val make_label_cmd : label -> (command, error) t

  (** Takes function and her argument and make call. Before call:
      + All arguments are passed according to SYSTEM V ABI calling convenction. *)
  val make_func_call : asm_value -> asm_value list -> (command list, error) t

  (** Make function preambula: save old rsp register, bind all arguments to the stack/registers. *)
  val make_func_preambula : string -> string list -> (command list, error) t

  (** Make exit from the function *)
  val make_func_ret : (command list, error) t

  (** The function can be used to bind any value stored in rax to the variable.
      After binding you don't have to be scared of lossing variable by writing something at rax.
      Monad will protected binding variable from owerriting and if necessary will place it on stack from register.
      Binded vatiable can be obtained using [get_data_from_environment] *)
  val save_return_value : string -> (command, error) t

  (** Converts all commands to string *)
  val commands_to_str : command list -> string

  (** Print commands *)
  val pp_commands : formatter -> command list -> unit

  (** Save commands to specified file *)
  val commands_to_file : string -> command list -> unit

  (** Should be called before 'then' or 'else' branch *)
  val start_branch : (unit, 'a) t

  (** Should be called after 'then' or 'else' branch *)
  val end_branch : (unit, 'a) t
end
