type t
(** Represents a virtual machine for executing Monkey bytecode. *)

type error =
  | Malformed of Code.Instructions.disassembly_error
  | Stack_overflow
  | Insufficient_operands of Code.Opcode.t * int * int
      (** (operation, expected number, actual number) *)
  | Invalid_infix_operation of Object.t * Token.t * Object.t
  | Invalid_prefix_operation of Token.t * Object.t
[@@deriving compare, sexp_of]

val error_to_string : error -> string

val create : Compiler.bytecode -> t
(** Creates a VM from bytecode. *)

val run : t -> (Object.t, error * int) result
(** Runs the VM, returning the top of the stack if successful (Object.Null if
    the stack is empty); otherwise, an error is returned alongside the PC at
    which the error occured. *)
