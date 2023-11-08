(** Represents a Monkey bytecode instruction opcode. Each constructor should
    match in name a constructor in [t] giving the associated instruction. *)
module Opcode : sig
  type t =
    | Constant
    | Add
  [@@deriving compare, sexp_of]

  val to_string : t -> string
end

(** Represents a Monkey bytecode instruction. *)
type t =
  | Constant of int
      (** A reference to a constant given by an index into a separately managed
          list of constant values. *)
  | Add
[@@deriving compare, sexp_of]

val to_opcode : t -> Opcode.t
(** Maps an instruction to its associated opcode. *)

(** Represents a sequence of Monkey bytecode instructions. *)
module Instructions : sig
  type instruction := t
  type t = bytes [@@deriving compare, sexp_of]

  type disassembly_error =
    | Unknown_opcode of char
    | Truncated of Opcode.t
  [@@deriving compare, sexp_of]

  val disassembly_error_to_string : disassembly_error -> string

  val append : t -> instruction -> t
  (** Appends an instruction. *)

  val assemble : instruction list -> t
  (** Assembles/encodes the given list of instructions. *)

  val disassemble_at : t -> int -> (instruction * int, disassembly_error) result
  (** Disassembles/decodes the instruction at the provided index or 'pc'
      (program counter). *)

  val disassemble : t -> (instruction list, disassembly_error * int) result
  (** Disassembles the entirety of the sequence of instructions. In the event
      of an error, the pc at which the error was encountered is returned as
      well. *)
end
