open Ppx_compare_lib.Builtin
open Sexplib.Std

let compare_bytes = Bytes.compare
let sexp_of_bytes b = sexp_of_string (Bytes.to_string b)

module Opcode = struct
  type t =
    | Constant
    | Add
  [@@deriving compare, enum, sexp_of, show { with_path = false }]

  let to_string = show

  (* The opcode space is byte-width. *)
  let _ = assert (max < 256)
  let _ = assert (min == 0)
  let to_value (opcode : t) = char_of_int (to_enum opcode)
  let of_value (ch : char) = of_enum (int_of_char ch)
end

type t =
  | Constant of int
  | Add
[@@deriving compare, sexp_of]

let to_opcode = function Constant _ -> Opcode.Constant | Add -> Opcode.Add

let to_bytes (insn : t) : bytes =
  let operand_width, write_operands =
    match insn with
    | Add -> (0, None)
    | Constant u16 -> (2, Some (fun b -> Bytes.set_uint16_be b 1 u16))
  in
  (* opcode + operands *)
  let b = Bytes.make (1 + operand_width) '\x00' in
  Bytes.set b 0 (Opcode.to_value (to_opcode insn));
  if operand_width > 0 then
    (Option.get write_operands) b;
  b

module Instructions = struct
  type instruction = t
  type t = bytes [@@deriving compare, sexp_of]

  type disassembly_error =
    | Unknown_opcode of char
    | Truncated of Opcode.t
  [@@deriving compare, sexp_of]

  let disassembly_error_to_string = function
    | Unknown_opcode ch -> Printf.sprintf "Unknown opcode: %c" ch
    | Truncated op -> Printf.sprintf "Truncated %s" (Opcode.show op)

  let append (insns : t) (insn : instruction) : t =
    let insn = to_bytes insn in
    let prev_length = Bytes.length insns in
    let insns = Bytes.extend insns 0 (Bytes.length insn) in
    Bytes.blit insn 0 insns prev_length (Bytes.length insn);
    insns

  let assemble (insns : instruction list) : t =
    let rec append_to assembled insns =
      match insns with
      | [] -> assembled
      | first :: rest -> append_to (append assembled first) rest
    in
    append_to Bytes.empty insns

  let disassemble_at (insns : t) (pc : int) :
      (instruction * int, disassembly_error) result =
    let ch = Bytes.get insns pc in
    match Opcode.of_value ch with
    | None -> Error (Unknown_opcode ch)
    | Some op ->
        let read_u16_operand () = Bytes.get_uint16_be insns (pc + 1) in
        let lazy_insn, operand_width =
          match op with
          | Opcode.Constant -> (lazy (Constant (read_u16_operand ())), 2)
          | Opcode.Add -> (lazy Add, 0)
        in
        let insn_end = pc + operand_width in
        if insn_end < Bytes.length insns then
          Ok (Lazy.force lazy_insn, insn_end + 1)
        else
          Error (Truncated op)

  let rec disassemble (insns : t) :
      (instruction list, disassembly_error * int) result =
    disassemble_from insns 0

  and disassemble_from (insns : t) (pc : int) :
      (instruction list, disassembly_error * int) result =
    let ( let* ) = Result.bind in

    if pc >= Bytes.length insns then
      Ok []
    else
      let* insn, pc =
        (Result.map_error (fun err -> (err, pc))) (disassemble_at insns pc)
      in
      let* insns = disassemble_from insns pc in
      Ok (insn :: insns)
end
