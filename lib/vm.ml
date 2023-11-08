open Ppx_compare_lib.Builtin
open Sexplib.Std

let ( let* ) = Result.bind
let stack_size = 2048

type t = {
  instructions : Code.Instructions.t;
  constants : Object.t list;
  stack : Object.t array;
  sp : int; (* the next free stack slot *)
}

type error =
  | Malformed of Code.Instructions.disassembly_error
  | Stack_overflow
  | Insufficient_operands of Code.Opcode.t * int * int
  | Invalid_infix_operation of Object.t * Token.t * Object.t
[@@deriving compare, sexp_of]

let error_to_string = function
  | Malformed err ->
      Printf.sprintf "Malformed instruction: %s"
        (Code.Instructions.disassembly_error_to_string err)
  | Stack_overflow -> "Stack overflow"
  | Insufficient_operands (op, expected, actual) ->
      Printf.sprintf "Insufficient operands for %s; expected %d; got %d"
        (Code.Opcode.to_string op) expected actual
  | Invalid_infix_operation (left, op, right) ->
      Printf.sprintf "%s %s %s" (Object.to_string left) (Token.to_string op)
        (Object.to_string right)

let create (code : Compiler.bytecode) : t =
  {
    instructions = code.instructions;
    constants = code.constants;
    stack = Array.make stack_size Object.Null;
    sp = 0;
  }

let stack_top (vm : t) : Object.t =
  if vm.sp == 0 then
    Object.Null
  else
    Array.get vm.stack (vm.sp - 1)

let push (vm : t) (obj : Object.t) : (t, error) result =
  if vm.sp >= stack_size then
    Error Stack_overflow
  else (
    Array.set vm.stack vm.sp obj;
    let vm = { vm with sp = vm.sp + 1 } in
    Ok vm)

let pop (vm : t) : t * Object.t =
  let () = assert (vm.sp > 0) in
  let top = stack_top vm in
  let vm = { vm with sp = vm.sp - 1 } in
  (vm, top)

let ensure_enough_objects (vm : t) (insn : Code.t) (count : int) :
    (unit, error) result =
  if vm.sp < count then
    Error (Insufficient_operands (Code.to_opcode insn, count, vm.sp))
  else
    Ok ()

let get_constant (vm : t) (index : int) : Object.t = List.nth vm.constants index

let rec run (vm : t) : (Object.t, error * int) result =
  let* vm = run_from vm 0 in
  Ok (stack_top vm)

and run_from (vm : t) (pc : int) : (t, error * int) result =
  if pc >= Bytes.length vm.instructions then
    Ok vm
  else
    let* vm, pc = Result.map_error (fun err -> (err, pc)) (step vm pc) in
    run_from vm pc

and step (vm : t) (pc : int) : (t * int, error) result =
  let* insn, pc =
    Result.map_error
      (fun err -> Malformed err)
      (Code.Instructions.disassemble_at vm.instructions pc)
  in
  match insn with
  | Code.Constant idx ->
      let* vm = push vm (get_constant vm idx) in
      Ok (vm, pc)
  | Code.Add ->
      let* () = ensure_enough_objects vm Code.Add 2 in
      let vm, right = pop vm in
      let vm, left = pop vm in
      let* obj =
        match (left, right) with
        | Object.Integer n, Object.Integer m -> Ok (Object.Integer (n + m))
        | _ -> Error (Invalid_infix_operation (left, Token.Plus, right))
      in
      let* vm = push vm obj in
      Ok (vm, pc)
