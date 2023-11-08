open Ppx_compare_lib.Builtin
open Sexplib.Std

let compare_result = Base.Result.compare
let sexp_of_result = Base.Result.sexp_of_t

type test_case = {
  disassembled : Code.t list;
  assembled : Code.Instructions.t;
}

type error_case = {
  bytes : bytes;
  error : Code.Instructions.disassembly_error * int;
}

let test (case : test_case) =
  let disasm = Code.Instructions.disassemble case.assembled in
  [%test_result:
    (Code.t list, Code.Instructions.disassembly_error * int) result]
    ~expect:(Ok case.disassembled) disasm;

  let asm = Code.Instructions.assemble case.disassembled in
  [%test_result: Code.Instructions.t] ~expect:case.assembled asm;

  let roundtrip_disasm = Code.Instructions.disassemble asm in
  [%test_result:
    (Code.t list, Code.Instructions.disassembly_error * int) result]
    ~expect:(Ok case.disassembled) roundtrip_disasm;

  if Result.is_ok disasm then
    let roundtrip_asm = Code.Instructions.assemble (Result.get_ok disasm) in
    [%test_result: Code.Instructions.t] ~expect:case.assembled roundtrip_asm

let test_error (case : error_case) =
  [%test_result:
    (Code.t list, Code.Instructions.disassembly_error * int) result]
    ~expect:(Error case.error)
    (Code.Instructions.disassemble case.bytes)

let%test_unit "assembly and disassembly" =
  let cases =
    [
      {
        disassembled = [ Code.Constant 0xabcd ];
        assembled = Bytes.of_string "\x00\xab\xcd";
      };
      { disassembled = [ Code.Add ]; assembled = Bytes.of_string "\x01" };
      {
        disassembled = [ Code.Constant 0xabcd; Code.Constant 0x1234; Code.Add ];
        assembled = Bytes.of_string "\x00\xab\xcd\x00\x12\x34\x01";
      };
    ]
  in
  List.iter test cases

let%test_unit "errors" =
  let cases =
    [
      {
        bytes = Bytes.of_string "\x00\xab\xcd\xff\x12";
        error = (Code.Instructions.Unknown_opcode '\xff', 3);
      };
      {
        bytes = Bytes.of_string "\x00\xab\xcd\x00\x12";
        error = (Code.Instructions.Truncated Code.Opcode.Constant, 3);
      };
    ]
  in
  List.iter test_error cases
