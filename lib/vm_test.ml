open Ppx_compare_lib.Builtin
open Sexplib.Std

let compare_result = Base.Result.compare
let sexp_of_result = Base.Result.sexp_of_t

type test_case = {
  code : Compiler.bytecode;
  expected : Object.t;
}

type error_case = {
  code : Compiler.bytecode;
  error : Vm.error * int;
}

let test (case : test_case) =
  let vm = Vm.create case.code in
  [%test_result: (Object.t, Vm.error * int) result] ~expect:(Ok case.expected)
    (Vm.run vm)

let test_error (case : error_case) =
  let vm = Vm.create case.code in
  [%test_result: (Object.t, Vm.error * int) result] ~expect:(Error case.error)
    (Vm.run vm)

let%test_unit "run" =
  let cases =
    [
      {
        code = { instructions = Bytes.empty; constants = [] };
        expected = Object.Null;
      };
      {
        code =
          {
            instructions =
              Code.Instructions.assemble
                [ Code.Constant 0; Code.Constant 1; Code.Add ];
            constants = [ Object.Integer 1; Object.Integer 2 ];
          };
        expected = Object.Integer 3;
      };
    ]
  in
  List.iter test cases

let%test_unit "errors" =
  let cases =
    [
      {
        code =
          {
            instructions = Bytes.of_string "\x00\x00\x00\x00\x00";
            constants = [ Object.Integer 5 ];
          };
        error =
          (Vm.Malformed (Code.Instructions.Truncated Code.Opcode.Constant), 3);
      };
      {
        code =
          {
            instructions = Bytes.of_string "\x00\x00\x00\x00\x00";
            constants = [ Object.Integer 5 ];
          };
        error =
          (Vm.Malformed (Code.Instructions.Truncated Code.Opcode.Constant), 3);
      };
      {
        code = { instructions = Bytes.of_string "\x01"; constants = [] };
        error = (Vm.Insufficient_operands (Code.Opcode.Add, 2, 0), 0);
      };
      {
        code =
          {
            instructions = Bytes.of_string "\x00\x00\x00\x01";
            constants = [ Object.Integer 5 ];
          };
        error = (Vm.Insufficient_operands (Code.Opcode.Add, 2, 1), 3);
      };
      {
        code =
          {
            instructions = Bytes.of_string "\x00\x00\x00\x00\x00\x01\x01";
            constants = [ Object.Null; Object.Null ];
          };
        error =
          (Vm.Invalid_infix_operation (Object.Null, Token.Plus, Object.Null), 6);
      };
    ]
  in
  List.iter test_error cases
