let compare_result = Base.Result.compare
let sexp_of_result = Base.Result.sexp_of_t

type test_case = {
  input : Ast.statement list;
  expected : Compiler.bytecode;
}

type error_case = {
  input : Ast.statement list;
  error : Compiler.error;
}

let test (case : test_case) =
  [%test_result: (Compiler.bytecode, Compiler.error) result]
    ~expect:(Ok case.expected)
    (Compiler.compile (Ast.Program case.input))

let test_error (case : error_case) =
  [%test_result: (Compiler.bytecode, Compiler.error) result]
    ~expect:(Error case.error)
    (Compiler.compile (Ast.Program case.input))

let%test_unit "compile" =
  let open Ast in
  let cases =
    [
      {
        input = [ Expression_statement (Integer 100) ];
        expected =
          {
            instructions = Code.Instructions.assemble [ Code.Constant 0 ];
            constants = [ Object.Integer 100 ];
          };
      };
      {
        input =
          [ Expression_statement (Infix (Integer 1, Token.Plus, Integer 2)) ];
        expected =
          {
            instructions =
              Code.Instructions.assemble
                [ Code.Constant 0; Code.Constant 1; Code.Add ];
            constants = [ Object.Integer 1; Object.Integer 2 ];
          };
      };
    ]
  in
  List.iter test cases

let%test_unit "errors" =
  let open Ast in
  let cases =
    [
      {
        input =
          [ Expression_statement (Infix (Integer 5, Token.Bang, Integer 5)) ];
        error = Compiler.Invalid_infix_operator Token.Bang;
      };
    ]
  in
  List.iter test_error cases
