open Ppx_compare_lib.Builtin
open Sexplib.Std

type ('input, 'expected) case = {
  input : 'input;
  expected : 'expected;
}

type expression_case = (Ast.expression, Object.t) case
type statements_case = (Ast.statement list, Object.t) case
type error_case = (Ast.expression, string) case

let test_node (input : Ast.node) (expected : Object.t) =
  match Evaluator.eval input Evaluator.Environment.create with
  | Error err -> failwith err
  | Ok (actual, _) -> [%test_result: Object.t] ~expect:expected actual

let test_expression (case : expression_case) =
  test_node (Ast.Expression case.input) case.expected

let test_statements (case : statements_case) =
  test_node (Ast.Program case.input) case.expected

let test_error (case : error_case) =
  match
    Evaluator.eval (Ast.Expression case.input) Evaluator.Environment.create
  with
  | Error actual -> [%test_result: string] ~expect:case.expected actual
  | Ok (value, _) ->
      failwith
        (Printf.sprintf "expected error %S; got %s" case.expected
           (Object.to_string value))

let%test_unit "integer expressions" =
  let open Ast in
  let cases =
    [
      (* 5 *)
      { input = Integer 5; expected = Object.Integer 5 };
      (* 10 *)
      { input = Integer 10; expected = Object.Integer 10 };
      (* -5 *)
      {
        input = Prefix (Token.Minus, Integer 5);
        expected = Object.Integer (-5);
      };
      (* -10 *)
      {
        input = Prefix (Token.Minus, Integer 10);
        expected = Object.Integer (-10);
      };
      (* 5 + 5 + 5 + 5 - 10 *)
      {
        input =
          Infix
            ( Infix
                ( Infix
                    ( Infix (Integer 5, Token.Plus, Integer 5),
                      Token.Plus,
                      Integer 5 ),
                  Token.Plus,
                  Integer 5 ),
              Token.Minus,
              Integer 10 );
        expected = Object.Integer 10;
      };
      (* 2 * 2 * 2 * 2 * 2 *)
      {
        input =
          Infix
            ( Infix
                ( Infix
                    ( Infix (Integer 2, Token.Asterisk, Integer 2),
                      Token.Asterisk,
                      Integer 2 ),
                  Token.Asterisk,
                  Integer 2 ),
              Token.Asterisk,
              Integer 2 );
        expected = Object.Integer 32;
      };
      (* -50 + 100 + -50 *)
      {
        input =
          Infix
            ( Infix (Prefix (Token.Minus, Integer 50), Token.Plus, Integer 100),
              Token.Plus,
              Prefix (Token.Minus, Integer 50) );
        expected = Object.Integer 0;
      };
      (* 5 * 2 + 10 *)
      {
        input =
          Infix
            ( Infix (Integer 5, Token.Asterisk, Integer 2),
              Token.Plus,
              Integer 10 );
        expected = Object.Integer 20;
      };
      (* 5 + 2 * 10 *)
      {
        input =
          Infix
            ( Integer 5,
              Token.Plus,
              Infix (Integer 2, Token.Asterisk, Integer 10) );
        expected = Object.Integer 25;
      };
      (* 20 + 2 * -10 *)
      {
        input =
          Infix
            ( Integer 20,
              Token.Plus,
              Infix (Integer 2, Token.Asterisk, Prefix (Token.Minus, Integer 10))
            );
        expected = Object.Integer 0;
      };
      (* 50 /2 * 2 + 10 *)
      {
        input =
          Infix
            ( Infix
                ( Infix (Integer 50, Token.Slash, Integer 2),
                  Token.Asterisk,
                  Integer 2 ),
              Token.Plus,
              Integer 10 );
        expected = Object.Integer 60;
      };
      (* 2 * (5 + 10) *)
      {
        input =
          Infix
            ( Integer 2,
              Token.Asterisk,
              Infix (Integer 5, Token.Plus, Integer 10) );
        expected = Object.Integer 30;
      };
      (* 3 * 3 * 3 + 10 *)
      {
        input =
          Infix
            ( Infix
                ( Infix (Integer 3, Token.Asterisk, Integer 3),
                  Token.Asterisk,
                  Integer 3 ),
              Token.Plus,
              Integer 10 );
        expected = Object.Integer 37;
      };
      (* 3 * (3 * 3) + 10 *)
      {
        input =
          Infix
            ( Infix
                ( Integer 3,
                  Token.Asterisk,
                  Infix (Integer 3, Token.Asterisk, Integer 3) ),
              Token.Plus,
              Integer 10 );
        expected = Object.Integer 37;
      };
      (* (5 + 10 * 2 + 15) - 10*)
      {
        input =
          Infix
            ( Infix
                ( Infix
                    ( Infix
                        ( Integer 5,
                          Token.Plus,
                          Infix (Integer 10, Token.Asterisk, Integer 2) ),
                      Token.Plus,
                      Infix (Integer 15, Token.Slash, Integer 3) ),
                  Token.Asterisk,
                  Integer 2 ),
              Token.Plus,
              Prefix (Token.Minus, Integer 10) );
        expected = Object.Integer 50;
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "booleans expressions" =
  let open Ast in
  let cases =
    [
      (* true *)
      { input = Boolean true; expected = Object.Boolean true };
      (* false *)
      { input = Boolean false; expected = Object.Boolean false };
      (* !true *)
      {
        input = Prefix (Token.Bang, Boolean true);
        expected = Object.Boolean false;
      };
      (* !false *)
      {
        input = Prefix (Token.Bang, Boolean false);
        expected = Object.Boolean true;
      };
      (* !5 *)
      {
        input = Prefix (Token.Bang, Integer 5);
        expected = Object.Boolean false;
      };
      (* !!true *)
      {
        input = Prefix (Token.Bang, Prefix (Token.Bang, Boolean true));
        expected = Object.Boolean true;
      };
      (* !!false *)
      {
        input = Prefix (Token.Bang, Prefix (Token.Bang, Boolean false));
        expected = Object.Boolean false;
      };
      (* !!5 *)
      {
        input = Prefix (Token.Bang, Prefix (Token.Bang, Integer 5));
        expected = Object.Boolean true;
      };
      (* 1 < 2 *)
      {
        input = Infix (Integer 1, Token.Less_than, Integer 2);
        expected = Object.Boolean true;
      };
      (* 1 > 2 *)
      {
        input = Infix (Integer 1, Token.Greater_than, Integer 2);
        expected = Object.Boolean false;
      };
      (* 1 < 1 *)
      {
        input = Infix (Integer 1, Token.Less_than, Integer 1);
        expected = Object.Boolean false;
      };
      (* 1 > 1 *)
      {
        input = Infix (Integer 1, Token.Greater_than, Integer 1);
        expected = Object.Boolean false;
      };
      (* 1 == 1 *)
      {
        input = Infix (Integer 1, Token.Equal, Integer 1);
        expected = Object.Boolean true;
      };
      (* 1 != 1 *)
      {
        input = Infix (Integer 1, Token.Not_equal, Integer 1);
        expected = Object.Boolean false;
      };
      (* 1 == 2 *)
      {
        input = Infix (Integer 1, Token.Equal, Integer 2);
        expected = Object.Boolean false;
      };
      (* 1 != 2 *)
      {
        input = Infix (Integer 1, Token.Not_equal, Integer 2);
        expected = Object.Boolean true;
      };
      (* true == true *)
      {
        input = Infix (Boolean true, Token.Equal, Boolean true);
        expected = Object.Boolean true;
      };
      (* false == false *)
      {
        input = Infix (Boolean false, Token.Equal, Boolean false);
        expected = Object.Boolean true;
      };
      (* true == false *)
      {
        input = Infix (Boolean true, Token.Equal, Boolean false);
        expected = Object.Boolean false;
      };
      (* true != false *)
      {
        input = Infix (Boolean true, Token.Not_equal, Boolean false);
        expected = Object.Boolean true;
      };
      (* (1 < 2) == true *)
      {
        input =
          Infix
            ( Infix (Integer 1, Token.Less_than, Integer 2),
              Token.Equal,
              Boolean true );
        expected = Object.Boolean true;
      };
      (* (1 < 2) == false *)
      {
        input =
          Infix
            ( Infix (Integer 1, Token.Less_than, Integer 2),
              Token.Equal,
              Boolean false );
        expected = Object.Boolean false;
      };
      (* (1 > 2) == true *)
      {
        input =
          Infix
            ( Infix (Integer 1, Token.Greater_than, Integer 2),
              Token.Equal,
              Boolean true );
        expected = Object.Boolean false;
      };
      (* (1 > 2) == false *)
      {
        input =
          Infix
            ( Infix (Integer 1, Token.Greater_than, Integer 2),
              Token.Equal,
              Boolean false );
        expected = Object.Boolean true;
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "if-else expressions" =
  let open Ast in
  let cases =
    [
      (* if (true) { 10 } *)
      {
        input =
          Ast.If (Boolean true, [ Expression_statement (Integer 10) ], None);
        expected = Object.Integer 10;
      };
      (* if (false) { 10 } *)
      {
        input =
          Ast.If (Boolean false, [ Expression_statement (Integer 10) ], None);
        expected = Object.Null;
      };
      (* if (1) { 10 } *)
      {
        input = Ast.If (Integer 1, [ Expression_statement (Integer 10) ], None);
        expected = Object.Integer 10;
      };
      (* if (1 < 2) { 10 } *)
      {
        input =
          Ast.If
            ( Infix (Integer 1, Token.Less_than, Integer 2),
              [ Expression_statement (Integer 10) ],
              None );
        expected = Object.Integer 10;
      };
      (* if (1 > 2) { 10 } *)
      {
        input =
          Ast.If
            ( Infix (Integer 1, Token.Greater_than, Integer 2),
              [ Expression_statement (Integer 10) ],
              None );
        expected = Object.Null;
      };
      (* if (1 > 2) { 10 } else { 20 } *)
      {
        input =
          Ast.If
            ( Infix (Integer 1, Token.Greater_than, Integer 2),
              [ Expression_statement (Integer 10) ],
              Some [ Expression_statement (Integer 20) ] );
        expected = Object.Integer 20;
      };
      (* if (1 < 2) { 10 } else { 20 } *)
      {
        input =
          Ast.If
            ( Infix (Integer 1, Token.Less_than, Integer 2),
              [ Expression_statement (Integer 10) ],
              Some [ Expression_statement (Integer 20) ] );
        expected = Object.Integer 10;
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "return statements" =
  let open Ast in
  let cases =
    [
      (* return 10; *)
      { input = [ Return (Integer 10) ]; expected = Object.Integer 10 };
      (* return 10; 9; *)
      {
        input = [ Return (Integer 10); Expression_statement (Integer 9) ];
        expected = Object.Integer 10;
      };
      (* return 2 * 5; 9; *)
      {
        input =
          [
            Return (Infix (Integer 2, Token.Asterisk, Integer 5));
            Expression_statement (Integer 9);
          ];
        expected = Object.Integer 10;
      };
      (* 9; return 2 * 5; 9; *)
      {
        input =
          [
            Expression_statement (Integer 9);
            Return (Infix (Integer 2, Token.Asterisk, Integer 5));
            Expression_statement (Integer 9);
          ];
        expected = Object.Integer 10;
      };
    ]
  in
  List.iter test_statements cases

let%test_unit "error handling" =
  let open Ast in
  let cases =
    [
      (* 5 + true *)
      {
        input = Infix (Integer 5, Token.Plus, Boolean true);
        expected = "invalid operation: INTEGER + BOOLEAN";
      };
      (* -true *)
      {
        input = Prefix (Token.Minus, Boolean true);
        expected = "invalid expression: -BOOLEAN";
      };
      (* true + true *)
      {
        input = Infix (Boolean true, Token.Plus, Boolean true);
        expected = "invalid operation: BOOLEAN + BOOLEAN";
      };
      (* if (10 > 1) { true + false } *)
      {
        input =
          If
            ( Infix (Integer 10, Token.Greater_than, Integer 1),
              [
                Expression_statement
                  (Infix (Boolean true, Token.Plus, Boolean false));
              ],
              None );
        expected = "invalid operation: BOOLEAN + BOOLEAN";
      };
    ]
  in
  List.iter test_error cases

let%test_unit "let statements" =
  let open Ast in
  let cases =
    [
      (* let a = 5; a; *)
      {
        input = [ Let ("a", Integer 5); Expression_statement (Identifier "a") ];
        expected = Object.Integer 5;
      };
      (* let a = 5 * 5; a; *)
      {
        input =
          [
            Let ("a", Infix (Integer 5, Token.Asterisk, Integer 5));
            Expression_statement (Identifier "a");
          ];
        expected = Object.Integer 25;
      };
      (* let a = 5; let b = a; b; *)
      {
        input =
          [
            Let ("a", Integer 5);
            Let ("b", Identifier "a");
            Expression_statement (Identifier "b");
          ];
        expected = Object.Integer 5;
      };
      (* let a = 5; let b = a; let c = a + b + 5; c; *)
      {
        input =
          [
            Let ("a", Integer 5);
            Let ("b", Identifier "a");
            Let
              ( "c",
                Infix
                  ( Infix (Identifier "a", Token.Plus, Identifier "b"),
                    Token.Plus,
                    Integer 5 ) );
            Expression_statement (Identifier "c");
          ];
        expected = Object.Integer 15;
      };
    ]
  in
  List.iter test_statements cases

let%test_unit "function application" =
  let open Ast in
  let cases =
    [
      (* let identity = fn(x) { x; }; identity(5); *)
      {
        input =
          [
            Let
              ( "identity",
                Function ([ "x" ], [ Expression_statement (Identifier "x") ]) );
            Expression_statement (Call (Identifier "identity", [ Integer 5 ]));
          ];
        expected = Object.Integer 5;
      };
      (* let identity = fn(x) { return x; }; identity(5); *)
      {
        input =
          [
            Let ("identity", Function ([ "x" ], [ Return (Identifier "x") ]));
            Expression_statement (Call (Identifier "identity", [ Integer 5 ]));
          ];
        expected = Object.Integer 5;
      };
      (* let double = fn(x) { x * 2; }; double(5); *)
      {
        input =
          [
            Let
              ( "double",
                Function
                  ( [ "x" ],
                    [
                      Expression_statement
                        (Infix (Identifier "x", Token.Asterisk, Integer 2));
                    ] ) );
            Expression_statement (Call (Identifier "double", [ Integer 5 ]));
          ];
        expected = Object.Integer 10;
      };
      (* let add = fn(x, y) { x + y; }; add(5, 5); *)
      {
        input =
          [
            Let
              ( "add",
                Function
                  ( [ "x"; "y" ],
                    [
                      Expression_statement
                        (Infix (Identifier "x", Token.Plus, Identifier "y"));
                    ] ) );
            Expression_statement
              (Call (Identifier "add", [ Integer 5; Integer 5 ]));
          ];
        expected = Object.Integer 10;
      };
      (* let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5)); *)
      {
        input =
          [
            Let
              ( "add",
                Function
                  ( [ "x"; "y" ],
                    [
                      Expression_statement
                        (Infix (Identifier "x", Token.Plus, Identifier "y"));
                    ] ) );
            Expression_statement
              (Call
                 ( Identifier "add",
                   [
                     Infix (Integer 5, Token.Plus, Integer 5);
                     Call (Identifier "add", [ Integer 5; Integer 5 ]);
                   ] ));
          ];
        expected = Object.Integer 20;
      };
      (* fn(x) { x; }(5); *)
      {
        input =
          [
            Expression_statement
              (Call
                 ( Function ([ "x" ], [ Expression_statement (Identifier "x") ]),
                   [ Integer 5 ] ));
          ];
        expected = Object.Integer 5;
      };
    ]
  in
  List.iter test_statements cases