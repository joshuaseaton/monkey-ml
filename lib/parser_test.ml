type 'a case = {
  input : string;
  expected : 'a;
}

let test_node (case : Ast.node case) =
  let lexer = Lexer.create case.input in
  let parser = Parser.create lexer in
  match Parser.parse parser with
  | Error err -> failwith err
  | Ok prog -> [%test_result: Ast.node] ~expect:case.expected prog

let test_expression (case : Ast.expression case) =
  test_node
    {
      input = case.input;
      expected = Ast.Program [ Ast.Expression_statement case.expected ];
    }

let test_statement (case : Ast.statement case) =
  test_node { input = case.input; expected = Ast.Program [ case.expected ] }

let%test_unit "let statements" =
  let open Ast in
  let cases =
    [
      { input = "let x = 5;"; expected = Let ("x", Integer 5) };
      { input = "let y = 10;"; expected = Let ("y", Integer 10) };
      {
        input = "let foobar = 838383;";
        expected = Let ("foobar", Integer 838383);
      };
      {
        input = {|let str = "this is a string";|};
        expected = Let ("str", Ast.String "this is a string");
      };
    ]
  in
  List.iter test_statement cases

let%test_unit "return statements" =
  let open Ast in
  let cases =
    [
      { input = "return 5;"; expected = Return (Integer 5) };
      { input = "return 10;"; expected = Return (Integer 10) };
      {
        input = "return add(15);";
        expected = Return (Call (Identifier "add", [ Integer 15 ]));
      };
    ]
  in
  List.iter test_statement cases

let%test_unit "prefix expressions" =
  let open Ast in
  let open Token in
  let cases =
    [
      { input = "!5"; expected = Prefix (Bang, Integer 5) };
      { input = "-15"; expected = Prefix (Minus, Integer 15) };
      { input = "!true"; expected = Prefix (Bang, Boolean true) };
      { input = "!false"; expected = Prefix (Bang, Boolean false) };
    ]
  in
  List.iter test_expression cases

let%test_unit "infix expressions" =
  let open Ast in
  let open Token in
  let cases =
    [
      { input = "5 + 5"; expected = Infix (Integer 5, Plus, Integer 5) };
      { input = "5 - 5"; expected = Infix (Integer 5, Minus, Integer 5) };
      { input = "5 * 5"; expected = Infix (Integer 5, Asterisk, Integer 5) };
      { input = "5\n   / 5"; expected = Infix (Integer 5, Slash, Integer 5) };
      { input = "5 < 5"; expected = Infix (Integer 5, Less_than, Integer 5) };
      { input = "5 > 5"; expected = Infix (Integer 5, Greater_than, Integer 5) };
      { input = "5 == 5"; expected = Infix (Integer 5, Equal, Integer 5) };
      { input = "5 != 5"; expected = Infix (Integer 5, Not_equal, Integer 5) };
      {
        input = "true ==\n   true";
        expected = Infix (Boolean true, Equal, Boolean true);
      };
      {
        input = "true != false";
        expected = Infix (Boolean true, Not_equal, Boolean false);
      };
      {
        input = "false == false";
        expected = Infix (Boolean false, Equal, Boolean false);
      };
      {
        input = {|"x" + "y"|};
        expected = Infix (Ast.String "x", Token.Plus, Ast.String "y");
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "operator precedence" =
  let open Ast in
  let open Token in
  let cases =
    [
      {
        input = "-a * b";
        expected =
          Infix (Prefix (Minus, Identifier "a"), Asterisk, Identifier "b");
      };
      {
        input = "!-a";
        expected = Prefix (Bang, Prefix (Minus, Identifier "a"));
      };
      {
        input = " a + b + c";
        expected =
          Infix
            (Infix (Identifier "a", Plus, Identifier "b"), Plus, Identifier "c");
      };
      {
        input = " a + b - c";
        expected =
          Infix
            (Infix (Identifier "a", Plus, Identifier "b"), Minus, Identifier "c");
      };
      {
        input = " a + b * c";
        expected =
          Infix
            ( Identifier "a",
              Plus,
              Infix (Identifier "b", Asterisk, Identifier "c") );
      };
      {
        input = " a + b / c";
        expected =
          Infix
            (Identifier "a", Plus, Infix (Identifier "b", Slash, Identifier "c"));
      };
      {
        input = "a + b * c + d /\n   e - f";
        expected =
          Infix
            ( Infix
                ( Infix
                    ( Identifier "a",
                      Plus,
                      Infix (Identifier "b", Asterisk, Identifier "c") ),
                  Plus,
                  Infix (Identifier "d", Slash, Identifier "e") ),
              Minus,
              Identifier "f" );
      };
      {
        input = "5 > 4 == 3 <\n   4";
        expected =
          Infix
            ( Infix (Integer 5, Greater_than, Integer 4),
              Equal,
              Infix (Integer 3, Less_than, Integer 4) );
      };
      {
        input = "3 + 4 * 5 == 3 * 1 +\n   4 * 5";
        expected =
          Infix
            ( Infix (Integer 3, Plus, Infix (Integer 4, Asterisk, Integer 5)),
              Equal,
              Infix
                ( Infix (Integer 3, Asterisk, Integer 1),
                  Plus,
                  Infix (Integer 4, Asterisk, Integer 5) ) );
      };
      {
        input = "3 > 5 ==\n   false";
        expected =
          Infix
            (Infix (Integer 3, Greater_than, Integer 5), Equal, Boolean false);
      };
      {
        input = "1 + (2 + 3) + 4";
        expected =
          Infix
            ( Infix (Integer 1, Plus, Infix (Integer 2, Plus, Integer 3)),
              Plus,
              Integer 4 );
      };
      {
        input = "(5 + 5) * 2";
        expected =
          Infix (Infix (Integer 5, Plus, Integer 5), Asterisk, Integer 2);
      };
      {
        input = "2 / (5 + 5)";
        expected = Infix (Integer 2, Slash, Infix (Integer 5, Plus, Integer 5));
      };
      {
        input = "-(5 +\n   5)";
        expected = Prefix (Minus, Infix (Integer 5, Plus, Integer 5));
      };
      {
        input = "!(true == true)";
        expected = Prefix (Bang, Infix (Boolean true, Equal, Boolean true));
      };
      {
        input = "a + add(b * c) + d";
        expected =
          Infix
            ( Infix
                ( Identifier "a",
                  Plus,
                  Call
                    ( Identifier "add",
                      [ Infix (Identifier "b", Asterisk, Identifier "c") ] ) ),
              Plus,
              Identifier "d" );
      };
      {
        input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))";
        expected =
          Call
            ( Identifier "add",
              [
                Identifier "a";
                Identifier "b";
                Integer 1;
                Infix (Integer 2, Asterisk, Integer 3);
                Infix (Integer 4, Plus, Integer 5);
                Call
                  ( Identifier "add",
                    [ Integer 6; Infix (Integer 7, Asterisk, Integer 8) ] );
              ] );
      };
      {
        input = "add(a + b + c * d / e + f)";
        expected =
          Call
            ( Identifier "add",
              [
                Infix
                  ( Infix
                      ( Infix (Identifier "a", Plus, Identifier "b"),
                        Plus,
                        Infix
                          ( Infix (Identifier "c", Asterisk, Identifier "d"),
                            Slash,
                            Identifier "e" ) ),
                    Plus,
                    Identifier "f" );
              ] );
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "if expressions" =
  let open Ast in
  let open Token in
  let cases =
    [
      {
        input = "if (x < y) { x }";
        expected =
          Ast.If
            ( Infix (Identifier "x", Less_than, Identifier "y"),
              [ Expression_statement (Identifier "x") ],
              None );
      };
      {
        input = "if (x < y) { x } else { y }";
        expected =
          Ast.If
            ( Infix (Identifier "x", Less_than, Identifier "y"),
              [ Expression_statement (Identifier "x") ],
              Some [ Expression_statement (Identifier "y") ] );
      };
    ]
  in
  List.iter test_expression cases

let%test_unit "function literals" =
  let open Ast in
  let cases =
    [
      { input = "fn() {}"; expected = Function ([], []) };
      { input = "fn(x) {}"; expected = Function ([ "x" ], []) };
      {
        input = "fn(x) {x}";
        expected = Function ([ "x" ], [ Expression_statement (Identifier "x") ]);
      };
      { input = "fn(x, y, z) {}"; expected = Function ([ "x"; "y"; "z" ], []) };
    ]
  in
  List.iter test_expression cases

let%test_unit "call expressions" =
  let open Ast in
  let open Token in
  let case =
    {
      input = "add(1, 2 * 3, 4 + 5)";
      expected =
        Call
          ( Identifier "add",
            [
              Integer 1;
              Infix (Integer 2, Asterisk, Integer 3);
              Infix (Integer 4, Plus, Integer 5);
            ] );
    }
  in
  test_expression case

let%test_unit "array expressions" =
  let open Ast in
  let open Token in
  let case =
    [
      {
        input = "[1, 2 * 2, 3 + 3]";
        expected =
          Array
            [
              Integer 1;
              Infix (Integer 2, Asterisk, Integer 2);
              Infix (Integer 3, Plus, Integer 3);
            ];
      };
      {
        input = "myArray[1 + 1]";
        expected =
          Index (Identifier "myArray", Infix (Integer 1, Token.Plus, Integer 1));
      };
      {
        input = "a * [1, 2, 3, 4][b * c] * d";
        expected =
          Infix
            ( Infix
                ( Identifier "a",
                  Token.Asterisk,
                  Index
                    ( Array [ Integer 1; Integer 2; Integer 3; Integer 4 ],
                      Infix (Identifier "b", Token.Asterisk, Identifier "c") )
                ),
              Token.Asterisk,
              Identifier "d" );
      };
      {
        input = "add(a * b[2], b[1], 2 * [1, 2][1])";
        expected =
          Call
            ( Identifier "add",
              [
                Infix
                  ( Identifier "a",
                    Token.Asterisk,
                    Index (Identifier "b", Integer 2) );
                Index (Identifier "b", Integer 1);
                Infix
                  ( Integer 2,
                    Token.Asterisk,
                    Index (Array [ Integer 1; Integer 2 ], Integer 1) );
              ] );
      };
    ]
  in
  List.iter test_expression case
