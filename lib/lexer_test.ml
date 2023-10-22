open Ppx_compare_lib.Builtin
open Sexplib.Std

type case = {
  input : string;
  expected : Token.t list;
}

let rec read_all (lexer : Lexer.t) : Token.t list =
  let lexer, token = Lexer.next_token lexer in
  match token with None -> [] | Some token -> token :: read_all lexer

let test_case case =
  let l = Lexer.create case.input in
  let actual = read_all l in
  [%test_result: Token.t list] ~expect:case.expected actual

let%test_unit "next token" =
  let open Token in
  let cases =
    [
      {
        input = "let five = 5;";
        expected = [ Let; Identifier "five"; Assign; Integer "5"; Semicolon ];
      };
      {
        input = "let ten = 10;";
        expected = [ Let; Identifier "ten"; Assign; Integer "10"; Semicolon ];
      };
      {
        input = "let add = fn(x, y) { x + y; };";
        expected =
          [
            Let;
            Identifier "add";
            Assign;
            Function;
            Left_paren;
            Identifier "x";
            Comma;
            Identifier "y";
            Right_paren;
            Left_brace;
            Identifier "x";
            Plus;
            Identifier "y";
            Semicolon;
            Right_brace;
            Semicolon;
          ];
      };
      {
        input = "let result = add(five, ten);";
        expected =
          [
            Let;
            Identifier "result";
            Assign;
            Identifier "add";
            Left_paren;
            Identifier "five";
            Comma;
            Identifier "ten";
            Right_paren;
            Semicolon;
          ];
      };
      {
        input = "!-/*5;";
        expected = [ Bang; Minus; Slash; Asterisk; Integer "5"; Semicolon ];
      };
      {
        input = "5 < 10 > 5;";
        expected =
          [
            Integer "5";
            Less_than;
            Integer "10";
            Greater_than;
            Integer "5";
            Semicolon;
          ];
      };
      {
        input = "if (5 < 10) { return true; } else { return false; }";
        expected =
          [
            If;
            Left_paren;
            Integer "5";
            Less_than;
            Integer "10";
            Right_paren;
            Left_brace;
            Return;
            Boolean true;
            Semicolon;
            Right_brace;
            Else;
            Left_brace;
            Return;
            Boolean false;
            Semicolon;
            Right_brace;
          ];
      };
      {
        input = "10 == 10;";
        expected = [ Integer "10"; Equal; Integer "10"; Semicolon ];
      };
      {
        input = "10 != 9;";
        expected = [ Integer "10"; Not_equal; Integer "9"; Semicolon ];
      };
      {
        input = {|"a string literal"|};
        expected = [ Token.String "a string literal" ];
      };
      {
        input = {|"an unterminated string|};
        expected = [ Token.Unterminated_string "an unterminated string" ];
      };
      {
        input = "[1, 2];";
        expected =
          [
            Left_bracket;
            Integer "1";
            Comma;
            Integer "2";
            Right_bracket;
            Semicolon;
          ];
      };
    ]
  in
  List.iter test_case cases
