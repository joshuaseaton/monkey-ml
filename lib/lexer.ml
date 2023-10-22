type t = {
  input : string;
  position : int;
}

let create (input : string) : t = { input; position = 0 }
let at_end (lexer : t) : bool = lexer.position >= String.length lexer.input

let peek (lexer : t) : char option =
  if at_end lexer then
    None
  else
    Some lexer.input.[lexer.position]

let advance (lexer : t) : t = { lexer with position = lexer.position + 1 }

let rec seek_while (lexer : t) (cond : char -> bool) : t =
  match peek lexer with
  | None -> lexer
  | Some ch ->
      if cond ch then
        seek_while (advance lexer) cond
      else
        lexer

let read_while (lexer : t) (cond : char -> bool) : t * string =
  let before = lexer.position in
  let lexer = seek_while lexer cond in
  let after = lexer.position in
  (lexer, String.sub lexer.input before (after - before))

let is_identifier_char (ch : char) : bool = Base.Char.is_alpha ch || ch == '_'

let read_identifier (lexer : t) : t * Token.t option =
  let lexer, str = read_while lexer is_identifier_char in
  match str with
  | "" -> (lexer, None)
  | "fn" -> (lexer, Some Token.Function)
  | "let" -> (lexer, Some Token.Let)
  | "true" -> (lexer, Some (Token.Boolean true))
  | "false" -> (lexer, Some (Token.Boolean false))
  | "if" -> (lexer, Some Token.If)
  | "else" -> (lexer, Some Token.Else)
  | "return" -> (lexer, Some Token.Return)
  | _ -> (lexer, Some (Token.Identifier str))

let read_int (lexer : t) : t * Token.t option =
  let lexer, str = read_while lexer Base.Char.is_digit in
  match str with "" -> (lexer, None) | _ -> (lexer, Some (Token.Integer str))

let read_string_literal (lexer : t) : t * Token.t option =
  match peek lexer with
  | Some ch when ch == '"' ->
      let lexer = advance lexer in
      let lexer, str = read_while lexer (fun ch -> ch != '"') in
      if at_end lexer then
        (lexer, Some (Token.Unterminated_string str))
      else
        (advance lexer, Some (Token.String str))
  | _ -> (lexer, None)

let skip_whitespace (lexer : t) : t = seek_while lexer Base.Char.is_whitespace

let next_token (lexer : t) : t * Token.t option =
  let lexer = skip_whitespace lexer in
  match peek lexer with
  | None -> (lexer, None)
  | Some ch -> (
      match ch with
      | '=' -> (
          let lexer = advance lexer in
          match peek lexer with
          | Some '=' -> (advance lexer, Some Token.Equal)
          | _ -> (lexer, Some Token.Assign))
      | '+' -> (advance lexer, Some Token.Plus)
      | '-' -> (advance lexer, Some Token.Minus)
      | ',' -> (advance lexer, Some Token.Comma)
      | '!' -> (
          let lexer = advance lexer in
          match peek lexer with
          | Some '=' -> (advance lexer, Some Token.Not_equal)
          | _ -> (lexer, Some Token.Bang))
      | '*' -> (advance lexer, Some Token.Asterisk)
      | '/' -> (advance lexer, Some Token.Slash)
      | '<' -> (advance lexer, Some Token.Less_than)
      | '>' -> (advance lexer, Some Token.Greater_than)
      | ';' -> (advance lexer, Some Token.Semicolon)
      | '(' -> (advance lexer, Some Token.Left_paren)
      | ')' -> (advance lexer, Some Token.Right_paren)
      | '{' -> (advance lexer, Some Token.Left_brace)
      | '}' -> (advance lexer, Some Token.Right_brace)
      | '[' -> (advance lexer, Some Token.Left_bracket)
      | ']' -> (advance lexer, Some Token.Right_bracket)
      | '"' -> read_string_literal lexer
      | _ ->
          if is_identifier_char ch then
            read_identifier lexer
          else if Base.Char.is_digit ch then
            read_int lexer
          else
            (advance lexer, Some (Token.Illegal ch)))
