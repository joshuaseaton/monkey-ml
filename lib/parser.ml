type precedence =
  | Lowest
  | Equality
  | Less_or_greater
  | Sum
  | Product
  | Prefix
  | Call
  | Index
[@@deriving ord]

let get_infix_precedence (tok : Token.t) : precedence =
  match tok with
  | Token.Equal | Token.Not_equal -> Equality
  | Token.Less_than | Token.Greater_than -> Less_or_greater
  | Token.Plus | Token.Minus -> Sum
  | Token.Asterisk | Token.Slash -> Product
  | Token.Left_paren -> Call
  | Token.Left_bracket -> Index
  | _ -> Lowest

type error = string

type t = {
  lexer : Lexer.t;
  token : Token.t option;
}

let ( let* ) = Result.bind

let token_opt_to_string = function
  | None -> "EOF"
  | Some token -> Token.to_string token

let rec create (lexer : Lexer.t) : t = advance { lexer; token = None }

and advance (parser : t) : t =
  let lexer, token = Lexer.next_token parser.lexer in
  { lexer; token }

(* Tries to advance past the expected token, returning an error if the current
   token differs from it. *)
let consume_token (parser : t) (token : Token.t) : (t, error) result =
  if parser.token = Some token then
    Ok (advance parser)
  else
    Error
      (Printf.sprintf "expected %s; got %s" (Token.to_string token)
         (token_opt_to_string parser.token))

(* Consumes the provided token if the current one matches. *)
let maybe_consume_token (parser : t) (token : Token.t) : t =
  if parser.token = Some token then
    advance parser
  else
    parser

let expression_as_identifier (expr : Ast.expression) : (string, error) result =
  match expr with
  | Ast.Identifier s -> Ok s
  | _ ->
      Error
        (Printf.sprintf "%S is not an identifier"
           (Ast.expression_to_string expr))

let rec parse (parser : t) : (Ast.node, error) result =
  let rec parse_statements parser =
    match parser.token with
    | None -> Ok (parser, [])
    | _ ->
        let* parser, stmt = parse_statement parser in
        let* parser, stmts = parse_statements parser in
        Ok (parser, stmt :: stmts)
  in
  let* _, stmts = parse_statements parser in
  Ok (Ast.Program stmts)

and parse_statement (parser : t) : (t * Ast.statement, error) result =
  match parser.token with
  | Some Token.Let -> parse_let_statement parser
  | Some Token.Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser

and parse_let_statement (parser : t) : (t * Ast.statement, error) result =
  let* parser = consume_token parser Token.Let in
  let* parser, ident = parse_identifier parser in
  let* ident = expression_as_identifier ident in
  let* parser = consume_token parser Token.Assign in
  let* parser, expr = parse_expression parser Lowest in
  let* parser = consume_token parser Token.Semicolon in
  Ok (parser, Ast.Let (ident, expr))

and parse_return_statement (parser : t) : (t * Ast.statement, error) result =
  let* parser = consume_token parser Token.Return in
  let* parser, expr = parse_expression parser Lowest in
  let* parser = consume_token parser Token.Semicolon in
  Ok (parser, Ast.Return expr)

and parse_expression_statement (parser : t) : (t * Ast.statement, error) result
    =
  let* parser, expr = parse_expression parser Lowest in
  let parser = maybe_consume_token parser Token.Semicolon in
  Ok (parser, Ast.Expression_statement expr)

and parse_expression (parser : t) (prec : precedence) :
    (t * Ast.expression, error) result =
  let parse_from_prefix parser =
    match parser.token with
    | Some (Token.Identifier _) -> parse_identifier parser
    | Some (Token.Boolean _) -> parse_boolean parser
    | Some (Token.Integer _) -> parse_integer parser
    | Some (Token.String _) -> parse_string_literal parser
    | Some (Token.Bang | Token.Minus) -> parse_prefix_expression parser
    | Some Token.Left_paren -> parse_grouped_expression parser
    | Some Token.Left_bracket -> parse_array_expression parser
    | Some Token.Left_brace -> parse_hash_expression parser
    | Some Token.If -> parse_if_expression parser
    | Some Token.Function -> parse_function_expression parser
    | Some (Token.Illegal c) -> Error (Printf.sprintf "illegal character: %c" c)
    | Some (Token.Unterminated_string s) ->
        Error (Printf.sprintf "unterminated string: \"%s" s)
    | _ ->
        Error
          (Printf.sprintf "unexpected expression token %s"
             (token_opt_to_string parser.token))
  in
  let rec parse_from_infix parser left prec =
    match parser.token with
    | Some token when compare_precedence (get_infix_precedence token) prec <= 0
      ->
        Ok (parser, left)
    | Some
        ( Token.Equal | Token.Not_equal | Token.Less_than | Token.Greater_than
        | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk ) ->
        let* parser, expr = parse_infix_expression parser left in
        parse_from_infix parser expr prec
    | Some Token.Left_paren ->
        let* parser, expr = parse_call_expression parser left in
        parse_from_infix parser expr prec
    | Some Token.Left_bracket ->
        let* parser, expr = parse_index_expression parser left in
        parse_from_infix parser expr prec
    | _ -> Ok (parser, left)
  in
  let* parser, left = parse_from_prefix parser in
  parse_from_infix parser left prec

and parse_identifier (parser : t) : (t * Ast.expression, error) result =
  match parser.token with
  | Some (Token.Identifier s) -> Ok (advance parser, Ast.Identifier s)
  | _ ->
      Error
        (Printf.sprintf "expected identifier; got %s"
           (token_opt_to_string parser.token))

and parse_boolean (parser : t) : (t * Ast.expression, error) result =
  match parser.token with
  | Some (Token.Boolean b) -> Ok (advance parser, Ast.Boolean b)
  | _ ->
      Error
        (Printf.sprintf "expected boolean; got %s"
           (token_opt_to_string parser.token))

and parse_integer (parser : t) : (t * Ast.expression, error) result =
  match parser.token with
  | Some (Token.Integer s) -> (
      match int_of_string_opt s with
      | None -> Error (Printf.sprintf "could not parse %s as an integer" s)
      | Some n -> Ok (advance parser, Ast.Integer n))
  | _ ->
      Error
        (Printf.sprintf "expected integer; got %s"
           (token_opt_to_string parser.token))

and parse_string_literal (parser : t) : (t * Ast.expression, error) result =
  match parser.token with
  | Some (Token.String s) -> Ok (advance parser, Ast.String s)
  | _ ->
      Error
        (Printf.sprintf "expected string; got %s"
           (token_opt_to_string parser.token))

and parse_array_expression (parser : t) : (t * Ast.expression, error) result =
  let rec parse_elements parser =
    match parser.token with
    | Some Token.Right_bracket -> Ok (parser, [])
    | _ ->
        let* parser, el = parse_expression parser Lowest in
        let parser = maybe_consume_token parser Token.Comma in
        let* parser, els = parse_elements parser in
        Ok (parser, el :: els)
  in
  let* parser = consume_token parser Token.Left_bracket in
  let* parser, els = parse_elements parser in
  let* parser = consume_token parser Token.Right_bracket in
  Ok (parser, Ast.Array els)

and parse_hash_expression (parser : t) : (t * Ast.expression, error) result =
  let rec parse_pairs parser =
    match parser.token with
    | Some Token.Right_brace -> Ok (parser, [])
    | _ ->
        let* parser, key = parse_expression parser Lowest in
        let* parser = consume_token parser Token.Colon in
        let* parser, value = parse_expression parser Lowest in
        let parser = maybe_consume_token parser Token.Comma in
        let* parser, pairs = parse_pairs parser in
        Ok (parser, (key, value) :: pairs)
  in
  let* parser = consume_token parser Token.Left_brace in
  let* parser, pairs = parse_pairs parser in
  let* parser = consume_token parser Token.Right_brace in
  Ok (parser, Ast.Hash pairs)

and parse_prefix_expression (parser : t) : (t * Ast.expression, error) result =
  let parse_prefix_operator parser =
    match parser.token with
    | Some ((Token.Bang | Token.Minus) as token) -> Ok (advance parser, token)
    | _ ->
        Error
          (Printf.sprintf "expected prefix operator; got %s"
             (token_opt_to_string parser.token))
  in
  let* parser, op = parse_prefix_operator parser in
  let* parser, expr = parse_expression parser Prefix in
  Ok (parser, Ast.Prefix (op, expr))

and parse_infix_expression (parser : t) (left : Ast.expression) :
    (t * Ast.expression, error) result =
  let parse_infix_operator parser =
    match parser.token with
    | Some
        (( Token.Equal | Token.Not_equal | Token.Less_than | Token.Greater_than
         | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk ) as token)
      ->
        Ok (advance parser, token)
    | _ ->
        Error
          (Printf.sprintf "expected infix operator; got %s"
             (token_opt_to_string parser.token))
  in
  let* parser, op = parse_infix_operator parser in
  let prec = get_infix_precedence op in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix (left, op, right))

and parse_grouped_expression (parser : t) : (t * Ast.expression, error) result =
  let* parser = consume_token parser Token.Left_paren in
  let* parser, expr = parse_expression parser Lowest in
  let* parser = consume_token parser Token.Right_paren in
  Ok (parser, expr)

and parse_if_expression (parser : t) : (t * Ast.expression, error) result =
  let* parser = consume_token parser Token.If in
  let* parser = consume_token parser Token.Left_paren in
  let* parser, cond = parse_expression parser Lowest in
  let* parser = consume_token parser Token.Right_paren in
  let* parser, consequence = parse_block_expression parser in
  let* parser, alternative =
    if parser.token = Some Token.Else then
      let parser = advance parser in
      let* parser, alternative = parse_block_expression parser in
      Ok (parser, Some alternative)
    else
      Ok (parser, None)
  in
  Ok (parser, Ast.If (cond, consequence, alternative))

and parse_block_expression (parser : t) : (t * Ast.statement list, error) result
    =
  let rec parse_statements parser =
    match parser.token with
    | Some Token.Right_brace -> Ok (parser, [])
    | _ ->
        let* parser, stmt = parse_statement parser in
        let* parser, stmts = parse_statements parser in
        Ok (parser, stmt :: stmts)
  in
  let* parser = consume_token parser Token.Left_brace in
  let* parser, stmts = parse_statements parser in
  let* parser = consume_token parser Token.Right_brace in
  Ok (parser, stmts)

and parse_function_expression (parser : t) : (t * Ast.expression, error) result
    =
  let rec parse_identifiers parser =
    match parser.token with
    | Some Token.Right_paren -> Ok (parser, [])
    | _ ->
        let* parser, ident = parse_identifier parser in
        let* ident = expression_as_identifier ident in
        let parser = maybe_consume_token parser Token.Comma in
        let* parser, idents = parse_identifiers parser in
        Ok (parser, ident :: idents)
  in
  let* parser = consume_token parser Token.Function in
  let* parser = consume_token parser Token.Left_paren in
  let* parser, idents = parse_identifiers parser in
  let* parser = consume_token parser Token.Right_paren in
  let* parser, body = parse_block_expression parser in
  Ok (parser, Ast.Function (idents, body))

and parse_call_expression (parser : t) (func : Ast.expression) :
    (t * Ast.expression, error) result =
  let rec parse_args parser =
    match parser.token with
    | Some Token.Right_paren -> Ok (parser, [])
    | _ ->
        let* parser, arg = parse_expression parser Lowest in
        let parser = maybe_consume_token parser Token.Comma in
        let* parser, args = parse_args parser in
        Ok (parser, arg :: args)
  in
  let* parser = consume_token parser Token.Left_paren in
  let* parser, args = parse_args parser in
  let* parser = consume_token parser Token.Right_paren in
  Ok (parser, Ast.Call (func, args))

and parse_index_expression (parser : t) (array : Ast.expression) :
    (t * Ast.expression, error) result =
  let* parser = consume_token parser Token.Left_bracket in
  let* parser, expr = parse_expression parser Lowest in
  let* parser = consume_token parser Token.Right_bracket in
  Ok (parser, Ast.Index (array, expr))
