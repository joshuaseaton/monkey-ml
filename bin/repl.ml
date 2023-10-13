let rec print_tokens (lexer : Lexer.t) =
  let lexer, token = Lexer.next_token lexer in
  match token with
  | None -> ()
  | Some token ->
      print_endline (Token.to_string token);
      print_tokens lexer

let rec start () =
  let open Stdio in
  print_string ">> ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      let lexer = Lexer.create line in
      print_tokens lexer;
      start ()

let () =
  print_endline "This is the Monkey programming language!";
  print_endline "Feel free to type in commands!";
  start ()
