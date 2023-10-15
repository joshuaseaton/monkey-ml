let rec start () =
  let open Stdio in
  print_string ">> ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      (let lexer = Lexer.create line in
       let parser = Parser.create lexer in
       match Parser.parse parser with
       | Error err -> print_endline err
       | Ok node -> print_endline (Ast.node_to_string node));
      start ()

let () =
  print_endline "This is the Monkey programming language!";
  print_endline "Feel free to type in commands!";
  start ()
