let rec start (env : Evaluator.Environment.t) =
  let open Stdio in
  print_string ">> ";
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> start env
  | Some line -> (
      let lexer = Lexer.create line in
      let parser = Parser.create lexer in
      match Parser.parse parser with
      | Error err ->
          print_endline err;
          start env
      | Ok node -> (
          match Evaluator.eval node env with
          | Error err ->
              Printf.printf "Error: %s\n" err;
              start env
          | Ok (obj, env) ->
              (match obj with
              | Null -> ()
              | _ -> print_endline (Object.to_string obj));
              start env))

let () =
  print_endline "This is the Monkey programming language!";
  print_endline "Feel free to type in commands!";
  start Evaluator.Environment.create
