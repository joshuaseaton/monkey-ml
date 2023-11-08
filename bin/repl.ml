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
       | Ok prog -> (
           match Compiler.compile prog with
           | Error err -> print_endline (Compiler.error_to_string err)
           | Ok code -> (
               let vm = Vm.create code in
               match Vm.run vm with
               | Error (err, pc) ->
                   print_endline
                     (Printf.sprintf "error at pc %#x: %s" pc
                        (Vm.error_to_string err))
               | Ok result -> (
                   match result with
                   | Object.Null | Object.Builtin _ -> ()
                   | _ -> print_endline (Object.to_string result)))));
      start ()

let () =
  print_endline "This is the Monkey programming language!";
  print_endline "Feel free to type in commands!";
  start ()
