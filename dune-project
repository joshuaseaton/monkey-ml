(lang dune 3.10)

(name monkeyml)

(generate_opam_files true)

(authors "Joshua Seaton")

(package
 (name monkeyml)
 (synopsis "Writing an Interpreter and Compiler in Go in OCaml")
 (description "This project is an OCaml transcription of the interpreter \
               and compiler developed over the course of Thorsten Ball's \
               \"Writing an Interpreter in Go\" and its sequel \"Writing a \
               Compiler in Go\". It serves as an exercise in learning about \
               interpreters and compilers, as well as OCaml.")
 (depends ocaml dune ppx_deriving ppx_inline_test ppx_sexp_conv sexplib))
