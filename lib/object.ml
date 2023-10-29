open Sexplib.Std

(* A built-in Monkey function. *)
type builtin =
  | Len
  | First
  | Last
  | Rest
  | Push
[@@deriving ord, sexp_of]

(* Object is an expedient module wrapper only intended to aid in the recursive
   module definitions below, necessitated by the definition of a map with keys
   of object type. *)
module rec Object : sig
  (* An evaluated Monkey object. *)
  type t =
    | Null
    | Integer of int
    | Boolean of bool
    | String of string
    | Array of t list
    | Hash of Hash.t
    | Builtin of builtin
    | Function of (string list * Ast.statement list)
  [@@deriving ord, sexp_of]

  include Base.Comparator.S with type t := t
end = struct
  module T = struct
    type t =
      | Null
      | Integer of int
      | Boolean of bool
      | String of string
      | Array of t list
      | Hash of Hash.t
      | Builtin of builtin
      | Function of (string list * Ast.statement list)
    [@@deriving ord, sexp_of]
  end

  include T
  include Base.Comparator.Make (T)
end

and Hash : sig
  type t [@@deriving compare, sexp_of]

  val create : unit -> t
  val add : t -> Object.t -> Object.t -> t * bool
  val find_opt : t -> Object.t -> Object.t option
  val iteri : t -> (key:Object.t -> data:Object.t -> unit) -> unit
end = struct
  type t = Object.t Base.Map.M(Object).t [@@deriving compare, sexp_of]

  let create () = Base.Map.empty (module Object)

  let add (hash : t) (key : Object.t) (value : Object.t) : t * bool =
    match key with
    | Object.String _ | Object.Boolean _ | Object.Integer _ -> (
        match Base.Map.add hash ~key ~data:value with
        | `Ok hash -> (hash, true)
        | `Duplicate -> (hash, false))
    | _ ->
        failwith "Only strings, booleans, and integers may be used as hash keys"

  let find_opt = Base.Map.find

  let iteri (hash : t) (f : key:Object.t -> data:Object.t -> unit) =
    Base.Map.iteri hash ~f
end

include Object

let rec to_string = function
  | Null -> "NULL"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | String s -> Printf.sprintf "%S" s
  | Array elems -> Printf.sprintf "[%s]" (concat elems ", ")
  | Hash hash ->
      let str = ref "{" in
      let first = ref true in
      let f ~key:k ~data:v =
        str :=
          !str
          ^ Printf.sprintf "%s%s: %s"
              (if !first then
                 ""
               else
                 ", ")
              (to_string k) (to_string v);
        first := false
      in
      Hash.iteri hash f;
      !str ^ "}"
  | Builtin builtin -> (
      match builtin with
      | Len -> "len()"
      | First -> "first()"
      | Last -> "last()"
      | Rest -> "rest()"
      | Push -> "push()")
  | Function (params, body) ->
      Ast.expression_to_string (Ast.Function (params, body))

and concat (objs : t list) (sep : string) : string =
  match objs with
  | [] -> ""
  | [ obj ] -> to_string obj
  | first :: rest -> to_string first ^ sep ^ concat rest sep
