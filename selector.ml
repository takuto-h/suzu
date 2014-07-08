
open Printf

type t =
  | Ident of string
  | Op of string

let string_of sel =
  begin match sel with
    | Ident str ->
      str
    | Op str ->
      str
  end

let show sel =
  begin match sel with
    | Ident str ->
      str
    | Op str ->
      sprintf "(%s)" str
  end
