
type t =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string

val show : t -> string
