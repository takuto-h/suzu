
type t =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string

val show : t -> string
