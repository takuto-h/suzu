
type t = 
  | EOF
  | Newline
  | Undent
  | Int of int
  | Char of char
  | String of string
  | Ident of string
  | Reserved of string
  | CmpOp of string
  | OrOp of string
  | AndOp of string
  | AddOp of string
  | MulOp of string
  | UnaryOp of string

val get_op : t -> string option
val show : t -> string
