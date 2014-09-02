
exception Error of Pos.t * string

type t

val create : Lexer.t -> t
val parse : t -> Expr.t option
