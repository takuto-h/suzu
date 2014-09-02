
exception Error of Pos.t * string

type t

val create : Source.t -> t
val indent : t -> unit
val next : t -> Token.t option * Pos.t
