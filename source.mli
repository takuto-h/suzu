
type t

val of_channel : string -> in_channel -> t
val of_string : string -> string -> t
val pos : t -> Pos.t
val peek : t -> char option
val junk : t -> unit
