
type t =
  | At of Pos.t
  | Push of Literal.t
  | FindVar of string
  | AddVar of string
