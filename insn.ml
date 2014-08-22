
type t =
  | At of Pos.t
  | Push of Literal.t
  | FindVar of string
  | FindMethod of Selector.t
  | AddVar of string
