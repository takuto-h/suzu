
type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Con of Literal.t
  | Var of string
  | Abs of string list * t
  | App of t * t list

let at pos raw = {
  pos = pos;
  raw = raw;
}
