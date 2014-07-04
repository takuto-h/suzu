
open Printf

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

let rec show {raw} =
  begin match raw with
    | Con lit ->
      sprintf "(Con %s)" (Literal.show lit)
    | Var x ->
      sprintf "(Var %s)" x
    | Abs (params, body) ->
      sprintf "(Abs (%s) %s)" (SnString.concat " " params) (show body)
    | App (func, args) ->
      sprintf "(App %s (%s))" (show func) (SnString.concat_map " " show args)
  end
