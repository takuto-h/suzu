
open Printf

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Con of Literal.t
  | Get of get
  | Abs of string list * t
  | App of t * t list
  | Block of t list
  | Def of string * t
  | MethodCall of t * string * t list

and get =
  | Var of string
  | Method of t * string

let at pos raw = {
  pos = pos;
  raw = raw;
}

let rec show {raw} =
  begin match raw with
    | Con lit ->
      sprintf "(Con %s)" (Literal.show lit)
    | Get (Var x) ->
      sprintf "(Get (Var %s))" x
    | Get (Method (klass, sel)) ->
      sprintf "(Get (Method %s %s))" (show klass) sel
    | Abs (params, body) ->
      sprintf "(Abs (%s) %s)" (SnString.concat " " params) (show body)
    | App (func, args) ->
      sprintf "(App %s (%s))" (show func) (SnString.concat_map " " show args)
    | Block exprs ->
      sprintf "(Block %s)" (SnString.concat_map " " show exprs)
    | Def (x, expr) ->
      sprintf "(Def %s %s)" x (show expr)
    | MethodCall (recv, sel, args) ->
      sprintf "(MethodCall %s %s (%s))" (show recv) sel (SnString.concat_map " " show args)
  end
