
open Printf

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of get
  | Lambda of string list * t list
  | FunCall of t * t list
  | Block of t list
  | Define of string * t
  | MethodCall of t * string * t list

and get =
  | Var of string
  | Method of string list * string * string

let at pos raw = {
  pos = pos;
  raw = raw;
}

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (Var x) ->
      sprintf "(GetVar %s)" x
    | Get (Method (mods, klass, sel)) ->
      sprintf "(GetMethod (%s %s) %s)" (SnString.concat " " mods) klass sel
    | Lambda (params, body) ->
      sprintf "(Lambda (%s) %s)" (SnString.concat " " params) (SnString.concat_map " " show body)
    | FunCall (func, args) ->
      sprintf "(FunCall %s %s)" (show func) (SnString.concat_map " " show args)
    | Block exprs ->
      sprintf "(Block %s)" (SnString.concat_map " " show exprs)
    | Define (x, expr) ->
      sprintf "(Define %s %s)" x (show expr)
    | MethodCall (recv, sel, args) ->
      sprintf "(MethodCall %s %s %s)" (show recv) sel (SnString.concat_map " " show args)
  end
