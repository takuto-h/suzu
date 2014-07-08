
open Printf

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * var_or_method
  | Lambda of string list * t list
  | FunCall of t * t list
  | Block of t list
  | Define of string * t
  | MethodCall of t * Selector.t * t list

and var_or_method =
  | Var of string
  | Method of string list * string * Selector.t

let at pos raw = {
  pos = pos;
  raw = raw;
}

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (mods, Var x) ->
      sprintf "(GetVar %s %s)" (SnString.concat " " mods) x
    | Get (mods1, Method (mods2, klass, sel)) ->
      sprintf "(Get %s (Method (%s %s) %s))" (SnString.concat " " mods1) (SnString.concat " " mods2) klass (Selector.show sel)
    | Lambda (params, body) ->
      sprintf "(Lambda (%s) %s)" (SnString.concat " " params) (SnString.concat_map " " show body)
    | FunCall (func, args) ->
      sprintf "(FunCall %s %s)" (show func) (SnString.concat_map " " show args)
    | Block exprs ->
      sprintf "(Block %s)" (SnString.concat_map " " show exprs)
    | Define (x, expr) ->
      sprintf "(Define %s %s)" x (show expr)
    | MethodCall (recv, sel, args) ->
      sprintf "(MethodCall %s %s %s)" (show recv) (Selector.show sel) (SnString.concat_map " " show args)
  end
