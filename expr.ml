
open Printf

type var_or_method =
  | Var of string
  | Method of string list * string * Selector.t

type export = bool
type mutabl = bool
type ctor = export * string * (mutabl * string) list

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * var_or_method
  | Def of export * var_or_method * t
  | Lambda of string list * t list
  | FunCall of t * t list
  | MethodCall of t * Selector.t * t list
  | And of t * t
  | Or of t * t
  | Module of export * string * t list
  | Class of export * string * ctor list

let at pos raw = {
  pos = pos;
  raw = raw;
}

let show_var_or_method vom =
  begin match vom with
    | Var x ->
      sprintf "(Var %s)" x
    | Method (mods, klass, sel) ->
      sprintf "(Method (%s %s) %s)" (SnString.concat " " mods) klass (Selector.show sel)
  end

let show_ctor_param (mutabl, name) =
  sprintf "(%b %s)" mutabl name

let show_ctor (export, name, params) =
  sprintf "(Ctor %b %s (%s))" export name (SnString.concat_map " " show_ctor_param params)

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (mods, vom) ->
      sprintf "(Get %s %s)" (SnString.concat " " mods) (show_var_or_method vom)
    | Def (export, vom, expr) ->
      sprintf "(Def %b %s %s)" export (show_var_or_method vom) (show expr)
    | Lambda (params, body) ->
      sprintf "(Lambda (%s) %s)" (SnString.concat " " params) (SnString.concat_map " " show body)
    | FunCall (func, args) ->
      sprintf "(FunCall %s %s)" (show func) (SnString.concat_map " " show args)
    | MethodCall (recv, sel, args) ->
      sprintf "(MethodCall %s %s %s)" (show recv) (Selector.show sel) (SnString.concat_map " " show args)
    | And (lhs, rhs) ->
      sprintf "(And %s %s)" (show lhs) (show rhs)
    | Or (lhs, rhs) ->
      sprintf "(Or %s %s)" (show lhs) (show rhs)
    | Module (export, name, exprs) ->
      sprintf "(Module %b %s %s)" export name (SnString.concat_map " " show exprs)
    | Class (export, name, ctors) ->
      sprintf "(Module %b %s %s)" export name (SnString.concat_map " " show_ctor ctors)
  end
