
open Printf

type var_or_method =
  | Var of string
  | Method of string list * string * Selector.t

type export = bool

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * var_or_method
  | Def of var_or_method * t
  | Lambda of string list * t list
  | FunCall of t * t list
  | MethodCall of t * Selector.t * t list
  | And of t * t
  | Or of t * t
  | Module of string * t list
  | Export of var_or_method list
  | Open of string list * string
  | Record of string * string * (string * bool) list
  | Trait of string list * t list

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

let show_field (field, mutabl) =
  if mutabl then
    sprintf "(mutable %s)" field
  else
    field

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (mods, vom) ->
      sprintf "(Get %s %s)" (SnString.concat " " mods) (show_var_or_method vom)
    | Def (vom, expr) ->
      sprintf "(Def %s %s)" (show_var_or_method vom) (show expr)
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
    | Module (name, exprs) ->
      sprintf "(Module %s %s)" name (SnString.concat_map " " show exprs)
    | Export voms ->
      sprintf "(Export %s)" (SnString.concat_map " " show_var_or_method voms)
    | Open (mods, modl) ->
      sprintf "(Open %s %s)" (SnString.concat " " mods) modl
    | Record (klass, ctor, fields) ->
      sprintf "(Record %s %s %s)" klass ctor (SnString.concat_map " " show_field fields)
    | Trait (params, body) ->
      sprintf "(Trait (%s) %s)" (SnString.concat " " params) (SnString.concat_map " " show body)
  end
