
open Printf

type export = bool

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * VarOrMethod.t
  | Def of VarOrMethod.t * t
  | Lambda of string list * t list
  | FunCall of t * t list
  | MethodCall of t * Selector.t * t list
  | And of t * t
  | Or of t * t
  | Module of string * t list
  | Export of VarOrMethod.t list
  | Open of t
  | Record of string * string * (string * bool) list
  | Trait of string list * t list
  | Except of t * VarOrMethod.t list

let at pos raw = {
  pos = pos;
  raw = raw;
}


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
      sprintf "(Get %s %s)" (SnString.concat " " mods) (VarOrMethod.show vom)
    | Def (vom, expr) ->
      sprintf "(Def %s %s)" (VarOrMethod.show vom) (show expr)
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
      sprintf "(Export %s)" (SnString.concat_map " " VarOrMethod.show voms)
    | Open expr ->
      sprintf "(Open %s)" (show expr)
    | Record (klass, ctor, fields) ->
      sprintf "(Record %s %s %s)" klass ctor (SnString.concat_map " " show_field fields)
    | Trait (params, body) ->
      sprintf "(Trait (%s) %s)" (SnString.concat " " params) (SnString.concat_map " " show body)
    | Except (modl, voms) ->
      sprintf "(Export %s %s)" (show modl) (SnString.concat_map " " VarOrMethod.show voms)
  end
