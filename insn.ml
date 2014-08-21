
open Printf

type t =
  | Push of Literal.t
  | GetVar of string list * string
  | GetMethod of string list * string list * string * string
  | Bind of Expr.Pattern.t
  | PushClosure of Expr.Params.t * t list
  | Call of int * string list
  | Send of string * int * string list
  | And of t list
  | Or of t list
  | Enter of string
  | Leave
  | ExportVar of string
  | ExportMethod of string list * string * string      
  | Open
  | Include
  | DefRecord of string * string * (string * bool) list
  | DefVariant of string * (string * Expr.Params.t) list
  | DefPhantom of string
  | PushTrait of Expr.Params.t * t list
  | UnexportVar of string
  | UnexportMethod of string list * string * string      
  | Match of int * string list * (Expr.Params.t * (t list) option * t list) list
  | PushTuple of int * string list

let rec show insn =
  begin match insn with
    | Push lit ->
      sprintf "(Push %s)" (Literal.show lit)
    | GetVar (mods, x) ->
      sprintf "(GetVar (%s) %s)" (SnString.concat " " mods) x
    | GetMethod (mods1, mods2, klass, sel) ->
      sprintf "(GetMethod (%s) (%s) %s %s)" (SnString.concat " " mods1) (SnString.concat " " mods2) klass sel
    | Bind pat ->
      sprintf "(Bind %s)" (Expr.Pattern.show pat)
    | PushClosure (params, insns) ->
      sprintf "(PushClosure %s (%s))" (Expr.Params.show params) (SnString.concat_map " " show insns)
    | Call (arg_count, keywords) ->
      sprintf "(Call %d (%s))" arg_count (SnString.concat " " keywords)
    | Send (sel, arg_count, keywords) ->
      sprintf "(Send %s %d (%s))" sel arg_count (SnString.concat " " keywords)
    | And insns ->
      sprintf "(And (%s))" (SnString.concat_map " " show insns)
    | Or insns ->
      sprintf "(Or (%s))" (SnString.concat_map " " show insns)
    | Enter name ->
      sprintf "(Enter %s)" name
    | Leave ->
      "Leave"
    | ExportVar x ->
      sprintf "(ExportVar %s)" x
    | ExportMethod (mods, klass, sel) ->
      sprintf "(ExportMethod (%s) %s %s)" (SnString.concat " " mods) klass sel
    | Open ->
      "Open"
    | Include ->
      "Include"
    | DefRecord (klass, ctor, fields) ->
      sprintf "(DefRecord %s %s (%s))" klass ctor (SnString.concat_map " " Expr.show_field fields)
    | DefVariant (klass, ctors) ->
      sprintf "(DefVariant %s (%s))" klass (SnString.concat_map " " Expr.show_ctor ctors)
    | DefPhantom klass ->
      sprintf "(DefPhantom %s)" klass
    | PushTrait (params, body) ->
      sprintf "(PushTrait %s (%s))" (Expr.Params.show params) (SnString.concat_map " " show body)
    | UnexportVar x ->
      sprintf "(UnexportVar %s)" x
    | UnexportMethod (mods, klass, sel) ->
      sprintf "(UnexportMethod (%s) %s %s)" (SnString.concat " " mods) klass sel
    | Match (arg_count, keywords, cases) ->
      sprintf "(Match %d (%s) (%s))" arg_count (SnString.concat " " keywords) (SnString.concat_map " " show_case cases)
    | PushTuple (arg_count, keywords) ->
      sprintf "(PushTuple %d %s)" arg_count (SnString.concat " " keywords)
  end

and show_case (params, guard, body) =
  begin match guard with
    | None ->
      sprintf "(Case %s None (%s))" (Expr.Params.show params) (SnString.concat_map " " show body)
    | Some guard ->
      sprintf "(Case %s (Some %s) (%s))" (Expr.Params.show params) (SnString.concat_map " " show guard) (SnString.concat_map " " show body)
  end
