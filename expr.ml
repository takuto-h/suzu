
open Printf

type var_or_method =
  | Var of string
  | Method of string list * string * Selector.t

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * var_or_method
  | Let of pat * t
  | Lambda of params * t list
  | Call of t * args
  | Send of t * Selector.t * args
  | Args of args
  | And of t * t
  | Or of t * t
  | Match of args * (params * t option * t list) list
  | Module of string * t list
  | Export of var_or_method list
  | Open of t
  | Include of t
  | Record of string * string * (string * bool) list
  | Variant of string * (string * params) list
  | Phantom of string
  | Trait of params * t list
  | Except of t * var_or_method list
  | TryCatch of t * (pat * t list) list
  | TryFinally of t * t list
  | Throw of t
  | Exception of string * params

and pat = {
  pat_pos : Pos.t;
  pat_raw : pat_raw;
}

and pat_raw = 
  | PatWildCard
  | PatConst of Literal.t
  | PatBind of var_or_method
  | PatVariant of string * params
  | PatParams of params
  | PatOr of pat * pat
  | PatAs of pat * string

and params = {
  normal_params : pat list;
  rest_param : pat option;
  labeled_params : (string * (pat * t option)) list;
}

and args = {
  normal_args : t list;
  rest_arg : t option;
  labeled_args : (string * t) list;
}

let at pos raw = {
  pos = pos;
  raw = raw;
}

let show_var_or_method vom =
  begin match vom with
    | Var x ->
      x
    | Method (mods, klass, sel) ->
      if mods = [] then
        sprintf "%s#%s" klass (Selector.show sel)
      else
        sprintf "%s:%s#%s" (SnString.concat ":" mods) klass (Selector.show sel)
  end

let rec show_pattern {pat_raw} =
  begin match pat_raw with
    | PatWildCard ->
      "_"
    | PatConst lit ->
      Literal.show lit
    | PatBind vom ->
      show_var_or_method vom
    | PatVariant (ctor, params) ->
      sprintf "%s%s" ctor (show_params params)
    | PatParams params ->
      sprintf "%s" (show_params params)
    | PatOr (lhs, rhs) ->
      sprintf "(%s | %s)" (show_pattern lhs) (show_pattern rhs)
    | PatAs (pat, x) ->
      sprintf "(%s as %s)" (show_pattern pat) x
  end

and show_params {normal_params;rest_param;labeled_params;} =
  let normal = List.map show_pattern normal_params in
  let labeled = List.map show_labeled_param labeled_params in
  begin match rest_param with
    | Some rest_param ->
      let rest = sprintf "*%s" (show_pattern rest_param) in
      sprintf "(%s)" (SnString.concat ", " (normal @ rest::labeled))
    | None ->
      sprintf "(%s)" (SnString.concat ", " (normal @ labeled))
  end

and show_labeled_param (label, (pat, _)) =
  sprintf ":%s %s = <expr>" label (show_pattern pat)

let show_field (field, mutabl) =
  if mutabl then
    sprintf "(mutable %s)" field
  else
    field

let show_ctor (ctor, params) =
  sprintf "(%s %s)" ctor (show_params params)

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (mods, vom) ->
      sprintf "(Get (%s) %s)" (SnString.concat " " mods) (show_var_or_method vom)
    | Let (pat, expr) ->
      sprintf "(Let %s %s)" (show_pattern pat) (show expr)
    | Lambda (params, body) ->
      sprintf "(Lambda %s (%s))" (show_params params) (SnString.concat_map " " show body)
    | Call (func, args) ->
      sprintf "(Call %s %s)" (show func) (show_args args)
    | Send (recv, sel, args) ->
      sprintf "(Send %s %s %s)" (show recv) (Selector.show sel) (show_args args)
    | Args args ->
      sprintf "(Args %s)" (show_args args)
    | And (lhs, rhs) ->
      sprintf "(And %s %s)" (show lhs) (show rhs)
    | Or (lhs, rhs) ->
      sprintf "(Or %s %s)" (show lhs) (show rhs)
    | Match (args, cases) ->
      sprintf "(Match %s (%s))" (show_args args) (SnString.concat_map " " show_case cases)
    | Module (name, exprs) ->
      sprintf "(Module %s (%s))" name (SnString.concat_map " " show exprs)
    | Export voms ->
      sprintf "(Export (%s))" (SnString.concat_map " " show_var_or_method voms)
    | Open expr ->
      sprintf "(Open %s)" (show expr)
    | Include expr ->
      sprintf "(Include %s)" (show expr)
    | Record (klass, ctor, fields) ->
      sprintf "(Record %s %s (%s))" klass ctor (SnString.concat_map " " show_field fields)
    | Variant (klass, ctors) ->
      sprintf "(Variant %s (%s))" klass (SnString.concat_map " " show_ctor ctors)
    | Phantom klass ->
      sprintf "(Phantom %s)" klass
    | Trait (params, body) ->
      sprintf "(Trait %s (%s))" (show_params params) (SnString.concat_map " " show body)
    | Except (modl, voms) ->
      sprintf "(Export %s (%s))" (show modl) (SnString.concat_map " " show_var_or_method voms)
    | TryCatch (body, catches) ->
      sprintf "(TryCatch %s (%s))" (show body) (SnString.concat_map " " show_catch catches)
    | TryFinally (body, finally) ->
      sprintf "(TryFinally %s (%s))" (show body) (SnString.concat_map " " show finally)
    | Throw expr ->
      sprintf "(Throw %s)" (show expr)
    | Exception (ctor, params) ->
      sprintf "(Exception %s)" (show_ctor (ctor, params))
  end

and show_args {normal_args;rest_arg;labeled_args} =
  let normal = List.map show normal_args in
  let labeled = List.map show_labeled_arg labeled_args in
  begin match rest_arg with
    | Some rest_arg ->
      let rest = sprintf "*%s" (show rest_arg) in
      sprintf "(%s)" (SnString.concat ", " (normal @ rest::labeled))
    | None ->
      sprintf "(%s)" (SnString.concat ", " (normal @ labeled))
  end

and show_labeled_arg (label, value) =
  sprintf ":%s %s" label (show value)

and show_case (params, guard, body) =
  begin match guard with
    | Some cond ->
      sprintf "(Case %s (Some %s) (%s))" (show_params params) (show cond) (SnString.concat_map " " show body)
    | None ->
      sprintf "(Case %s None (%s))" (show_params params) (SnString.concat_map " " show body)
  end

and show_catch (pat, body) =
  sprintf "(Catch %s (%s))" (show_pattern pat) (SnString.concat_map " " show body)

module Pattern = struct
  let at pos raw = {
    pat_pos = pos;
    pat_raw = raw;
  }

  let show = show_pattern
end

module Params = struct
  let make normal rest labeled = {
    normal_params = normal;
    rest_param = rest;
    labeled_params = labeled;
  }

  let n_ary normal =
    make normal None []

  let nullary =
    n_ary []
  
  let show = show_params
end

module Args = struct
  let make normal rest labeled = {
    normal_args = normal;
    rest_arg = rest;
    labeled_args = labeled;
  }

  let n_ary normal =
    make normal None []

  let nullary =
    n_ary []

  let show = show_args
end
