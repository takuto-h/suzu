
open Printf

type export = bool

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * VarOrMethod.t
  | Let of pat * t
  | Lambda of params * t list
  | FunCall of t * args
  | MethodCall of t * Selector.t * args
  | And of t * t
  | Or of t * t
  | Module of string * t list
  | Export of VarOrMethod.t list
  | Open of t
  | Include of t
  | Record of string * string * (string * bool) list
  | Variant of string * (string * params) list
  | Phantom of string
  | Trait of params * t list
  | Except of t * VarOrMethod.t list
  | Match of args * (params * t option * t list) list
  | Args of args
  | TryFinally of t * t list
  | TryCatch of t * (pat * t list) list
  | Throw of t

and pat = {
  pat_pos : Pos.t;
  pat_raw : pat_raw;
}

and pat_raw = 
  | PatWildCard
  | PatConst of Literal.t
  | PatBind of VarOrMethod.t
  | PatOr of pat * pat
  | PatAs of pat * string
  | PatVariant of string * params
  | PatParams of params

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

let rec show_pattern {pat_raw} =
  begin match pat_raw with
    | PatWildCard ->
      "_"
    | PatConst lit ->
      Literal.show lit
    | PatBind vom ->
      VarOrMethod.show vom
    | PatOr (lhs, rhs) ->
      sprintf "(%s | %s)" (show_pattern lhs) (show_pattern rhs)
    | PatAs (pat, x) ->
      sprintf "(%s as %s)" (show_pattern pat) x
    | PatVariant (ctor, params) ->
      sprintf "%s%s" ctor (show_params params)
    | PatParams params ->
      sprintf "%s" (show_params params)
  end

and show_params {normal_params;rest_param;labeled_params;} =
  let str_normal = SnString.concat_map ", " show_pattern normal_params in
  let str_labeled = SnString.concat_map ", " show_labeled_param labeled_params in
  begin match rest_param with
    | Some rest_param ->
      let str_rest = sprintf "*%s" (show_pattern rest_param) in
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_rest; str_labeled])
    | None ->
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_labeled])
  end

and show_labeled_param (label, (pat, _)) =
  sprintf ":%s %s = <expr>" label (show_pattern pat)

let show_field (field, mutabl) =
  if mutabl then
    sprintf "(mutable %s)" field
  else
    field

let show_ctor (ctor_name, params) =
  sprintf "(%s %s)" ctor_name (show_params params)

let rec show {raw} =
  begin match raw with
    | Const lit ->
      sprintf "(Const %s)" (Literal.show lit)
    | Get (mods, vom) ->
      sprintf "(Get %s %s)" (SnString.concat " " mods) (VarOrMethod.show vom)
    | Let (pat, expr) ->
      sprintf "(Let %s %s)" (show_pattern pat) (show expr)
    | Lambda (params, body) ->
      sprintf "(Lambda %s %s)" (show_params params) (SnString.concat_map " " show body)
    | FunCall (func, args) ->
      sprintf "(FunCall %s %s)" (show func) (show_args args)
    | MethodCall (recv, sel, args) ->
      sprintf "(MethodCall %s %s %s)" (show recv) (Selector.show sel) (show_args args)
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
    | Include expr ->
      sprintf "(Include %s)" (show expr)
    | Record (klass, ctor, fields) ->
      sprintf "(Record %s %s %s)" klass ctor (SnString.concat_map " " show_field fields)
    | Variant (klass, ctors) ->
      sprintf "(Variant %s %s)" klass (SnString.concat_map " " show_ctor ctors)
    | Phantom klass ->
      sprintf "(Phantom %s)" klass
    | Trait (params, body) ->
      sprintf "(Trait %s %s)" (show_params params) (SnString.concat_map " " show body)
    | Except (modl, voms) ->
      sprintf "(Export %s %s)" (show modl) (SnString.concat_map " " VarOrMethod.show voms)
    | Match (args, cases) ->
      sprintf "(Match %s %s)" (show_args args) (SnString.concat_map " " show_case cases)
    | Args args ->
      sprintf "(Args %s)" (show_args args)
    | TryFinally (body, finally) ->
      sprintf "(TryFinally %s (%s))" (show body) (SnString.concat_map " " show finally)
    | TryCatch (body, catches) ->
      sprintf "(TryCatch %s (%s))" (show body) (SnString.concat_map " " show_catch catches)
    | Throw expr ->
      sprintf "(Throw %s)" (show expr)
  end

and show_case (params, guard, body) =
  begin match guard with
    | Some cond ->
      sprintf "(Case %s (Some %s) %s)" (show_params params) (show cond) (SnString.concat_map " " show body)
    | None ->
      sprintf "(Case %s None %s)" (show_params params) (SnString.concat_map " " show body)
  end

and show_args {normal_args;rest_arg;labeled_args} =
  let str_normal = SnString.concat_map ", " show normal_args in
  let str_labeled = SnString.concat_map ", " show_labeled_arg labeled_args in
  begin match rest_arg with
    | Some rest_arg ->
      let str_rest = sprintf "*%s" (show rest_arg) in
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_rest; str_labeled])
    | None ->
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_labeled])
  end

and show_labeled_arg (label, value) =
  sprintf ":%s %s" label (show value)

and show_catch (pat, body) =
  sprintf "(Catch %s %s)" (show_pattern pat) (SnString.concat_map " " show body)

module Pattern = struct
  type t = pat

  let at pos raw = {
    pat_pos = pos;
    pat_raw = raw;
  }

  let show = show_pattern
end

module Params = struct
  type t = params

  let make normal rest labeled = {
    normal_params = normal;
    rest_param = rest;
    labeled_params = labeled;
  }
  
  let show = show_params
end

module Args = struct
  type t = args

  let make normal rest labeled = {
    normal_args = normal;
    rest_arg = rest;
    labeled_args = labeled;
  }
  
  let show = show_args
end
