
open Printf

type t = {
  env : Value.Env.t;
  dummy : unit;
}

let create env = {
  env = env;
  dummy = ();
}

let value_of_literal lit =
  begin match lit with
    | Literal.Unit ->
      Value.Unit
    | Literal.Int i ->
      Value.Int i
    | Literal.String s ->
      Value.String s
    | Literal.Char c ->
      Value.Char c
    | Literal.Bool b ->
      Value.Bool b
  end

let wrong_number_of_arguments pos param_count arg_count =
  let message = sprintf "wrong number of arguments: required %d, but got %d\n" param_count arg_count in
  failwith (Pos.show_error pos message)

let required pos req_str got_value =
  Pos.show_error pos (sprintf "%s required, but got: %s\n" req_str (Value.show got_value))

let find_binding thunk pos =
  begin try
    thunk ()
  with
    | Value.Env.Module_not_found mod_name ->
      failwith (Pos.show_error pos (sprintf "module not found: %s\n" mod_name))
    | Value.Env.Not_a_module (mod_name, value) ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a module: %s\n" mod_name (Value.show value)))
  end

let find_var env pos mods x =
  find_binding (fun () -> Value.Env.find_var env mods x) pos

let find_method env pos mods klass sel =
  find_binding (fun () -> Value.Env.find_method env mods klass sel) pos

let find_klass env pos mods klass_name =
  let klass = begin try
    find_var env pos mods klass_name
  with
    | Not_found ->
      failwith (Pos.show_error pos (sprintf "class not found: %s\n" (SnString.concat ":" (mods @ [klass_name]))))
  end
  in
  begin match klass with
    | Value.Class klass ->
      klass
    | _ ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a class: %s\n" klass_name (Value.show klass)))
  end

let rec eval eva {Expr.pos;Expr.raw;} =
  begin match raw with
    | Expr.Const lit ->
      value_of_literal lit
    | Expr.Get (mods, Expr.Var x) ->
      begin try
        find_var eva.env pos mods x
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "variable not found: %s\n" (SnString.concat ":" (mods @ [x]))))
      end
    | Expr.Get (mods1, Expr.Method (mods2, klass, sel)) ->
      let klass = find_klass eva.env pos mods2 klass in
      begin try
        find_method eva.env pos mods1 klass (Selector.string_of sel)
      with
        | Not_found ->
          let pair = sprintf "%s#%s" klass (Selector.show sel) in
          let pair = if mods1 <> [] then sprintf "(%s)" pair else pair in
          failwith (Pos.show_error pos (sprintf "method not found: %s\n" (SnString.concat ":" (mods1 @ [pair]))))
      end
    | Expr.Def (Expr.Var x, expr) ->
      let value = eval eva expr in
      begin
        Value.Env.add_var eva.env x value;
        value
      end
    | Expr.Def (Expr.Method (mods, klass, sel), expr) ->
      let value = eval eva expr in
      let klass = find_klass eva.env pos mods klass in
      begin
        Value.Env.add_method eva.env klass (Selector.string_of sel) value;
        value
      end
    | Expr.Lambda (params, body) ->
      Value.Closure (eva.env, params, body)
    | Expr.FunCall (func, args) ->
      let func = eval eva func in
      let args = List.map (eval eva) args in
      funcall eva pos func args
    | Expr.MethodCall (recv, sel, args) ->
      let recv = eval eva recv in
      let klass = Value.class_of recv in
      let meth = begin try
        find_method eva.env pos [] klass (Selector.string_of sel)
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
      end
      in
      let args = List.map (eval eva) args in
      funcall eva pos meth (recv::args)
    | Expr.And (lhs, rhs) ->
      let lhs = eval eva lhs in
      begin match lhs with
        | Value.Bool false ->
          lhs
        | _ ->
          eval eva rhs
      end
    | Expr.Or (lhs, rhs) ->
      let lhs = eval eva lhs in
      begin match lhs with
        | Value.Bool false ->
          eval eva rhs
        | _ ->
          lhs
      end
  end

and funcall eva pos func args =
  let arg_count = List.length args in
  begin match func with
    | Value.Closure (env, params, body) ->
      let env = Value.Env.create_local env in
      let eva = { eva with env = env } in
      begin
        begin try
          List.iter2 (Value.Env.add_var env) params args
        with
          | Invalid_argument _ ->
            let param_count = List.length params in
            failwith (wrong_number_of_arguments pos param_count arg_count)
        end;
        List.fold_left begin fun _ elem ->
          eval eva elem
        end Value.Unit body
      end
    | Value.Subr (param_count, subr) ->
      if arg_count <> param_count then
        failwith (wrong_number_of_arguments pos param_count arg_count)
      else
        subr pos args
    | _ ->
      failwith (required pos "function" func)
  end

let int_of_value pos v =
  begin match v with
    | Value.Int i ->
      i
    | _ ->
      failwith (required pos "int" v)
  end

let bool_of_value pos v =
  begin match v with
    | Value.Bool b ->
      b
    | _ ->
      failwith (required pos "bool" v)
  end
