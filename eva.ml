
open Printf

type t = {
  env : Value.Env.t;
  dummy : unit;
}

let create () = {
  env = Value.Env.create_global ();
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

let find_var env pos mods x =
  begin try
    Value.Env.find_var env mods x
  with
    | Value.Env.Module_not_found mod_name ->
      failwith (Pos.show_error pos (sprintf "module not found: %s\n" mod_name))
    | Value.Env.Not_a_module (mod_name, value) ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a module: %s\n" mod_name (Value.show value)))
  end

let find_method env pos mods klass sel =
  begin try
    Value.Env.find_method env mods klass sel
  with
    | Value.Env.Module_not_found mod_name ->
      failwith (Pos.show_error pos (sprintf "module not found: %s\n" mod_name))
    | Value.Env.Not_a_module (mod_name, value) ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a module: %s\n" mod_name (Value.show value)))
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
      let klass = begin try
        find_var eva.env pos mods2 klass
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "class not found: %s\n" (SnString.concat ":" (mods2 @ [klass]))))
      end
      in
      begin match klass with
        | Value.Class klass ->
          begin try
            find_method eva.env pos mods1 klass sel
          with
            | Not_found ->
              let pair = sprintf "(%s#%s)" klass sel in
              failwith (Pos.show_error pos (sprintf "method not found: %s\n" (SnString.concat ":" (mods1 @ [pair]))))
          end
        | _ ->
          failwith (Pos.show_error pos (sprintf "class required, but got: %s\n" (Value.show klass)))
      end
    | Expr.Lambda (params, body) ->
      Value.Closure (eva.env, params, body)
    | Expr.FunCall (func, args) ->
      let func = eval eva func in
      let args = List.map (eval eva) args in
      funcall eva pos func args
    | Expr.Block exprs ->
      let eva = { eva with env = Value.Env.create_local eva.env } in
      List.fold_left begin fun _ elem ->
        eval eva elem
      end Value.Unit exprs
    | Expr.Define (x, expr) ->
      let value = eval eva expr in
      begin
        Value.Env.add_var eva.env x value;
        value
      end
    | Expr.MethodCall (recv, sel, args) ->
      let recv = eval eva recv in
      let klass = Value.class_of recv in
      let meth = begin try
        find_method eva.env pos [] klass sel
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass sel))
      end
      in
      let args = List.map (eval eva) args in
      funcall eva pos meth args
  end

and funcall eva pos func args =
  begin match func with
    | Value.Closure (env, params, body) ->
      let env = Value.Env.create_local env in
      let eva = { eva with env = env } in
      begin try
        begin
          List.iter2 (Value.Env.add_var env) params args;
          List.fold_left begin fun _ elem ->
            eval eva elem
          end Value.Unit body
        end
      with
        | Invalid_argument _ ->
          let req_num = List.length params in
          let got_num = List.length args in
          let message = sprintf "wrong number of arguments: required %d, but got %d\n" req_num got_num in
          failwith (Pos.show_error pos message)
      end
    | _ ->
      failwith (Pos.show_error pos (sprintf "function required, but got: %s\n" (Value.show func)))
  end
