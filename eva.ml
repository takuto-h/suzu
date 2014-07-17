
open Printf

type t = {
  env : Value.Env.t;
  curr_mod_path : string list;  (* reversed *)
  dummy : unit;
}

let create env = {
  env = env;
  curr_mod_path = [];
  dummy = ();
}

let initial_field_table_size = 4

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
  begin try
    find_binding (fun () -> Value.Env.find_var env mods x) pos
  with
    | Value.Env.Not_exported ->
      failwith (Pos.show_error pos (sprintf "'%s' is not exported from '%s'\n" x (SnString.concat ":" mods)))
  end

let find_method env pos mods klass sel =
  begin try
    find_binding (fun () -> Value.Env.find_method env mods klass (Selector.string_of sel)) pos
  with
    | Value.Env.Not_exported ->
      failwith (Pos.show_error pos (sprintf "'%s#%s' is not exported from '%s'\n" klass (Selector.show sel) (SnString.concat ":" mods)))
  end

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

let make_ctor klass fields =
  Value.Subr begin (List.length fields), fun pos args ->
    let table = Hashtbl.create initial_field_table_size in
    begin
      List.iter2 begin fun (field, _) arg ->
        Hashtbl.add table field arg
      end fields args;
      Value.Record (klass, table)
    end
  end

let make_getter klass field =
  Value.Subr begin 1, fun pos args ->
    let self = List.nth args 0 in
    begin match self with
      | Value.Record (klass2, table) when klass2 = klass ->
        Hashtbl.find table field
      | _ ->
        failwith (required pos (sprintf "some instance of %s" klass) self)
    end
  end      

let make_setter klass field =
  Value.Subr begin 2, fun pos args ->
    let self = List.nth args 0 in
    let value = List.nth args 1 in
    begin match self with
      | Value.Record (klass2, table) when klass2 = klass ->
        begin
          Hashtbl.replace table field value;
          value
        end
      | _ ->
        failwith (required pos (sprintf "some instance of %s" klass) self)
    end
  end      

let add_accessors env klass fields =
  List.iter begin fun (field, mutabl) ->
    begin
      Value.Env.add_method env klass field (make_getter klass field);
      begin if mutabl then
        Value.Env.add_method env klass (sprintf "%s=" field) (make_setter klass field)
      end
    end
  end fields

let bind_params env pos params args =
  begin try
    List.iter2 (Value.Env.add_var env) params args
  with
    | Invalid_argument _ ->
      let arg_count = List.length args in
      let param_count = List.length params in
      failwith (wrong_number_of_arguments pos param_count arg_count)
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
        find_method eva.env pos mods1 klass sel
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
        find_method eva.env pos [] klass sel
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
    | Expr.Module (name, exprs) ->
      let env_in_mod = Value.Env.create_local eva.env in
      let eva_in_mod = { eva with env = env_in_mod; curr_mod_path = name::eva.curr_mod_path } in
      let modl = Value.Module env_in_mod in
      begin
        Value.Env.add_var eva.env name modl;
        List.iter begin fun elem ->
          ignore (eval eva_in_mod elem)
        end exprs;
        modl
      end
    | Expr.Export voms ->
      begin
        List.iter begin function
          | Expr.Var x ->
            begin try
              Value.Env.export_var eva.env x;
            with
              | Not_found ->
                failwith (Pos.show_error pos (sprintf "variable not found: %s\n" x))
            end
          | Expr.Method (mods, klass, sel) ->
            let klass = find_klass eva.env pos mods klass in
            begin try
              Value.Env.export_method eva.env klass (Selector.string_of sel);
            with
              | Not_found ->
                failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
            end
        end voms;
        Value.Unit
      end
    | Expr.Open expr ->
      let value = eval eva expr in
      begin match value with
        | Value.Module modl ->
          begin
            Value.Env.open_module eva.env modl;
            value
          end
        | _ ->
          failwith (required pos "module" value)
      end
    | Expr.Record (klass_name, ctor_name, fields) ->
      let klass = SnString.concat ":" (List.rev (klass_name::eva.curr_mod_path)) in
      begin
        Value.Env.add_var eva.env klass_name (Value.Class klass);
        Value.Env.add_var eva.env ctor_name (make_ctor klass fields);
        add_accessors eva.env klass fields;
        Value.Class klass
      end
    | Expr.Trait (params, body) ->
      Value.Trait (eva.env, params, body)
  end

and funcall eva pos func args =
  begin match func with
    | Value.Closure (env, params, body) ->
      let env = Value.Env.create_local env in
      let eva = { eva with env = env } in
      begin
        bind_params env pos params args;
        List.fold_left begin fun _ elem ->
          eval eva elem
        end Value.Unit body
      end
    | Value.Subr (param_count, subr) ->
      let arg_count = List.length args in
      if arg_count <> param_count then
        failwith (wrong_number_of_arguments pos param_count arg_count)
      else
        subr pos args
    | Value.Trait (env, params, body) ->
      let env = Value.Env.create_local env in
      let eva = { eva with env = env } in
      begin
        bind_params env pos params args;
        List.iter begin fun elem ->
          ignore (eval eva elem)
        end body;
        Value.Module env;
      end
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

let string_of_value pos v =
  begin match v with
    | Value.String str ->
      str
    | _ ->
      failwith (required pos "string" v)
  end
