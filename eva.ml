
open Printf

module VarSet = Set.Make(struct type t = string let compare = compare end)
module MethodSet = Set.Make(struct type t = string * string let compare = compare end)

exception Error of Pos.t * string * Pos.t list

type t = {
  env : env;
  curr_mod_path : string list;  (* reversed *)
  dummy : unit;
}

and value =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Closure of env * Expr.Params.t * Expr.t list
  | Subr of int * bool * string list * string list * (t -> Pos.t -> args -> value)
  | Module of env
  | Class of string
  | Record of string * (string, value) Hashtbl.t
  | Variant of string * string * args
  | Trait of env * Expr.Params.t * Expr.t list
  | Tuple of args

and args = {
  normal_args : value list;
  keyword_args : (string * value) list;
}

and env =
  | Global of frame
  | Local of frame * env

and frame = {
  mutable exported_vars : VarSet.t;
  mutable exported_methods : MethodSet.t;
  var_table : (string, value) Hashtbl.t;
  method_table : (string * string, value) Hashtbl.t;
}

exception Match_failure of Pos.t * Expr.Pattern.t * value
exception Match_success of value

let rec show_value value =
  begin match value with
    | Unit ->
      "()"
    | Int i ->
      sprintf "%d" i
    | String s ->
      sprintf "%S" s
    | Char c ->
      sprintf "%C" c
    | Bool b ->
      sprintf "%B" b
    | Tuple args ->
      sprintf "%s" (show_args args)
    | Subr (_, _, _, _, _) ->
      "<subr>"
    | Trait (_,  _, _) ->
      "<trait>"
    | Closure (_,  _, _) ->
      "<closure>"
    | Module _ ->
      "<module>"
    | Class klass ->
      sprintf "<class %s>" klass
    | Record (klass, fields) ->
      sprintf "{%s}" (show_fields fields)
    | Variant (klass, ctor, args) ->
      sprintf "%s%s" ctor (show_args args)
  end

and show_fields table =
  let rev_strs = Hashtbl.fold begin fun key value acc ->
      (sprintf "%s=%s" key (show_value value))::acc
  end table []
  in
  SnString.concat ", " (List.rev rev_strs)

and show_args {normal_args;keyword_args;} =
  let str_normals = SnString.concat_map ", " show_value normal_args in
  let str_keywords = SnString.concat_map ", " show_keyword_arg keyword_args in
  if List.length normal_args <> 0 && List.length keyword_args <> 0 then
    sprintf "(%s, %s)" str_normals str_keywords
  else
    sprintf "(%s%s)" str_normals str_keywords

and show_keyword_arg (key, value) =
  sprintf ":%s %s" key (show_value value)

module Value = struct
  type t = value

  let class_of value =
    begin match value with
      | Unit ->
        "Unit::C"
      | Bool _ ->
        "Bool::C"
      | Int _ ->
        "Int::C"
      | Char _ ->
        "Char::C"
      | String _ ->
        "String::C"
      | Tuple _ ->
        "Tuple::C"
      | Closure (_,  _, _) ->
        "Proc::C"
      | Subr (_, _, _, _, _) ->
        "Proc::C"
      | Trait (_, _, _) ->
        "Proc::C"
      | Module _ ->
        "Module::C"
      | Class _ ->
        "Class::C"
      | Record (klass, _) ->
        klass
      | Variant (klass, _, _) ->
        klass
    end

  let show = show_value
end

module Args = struct
  type t = args

  let make normals keywords = {
    normal_args = normals;
    keyword_args = keywords;
  }

  let nth {normal_args} n =
    List.nth normal_args n

  let find {keyword_args} key =
    List.assoc key keyword_args

  let get args key =
    begin try
        Some (find args key)
      with
      | Not_found ->
        None
    end

  let show = show_args
end

module Frame = struct
  type t = frame

  exception Not_exported

  type var_or_method =
    | Var of string
    | Method of string * string

  type find_from =
    | Inside
    | Outside

  let create initial_table_size = {
    exported_vars = VarSet.empty;
    exported_methods = MethodSet.empty;
    var_table = Hashtbl.create initial_table_size;
    method_table = Hashtbl.create initial_table_size;
  }

  let print_all_bindings frame =
    Hashtbl.iter begin fun x v ->
      printf "%s = %s\n" x (Value.show v)
    end frame.var_table;
    Hashtbl.iter begin fun (k, s) v ->
      printf "%s#%s = %s\n" k s (Value.show v)
    end frame.method_table

  let find_var {exported_vars;var_table;} x from =
    begin match from with
      | Outside when not (VarSet.mem x exported_vars) ->
        raise Not_exported
      | _ ->
        Hashtbl.find var_table x
    end

  let find_method {exported_methods;method_table;} klass sel from =
    begin match from with
      | Outside when not (MethodSet.mem (klass, sel) exported_methods) ->
        raise Not_exported
      | _ ->
        Hashtbl.find method_table (klass, sel)
    end

  let find_binding frame vom from =
    begin match vom with
      | Var x ->
        find_var frame x from
      | Method (klass, sel) ->
        find_method frame klass sel from
    end

  let export_var frame x =
    if Hashtbl.mem frame.var_table x then
      frame.exported_vars <- VarSet.add x frame.exported_vars
    else
      raise Not_found

  let export_method frame klass sel =
    if Hashtbl.mem frame.method_table (klass, sel) then
      frame.exported_methods <- MethodSet.add (klass, sel) frame.exported_methods
    else
      raise Not_found

  let add_var frame x v export =
    Hashtbl.add frame.var_table x v;
    if export then
      export_var frame x

  let add_method frame klass sel meth export =
    Hashtbl.add frame.method_table (klass, sel) meth;
    if export then
      export_method frame klass sel

  let open_module frame modl =
    VarSet.iter begin fun x ->
      add_var frame x (find_var modl x Outside) false
    end modl.exported_vars;
    MethodSet.iter begin fun (klass, sel) ->
      add_method frame klass sel (find_method modl klass sel Outside) false
    end modl.exported_methods

  let include_module frame modl =
    open_module frame modl;
    VarSet.iter begin fun x ->
      export_var frame x
    end modl.exported_vars;
    MethodSet.iter begin fun (klass, sel) ->
      export_method frame klass sel
    end modl.exported_methods

  let unexport_var frame x =
    if VarSet.mem x frame.exported_vars then
      frame.exported_vars <- VarSet.remove x frame.exported_vars
    else
      raise Not_found

  let unexport_method frame klass sel =
    if MethodSet.mem (klass, sel) frame.exported_methods then
      frame.exported_methods <- MethodSet.remove (klass, sel) frame.exported_methods
    else
      raise Not_found
end

module Env = struct
  type t = env

  exception Module_not_found of string
  exception Not_a_module of string * value

  let initial_global_table_size = 16
  let initial_local_table_size = 4

  let create_global () = Global (Frame.create initial_global_table_size)
  let create_local outer = Local (Frame.create initial_local_table_size, outer)

  let rec lookup env vom =
    begin match env with
      | Global frame ->
        Frame.find_binding frame vom Frame.Inside
      | Local (frame, outer) ->
        begin try
            Frame.find_binding frame vom Frame.Inside
          with
          | Not_found ->
            lookup outer vom
        end
    end

  let with_current_frame proc env =
    begin match env with
      | Global frame ->
        proc frame 
      | Local (frame, _) ->
        proc frame
    end

  let rec find_module_binding modl mods vom =
    begin match mods with
      | [] ->
        with_current_frame (fun frame -> Frame.find_binding frame vom Frame.Outside) modl
      | mod_name::mods ->
        let modl = begin try
            with_current_frame (fun frame -> Frame.find_var frame mod_name Frame.Outside) modl
          with
          | Not_found ->
            raise (Module_not_found mod_name)
        end
        in
        begin match modl with
          | Module modl ->
            find_module_binding modl mods vom
          | _ ->
            raise (Not_a_module (mod_name, modl))
        end
    end

  let find_binding env mods vom =
    begin match mods with
      | [] ->
        lookup env vom
      | mod_name::mods ->
        let modl = begin try
            lookup env (Frame.Var mod_name)
          with
          | Not_found ->
            raise (Module_not_found mod_name)
        end
        in
        begin match modl with
          | Module modl ->
            find_module_binding modl mods vom
          | _ ->
            raise (Not_a_module (mod_name, modl))
        end
    end

  let find_var env mods x =
    find_binding env mods (Frame.Var x)

  let find_method env mods klass sel =
    find_binding env mods (Frame.Method (klass, sel))

  let add_var ?(export=false) env x v =
    with_current_frame (fun frame -> Frame.add_var frame x v export) env

  let add_method ?(export=false) env klass sel meth =
    with_current_frame (fun frame -> Frame.add_method frame klass sel meth export) env

  let export_var env x =
    with_current_frame (fun frame -> Frame.export_var frame x) env

  let export_method env klass sel =
    with_current_frame (fun frame -> Frame.export_method frame klass sel) env

  let unexport_var env x =
    with_current_frame (fun frame -> Frame.unexport_var frame x) env

  let unexport_method env klass sel =
    with_current_frame (fun frame -> Frame.unexport_method frame klass sel) env

  let open_module env modl =
    with_current_frame begin fun frame_env ->
      with_current_frame begin fun frame_mod ->
        Frame.open_module frame_env frame_mod
      end modl
    end env

  let include_module env modl =
    with_current_frame begin fun frame_env ->
      with_current_frame begin fun frame_mod ->
        Frame.include_module frame_env frame_mod
      end modl
    end env

  let rec print_all_bindings env =
    begin match env with
      | Global frame ->
        Frame.print_all_bindings frame
      | Local (frame, outer) ->
        Frame.print_all_bindings frame;
        printf "---\n";
        print_all_bindings outer;
    end
end

let create () = {
  env = Env.create_global ();
  curr_mod_path = [];
  dummy = ();
}

let create_subr req_count ?(allows_rest=false) ?(req_keys=[]) ?(opt_keys=[]) proc =
  Subr (req_count, allows_rest, req_keys, opt_keys, proc)

let initial_field_table_size = 4
let initial_param_table_size = 4

let required pos req_str got_value =
  Error (pos, sprintf "%s required, but got: %s\n" req_str (Value.show got_value), [])

let wrong_number_of_arguments pos param_count arg_count =
  let message = sprintf "wrong number of arguments: required %d, but got %d\n" param_count arg_count in
  Error (pos, message, [])

let lack_of_keyword_argument pos key =
  let message = sprintf "lack of keyword argument: %s\n" key in
  Error (pos, message, [])

let extra_keyword_arguments pos rest_keyword_args =
  let message = sprintf "extra keyword arguments: %s\n" (SnString.concat_map ", " fst rest_keyword_args) in
  Error (pos, message, [])

let match_failure pos pat value =
  let pos = pat.Expr.pat_pos in
  let message = sprintf "match failure of %s with pattern %s\n" (Value.show value) (Expr.Pattern.show pat) in
  Error (pos, message, [])

let find_binding thunk pos =
  begin try
      thunk ()
    with
    | Env.Module_not_found mod_name ->
      raise (Error (pos, sprintf "module not found: %s\n" mod_name, []))
    | Env.Not_a_module (mod_name, value) ->
      raise (Error (pos, sprintf "'%s' is not a module: %s\n" mod_name (Value.show value), []))
  end

let find_var env pos mods x =
  begin try
      find_binding (fun () -> Env.find_var env mods x) pos
    with
    | Frame.Not_exported ->
      raise (Error (pos, sprintf "'%s' is not exported from '%s'\n" x (SnString.concat "::" mods), []))
  end

let find_method env pos mods klass sel =
  begin try
      find_binding (fun () -> Env.find_method env mods klass (Selector.string_of sel)) pos
    with
    | Frame.Not_exported ->
      raise (Error (pos, sprintf "'%s#%s' is not exported from '%s'\n" klass (Selector.show sel) (SnString.concat "::" mods), []))
  end

let find_klass env pos mods klass_name =
  let klass = begin try
      find_var env pos mods klass_name
    with
    | Not_found ->
      raise (Error (pos, sprintf "class not found: %s\n" (SnString.concat "::" (mods @ [klass_name])), []))
  end
  in
  begin match klass with
    | Class klass ->
      klass
    | _ ->
      raise (Error (pos, sprintf "'%s' is not a class: %s\n" klass_name (Value.show klass), []))
  end

let make_record_ctor klass fields =
  create_subr (List.length fields) begin fun eva pos args ->
    let table = Hashtbl.create initial_field_table_size in
    List.iter2 begin fun (field, _) arg ->
      Hashtbl.add table field arg
    end fields args.normal_args;
    Record (klass, table)
  end

let make_getter klass field =
  create_subr 1 begin fun eva pos args ->
    let self = Args.nth args 0 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        Hashtbl.find table field
      | _ ->
        raise (required pos (sprintf "some instance of %s" klass) self)
    end
  end      

let make_setter klass field =
  create_subr 2 begin fun eva pos args ->
    let self = Args.nth args 0 in
    let value = Args.nth args 1 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        Hashtbl.replace table field value;
        value
      | _ ->
        raise (required pos (sprintf "some instance of %s" klass) self)
    end
  end     

let add_accessors env klass fields =
  List.iter begin fun (field, mutabl) ->
    Env.add_method env klass field (make_getter klass field);
    if mutabl then
      Env.add_method env klass (sprintf "%s=" field) (make_setter klass field)
  end fields

let make_variant_ctor klass ctor_name params =
  create_subr (List.length params.Expr.normal_params) begin fun eva pos args ->
    Variant (klass, ctor_name, args)
  end

let add_ctors env klass ctors =
  List.iter begin fun (ctor_name, params) ->
    Env.add_var env ctor_name (make_variant_ctor klass ctor_name params);
  end ctors

let value_of_literal lit =
  begin match lit with
    | Literal.Unit ->
      Unit
    | Literal.Int i ->
      Int i
    | Literal.String s ->
      String s
    | Literal.Char c ->
      Char c
    | Literal.Bool b ->
      Bool b
  end

let int_of_value pos v =
  begin match v with
    | Int i ->
      i
    | _ ->
      raise (required pos "int" v)
  end

let unit_of_value pos v =
  begin match v with
    | Unit ->
      ()
    | _ ->
      raise (required pos "unit" v)
  end

let bool_of_value pos v =
  begin match v with
    | Bool b ->
      b
    | _ ->
      raise (required pos "bool" v)
  end

let char_of_value pos v =
  begin match v with
    | Char c ->
      c
    | _ ->
      raise (required pos "char" v)
  end

let string_of_value pos v =
  begin match v with
    | String str ->
      str
    | _ ->
      raise (required pos "string" v)
  end

let value_of_int i = Int i
let value_of_unit u = Unit
let value_of_bool b = Bool b
let value_of_char c = Char c
let value_of_string str = String str

let rec eval eva {Expr.pos;Expr.raw;} =
  begin match raw with
    | Expr.Const lit ->
      value_of_literal lit
    | Expr.Get (mods, VarOrMethod.Var x) ->
      begin try
          find_var eva.env pos mods x
        with
        | Not_found ->
          raise (Error (pos, sprintf "variable not found: %s\n" (SnString.concat "::" (mods @ [x])), []))
      end
    | Expr.Get (mods1, VarOrMethod.Method (mods2, klass, sel)) ->
      let klass = find_klass eva.env pos mods2 klass in
      begin try
          find_method eva.env pos mods1 klass sel
        with
        | Not_found ->
          let pair = sprintf "%s#%s" klass (Selector.show sel) in
          let pair = if mods1 <> [] then sprintf "(%s)" pair else pair in
          raise (Error (pos, sprintf "method not found: %s\n" (SnString.concat "::" (mods1 @ [pair])), []))
      end
    | Expr.Let (pat, expr) ->
      let value = eval eva expr in
      begin try
          bind_param eva pos pat value
        with
        | Match_failure (pos, pat, value) ->
          raise (match_failure pos pat value)
      end;
      value
    | Expr.Lambda (params, body) ->
      Closure (eva.env, params, body)
    | Expr.FunCall (func, args) ->
      let func = eval eva func in
      let args = eval_args eva args in
      call_fun eva pos func args
    | Expr.MethodCall (recv, sel, args) ->
      let recv = eval eva recv in
      let args = eval_args eva args in
      call_method eva pos recv sel args
    | Expr.And (lhs, rhs) ->
      let lhs = eval eva lhs in
      begin match lhs with
        | Bool false ->
          lhs
        | _ ->
          eval eva rhs
      end
    | Expr.Or (lhs, rhs) ->
      let lhs = eval eva lhs in
      begin match lhs with
        | Bool false ->
          eval eva rhs
        | _ ->
          lhs
      end
    | Expr.Module (name, exprs) ->
      let env_in_mod = Env.create_local eva.env in
      let eva_in_mod = { eva with env = env_in_mod; curr_mod_path = name::eva.curr_mod_path } in
      let modl = Module env_in_mod in
      Env.add_var eva.env name modl;
      ignore (eval_exprs eva_in_mod exprs);
      modl
    | Expr.Export voms ->
      List.iter begin function
        | VarOrMethod.Var x ->
          begin try
              Env.export_var eva.env x;
            with
            | Not_found ->
              raise (Error (pos, sprintf "variable not found: %s\n" x, []))
          end
        | VarOrMethod.Method (mods, klass, sel) ->
          let klass = find_klass eva.env pos mods klass in
          begin try
              Env.export_method eva.env klass (Selector.string_of sel);
            with
            | Not_found ->
              raise (Error (pos, sprintf "method not found: %s#%s\n" klass (Selector.show sel), []))
          end
      end voms;
      Unit
    | Expr.Open expr ->
      let value = eval eva expr in
      begin match value with
        | Module modl ->
          Env.open_module eva.env modl;
          value
        | _ ->
          raise (required pos "module" value)
      end
    | Expr.Include expr ->
      let value = eval eva expr in
      begin match value with
        | Module modl ->
          Env.include_module eva.env modl;
          value
        | _ ->
          raise (required pos "module" value)
      end
    | Expr.Record (klass_name, ctor_name, fields) ->
      let klass = SnString.concat "::" (List.rev (klass_name::eva.curr_mod_path)) in
      Env.add_var eva.env klass_name (Class klass);
      Env.add_var eva.env ctor_name (make_record_ctor klass fields);
      add_accessors eva.env klass fields;
      Class klass
    | Expr.Variant (klass_name, ctors) ->
      let klass = SnString.concat "::" (List.rev (klass_name::eva.curr_mod_path)) in
      Env.add_var eva.env klass_name (Class klass);
      add_ctors eva.env klass ctors;
      Class klass
    | Expr.Phantom klass_name ->
      let klass = SnString.concat "::" (List.rev (klass_name::eva.curr_mod_path)) in
      Env.add_var eva.env klass_name (Class klass);
      Class klass
    | Expr.Trait (params, body) ->
      Trait (eva.env, params, body)
    | Expr.Except (modl, voms) ->
      let modl = eval eva modl in
      begin match modl with
        | Module modl ->
          List.iter begin function
            | VarOrMethod.Var x ->
              begin try
                  Env.unexport_var modl x;
                with
                | Not_found ->
                  raise (Error (pos, sprintf "variable not found: %s\n" x, []))
              end
            | VarOrMethod.Method (mods, klass, sel) ->
              let klass = find_klass eva.env pos mods klass in
              begin try
                  Env.unexport_method modl klass (Selector.string_of sel);
                with
                | Not_found ->
                  raise (Error (pos, sprintf "method not found: %s#%s\n" klass (Selector.show sel), []))
              end
          end voms
        | _ ->
          raise (required pos "module" modl)
      end;
      modl
    | Expr.Match (target, cases) ->
      let target = eval eva target in
      begin try
          List.iter begin fun (pat, guard, body) ->
            begin try
                let env = Env.create_local eva.env in
                let eva = { eva with env = env } in
                bind_param eva pos pat target;
                begin match guard with
                  | Some guard when not (bool_of_value pos (eval eva guard)) ->
                    raise (Match_failure (pos, pat, target))
                  | None | Some _ ->
                    raise (Match_success (eval_exprs eva body))
                end
              with
              | Match_failure (pos, pat, value) ->
                ()
            end
          end cases;
          raise (Error (pos, sprintf "match failure of %s\n" (Value.show target), []))
        with
        | Match_success result ->
          result
      end
    | Expr.Tuple args ->
      let args = eval_args eva args in
      Tuple args
  end

and eval_args eva {Expr.normal_args;Expr.keyword_args;} = {
  normal_args = List.map (eval eva) normal_args;
  keyword_args = List.map (fun (key, expr) -> (key, eval eva expr)) keyword_args;
}

and eval_exprs eva exprs =
  List.fold_left begin fun _ elem ->
    eval eva elem
  end Unit exprs

and call_fun eva pos func args =
  begin match func with
    | Subr (required_count, allows_rest, req_keys, opt_keys, subr) ->
      call_subr eva pos required_count allows_rest req_keys opt_keys subr args
    | Closure (env, params, body) ->
        begin try
            call_closure eva pos env params body args
          with
          | Error (pos_error, message, rev_stack_trace) ->
            raise (Error (pos_error, message, pos::rev_stack_trace))
        end
    | Trait (env, params, body) ->
        begin try
            call_trait eva pos env params body args
          with
          | Error (pos_error, message, rev_stack_trace) ->
            raise (Error (pos_error, message, pos::rev_stack_trace))
        end
    | _ ->
      raise (required pos "function" func)
  end

and call_closure eva pos env params body args =
  let env = Env.create_local env in
  let eva = { eva with env = env } in
  begin try
      bind_params eva pos params args;
    with
    | Match_failure (pos, pat, value) ->
      raise (match_failure pos pat value)
  end;
  eval_exprs eva body

and call_subr eva pos required_count allows_rest req_keys opt_keys subr args =
  let arg_count = List.length args.normal_args in
  begin if arg_count < required_count || arg_count > required_count && not allows_rest then
      raise (wrong_number_of_arguments pos required_count arg_count)
  end;
  let rest_keyword_args = List.fold_left begin fun keyword_args req_key ->
      if List.mem_assoc req_key keyword_args then
        List.remove_assoc req_key keyword_args
      else
        raise (lack_of_keyword_argument pos req_key)
    end args.keyword_args req_keys
  in
  let rest_keyword_args = List.fold_left begin fun keyword_args opt_key ->
      List.remove_assoc opt_key keyword_args
    end rest_keyword_args opt_keys
  in
  begin if List.length rest_keyword_args <> 0 then
      raise (extra_keyword_arguments pos rest_keyword_args)
  end;
  subr eva pos args

and call_trait eva pos env params body args =
  let env = Env.create_local env in
  let eva = { eva with env = env } in
  begin try
      bind_params eva pos params args;
    with
    | Match_failure (pos, pat, value) ->
      raise (match_failure pos pat value)
  end;
  ignore (eval_exprs eva body);
  Module env;
        
and call_method eva pos recv sel args =
  let klass = Value.class_of recv in
  let meth = begin try
      find_method eva.env pos [] klass sel
    with
    | Not_found ->
      raise (Error (pos, sprintf "method not found: %s#%s\n" klass (Selector.show sel), []))
  end
  in
  call_fun eva pos meth {args with normal_args = recv::args.normal_args}

and bind_params eva pos params args =
  begin try
      List.iter2 (bind_param eva pos) params.Expr.normal_params args.normal_args
    with
    | Invalid_argument _ ->
      let arg_count = List.length args.normal_args in
      let param_count = List.length params.Expr.normal_params in
      raise (wrong_number_of_arguments pos param_count arg_count)
  end;
  let rest_keyword_args = List.fold_left begin fun keyword_args (key, (param, opt_expr)) ->
      begin match opt_expr with
        | _ when List.mem_assoc key keyword_args ->
          let arg = List.assoc key keyword_args in
          bind_param eva pos param arg;
          List.remove_assoc key keyword_args
        | Some expr ->
          let arg = eval eva expr in
          bind_param eva pos param arg;
          keyword_args
        | None ->
          raise (lack_of_keyword_argument pos key)
      end
    end args.keyword_args params.Expr.keyword_params
  in
  if List.length rest_keyword_args <> 0 then
    raise (extra_keyword_arguments pos rest_keyword_args)

and bind_param eva pos pat value =
  begin match pat.Expr.pat_raw with
    | Expr.PatWildCard ->
      ()
    | Expr.PatConst lit ->
      if value = value_of_literal lit then
        ()
      else
        raise (Match_failure (pos, pat, value))
    | Expr.PatBind (VarOrMethod.Var x) ->
      Env.add_var eva.env x value
    | Expr.PatBind (VarOrMethod.Method (mods, klass, sel)) ->
      let klass = find_klass eva.env pos mods klass in
      Env.add_method eva.env klass (Selector.string_of sel) value
    | Expr.PatOr (lhs, rhs) ->
      begin try
          bind_param eva pos lhs value
        with
        | Failure message ->
          bind_param eva pos rhs value
      end
    | Expr.PatAs (pat, x) ->
      bind_param eva pos pat value;
      Env.add_var eva.env x value
    | Expr.PatVariant (ctor_req, params) ->
      begin match value with
        | Variant (_, ctor_got, values) when ctor_got = ctor_req ->
          bind_params eva pos params values
        | _ ->
          raise (Match_failure (pos, pat, value))
      end
    | Expr.PatTuple params ->
      begin match value with
        | Tuple args ->
          bind_params eva pos params args
        | _ ->
          raise (Match_failure (pos, pat, value))
      end
  end
