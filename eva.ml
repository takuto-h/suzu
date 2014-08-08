
open Printf

module VarSet = Set.Make(struct type t = string let compare = compare end)
module MethodSet = Set.Make(struct type t = string * string let compare = compare end)

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
  | Subr of int * bool * (t -> Pos.t -> args -> value)
  | Module of env
  | Class of string
  | Record of string * (string, value) Hashtbl.t
  | Variant of string * string * args
  | Trait of env * Expr.Params.t * Expr.t list

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

exception Match_success of value

module Value = struct
  type t = value

  let class_of value =
    begin match value with
      | Unit ->
        "Unit:C"
      | Int _ ->
        "Int:C"
      | String _ ->
        "String:C"
      | Char _ ->
        "Char:C"
      | Bool _ ->
        "Bool:C"
      | Closure (_,  _, _) ->
        "Closure:C"
      | Subr (_, _, _) ->
        "Subr:C"
      | Module _ ->
        "Module:C"
      | Class _ ->
        "Class:C"
      | Record (klass, _) ->
        klass
      | Variant (klass, _, _) ->
        klass
      | Trait (_, _, _) ->
        "Trait:C"
    end

  let show value =
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
      | Closure (_,  _, _) ->
        "<closure>"
      | Subr (_, _, _) ->
        "<subr>"
      | Module _ ->
        "<module>"
      | Class klass ->
        sprintf "<class %s>" klass
      | Record (klass, _) ->
        sprintf "<record %s>" klass
      | Variant (klass, _, _) ->
        sprintf "<variant %s>" klass
      | Trait (_,  _, _) ->
        "<trait>"
    end
end

module Args = struct
  type t = args

  let make normals keywords = {
    normal_args = normals;
    keyword_args = keywords;
  }

  let nth {normal_args} n =
    List.nth normal_args n
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

let create env = {
  env = env;
  curr_mod_path = [];
  dummy = ();
}

let create_subr req_count ?(allows_rest=false) proc =
  Subr (req_count, allows_rest, proc)

let initial_field_table_size = 4
let initial_param_table_size = 4

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

let required pos req_str got_value =
  Pos.show_error pos (sprintf "%s required, but got: %s\n" req_str (Value.show got_value))

let wrong_number_of_arguments pos param_count arg_count =
  let message = sprintf "wrong number of arguments: required %d, but got %d\n" param_count arg_count in
  failwith (Pos.show_error pos message)

let match_failure pos pat value =
  let message = Pos.show_error pos (sprintf "match failure of %s\n" (Value.show value)) in
  let message = message ^ Pos.show_message pat.Expr.pat_pos (sprintf "with pattern %s\n" (Expr.Pattern.show pat)) in
  failwith message

let find_binding thunk pos =
  begin try
      thunk ()
    with
    | Env.Module_not_found mod_name ->
      failwith (Pos.show_error pos (sprintf "module not found: %s\n" mod_name))
    | Env.Not_a_module (mod_name, value) ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a module: %s\n" mod_name (Value.show value)))
  end

let find_var env pos mods x =
  begin try
      find_binding (fun () -> Env.find_var env mods x) pos
    with
    | Frame.Not_exported ->
      failwith (Pos.show_error pos (sprintf "'%s' is not exported from '%s'\n" x (SnString.concat ":" mods)))
  end

let find_method env pos mods klass sel =
  begin try
      find_binding (fun () -> Env.find_method env mods klass (Selector.string_of sel)) pos
    with
    | Frame.Not_exported ->
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
    | Class klass ->
      klass
    | _ ->
      failwith (Pos.show_error pos (sprintf "'%s' is not a class: %s\n" klass_name (Value.show klass)))
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
        failwith (required pos (sprintf "some instance of %s" klass) self)
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
        failwith (required pos (sprintf "some instance of %s" klass) self)
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

let rec bind_param env pos pat value =
  begin match pat.Expr.pat_raw with
    | Expr.PatWildCard ->
      ()
    | Expr.PatConst lit ->
      if value = value_of_literal lit then
        ()
      else
        failwith (match_failure pos pat value)
    | Expr.PatBind (VarOrMethod.Var x) ->
      Env.add_var env x value
    | Expr.PatBind (VarOrMethod.Method (mods, klass, sel)) ->
      let klass = find_klass env pos mods klass in
      Env.add_method env klass (Selector.string_of sel) value
    | Expr.PatOr (lhs, rhs) ->
      begin try
          bind_param env pos lhs value
        with
        | Failure message ->
          bind_param env pos rhs value
      end
    | Expr.PatAs (pat, x) ->
      bind_param env pos pat value;
      Env.add_var env x value
    | Expr.PatVariant (ctor_req, params) ->
      begin match value with
        | Variant (_, ctor_got, values) when ctor_got = ctor_req ->
          bind_params env pos params values
        | _ ->
          failwith (match_failure pos pat value)
      end
  end

and bind_params env pos params args =
  begin try
      List.iter2 (bind_param env pos) params.Expr.normal_params args.normal_args
    with
    | Invalid_argument _ ->
      let arg_count = List.length args.normal_args in
      let param_count = List.length params.Expr.normal_params in
      failwith (wrong_number_of_arguments pos param_count arg_count)
  end

let rec eval eva {Expr.pos;Expr.raw;} =
  begin match raw with
    | Expr.Const lit ->
      value_of_literal lit
    | Expr.Get (mods, VarOrMethod.Var x) ->
      begin try
          find_var eva.env pos mods x
        with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "variable not found: %s\n" (SnString.concat ":" (mods @ [x]))))
      end
    | Expr.Get (mods1, VarOrMethod.Method (mods2, klass, sel)) ->
      let klass = find_klass eva.env pos mods2 klass in
      begin try
          find_method eva.env pos mods1 klass sel
        with
        | Not_found ->
          let pair = sprintf "%s#%s" klass (Selector.show sel) in
          let pair = if mods1 <> [] then sprintf "(%s)" pair else pair in
          failwith (Pos.show_error pos (sprintf "method not found: %s\n" (SnString.concat ":" (mods1 @ [pair]))))
      end
    | Expr.Def (pat, expr) ->
      let value = eval eva expr in
      bind_param eva.env pos pat value;
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
              failwith (Pos.show_error pos (sprintf "variable not found: %s\n" x))
          end
        | VarOrMethod.Method (mods, klass, sel) ->
          let klass = find_klass eva.env pos mods klass in
          begin try
              Env.export_method eva.env klass (Selector.string_of sel);
            with
            | Not_found ->
              failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
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
          failwith (required pos "module" value)
      end
    | Expr.Record (klass_name, ctor_name, fields) ->
      let klass = SnString.concat ":" (List.rev (klass_name::eva.curr_mod_path)) in
      Env.add_var eva.env klass_name (Class klass);
      Env.add_var eva.env ctor_name (make_record_ctor klass fields);
      add_accessors eva.env klass fields;
      Class klass
    | Expr.Variant (klass_name, ctors) ->
      let klass = SnString.concat ":" (List.rev (klass_name::eva.curr_mod_path)) in
      Env.add_var eva.env klass_name (Class klass);
      add_ctors eva.env klass ctors;
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
                  failwith (Pos.show_error pos (sprintf "variable not found: %s\n" x))
              end
            | VarOrMethod.Method (mods, klass, sel) ->
              let klass = find_klass eva.env pos mods klass in
              begin try
                  Env.unexport_method modl klass (Selector.string_of sel);
                with
                | Not_found ->
                  failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
              end
          end voms
        | _ ->
          failwith (required pos "module" modl)
      end;
      modl
    | Expr.Match (target, cases) ->
      let target = eval eva target in
      begin try
          List.iter begin fun (pat, guard, body) ->
            begin try
                let env = Env.create_local eva.env in
                let eva = { eva with env = env } in
                bind_param env pos pat target;
                raise (Match_success (eval_exprs eva body))
              with
              | Failure message ->
                ()
            end
          end cases;
          failwith (Pos.show_error pos (sprintf "match failure of %s\n" (Value.show target)))
        with
        | Match_success result ->
          result
      end
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
    | Closure (env, params, body) ->
      let env = Env.create_local env in
      let eva = { eva with env = env } in
      bind_params env pos params args;
      eval_exprs eva body
    | Subr (required_count, allows_rest, subr) ->
      let arg_count = List.length args.normal_args in
      if arg_count < required_count || arg_count > required_count && not allows_rest then
        failwith (wrong_number_of_arguments pos required_count arg_count)
      else
        subr eva pos args
    | Trait (env, params, body) ->
      let env = Env.create_local env in
      let eva = { eva with env = env } in
      bind_params env pos params args;
      ignore (eval_exprs eva body);
      Module env;
    | _ ->
      failwith (required pos "function" func)
  end

and call_method eva pos recv sel args =
  let klass = Value.class_of recv in
  let meth = begin try
      find_method eva.env pos [] klass sel
    with
    | Not_found ->
      failwith (Pos.show_error pos (sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
  end
  in
  call_fun eva pos meth {args with normal_args = recv::args.normal_args}

let int_of_value pos v =
  begin match v with
    | Int i ->
      i
    | _ ->
      failwith (required pos "int" v)
  end

let unit_of_value pos v =
  begin match v with
    | Unit ->
      ()
    | _ ->
      failwith (required pos "unit" v)
  end

let bool_of_value pos v =
  begin match v with
    | Bool b ->
      b
    | _ ->
      failwith (required pos "bool" v)
  end

let char_of_value pos v =
  begin match v with
    | Char c ->
      c
    | _ ->
      failwith (required pos "char" v)
  end

let string_of_value pos v =
  begin match v with
    | String str ->
      str
    | _ ->
      failwith (required pos "string" v)
  end

let value_of_int i = Int i
let value_of_unit u = Unit
let value_of_bool b = Bool b
let value_of_char c = Char c
let value_of_string str = String str

let make_binary_subr proc_out proc_body proc_in =
  create_subr 2 begin fun eva pos args ->
    let arg0 = Args.nth args 0 in
    let arg1 = Args.nth args 1 in
    proc_out (proc_body (proc_in pos arg0) (proc_in pos arg1))
  end

let make_unary_subr proc_out proc_body proc_in =
  create_subr 1 begin fun eva pos args ->
    let arg0 = Args.nth args 0 in
    proc_out (proc_body (proc_in pos arg0))
  end
