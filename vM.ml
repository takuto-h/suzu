
open Printf

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
module MethodSet = Set.Make(struct type t = string * string let compare = Pervasives.compare end)

type t = {
  mutable insns : Insn.t list;
  mutable stack : value list;
  mutable env : env;
  mutable pos : Pos.t;
  mutable controls  : control list;
  mutable curr_mod_path : string list;
}

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string
  | Class of string
  | Module of frame
  | Args of args
  | Variant of string * string * args
  | Record of string * (string, value) Hashtbl.t
  | Closure of env * Insn.t list
  | Subr of int * bool * string list * (t -> args -> value)

and args = {
  normal_args : value list;
  labeled_args : (string * value) list;
}

and env = frame list

and frame = {
  vars : (string, value) Hashtbl.t;
  methods : (string * string, value) Hashtbl.t;
  mutable exported_vars : VarSet.t;
  mutable exported_methods : MethodSet.t;
}

and control =
  | Dump of Insn.t list * value list * env * Pos.t
  | Finally of value

exception InternalError of string
exception Match_failure of exn
exception Not_exported

let initial_binding_table_size = 4
let initial_field_table_size = 4

let create insns env = {
  insns = insns;
  stack = [];
  env = env;
  pos = Pos.dummy;
  controls = [];
  curr_mod_path = [];
}

let make_args normal_args labeled_args = {
  normal_args = normal_args;
  labeled_args = labeled_args;  
}

let create_frame () = {
  vars = Hashtbl.create initial_binding_table_size;
  methods = Hashtbl.create initial_binding_table_size;
  exported_vars = VarSet.empty;
  exported_methods = MethodSet.empty;
}

let create_subr req_count ?(allows_rest=false) ?(req_labels=[]) proc =
  Subr (req_count, allows_rest, req_labels, proc)

let get_class value =
  begin match value with
    | Unit ->
      "Unit::C"
    | Int _ ->
      "Int::C"
    | Bool _ ->
      "Bool::C"
    | Char _ ->
      "Char::C"
    | String _ ->
      "String::C"
    | Class _ ->
      "Class::C"
    | Module _ ->
      "Module::C"
    | Args _ ->
      "Args::C"
    | Variant (klass, _, _) ->
      klass
    | Closure (_, _) ->
      "Proc::C"
    | Subr (_, _, _, _) ->
      "Proc::C"
    | Record (klass, _) ->
      klass
  end

let rec show_value value =
  begin match value with
    | Unit ->
      "()"
    | Int i ->
      sprintf "%d" i
    | Bool b ->
      sprintf "%B" b
    | Char c ->
      sprintf "%C" c
    | String str ->
      sprintf "%S" str
    | Class klass ->
      sprintf "<class %s>" klass
    | Module _ ->
      "<module>"
    | Args args ->
      show_args args
    | Variant (_, tag, args) ->
      sprintf "%s%s" tag (show_args args)
    | Closure (_, _) ->
      "<closure>"
    | Subr (_, _, _, _) ->
      "<subr>"
    | Record (_, fields) ->
      sprintf "{%s}" (show_fields fields)
  end

and show_args {normal_args;labeled_args} =
  let str_normal = SnString.concat_map ", " show_value normal_args in
  let str_labeled = SnString.concat_map ", " show_labeled_arg labeled_args in
  if List.length normal_args <> 0 && List.length labeled_args <> 0 then
    sprintf "(%s, %s)" str_normal str_labeled
  else
    sprintf "(%s%s)" str_normal str_labeled

and show_labeled_arg (label, value) =
  sprintf ":%s %s" label (show_value value)

and show_fields table =
  let rev_strs = Hashtbl.fold begin fun key value acc ->
      (sprintf "%s=%s" key (show_value value))::acc
  end table []
  in
  SnString.concat ", " (List.rev rev_strs)

let required req_str got_value =
  InternalError (sprintf "%s required, but got %s\n" req_str (show_value got_value))

let wrong_number_of_arguments req_count got_count =
  InternalError (sprintf "wrong number of arguments: required %d, but got %d\n" req_count got_count)

let lack_of_labeled_argument label =
  InternalError (sprintf "lack of labeled argument: %s\n" label)

let variable_not_found x =
  InternalError (sprintf "variable not found: %s\n" x)

let method_not_found klass sel = 
  InternalError (sprintf "method not found: %s#%s\n" klass (Selector.show sel))

let variable_not_exported x =
  InternalError (sprintf "variable not exported: %s\n" x)

let method_not_exported klass sel = 
  InternalError (sprintf "method not exported: %s#%s\n" klass (Selector.show sel))

let push_value vm value =
  vm.stack <- value::vm.stack

let peek_value vm =
  List.hd vm.stack

let pop_value vm =
  let top = peek_value vm in
  vm.stack <- List.tl vm.stack;
  top

let value_of_literal lit =
  begin match lit with
    | Literal.Unit ->
      Unit
    | Literal.Int i ->
      Int i
    | Literal.Bool b ->
      Bool b
    | Literal.Char c ->
      Char c
    | Literal.String s ->
      String s
  end

let rec find proc env =
  begin match env with
    | [] ->
      raise Not_found
    | frame::env ->
      begin try
        proc frame
        with
        | Not_found ->
          find proc env
      end
  end

let find_var env x =
  find (fun {vars} -> Hashtbl.find vars x) env

let find_method env klass sel =
  find (fun {methods} -> Hashtbl.find methods (klass, sel)) env

let access_var {vars;exported_vars} x =
  let value = Hashtbl.find vars x in
  if VarSet.mem x exported_vars then
    value
  else
    raise Not_exported

let access_method {methods;exported_methods} klass sel =
  let value = Hashtbl.find methods (klass, sel) in
  if MethodSet.mem (klass, sel) exported_methods then
    value
  else
    raise Not_exported

let with_current_frame proc env =
  proc (List.hd env)

let export_var env x =
  with_current_frame begin fun frame ->
    if Hashtbl.mem frame.vars x then
      frame.exported_vars <- VarSet.add x frame.exported_vars
    else
      raise Not_found
  end env

let export_method env klass sel =
  with_current_frame begin fun frame ->
    if Hashtbl.mem frame.methods (klass, sel) then
      frame.exported_methods <- MethodSet.add (klass, sel) frame.exported_methods
    else
      raise Not_found
  end env

let add_var ?(export=false) env x value =
  with_current_frame begin fun frame ->
    Hashtbl.add frame.vars x value
  end env;
  if export then
    export_var env x

let add_method ?(export=false) env klass sel value =
  with_current_frame begin fun frame ->
    Hashtbl.add frame.methods (klass, sel) value
  end env;
  if export then
    export_method env klass sel

let unexport_var frame x =
  if VarSet.mem x frame.exported_vars then
    frame.exported_vars <- VarSet.remove x frame.exported_vars
  else
    raise Not_exported

let unexport_method frame klass sel =
  if MethodSet.mem (klass, sel) frame.exported_methods then
    frame.exported_methods <- MethodSet.remove (klass, sel) frame.exported_methods
  else
    raise Not_exported

let open_module vm modl =
  VarSet.iter begin fun x ->
    add_var vm.env x (access_var modl x)
  end modl.exported_vars;
  MethodSet.iter begin fun (klass, sel) ->
    add_method vm.env klass sel (access_method modl klass sel)
  end modl.exported_methods

let include_module vm modl =
  open_module vm modl;
  VarSet.iter begin fun x ->
    export_var vm.env x
  end modl.exported_vars;
  MethodSet.iter begin fun (klass, sel) ->
    export_method vm.env klass sel
  end modl.exported_methods

let unit_of_value value =
  begin match value with
    | Unit ->
      ()
    | _ ->
      raise (required "unit" value)
  end

let int_of_value value =
  begin match value with
    | Int i ->
      i
    | _ ->
      raise (required "int" value)
  end

let bool_of_value value =
  begin match value with
    | Bool b ->
      b
    | _ ->
      raise (required "bool" value)
  end

let char_of_value value =
  begin match value with
    | Char c ->
      c
    | _ ->
      raise (required "char" value)
  end

let string_of_value value =
  begin match value with
    | String str ->
      str
    | _ ->
      raise (required "string" value)
  end

let class_of_value value =
  begin match value with
    | Class klass ->
      klass
    | _ ->
      raise (required "class" value)
  end

let module_of_value value =
  begin match value with
    | Module modl ->
      modl
    | _ ->
      raise (required "module" value)
  end

let args_of_value value =
  begin match value with
    | Args args ->
      args
    | _ ->
      raise (required "arguments" value)
  end

let value_of_unit u = Unit
let value_of_int i = Int i
let value_of_bool b = Bool b
let value_of_char c = Char c
let value_of_string str = String str

let nth {normal_args} n =
  List.nth normal_args n

let labeled {labeled_args} label =
  List.assoc label labeled_args

let opt_labeled args label =
  begin try
      Some (labeled args label)
    with
    | Not_found ->
      None
  end

let rec check_value pat value =
  begin match (pat, value) with
    | (Pattern.Any, _) ->
      ()
    | (Pattern.Const lit, _) when value_of_literal lit = value ->
      ()
    | (Pattern.Params params, Args args) ->
      check_args params args
    | (Pattern.Variant (tag1, params), Variant (_, tag2, args)) when tag1 = tag2 ->
      check_args params args
    | (Pattern.Or (lhs, rhs), _) ->
      begin try check_value lhs value with Match_failure _ ->
        begin try check_value rhs value with Match_failure _ ->
          raise (Match_failure (required (Pattern.show pat) value))
        end
      end
    | _ ->
      raise (Match_failure (required (Pattern.show pat) value))
  end

and check_args {Pattern.normal_params;Pattern.rest_param;Pattern.labeled_params} {normal_args;labeled_args} =
  let req_count = List.length normal_params in
  let got_count = List.length normal_args in
  let allows_rest = rest_param <> None in
  begin if got_count < req_count || got_count > req_count && not allows_rest then
      raise (Match_failure (wrong_number_of_arguments req_count got_count))
  end;
  let normal_args = List.fold_left begin fun normal_args normal_param ->
      check_value normal_param (List.hd normal_args);
      List.tl normal_args
    end normal_args normal_params
  in
  ignore normal_args;
  check_labeled_args labeled_params labeled_args

and check_labeled_args labeled_params labeled_args =
  let labeled_args = List.fold_left begin fun labeles_args labeled_param ->
      begin match labeled_param with
        | (label, (param, _)) when List.mem_assoc label labeled_args ->
          let arg = List.assoc label labeled_args in
          let labeled_args = List.remove_assoc label labeled_args in
          check_value param arg;
          labeled_args
        | (_, (_, has_default)) when has_default ->
          labeled_args
        | (label, (_, _)) ->
          raise (Match_failure (lack_of_labeled_argument label))
      end
    end labeled_args labeled_params
  in
  ignore labeled_args

let check_args_for_subr req_count allows_rest req_labels args =
  let {normal_args;labeled_args} = args in
  let got_count = List.length normal_args in
  begin if got_count < req_count || got_count > req_count && not allows_rest then
      raise (wrong_number_of_arguments req_count got_count)
  end;
  let labeled_args = List.fold_left begin fun labeled_args req_label ->
      if List.mem_assoc req_label labeled_args then
        List.remove_assoc req_label labeled_args
      else
        raise (lack_of_labeled_argument req_label)
    end labeled_args req_labels
  in
  ignore labeled_args

let call vm func args =
  begin match func with
    | Closure (env, insns) ->
      let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
      vm.insns <- insns;
      vm.stack <- [args];
      vm.env <- create_frame ()::env;
      vm.controls <- dump::vm.controls;
    | Subr (req_count, allows_rest, req_labels, proc) ->
      let args = args_of_value args in
      check_args_for_subr req_count allows_rest req_labels args;
      push_value vm (proc vm args)
    | _ ->
      raise (required "procedure" func)
  end

let return vm value =
  begin match vm.controls with
    | [] ->
      ()
    | Dump (insns, stack, env, pos)::controls ->
      vm.insns <- insns;
      vm.stack <- value::stack;
      vm.env <- env;
      vm.pos <- pos;
      vm.controls <- controls
    | Finally func::controls ->
      vm.insns <- [Insn.Return];
      vm.stack <- [value];
      vm.controls <- controls;
      call vm func (Args (make_args [] []))
  end

let make_record_ctor klass fields =
  create_subr (List.length fields) begin fun vm args ->
    let table = Hashtbl.create initial_field_table_size in
    List.iter2 begin fun field arg ->
      Hashtbl.add table field arg
    end fields args.normal_args;
    Record (klass, table)
  end

let make_getter klass field =
  create_subr 1 begin fun vm args ->
    let self = nth args 0 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        Hashtbl.find table field
      | _ ->
        raise (required klass self)
    end
  end      

let make_setter klass field =
  create_subr 2 begin fun vm args ->
    let self = nth args 0 in
    let value = nth args 1 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        Hashtbl.replace table field value;
        value
      | _ ->
        raise (required klass self)
    end
  end     

let make_variant_ctor klass ctor {Pattern.normal_params;Pattern.rest_param;Pattern.labeled_params} =
  let count = List.length normal_params in
  let allows_rest = rest_param <> None in
  let req_labels = List.fold_right begin fun (label, (_, has_default)) req_labels ->
      if has_default then
        req_labels
      else
        label::req_labels
    end labeled_params []
  in
  create_subr count ~allows_rest:allows_rest ~req_labels:req_labels begin fun vm args ->
    Variant (klass, ctor, args)
  end

let execute vm insn =
  begin match insn with
    | Insn.At pos ->
      vm.pos <- pos
    | Insn.Push lit ->
      push_value vm (value_of_literal lit)
    | Insn.Pop ->
      ignore (pop_value vm)
    | Insn.Dup ->
      let top = peek_value vm in
      push_value vm top
    | Insn.Split ->
      let args = args_of_value (pop_value vm) in
      let arg = List.hd args.normal_args in
      let args = Args {args with normal_args = List.tl args.normal_args} in
      push_value vm args;
      push_value vm arg
    | Insn.GetLabeled (label, default) ->
      let args = args_of_value (peek_value vm) in
      begin match (opt_labeled args label, default)  with
        | (Some value, _) ->
          push_value vm value
        | (None, Some insns) ->
          vm.insns <- insns @ vm.insns
        | (None, None) ->
          raise (lack_of_labeled_argument label)
      end
    | Insn.RemoveTag tag ->
      let value = pop_value vm in
      begin match value with
        | Variant (_, tag2, args) when tag2 = tag ->
          push_value vm (Args args)
        | _ ->
          raise (required tag value)
      end
    | Insn.AssertEqual lit ->
      let value = value_of_literal lit in
      let top = pop_value vm in
      if top <> value then
        raise (required (Literal.show lit) top)
    | Insn.Test pat ->
      let value = peek_value vm in
      begin try
          check_value pat value;
          push_value vm (Bool true)
        with
        | Match_failure _ ->
          push_value vm (Bool false)
      end
    | Insn.Check pat ->
      let value = peek_value vm in
      begin try
          check_value pat value
        with
        | Match_failure exn ->
          raise exn
      end
    | Insn.Branch (then_insns, else_insns) ->
      let cond = bool_of_value (pop_value vm) in
      if cond then
        vm.insns <- then_insns @ vm.insns
      else
        vm.insns <- else_insns @ vm.insns
    | Insn.Call ->
      let args = pop_value vm in
      let func = pop_value vm in
      call vm func args
    | Insn.Send sel ->
      let args = args_of_value (pop_value vm) in
      let recv = pop_value vm in
      let args = Args {args with normal_args = recv::args.normal_args} in
      let klass = get_class recv in
      begin try
          let func = find_method vm.env klass (Selector.string_of sel) in
          call vm func args
        with
        | Not_found ->
          raise (method_not_found klass sel)
      end
    | Insn.Return ->
      let value = pop_value vm in
      return vm value
    | Insn.ReturnModule ->
      let value = Module (List.hd vm.env) in
      return vm value
    | Insn.Fail ->
      let value = pop_value vm in
      raise (InternalError (sprintf "%s didn't match any cases\n" (show_value value)))
    | Insn.Begin ->
      vm.env <- create_frame ()::vm.env
    | Insn.End ->
      vm.env <- List.tl vm.env
    | Insn.BeginModule name ->
      vm.env <- create_frame ()::vm.env;
      vm.curr_mod_path <- name::vm.curr_mod_path
    | Insn.EndModule name ->
      let modl = List.hd vm.env in
      vm.env <- List.tl vm.env;
      add_var vm.env name (Module modl);
      vm.curr_mod_path <- List.tl vm.curr_mod_path
    | Insn.FindVar x ->
      begin try
          push_value vm (find_var vm.env x)
        with
        | Not_found ->
          raise (variable_not_found x)
      end
    | Insn.FindMethod sel ->
      let klass = class_of_value (pop_value vm) in
      begin try
          push_value vm (find_method vm.env klass (Selector.string_of sel))
        with
        | Not_found ->
          raise (method_not_found klass sel)
      end
    | Insn.AccessVar x ->
      let modl = module_of_value (pop_value vm) in
      begin try
          push_value vm (access_var modl x)
        with
        | Not_found ->
          raise (variable_not_found x)
        | Not_exported ->
          raise (variable_not_exported x)
      end
    | Insn.AccessMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let modl = module_of_value (pop_value vm) in
      begin try
          push_value vm (access_method modl klass (Selector.string_of sel))
        with
        | Not_found ->
          raise (method_not_found klass sel)
        | Not_exported ->
          raise (method_not_exported klass sel)
      end
    | Insn.AddVar x ->
      let value = pop_value vm in
      add_var vm.env x value;
    | Insn.AddMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let value = pop_value vm in
      add_method vm.env klass (Selector.string_of sel) value
    | Insn.ExportVar x ->
      begin try
          export_var vm.env x
        with
        | Not_found ->
          raise (variable_not_found x)
      end
    | Insn.ExportMethod sel ->
      let klass = class_of_value (pop_value vm) in
      begin try
          export_method vm.env klass (Selector.string_of sel)
        with
        | Not_found ->
          raise (method_not_found klass sel)
      end
    | Insn.UnexportVar x ->
      let modl = module_of_value (pop_value vm) in
      begin try
          unexport_var modl x;
          push_value vm (Module modl)
        with
        | Not_exported ->
          raise (variable_not_exported x)
      end
    | Insn.UnexportMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let modl = module_of_value (pop_value vm) in
      begin try
          unexport_method modl klass (Selector.string_of sel);
          push_value vm (Module modl)
        with
        | Not_exported ->
          raise (method_not_exported klass sel)
      end
    | Insn.Open ->
      let modl = module_of_value (pop_value vm) in
      open_module vm modl
    | Insn.Include ->
      let modl = module_of_value (pop_value vm) in
      include_module vm modl
    | Insn.MakeArgs (count, has_rest, labels) ->
      let labeled_args = List.fold_right begin fun label labeled_args ->
          let value = pop_value vm in
          (label, value)::labeled_args
        end labels []
      in
      let (normal_args, labeled_args) = if has_rest then
          let args = args_of_value (pop_value vm) in
          let labeled_args = List.fold_right begin fun (label, value) labeled ->
              if List.mem_assoc label labeled_args then
                labeled
              else
                (label, value)::labeled
            end args.labeled_args labeled_args
          in
          (args.normal_args, labeled_args)
        else
          ([], labeled_args)
      in
      let normal_args = ref normal_args in
      for i = 1 to count do
        let value = pop_value vm in
        normal_args := value::!normal_args
      done;
      let normal_args = !normal_args in
      push_value vm (Args (make_args normal_args labeled_args))
    | Insn.MakeClosure insns ->
      push_value vm (Closure (vm.env, insns))
    | Insn.MakeClass klass ->
      let klass = SnString.concat "::" (List.rev (klass::vm.curr_mod_path)) in
      push_value vm (Class klass)
    | Insn.MakeRecordCtor (klass, fields) ->
      let klass = SnString.concat "::" (List.rev (klass::vm.curr_mod_path)) in
      push_value vm (make_record_ctor klass fields)
    | Insn.MakeGetter (klass, field) ->
      let klass = SnString.concat "::" (List.rev (klass::vm.curr_mod_path)) in
      push_value vm (make_getter klass field)
    | Insn.MakeSetter (klass, field) ->
      let klass = SnString.concat "::" (List.rev (klass::vm.curr_mod_path)) in
      push_value vm (make_setter klass field)
    | Insn.MakeVariantCtor (klass, ctor, params) ->
      let klass = SnString.concat "::" (List.rev (klass::vm.curr_mod_path)) in
      push_value vm (make_variant_ctor klass ctor params)
  end

let on_error vm message =
  let pos = vm.pos in
  let trace = ref [] in
  let subr_report_error =
    create_subr 0 begin fun vm args ->
      printf "%s" (Pos.show_message pos (sprintf "error: %s" message));
      List.iter begin fun pos ->
        printf "%s" (Pos.show_message pos "note: error from here\n")
      end !trace;
      Unit
    end
  in
  let controls = List.fold_right begin fun control controls ->
      begin match control with
        | Dump (_, _, _, pos) ->
          trace := pos::!trace;
          controls
        | Finally func ->
          control::controls
      end
    end vm.controls [Finally subr_report_error]
  in
  vm.insns <- [Insn.Return];
  vm.stack <- [Unit];
  vm.controls <- controls

let rec run vm =
  begin match vm.insns with
    | [] ->
      pop_value vm
    | insn::insns ->
      vm.insns <- insns;
      (*printf "%s\n" (Insn.show insn);*)
      begin try
          execute vm insn;
        with
        | InternalError message ->
          on_error vm message
      end;
      run vm
  end
