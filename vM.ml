
open Printf

module VarSet = Set.Make(String)
module VarMap = Map.Make(String)

module Method = struct
  type t = string * Selector.t
  let compare = Pervasives.compare
end
module MethodSet = Set.Make(Method)
module MethodMap = Map.Make(Method)

type t = {
  mutable insns : Insn.t list;
  mutable stack : value list;
  mutable env : env;
  mutable pos : Pos.t;
  mutable controls  : control list;
  mutable curr_mod_path : string list;
}

and value =
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Class of string
  | Module of frame
  | Args of args
  | Variant of string * string * args
  | Record of string * (string, value) Hashtbl.t
  | Closure of env * Insn.t list
  | Subr of int * bool * string list * (t -> args -> unit)
  | Cont of control list
  | Buffer of Buffer.t
  | Hash of (value, value) Hashtbl.t

and args = {
  normal_args : value list;
  labeled_args : (string * value) list;
}

and env = frame list

and frame = {
  mutable vars : value VarMap.t;
  mutable methods : value MethodMap.t;
  mutable exported_vars : VarSet.t;
  mutable exported_methods : MethodSet.t;
}

and control =
  | Dump of Insn.t list * value list * env * Pos.t
  | Catch of Pattern.t * Insn.t list * env * Pos.t
  | Finally of value
  | Reset

exception Error of Pos.t * string * Pos.t list
exception InternalError of string

exception Match_failure of exn
exception Not_exported

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

let unit = Args (make_args [] [])

let create_subr req_count ?(allows_rest=false) ?(req_labels=[]) proc =
  Subr (req_count, allows_rest, req_labels, proc)

let create_frame () = {
  vars = VarMap.empty;
  methods = MethodMap.empty;
  exported_vars = VarSet.empty;
  exported_methods = MethodSet.empty;
}

let get_class value =
  begin match value with
    | Bool _ ->
      "Bool::C"
    | Int _ ->
      "Int::C"
    | Float _ ->
      "Float::C"
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
    | Record (klass, _) ->
      klass
    | Closure (_, _) ->
      "Proc::C"
    | Subr (_, _, _, _) ->
      "Proc::C"
    | Cont _ ->
      "Proc::C"
    | Buffer _ ->
      "Buffer::C"
    | Hash _ ->
      "Hash::C"
  end

let rec show_value value =
  begin match value with
    | Bool b ->
      sprintf "%B" b
    | Int i ->
      sprintf "%d" i
    | Float f ->
      sprintf "%f" f
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
    | Variant ("List::C", "Nil", {normal_args=[];labeled_args=[]}) ->
      "[]"
    | Variant ("List::C", "Cons", {
        normal_args=[x; Variant ("List::C", _, _) as xs];
        labeled_args=[];
      }) ->
      sprintf "[%s]" (show_list (show_value x) xs)
    | Variant ("List::C", "Cons", {
        normal_args=[x; xs];
        labeled_args=[];
      }) ->
      sprintf "[%s, *%s]" (show_value x) (show_value xs)
    | Variant (_, tag, args) ->
      sprintf "%s%s" tag (show_args args)
    | Record (_, fields) ->
      sprintf "{%s}" (show_fields fields)
    | Closure (_, _) ->
      "<closure>"
    | Subr (_, _, _, _) ->
      "<subr>"
    | Cont _ ->
      "<cont>"
    | Buffer _ ->
      "<buffer>"
    | Hash table ->
      sprintf "%%{%s}" (show_table table)
  end

and show_args {normal_args;labeled_args} =
  let normal = List.map show_value normal_args in
  let labeled = List.map show_labeled_arg labeled_args in
  sprintf "(%s)" (SnString.concat ", " (normal @ labeled))

and show_labeled_arg (label, value) =
  sprintf ":%s %s" label (show_value value)

and show_list acc xs =
  begin match xs with
    | Variant ("List::C", "Nil", {normal_args=[];labeled_args=[]}) ->
      acc
    | Variant ("List::C", "Cons", {
        normal_args=[x; Variant ("List::C", _, _) as xs];
        labeled_args=[];
      }) ->
      show_list (sprintf "%s, %s" acc (show_value x)) xs
    | Variant ("List::C", "Cons", {
        normal_args=[x; xs];
        labeled_args=[];
      }) ->
      sprintf "%s, %s, *%s" acc (show_value x) (show_value xs)
    | _ ->
      assert false
  end

and show_fields table =
  let rev_strs = Hashtbl.fold begin fun key value acc ->
      (sprintf "%s=%s" key (show_value value))::acc
  end table []
  in
  SnString.concat ", " (List.rev rev_strs)

and show_table table =
  let rev_strs = Hashtbl.fold begin fun key value acc ->
      (sprintf "%s=>%s" (show_value key) (show_value value))::acc
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

let variable_not_exported x =
  InternalError (sprintf "variable not exported: %s\n" x)

let variable_already_defined x =
  InternalError (sprintf "variable already defined: %s\n" x)

let method_not_found klass sel = 
  InternalError (sprintf "method not found: %s#%s\n" klass (Selector.show sel))

let method_not_exported klass sel = 
  InternalError (sprintf "method not exported: %s#%s\n" klass (Selector.show sel))

let method_already_defined klass sel = 
  InternalError (sprintf "method already defined: %s#%s\n" klass (Selector.show sel))

let bool_of_value value =
  begin match value with
    | Bool b ->
      b
    | _ ->
      raise (required "bool" value)
  end

let int_of_value value =
  begin match value with
    | Int i ->
      i
    | _ ->
      raise (required "int" value)
  end

let float_of_value value =
  begin match value with
    | Float f ->
      f
    | _ ->
      raise (required "float" value)
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

let buffer_of_value value =
  begin match value with
    | Buffer buff ->
      buff
    | _ ->
      raise (required "buffer" value)
  end

let hashtbl_of_value value =
  begin match value with
    | Hash table ->
      table
    | _ ->
      raise (required "hash" value)
  end

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
      unit
    | Literal.Int i ->
      Int i
    | Literal.Float f ->
      Float f
    | Literal.Bool b ->
      Bool b
    | Literal.Char c ->
      Char c
    | Literal.String str ->
      String str
  end

let rec find_binding proc env =
  begin match env with
    | [] ->
      raise Not_found
    | frame::env ->
      begin try
        proc frame
        with
        | Not_found ->
          find_binding proc env
      end
  end

let find_var env x =
  begin try
      find_binding (fun {vars} -> VarMap.find x vars) env
    with
    | Not_found ->
      raise (variable_not_found x)
  end

let find_method env klass sel =
  begin try
      find_binding (fun {methods} -> MethodMap.find (klass, sel) methods) env
    with
    | Not_found ->
      raise (method_not_found klass sel)
  end

let access_var {vars;exported_vars} x =
  let value = VarMap.find x vars in
  if VarSet.mem x exported_vars then
    value
  else
    raise (variable_not_exported x)

let access_method {methods;exported_methods} klass sel =
  let value = MethodMap.find (klass, sel) methods in
  if MethodSet.mem (klass, sel) exported_methods then
    value
  else
    raise (method_not_exported klass sel)

let with_current_frame proc env =
  proc (List.hd env)

let export_var env x =
  with_current_frame begin fun frame ->
    if VarMap.mem x frame.vars then
      frame.exported_vars <- VarSet.add x frame.exported_vars
    else
      raise (variable_not_found x)
  end env

let export_method env klass sel =
  with_current_frame begin fun frame ->
    if MethodMap.mem (klass, sel) frame.methods then
      frame.exported_methods <- MethodSet.add (klass, sel) frame.exported_methods
    else
      raise (method_not_found klass sel)
  end env

let add_var ?(export=false) env x value =
  with_current_frame begin fun frame ->
    if VarMap.mem x frame.vars then
      raise (variable_already_defined x)
    else
      frame.vars <- VarMap.add x value frame.vars
  end env;
  if export then
    export_var env x

let add_method ?(export=false) env klass sel value =
  with_current_frame begin fun frame ->
    if MethodMap.mem (klass, sel) frame.methods then
      raise (method_already_defined klass sel)
    else
      frame.methods <- MethodMap.add (klass, sel) value frame.methods
  end env;
  if export then
    export_method env klass sel

let unexport_var frame x =
  if VarSet.mem x frame.exported_vars then
    {frame with exported_vars = VarSet.remove x frame.exported_vars}
  else
    raise (variable_not_exported x)

let unexport_method frame klass sel =
  if MethodSet.mem (klass, sel) frame.exported_methods then
    {frame with exported_methods = MethodSet.remove (klass, sel) frame.exported_methods}
  else
    raise (method_not_exported klass sel)

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

let get_arg {normal_args} n =
  List.nth normal_args n

let get_labeled_arg {labeled_args} label =
  List.assoc label labeled_args

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
    | (_, _) ->
      raise (Match_failure (required (Pattern.show pat) value))
  end

and check_args params args =
  let {Pattern.normal_params;Pattern.rest_param;Pattern.labeled_params} = params in
  let {normal_args;labeled_args} = args in
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
  let labeled_args = List.fold_left begin fun labeled_args label ->
      if List.mem_assoc label labeled_args then
        List.remove_assoc label labeled_args
      else
        raise (lack_of_labeled_argument label)
    end labeled_args req_labels
  in
  ignore labeled_args

let test_value pat value =
  begin try
      check_value pat value;
      true
    with
    | Match_failure _ ->
      false
  end

let make_record_ctor klass fields =
  create_subr (List.length fields) begin fun vm args ->
    let table = Hashtbl.create initial_field_table_size in
    List.iter2 begin fun field arg ->
      Hashtbl.add table field arg
    end fields args.normal_args;
    push_value vm (Record (klass, table))
  end

let make_getter klass field =
  create_subr 1 begin fun vm args ->
    let self = get_arg args 0 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        push_value vm (Hashtbl.find table field)
      | _ ->
        raise (required klass self)
    end
  end

let make_setter klass field =
  create_subr 2 begin fun vm args ->
    let self = get_arg args 0 in
    let value = get_arg args 1 in
    begin match self with
      | Record (klass2, table) when klass2 = klass ->
        Hashtbl.replace table field value;
        push_value vm unit
      | _ ->
        raise (required klass self)
    end
  end

let make_variant_ctor klass ctor params = 
  let {Pattern.normal_params;Pattern.rest_param;Pattern.labeled_params} = params in
  let req_count = List.length normal_params in
  let allows_rest = rest_param <> None in
  let req_labels = List.fold_right begin fun (label, (_, has_default)) req_labels ->
      if has_default then
        req_labels
      else
        label::req_labels
    end labeled_params []
  in
  create_subr req_count ~allows_rest:allows_rest ~req_labels:req_labels begin fun vm args ->
    push_value vm (Variant (klass, ctor, args))
  end

let copy_frame {vars;methods;exported_vars;exported_methods} = {
  vars = vars;
  methods = methods;
  exported_vars = exported_vars;
  exported_methods = exported_methods;
}

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
      proc vm args;
    | Cont controls ->
      let controls = List.fold_right begin fun control controls ->
          let control = begin match control with
            | Dump (insns, stack, env, pos) ->
              Dump (insns, stack, List.map copy_frame env, pos)
            | Catch (pat, insns, env, pos) ->
              Catch (pat, insns, List.map copy_frame env, pos)
            | _ ->
              control
          end
          in
          control::controls
        end controls []
      in
      let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
      let args = args_of_value args in
      check_args_for_subr 1 false [] args;
      vm.insns <- [Insn.Return];
      vm.stack <- [get_arg args 0];
      vm.controls <- controls @ (dump::vm.controls);
    | _ ->
      raise (required "procedure" func)
  end

let rec return vm value =
  begin match vm.controls with
    | [] ->
      vm.insns <- [];
      vm.stack <- [value];
    | Dump (insns, stack, env, pos)::controls ->
      vm.insns <- insns;
      vm.stack <- value::stack;
      vm.env <- env;
      vm.pos <- pos;
      vm.controls <- controls;
    | Catch (_, _, _, _)::controls ->
      vm.controls <- controls;
      return vm value;
    | Finally func::controls ->
      vm.insns <- [Insn.Pop; Insn.Return];
      vm.stack <- [value];
      vm.controls <- controls;
      call vm func (Args (make_args [] []));
    | Reset::controls ->
      vm.controls <- controls;
      return vm value;
  end

let throw vm value =
  let pos = vm.pos in
  let message = sprintf "uncaught exception: %s\n" (show_value value) in
  let trace = ref [] in
  let subr_report_error = create_subr 0 (fun vm args -> raise (Error (pos, message, !trace))) in
  let rec loop controls =
    begin match controls with
      | [] ->
        ([Finally subr_report_error], [])
      | Dump (_, _, _, pos)::controls ->
        let (controls, trace) = loop controls in
        (controls, pos::trace)
      | Catch (pat, insns, env, pos)::controls when test_value pat value ->
        let dump = Dump (insns, [], env, pos) in
        (Catch (pat, insns, env, pos)::dump::controls, [])
      | Catch (_, _, _, _)::controls ->
        loop controls
      | Finally func::controls ->
        let (controls, trace) = loop controls in
        (Finally func::controls, trace)
      | Reset::controls ->
        loop controls
    end
  in
  let controls_and_trace = loop vm.controls in
  trace := snd controls_and_trace;
  vm.insns <- [Insn.Return];
  vm.stack <- [value];
  vm.controls <- fst controls_and_trace

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
    | Insn.SplitLabeled (label, default) ->
      let args = args_of_value (peek_value vm) in
      begin try
          let arg = List.assoc label args.labeled_args in
          let args = Args {args with labeled_args = List.remove_assoc label args.labeled_args} in
          ignore (pop_value vm);
          push_value vm args;
          push_value vm arg;
        with
        | Not_found ->
          begin match default with
            | Some insns ->
              vm.insns <- insns @ vm.insns
            | None ->
              raise (lack_of_labeled_argument label)
          end
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
      push_value vm (Bool (test_value pat value))
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
      let func = find_method vm.env klass sel in
      call vm func args
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
      vm.curr_mod_path <- List.tl vm.curr_mod_path;
      add_var vm.env name (Module modl);
    | Insn.FindVar x ->
      push_value vm (find_var vm.env x)
    | Insn.FindMethod sel ->
      let klass = class_of_value (pop_value vm) in
      push_value vm (find_method vm.env klass sel)
    | Insn.AccessVar x ->
      let modl = module_of_value (pop_value vm) in
      push_value vm (access_var modl x)
    | Insn.AccessMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let modl = module_of_value (pop_value vm) in
      push_value vm (access_method modl klass sel)
    | Insn.AddVar x ->
      let value = pop_value vm in
      add_var vm.env x value;
    | Insn.AddMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let value = pop_value vm in
      add_method vm.env klass sel value
    | Insn.ExportVar x ->
      export_var vm.env x
    | Insn.ExportMethod sel ->
      let klass = class_of_value (pop_value vm) in
      export_method vm.env klass sel
    | Insn.UnexportVar x ->
      let modl = module_of_value (pop_value vm) in
      let modl = unexport_var modl x in
      push_value vm (Module modl)
    | Insn.UnexportMethod sel ->
      let klass = class_of_value (pop_value vm) in
      let modl = module_of_value (pop_value vm) in
      let modl = unexport_method modl klass sel in
      push_value vm (Module modl)
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
      let (normal_args, labeled_args) =
        if has_rest then
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
    | Insn.MakeExceptionCtor (ctor, params) ->
      let klass = "Exn::C" in
      push_value vm (make_variant_ctor klass ctor params)
    | Insn.TryCatch (pat, insns) ->
      let env = vm.env in
      let pos = vm.pos in
      let body = pop_value vm in
      let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
      vm.insns <- [Insn.Return];
      vm.stack <- [];
      vm.controls <- Catch (pat, insns, env, pos)::dump::vm.controls;
      call vm body (Args (make_args [] []));
    | Insn.TryFinally ->
      let finally = Finally (pop_value vm) in
      let body = pop_value vm in
      let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
      vm.insns <- [Insn.Return];
      vm.stack <- [];
      vm.controls <- finally::dump::vm.controls;
      call vm body (Args (make_args [] []));
    | Insn.Throw ->
      let value = pop_value vm in
      throw vm value
  end

let subr_reset =
  create_subr 1 begin fun vm args ->
    let func = get_arg args 0 in
    let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
    vm.insns <- [Insn.Return];
    vm.stack <- [];
    vm.controls <- Reset::dump::vm.controls;
    call vm func (Args (make_args [] []));
  end

let subr_shift =
  create_subr 1 begin fun vm args ->
    let func = get_arg args 0 in
    let rec loop rev_left right =
      begin match right with
        | [] ->
          (List.rev rev_left, right)
        | Reset::right ->
          (List.rev (Reset::rev_left), Reset::right)
        | control::right ->
          loop (control::rev_left) right
      end
    in
    let (left, right) = loop [] vm.controls in
    let left = Dump (vm.insns, vm.stack, vm.env, vm.pos)::left in
    vm.insns <- [Insn.Return];
    vm.stack <- [];
    vm.controls <- right;
    call vm func (Args (make_args [Cont left] []));
  end

let on_error vm message =
  let pos = vm.pos in
  let trace = ref [] in
  let subr_report_error = create_subr 0 (fun vm args -> raise (Error (pos, message, !trace))) in
  let controls = List.fold_right begin fun control controls ->
      begin match control with
        | Dump (_, _, _, pos) ->
          trace := pos::!trace;
          controls
        | Catch (_, _, _, _) ->
          controls
        | Finally func ->
          control::controls
        | Reset ->
          controls
      end
    end vm.controls [Finally subr_report_error]
  in
  vm.insns <- [Insn.Return];
  vm.stack <- [unit];
  vm.controls <- controls

let rec run vm =
  begin match vm.insns with
    | [] ->
      pop_value vm
    | insn::insns ->
      vm.insns <- insns;
      (*printf "%s\n" (SnString.concat_map " " show_value vm.stack);*)
      (*printf "%s\n" (Insn.show insn);*)
      begin try
          execute vm insn;
        with
        | InternalError message ->
          on_error vm message
      end;
      run vm
  end

let get_env {env} =
  env
