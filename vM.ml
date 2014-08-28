
open Printf

module OrderedVar = struct
  type t = string
  let compare = Pervasives.compare
end

module OrderedMethod = struct
  type t = string * string
  let compare = Pervasives.compare
end

module VarMap = Map.Make(OrderedVar)
module VarSet = Set.Make(OrderedVar)
module MethodMap = Map.Make(OrderedMethod)
module MethodSet = Set.Make(OrderedMethod)

type t = {
  mutable insns : Insn.t list;
  mutable stack : value list;
  mutable env : env;
  mutable pos : Pos.t;
  mutable controls  : control list;
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
  | Closure of env * Insn.t list
  | Subr of int * bool * string list * string list * (t -> args -> unit)

and args = {
  normal_args : value list;
  labeled_args : (string * value) list;
}

and env = frame list

and frame = {
  vars : value VarMap.t;
  methods : value MethodMap.t;
  exported_vars : VarSet.t;
  exported_methods : MethodSet.t;
}

and control =
  | Dump of Insn.t list * value list * env * Pos.t

exception Error of Pos.t * string * Pos.t list

exception InternalError of t * string
exception Match_failure of exn
exception Not_exported

let create insns env = {
  insns = insns;
  stack = [];
  env = env;
  pos = Pos.dummy;
  controls = [];
}

let make_args normal_args labeled_args = {
  normal_args = normal_args;
  labeled_args = labeled_args;  
}

let create_frame () = {
  vars = VarMap.empty;
  methods = MethodMap.empty;
  exported_vars = VarSet.empty;
  exported_methods = MethodSet.empty;
}

let create_subr req_count ?(allows_rest=false) ?(req_labels=[]) ?(opt_labels=[]) proc =
  Subr (req_count, allows_rest, req_labels, opt_labels, proc)

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
    | Subr (_, _, _, _, _) ->
      "Proc::C"
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
    | Subr (_, _, _, _, _) ->
      "<subr>"
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

let required vm req_str got_value =
  InternalError (vm, sprintf "%s required, but got %s\n" req_str (show_value got_value))

let wrong_number_of_arguments vm req_count got_count =
  InternalError (vm, sprintf "wrong number of arguments: required %d, but got %d\n" req_count got_count)

let lack_of_labeled_argument vm label =
  InternalError (vm, sprintf "lack of labeled argument: %s\n" label)

let extra_labeled_arguments vm labeled_args =
  InternalError (vm, sprintf "extra labeled arguments: %s\n" (SnString.concat_map ", " fst labeled_args))

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
  find (fun {vars} -> VarMap.find x vars) env

let find_method env klass sel =
  find (fun {methods} -> MethodMap.find (klass, sel) methods) env

let access_var {vars;exported_vars} x =
  let value = VarMap.find x vars in
  if VarSet.mem x exported_vars then
    value
  else
    raise Not_exported

let access_method {methods;exported_methods} klass sel =
  let value = MethodMap.find (klass, sel) methods in
  if MethodSet.mem (klass, sel) exported_methods then
    value
  else
    raise Not_exported

let update_current_frame proc env =
  proc (List.hd env)::List.tl env

let add_var env x value =
  let proc frame =
    {frame with vars = VarMap.add x value frame.vars}
  in
  update_current_frame proc env

let add_method env klass sel value =
  let proc frame =
    {frame with methods = MethodMap.add (klass, sel) value frame.methods}
  in
  update_current_frame proc env

let unit_of_value vm value =
  begin match value with
    | Unit ->
      ()
    | _ ->
      raise (required vm "unit" value)
  end

let int_of_value vm value =
  begin match value with
    | Int i ->
      i
    | _ ->
      raise (required vm "int" value)
  end

let bool_of_value vm value =
  begin match value with
    | Bool b ->
      b
    | _ ->
      raise (required vm "bool" value)
  end

let char_of_value vm value =
  begin match value with
    | Char c ->
      c
    | _ ->
      raise (required vm "char" value)
  end

let string_of_value vm value =
  begin match value with
    | String str ->
      str
    | _ ->
      raise (required vm "string" value)
  end

let class_of_value vm value =
  begin match value with
    | Class klass ->
      klass
    | _ ->
      raise (required vm "class" value)
  end

let module_of_value vm value =
  begin match value with
    | Module modl ->
      modl
    | _ ->
      raise (required vm "module" value)
  end

let args_of_value vm value =
  begin match value with
    | Args args ->
      args
    | _ ->
      raise (required vm "arguments" value)
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

let opt_nth args n =
  begin try
      Some (nth args n)
    with
    | Failure "nth" ->
      None
  end

let opt_labeled args label =
  begin try
      Some (labeled args label)
    with
    | Not_found ->
      None
  end

let rec check_value vm pat value =
  begin match (pat, value) with
    | (Insn.Any, _) ->
      ()
    | (Insn.Const lit, _) when value_of_literal lit = value ->
      ()
    | (Insn.Params params, Args args) ->
      check_args vm params args
    | (Insn.Variant (tag1, params), Variant (_, tag2, args)) when tag1 = tag2 ->
      check_args vm params args
    | (Insn.Or (lhs, rhs), _) ->
      begin try check_value vm lhs value with Match_failure _ ->
        begin try check_value vm rhs value with Match_failure _ ->
          raise (Match_failure (required vm (Insn.show_pattern pat) value))
        end
      end
    | _ ->
      raise (Match_failure (required vm (Insn.show_pattern pat) value))
  end

and check_args vm {Insn.normal_params;Insn.labeled_params} {normal_args;labeled_args} =
  let req_count = List.length normal_params in
  let got_count = List.length normal_args in
  begin if req_count <> got_count then
      raise (Match_failure (wrong_number_of_arguments vm req_count got_count))
  end;
  List.iter2 (check_value vm) normal_params normal_args;
  check_labeled_args vm labeled_params labeled_args

and check_labeled_args vm labeled_params labeled_args =
  begin match labeled_params with
    | [] when List.length labeled_args = 0 ->
      ()
    | [] ->
      raise (Match_failure (extra_labeled_arguments vm labeled_args))
    | (label, (param, _))::labeled_params when List.mem_assoc label labeled_args ->
      let arg = List.assoc label labeled_args in
      let labeled_args = List.remove_assoc label labeled_args in
      check_value vm param arg;
      check_labeled_args vm labeled_params labeled_args
    | (_, (_, has_default))::labeled_params when has_default ->
      check_labeled_args vm labeled_params labeled_args
    | (label, (_, _))::_ ->
      ()
  end

let call_subr vm req_count allows_rest req_labels opt_labels proc args =
  let {normal_args;labeled_args} = args in
  let got_count = List.length normal_args in
  begin if got_count < req_count || got_count > req_count && not allows_rest then
      raise (wrong_number_of_arguments vm req_count got_count)
  end;
  let labeled_args = List.fold_left begin fun labeled_args req_label ->
      if List.mem_assoc req_label labeled_args then
        List.remove_assoc req_label labeled_args
      else
        raise (lack_of_labeled_argument vm req_label)
    end labeled_args req_labels
  in
  let labeled_args = List.fold_left begin fun labeled_args opt_label ->
      List.remove_assoc opt_label labeled_args
    end labeled_args opt_labels
  in
  begin if List.length labeled_args <> 0 then
      raise (extra_labeled_arguments vm labeled_args)
  end;
  proc vm args

let call vm func args =
  let dump = Dump (vm.insns, vm.stack, vm.env, vm.pos) in
  begin match func with
    | Closure (env, insns) ->
      vm.insns <- insns;
      vm.stack <- [args];
      vm.env <- create_frame ()::env;
      vm.controls <- dump::vm.controls;
    | Subr (req_count, allows_rest, req_labels, opt_labels, proc) ->
      let args = args_of_value vm args in
      call_subr vm req_count allows_rest req_labels opt_labels proc args
    | _ ->
      raise (required vm "procedure" func)
  end

let return vm value =
  let Dump (insns, stack, env, pos) = List.hd vm.controls in
  vm.insns <- insns;
  vm.stack <- value::stack;
  vm.env <- env;
  vm.pos <- pos;
  vm.controls <- List.tl vm.controls

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
    | Insn.FindVar x ->
      begin try
          push_value vm (find_var vm.env x)
        with
        | Not_found ->
          raise (InternalError (vm, sprintf "variable not found: %s\n" x))
      end
    | Insn.FindMethod sel ->
      let klass = pop_value vm in
      let klass = class_of_value vm klass in
      begin try
          push_value vm (find_method vm.env klass (Selector.string_of sel))
        with
        | Not_found ->
          raise (InternalError (vm, sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
      end
    | Insn.AccessVar x ->
      let modl = pop_value vm in
      let modl = module_of_value vm modl in
      begin try
          push_value vm (access_var modl x)
        with
        | Not_found ->
          raise (InternalError (vm, sprintf "variable not found: %s\n" x))
        | Not_exported ->
          raise (InternalError (vm, sprintf "variable not exported: %s\n" x))
      end
    | Insn.AccessMethod sel ->
      let klass = pop_value vm in
      let klass = class_of_value vm klass in
      let modl = pop_value vm in
      let modl = module_of_value vm modl in
      begin try
          push_value vm (access_method modl klass (Selector.string_of sel))
        with
        | Not_found ->
          raise (InternalError (vm, sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
        | Not_exported ->
          raise (InternalError (vm, sprintf "method not exported: %s#%s\n" klass (Selector.show sel)))
      end
    | Insn.AddVar x ->
      let value = pop_value vm in
      vm.env <- add_var vm.env x value;
    | Insn.AddMethod sel ->
      let klass = pop_value vm in
      let klass = class_of_value vm klass in
      let value = pop_value vm in
      vm.env <- add_method vm.env klass (Selector.string_of sel) value
    | Insn.AssertEqual lit ->
      let value = value_of_literal lit in
      let top = pop_value vm in
      if top <> value then
        raise (required vm (Literal.show lit) top)
    | Insn.GetNth n ->
      let args = peek_value vm in
      let args = args_of_value vm args in
      begin match (opt_nth args n) with
        | Some value ->
          push_value vm value 
        | None ->
          raise (InternalError (vm, sprintf "argument not found at '%d': %s\n" n (show_args args)))
      end
    | Insn.GetLabeled (label, default) ->
      let args = peek_value vm in
      let args = args_of_value vm args in
      begin match (opt_labeled args label, default)  with
        | (Some value, _) ->
          push_value vm value
        | (None, Some insns) ->
          vm.insns <- insns @ vm.insns
        | (None, None) ->
          raise (InternalError (vm, sprintf "argument not found for '%s': %s\n" label (show_args args)))
      end
    | Insn.RemoveTag tag ->
      let value = pop_value vm in
      begin match value with
        | Variant (_, tag2, args) when tag2 = tag ->
          push_value vm (Args args)
        | _ ->
          raise (required vm tag value)
      end
    | Insn.Test pat ->
      let value = peek_value vm in
      begin try
          check_value vm pat value;
          push_value vm (Bool true)
        with
        | Match_failure _ ->
          push_value vm (Bool false)
      end
    | Insn.Check pat ->
      let value = peek_value vm in
      begin try
          check_value vm pat value
        with
        | Match_failure exn ->
          raise exn
      end
    | Insn.Branch (then_insns, else_insns) ->
      let cond = pop_value vm in
      let cond = bool_of_value vm cond in
      if cond then
        vm.insns <- then_insns @ vm.insns
      else
        vm.insns <- else_insns @ vm.insns
    | Insn.Call ->
      let args = pop_value vm in
      let func = pop_value vm in
      call vm func args
    | Insn.Send sel ->
      let args = pop_value vm in
      let recv = pop_value vm in
      let klass = get_class recv in
      begin try
          let func = find_method vm.env klass (Selector.string_of sel) in
          call vm func args
        with
        | Not_found ->
          raise (InternalError (vm, sprintf "method not found: %s#%s\n" klass (Selector.show sel)))
      end
    | Insn.Return ->
      let value = pop_value vm in
      return vm value
    | Insn.MakeArgs (count, labels) ->
      let labeled_args = List.fold_right begin fun label labeled_args ->
          let value = pop_value vm in
          (label, value)::labeled_args
        end labels []
      in
      let normal_args = ref [] in
      for i = 1 to count do
        let value = pop_value vm in
        normal_args := value::!normal_args
      done;
      let normal_args = !normal_args in
      push_value vm (Args (make_args normal_args labeled_args))
    | Insn.MakeClosure insns ->
      push_value vm (Closure (vm.env, insns))
    | Insn.Fail ->
      let value = pop_value vm in
      raise (InternalError (vm, sprintf "match failure of %s\n" (show_value value)))
    | Insn.Begin ->
      vm.env <- create_frame ()::vm.env
    | Insn.End ->
      vm.env <- List.tl vm.env
  end

let rec run vm =
  begin try
      begin match vm.insns with
        | [] ->
          pop_value vm
        | insn::insns ->
          vm.insns <- insns;
          execute vm insn;
          run vm
      end
    with
    | InternalError (vm, message) ->
      raise (Error (vm.pos, message, []))
  end
