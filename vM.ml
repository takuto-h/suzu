
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
  mutable env : modl list;
  mutable pos : Pos.t;
}

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string
  | Class of string
  | Module of modl
  | Args of args
  | Variant of string * args

and modl = {
  vars : value VarMap.t;
  methods : value MethodMap.t;
  exported_vars : VarSet.t;
  exported_methods : MethodSet.t;
}

and args = {
  normal_args : value list;
  labeled_args : (string * value) list;
}

exception Error of Pos.t * string * Pos.t list

exception InternalError of t * string
exception Not_exported

let create_module () = {
  vars = VarMap.empty;
  methods = MethodMap.empty;
  exported_vars = VarSet.empty;
  exported_methods = MethodSet.empty;
}

let create insns modl = {
  insns = insns;
  stack = [];
  env = [modl];
  pos = Pos.dummy;
}

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
    | Variant (tag, args) ->
      sprintf "%s%s" tag (show_args args)
  end

and show_args {normal_args;labeled_args} =
  let str_normal = SnString.concat_map ", " show_value normal_args in
  let str_labeled = SnString.concat_map ", " show_keyword_arg labeled_args in
  if List.length normal_args <> 0 && List.length labeled_args <> 0 then
    sprintf "(%s, %s)" str_normal str_labeled
  else
    sprintf "(%s%s)" str_normal str_labeled

and show_keyword_arg (key, value) =
  sprintf ":%s %s" key (show_value value)

let required vm req_str got_value =
  InternalError (vm, sprintf "%s required, but got %s\n" req_str (show_value got_value))

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

let rec find proc mods =
  begin match mods with
    | [] ->
      raise Not_found
    | modl::mods ->
      begin try
        proc modl
        with
        | Not_found ->
          find proc mods
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

let update_current_module proc mods =
  proc (List.hd mods)::List.tl mods

let add_var vm x value =
  let proc modl =
    {modl with vars = VarMap.add x value modl.vars}
  in
  update_current_module proc vm.env

let add_method vm klass sel value =
  let proc modl =
    {modl with methods = MethodMap.add (klass, sel) value modl.methods}
  in
  update_current_module proc vm.env

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

let variant_of_value vm value =
  begin match value with
    | Variant (tag, args) ->
      (tag, args)
    | _ ->
      raise (required vm "variant" value)
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

let rec test_pattern pattern value =
  begin match (pattern, value) with
    | (Insn.Any, _) ->
      true
    | (Insn.Const lit, _) when value_of_literal lit = value ->
      true
    | (Insn.Params params, Args args) ->
      test_params params args
    | (Insn.Variant (tag1, params), Variant (tag2, args)) when tag1 = tag2 ->
      test_params params args
    | _ ->
      false
  end

and test_params params args =
  begin try
      List.for_all2 test_pattern params.Insn.normal_params args.normal_args
    with
    | Invalid_argument _ ->
      false
  end
  && test_labeled_params params.Insn.labeled_params args.labeled_args

and test_labeled_params labeled_params labeled_args =
  begin match labeled_params with
    | [] ->
      List.length labeled_args = 0
    | (label, (param, _))::labeled_params when List.mem_assoc label labeled_args ->
      let arg = List.assoc label labeled_args in
      let labeled_args = List.remove_assoc label labeled_args in
      test_pattern param arg && test_labeled_params labeled_params labeled_args
    | (_, (_, has_default))::labeled_params when has_default ->
      test_labeled_params labeled_params labeled_args
    | (_, (_, _))::_ ->
      false
  end

let execute vm insn =
  begin match insn with
    | Insn.At pos ->
      vm.pos <- pos
    | Insn.Push lit ->
      push_value vm (value_of_literal lit)
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
    | Insn.Pop ->
      ignore (pop_value vm)
    | Insn.AssertEqual lit ->
      let value = value_of_literal lit in
      let top = pop_value vm in
      if top <> value then
        raise (required vm (Literal.show lit) top)
    | Insn.AddVar x ->
      let value = pop_value vm in
      vm.env <- add_var vm x value;
    | Insn.AddMethod sel ->
      let klass = pop_value vm in
      let klass = class_of_value vm klass in
      let value = pop_value vm in
      vm.env <- add_method vm klass (Selector.string_of sel) value
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
      let (tag2, args) = variant_of_value vm value in
      if tag2 = tag then
        push_value vm (Args args)
      else
        raise (required vm tag value)
    | Insn.Dup ->
      let top = peek_value vm in
      push_value vm top
    | Insn.Test pattern ->
      let value = peek_value vm in
      let result = test_pattern pattern value in
      push_value vm (value_of_bool result)
    | Insn.Branch (then_insns, else_insns) ->
      let cond = pop_value vm in
      let cond = bool_of_value vm cond in
      if cond then
        vm.insns <- then_insns @ vm.insns
      else
        vm.insns <- else_insns @ vm.insns
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

let get_current_module vm =
  List.hd vm.env
