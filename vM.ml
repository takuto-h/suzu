
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

and modl = {
  vars : value VarMap.t;
  methods : value MethodMap.t;
  exported_vars : VarSet.t;
  exported_methods : MethodSet.t;
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

let show_value value =
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
  end

let required vm req_str got_value =
  InternalError (vm, sprintf "%s required, but got: %s\n" req_str (show_value got_value))

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
  let proc frame =
    {frame with vars = VarMap.add x value frame.vars}
  in
  vm.env <- update_current_module proc vm.env

let add_method vm klass sel value =
  let proc frame =
    {frame with methods = MethodMap.add (klass, sel) value frame.methods}
  in
  vm.env <- update_current_module proc vm.env

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
    | Insn.AddVar x ->
      let value = peek_value vm in
      add_var vm x value
    | Insn.AddMethod sel ->
      let klass = pop_value vm in
      let klass = class_of_value vm klass in
      let value = peek_value vm in
      add_method vm klass (Selector.string_of sel) value
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
