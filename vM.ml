
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

exception Error of Pos.t * string

type t = {
  insns : Insn.t list;
  stack : value list;
  env : env;
  pos : Pos.t;
}

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string

and env =
  | Global of frame
  | Local of frame * env

and frame = {
  vars : value VarMap.t;
  methods : value MethodMap.t;
  exported_vars : VarSet.t;
  exported_methods : MethodSet.t;
}

let create_frame () = {
  vars = VarMap.empty;
  methods = MethodMap.empty;
  exported_vars = VarSet.empty;
  exported_methods = MethodSet.empty;
}

let create_global () =
  Global (create_frame ())

let create_local env =
  Local (create_frame (), env)

let create insns env = {
  insns = insns;
  stack = [];
  env = env;
  pos = Pos.dummy;
}

let push_value vm value =
  {vm with stack = value::vm.stack}

let pop_value vm =
  begin match vm.stack with
    | [] ->
      assert false
    | top::stack ->
      (top, {vm with stack = stack})
  end

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
    | String s ->
      sprintf "%S" s
  end

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
    | Global frame ->
      proc frame
    | Local (frame, env) ->
      begin try
          proc frame
        with
        | Not_found ->
          find proc env
      end
  end

let find_var env x =
  find (fun {vars;exported_vars} -> VarMap.find x vars) env

let update_current_frame proc env =
  begin match env with
    | Global frame ->
      Global (proc frame)
    | Local (frame, env) ->
      Local (proc frame, env)
  end

let add_var vm x v =
  let proc frame =
    {frame with vars = VarMap.add x v frame.vars}
  in
  {vm with env = update_current_frame proc vm.env}

let execute vm insn =
  begin match insn with
    | Insn.At pos ->
      {vm with pos = pos}
    | Insn.Push lit ->
      push_value vm (value_of_literal lit)
    | Insn.FindVar x ->
      begin try
          push_value vm (find_var vm.env x)
        with
        | Not_found ->
          raise (Error (vm.pos, sprintf "variable not found: %s\n" x))
      end
    | Insn.AddVar x ->
      let (value, vm) = pop_value vm in
      add_var vm x value
  end

let rec run vm =
  begin match (vm.insns, vm.stack) with
    | ([], [top]) ->
      top
    | ([], _) ->
      assert false
    | (insn::insns, _) ->
      let vm = execute {vm with insns = insns} insn in
      run vm
  end
