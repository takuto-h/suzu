
let compile_mods insns modl mods =
  Stack.push (Insn.FindVar modl) insns;
  List.iter begin fun modl ->
    Stack.push (Insn.AccessVar modl) insns;
  end mods

let compile_class insns mods klass =
  begin match mods with
    | [] ->
      Stack.push (Insn.FindVar klass) insns;
    | modl::mods ->
      Stack.push (Insn.FindVar modl) insns;
      List.iter begin fun modl ->
        Stack.push (Insn.AccessVar modl) insns;
      end mods;
      Stack.push (Insn.AccessVar klass) insns;
  end

let list_of_stack stack =
  let rec loop acc =
    begin try
        let top = Stack.pop stack in
        loop (top::acc)
      with
      | Stack.Empty ->
        acc
    end
  in
  loop []

let rec compile expr =
  let insns = Stack.create () in
  compile_expr insns expr;
  list_of_stack insns

and compile_expr insns {Expr.pos;Expr.raw} =
  Stack.push (Insn.At pos) insns;
  begin match raw with
    | Expr.Const lit ->
      Stack.push (Insn.Push lit) insns
    | Expr.Get ([], VarOrMethod.Var x) ->
      Stack.push (Insn.FindVar x) insns
    | Expr.Get ([], VarOrMethod.Method (mods_k, klass, sel)) ->
      compile_class insns mods_k klass;
      Stack.push (Insn.FindMethod sel) insns
    | Expr.Get (modl::mods, VarOrMethod.Var x) ->
      compile_mods insns modl mods;
      Stack.push (Insn.AccessVar x) insns
    | Expr.Get (modl::mods, VarOrMethod.Method (mods_k, klass, sel)) ->
      compile_mods insns modl mods;
      compile_class insns mods_k klass;
      Stack.push (Insn.AccessMethod sel) insns
    | Expr.Let (pat, expr) ->
      compile_expr insns expr;
      compile_bind insns pat;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Lambda (params, body) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.FunCall (func, args) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.MethodCall (recv, sel, args) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.And (lhs, rhs) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Or (lhs, rhs) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Module (name, exprs) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Export voms ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Open expr ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Include expr ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Record (klass, ctor, fields) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Variant (klass, ctors) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Phantom klass ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Trait (params, body) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Except (modl, voms) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Match (args, cases) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Tuple args ->
      Stack.push (Insn.Push Literal.Unit) insns
  end

and compile_bind insns {Expr.pat_pos;Expr.pat_raw} =
  Stack.push (Insn.At pat_pos) insns;
  begin match pat_raw with
    | Expr.PatWildCard ->
      Stack.push Insn.Pop insns
    | Expr.PatConst lit ->
      Stack.push (Insn.AssertEqual lit) insns;
    | Expr.PatTuple params ->
      compile_params insns params;
      Stack.push Insn.Pop insns
    | Expr.PatVariant (tag, params) ->
      Stack.push (Insn.RemoveTag tag) insns;
      compile_params insns params;
      Stack.push Insn.Pop insns
    | Expr.PatBind (VarOrMethod.Var x) ->
      Stack.push (Insn.AddVar x) insns
    | Expr.PatBind (VarOrMethod.Method (mods, klass, sel)) ->
      compile_class insns mods klass; 
      Stack.push (Insn.AddMethod sel) insns;
    | Expr.PatOr (lhs, rhs) ->
      Stack.push Insn.Pop insns  (* TODO *)
    | Expr.PatAs (pat, x) ->
      Stack.push Insn.Dup insns;
      compile_bind insns pat;
      Stack.push (Insn.AddVar x) insns
  end

and compile_params insns {Expr.normal_params;Expr.keyword_params} =
  List.iteri begin fun i pat ->
    Stack.push (Insn.GetNth i) insns;
    compile_bind insns pat
  end normal_params;
  List.iter begin fun (label, (pat, default)) ->
    begin match default with
      | None ->
        Stack.push (Insn.GetLabeled (label, None)) insns
      | Some expr ->
        let default = compile expr in
        Stack.push (Insn.GetLabeled (label, Some default)) insns
    end;
    compile_bind insns pat
  end keyword_params
