
let compile_mods modl mods insns =
  Stack.push (Insn.FindVar modl) insns;
  List.iter begin fun modl ->
    Stack.push (Insn.AccessVar modl) insns;
  end mods

let compile_class mods klass insns =
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

let compile_with proc =
  let insns = Stack.create () in
  proc insns;
  list_of_stack insns

let rec compile_pattern {Expr.pat_raw} =
  begin match pat_raw with
    | Expr.PatWildCard ->
      Pattern.Any
    | Expr.PatConst lit ->
      Pattern.Const lit
    | Expr.PatTuple params ->
      Pattern.Params (compile_params params)
    | Expr.PatVariant (tag, params) ->
      Pattern.Variant (tag, compile_params params)
    | Expr.PatBind _ ->
      Pattern.Any
    | Expr.PatOr (lhs, rhs) ->
      Pattern.Or (compile_pattern lhs, compile_pattern rhs)
    | Expr.PatAs (pat, _) ->
      compile_pattern pat
  end
  
and compile_params {Expr.normal_params;Expr.labeled_params} =
  let normal_params = List.map compile_pattern normal_params in
  let labeled_params = List.map begin fun (label, (pat, default)) ->
      (label, (compile_pattern pat, default <> None))
    end labeled_params
  in
  Pattern.make_params normal_params labeled_params

let rec compile_expr {Expr.pos;Expr.raw} insns =
  Stack.push (Insn.At pos) insns;
  begin match raw with
    | Expr.Const lit ->
      Stack.push (Insn.Push lit) insns
    | Expr.Get ([], VarOrMethod.Var x) ->
      Stack.push (Insn.FindVar x) insns
    | Expr.Get ([], VarOrMethod.Method (mods_k, klass, sel)) ->
      compile_class mods_k klass insns;
      Stack.push (Insn.FindMethod sel) insns
    | Expr.Get (modl::mods, VarOrMethod.Var x) ->
      compile_mods modl mods insns;
      Stack.push (Insn.AccessVar x) insns
    | Expr.Get (modl::mods, VarOrMethod.Method (mods_k, klass, sel)) ->
      compile_mods modl mods insns;
      compile_class mods_k klass insns;
      Stack.push (Insn.AccessMethod sel) insns
    | Expr.Let (pat, expr) ->
      compile_expr expr insns;
      Stack.push (Insn.Check (compile_pattern pat)) insns;
      compile_bind pat insns;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Lambda (params, body) ->
      let body = compile_with begin fun insns ->
          Stack.push (Insn.At pos) insns;
          Stack.push (Insn.Check (Pattern.Params (compile_params params))) insns;
          compile_multiple_bind params insns;
          compile_body body insns;
          Stack.push Insn.Return insns
        end
      in
      Stack.push (Insn.MakeClosure body) insns
    | Expr.FunCall (func, args) ->
      compile_expr func insns;
      compile_args args insns;
      Stack.push Insn.Call insns
    | Expr.MethodCall (recv, sel, args) ->
      compile_expr recv insns;
      compile_args args insns;
      Stack.push (Insn.Send sel) insns
    | Expr.Tuple args ->
      compile_args args insns
    | Expr.And (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (Insn.Branch (rhs, [Insn.Push (Literal.Bool false)])) insns
    | Expr.Or (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (Insn.Branch ([Insn.Push (Literal.Bool true)], rhs)) insns
    | Expr.Match (args, cases) ->
      compile_args args insns;
      compile_cases cases insns
    | Expr.Module (name, exprs) ->
      Stack.push (Insn.BeginModule name) insns;
      List.iter begin fun expr ->
        compile_expr expr insns;
        Stack.push (Insn.AssertEqual Literal.Unit) insns
      end exprs;
      Stack.push (Insn.Push Literal.Unit) insns;
      Stack.push (Insn.EndModule name) insns
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
  end

and compile_cases cases insns =
  begin match cases with
    | [] ->
      Stack.push Insn.Fail insns
    | (params, guard, body)::cases ->
      Stack.push (Insn.Test (Pattern.Params (compile_params params))) insns;
      let else_insns = compile_with (compile_cases cases) in
      let then_insns = compile_with begin fun insns ->
          Stack.push Insn.Begin insns;
          compile_multiple_bind params insns;
          begin match guard with
            | None ->
              compile_body body insns;
              Stack.push Insn.End insns
            | Some guard ->
              compile_expr guard insns;
              let then_insns = compile_with begin fun insns ->
                  compile_body body insns;
                  Stack.push Insn.End insns
                end
              in
              let else_insns = Insn.End::else_insns in
              Stack.push (Insn.Branch (then_insns, else_insns)) insns
          end;
        end
      in
      Stack.push (Insn.Branch (then_insns, else_insns)) insns
  end

and compile_args {Expr.normal_args;Expr.labeled_args} insns =
  let count = List.length normal_args in
  List.iter (fun expr -> compile_expr expr insns) normal_args;
  let rev_labels = List.fold_left begin fun labels (label, expr) ->
      compile_expr expr insns;
      label::labels
    end [] labeled_args
  in
  Stack.push (Insn.MakeArgs (count, List.rev rev_labels)) insns

and compile_bind {Expr.pat_pos;Expr.pat_raw} insns =
  Stack.push (Insn.At pat_pos) insns;
  begin match pat_raw with
    | Expr.PatWildCard ->
      Stack.push Insn.Pop insns
    | Expr.PatConst lit ->
      Stack.push (Insn.AssertEqual lit) insns;
    | Expr.PatTuple params ->
      compile_multiple_bind params insns;
      Stack.push Insn.Pop insns
    | Expr.PatVariant (tag, params) ->
      Stack.push (Insn.RemoveTag tag) insns;
      compile_multiple_bind params insns;
      Stack.push Insn.Pop insns
    | Expr.PatBind (VarOrMethod.Var x) ->
      Stack.push (Insn.AddVar x) insns
    | Expr.PatBind (VarOrMethod.Method (mods, klass, sel)) ->
      compile_class mods klass insns;
      Stack.push (Insn.AddMethod sel) insns;
    | Expr.PatOr (lhs, rhs) ->
      let pattern = compile_pattern lhs in
      Stack.push (Insn.Test pattern) insns;
      let then_insns = compile_with (compile_bind lhs) in
      let else_insns = compile_with (compile_bind rhs) in
      Stack.push (Insn.Branch (then_insns, else_insns)) insns
    | Expr.PatAs (pat, x) ->
      Stack.push Insn.Dup insns;
      compile_bind pat insns;
      Stack.push (Insn.AddVar x) insns
  end

and compile_multiple_bind {Expr.normal_params;Expr.labeled_params} insns =
  List.iteri begin fun i pat ->
    Stack.push (Insn.GetNth i) insns;
    compile_bind pat insns
  end normal_params;
  List.iter begin fun (label, (pat, default)) ->
    begin match default with
      | None ->
        Stack.push (Insn.GetLabeled (label, None)) insns
      | Some expr ->
        let default = compile_with (compile_expr expr) in
        Stack.push (Insn.GetLabeled (label, Some default)) insns
    end;
    compile_bind pat insns
  end labeled_params

and compile_body exprs insns =
  List.iter begin fun expr ->
    compile_expr expr insns;
    Stack.push (Insn.AssertEqual Literal.Unit) insns
  end exprs;
  if exprs = [] then
    Stack.push (Insn.Push Literal.Unit) insns
  else
    ignore (Stack.pop insns)

let compile expr =
  compile_with (compile_expr expr)
