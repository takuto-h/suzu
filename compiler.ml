
open Printf

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
    | Expr.PatParams params ->
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
  
and compile_params {Expr.normal_params;Expr.rest_param;Expr.labeled_params} =
  let normal_params = List.map compile_pattern normal_params in
  let rest_param = begin match rest_param with
    | None ->
      None
    | Some rest_param ->
      Some (compile_pattern rest_param)
  end
  in
  let labeled_params = List.map begin fun (label, (pat, default)) ->
      (label, (compile_pattern pat, default <> None))
    end labeled_params
  in
  Pattern.make_params normal_params rest_param labeled_params

let rec compile_expr {Expr.pos;Expr.raw} insns =
  begin match raw with
    | Expr.Const lit ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.Push lit) insns
    | Expr.Get ([], VarOrMethod.Var x) ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.FindVar x) insns
    | Expr.Get ([], VarOrMethod.Method (mods_k, klass, sel)) ->
      Stack.push (Insn.At pos) insns;
      compile_class mods_k klass insns;
      Stack.push (Insn.FindMethod sel) insns
    | Expr.Get (modl::mods, VarOrMethod.Var x) ->
      Stack.push (Insn.At pos) insns;
      compile_mods modl mods insns;
      Stack.push (Insn.AccessVar x) insns
    | Expr.Get (modl::mods, VarOrMethod.Method (mods_k, klass, sel)) ->
      Stack.push (Insn.At pos) insns;
      compile_mods modl mods insns;
      compile_class mods_k klass insns;
      Stack.push (Insn.AccessMethod sel) insns
    | Expr.Let (pat, expr) ->
      compile_expr expr insns;
      Stack.push (Insn.At pos) insns;
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
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.MakeClosure body) insns
    | Expr.FunCall (func, args) ->
      compile_expr func insns;
      compile_args args insns;
      Stack.push (Insn.At pos) insns;
      Stack.push Insn.Call insns
    | Expr.MethodCall (recv, sel, args) ->
      compile_expr recv insns;
      compile_args args insns;
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.Send sel) insns
    | Expr.Args args ->
      compile_args args insns
    | Expr.And (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.Branch (rhs, [Insn.Push (Literal.Bool false)])) insns
    | Expr.Or (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.Branch ([Insn.Push (Literal.Bool true)], rhs)) insns
    | Expr.Match (args, cases) ->
      compile_args args insns;
      Stack.push (Insn.At pos) insns;
      compile_cases cases insns
    | Expr.Module (name, exprs) ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.BeginModule name) insns;
      List.iter begin fun expr ->
        compile_expr expr insns;
        Stack.push (Insn.AssertEqual Literal.Unit) insns
      end exprs;
      Stack.push (Insn.Push Literal.Unit) insns;
      Stack.push (Insn.EndModule name) insns
    | Expr.Export voms ->
      Stack.push (Insn.At pos) insns;
      List.iter begin fun vom ->
        begin match vom with
          | VarOrMethod.Var x ->
            Stack.push (Insn.ExportVar x) insns
          | VarOrMethod.Method (mods_k, klass, sel) ->
            compile_class mods_k klass insns;
            Stack.push (Insn.ExportMethod sel) insns
        end
      end voms;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Open expr ->
      compile_expr expr insns;
      Stack.push (Insn.At pos) insns;
      Stack.push Insn.Open insns;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Include expr ->
      compile_expr expr insns;
      Stack.push (Insn.At pos) insns;
      Stack.push Insn.Include insns;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Trait (params, body) ->
      let body = compile_with begin fun insns ->
          Stack.push (Insn.At pos) insns;
          Stack.push (Insn.Check (Pattern.Params (compile_params params))) insns;
          compile_multiple_bind params insns;
          List.iter begin fun expr ->
            compile_expr expr insns;
            Stack.push (Insn.AssertEqual Literal.Unit) insns
          end body;
          Stack.push Insn.ReturnModule insns
        end
      in
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.MakeClosure body) insns
    | Expr.Except (expr, voms) ->
      compile_expr expr insns;
      Stack.push (Insn.At pos) insns;
      List.iter begin fun vom ->
        begin match vom with
          | VarOrMethod.Var x ->
            Stack.push (Insn.UnexportVar x) insns
          | VarOrMethod.Method (mods_k, klass, sel) ->
            compile_class mods_k klass insns;
            Stack.push (Insn.UnexportMethod sel) insns
        end
      end voms;
    | Expr.Record (klass, ctor, fields) ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.MakeClass klass) insns;
      Stack.push (Insn.AddVar klass) insns;
      Stack.push (Insn.MakeRecordCtor (klass, List.map fst fields)) insns;
      Stack.push (Insn.AddVar ctor) insns;
      List.iter begin fun (field, is_mutable) ->
        Stack.push (Insn.MakeGetter (klass, field)) insns;
        Stack.push (Insn.MakeClass klass) insns;
        Stack.push (Insn.AddMethod (Selector.Ident field)) insns;
        if is_mutable then begin
          Stack.push (Insn.MakeSetter (klass, field)) insns;
          Stack.push (Insn.MakeClass klass) insns;
          Stack.push (Insn.AddMethod (Selector.Op (sprintf "%s=" field))) insns;
        end
      end fields;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Variant (klass, ctors) ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.MakeClass klass) insns;
      Stack.push (Insn.AddVar klass) insns;
      List.iter begin fun (ctor, params) ->
        let params = compile_params params in
        Stack.push (Insn.MakeVariantCtor (klass, ctor, params)) insns;
        Stack.push (Insn.AddVar ctor) insns
      end ctors;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Phantom klass ->
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.MakeClass klass) insns;
      Stack.push (Insn.AddVar klass) insns;
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.TryFinally (body, finally) ->
      compile_expr body insns;
      let finally = compile_with begin fun insns ->
          Stack.push Insn.Pop insns;
          List.iter begin fun expr ->
            compile_expr expr insns;
            Stack.push (Insn.AssertEqual Literal.Unit) insns
          end finally;
          Stack.push (Insn.Push Literal.Unit) insns;
          Stack.push Insn.Return insns;
        end
      in
      Stack.push (Insn.MakeClosure finally) insns;
      Stack.push (Insn.At pos) insns;
      Stack.push Insn.TryFinally insns;
    | Expr.TryCatch (body, catches) ->
      compile_expr body insns;
      let (pat, catches) = compile_catches catches in
      Stack.push (Insn.At pos) insns;
      Stack.push (Insn.TryCatch (pat, catches)) insns;
    | Expr.Throw expr ->
      compile_expr expr insns;
      Stack.push Insn.Throw insns;
    | Expr.Exception (ctor, params) ->
      let params = compile_params params in
      Stack.push (Insn.MakeExceptionCtor (ctor, params)) insns;
      Stack.push (Insn.AddVar ctor) insns;
      Stack.push (Insn.Push Literal.Unit) insns;
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
          Stack.push Insn.Dup insns;
          compile_multiple_bind params insns;
          begin match guard with
            | None ->
              Stack.push Insn.Pop insns;
              compile_body body insns;
              Stack.push Insn.End insns
            | Some guard ->
              compile_expr guard insns;
              let then_insns = compile_with begin fun insns ->
                  Stack.push Insn.Pop insns;
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

and compile_args {Expr.normal_args;Expr.rest_arg;Expr.labeled_args} insns =
  let count = List.length normal_args in
  List.iter (fun expr -> compile_expr expr insns) normal_args;
  let has_rest = begin match rest_arg with
    | None ->
      false
    | Some rest_arg ->
      compile_expr rest_arg insns;
      true
  end
  in
  let rev_labels = List.fold_left begin fun labels (label, expr) ->
      compile_expr expr insns;
      label::labels
    end [] labeled_args
  in
  Stack.push (Insn.MakeArgs (count, has_rest, List.rev rev_labels)) insns

and compile_bind {Expr.pat_pos;Expr.pat_raw} insns =
  Stack.push (Insn.At pat_pos) insns;
  begin match pat_raw with
    | Expr.PatWildCard ->
      Stack.push Insn.Pop insns
    | Expr.PatConst lit ->
      Stack.push (Insn.AssertEqual lit) insns;
    | Expr.PatParams params ->
      compile_multiple_bind params insns;
    | Expr.PatVariant (tag, params) ->
      Stack.push (Insn.RemoveTag tag) insns;
      compile_multiple_bind params insns;
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

and compile_multiple_bind {Expr.normal_params;Expr.rest_param;Expr.labeled_params} insns =
  List.iteri begin fun i pat ->
    Stack.push Insn.Split insns;
    compile_bind pat insns
  end normal_params;
  begin match rest_param with
    | None ->
      ()
    | Some rest_param ->
      Stack.push Insn.Dup insns;
      compile_bind rest_param insns
  end;
  List.iter begin fun (label, (pat, default)) ->
    begin match default with
      | None ->
        Stack.push (Insn.GetLabeled (label, None)) insns
      | Some expr ->
        let default = compile_with (compile_expr expr) in
        Stack.push (Insn.GetLabeled (label, Some default)) insns
    end;
    compile_bind pat insns
  end labeled_params;
  Stack.push Insn.Pop insns;

and compile_body exprs insns =
  List.iter begin fun expr ->
    compile_expr expr insns;
    Stack.push (Insn.AssertEqual Literal.Unit) insns
  end exprs;
  if exprs = [] then
    Stack.push (Insn.Push Literal.Unit) insns
  else
    ignore (Stack.pop insns)

and compile_catches catches =
  let rec loop catches =
    begin match catches with
      | [] ->
        assert false
      | [(pat, body)] ->
        let insns = Stack.create () in
        Stack.push Insn.Begin insns;
        compile_bind pat insns;
        compile_body body insns;
        Stack.push Insn.End insns;
        (compile_pattern pat, list_of_stack insns)
      | (pat, body)::catches ->
        let insns = Stack.create () in
        let lhs = compile_pattern pat in
        Stack.push (Insn.Test lhs) insns;
        let (rhs, else_insns) = loop catches in
        let then_insns = compile_with begin fun insns ->
            Stack.push Insn.Begin insns;
            compile_bind pat insns;
            compile_body body insns;
            Stack.push Insn.End insns
          end
        in
        Stack.push (Insn.Branch (then_insns, else_insns)) insns;
        (Pattern.Or (lhs, rhs), list_of_stack insns)
    end
  in
  let (pat, insns) = loop catches in
  (pat, insns @ [Insn.Return])

let compile expr =
  compile_with begin fun insns ->
    compile_expr expr insns;
    Stack.push Insn.Return insns;
  end
