
open Printf

let initial_inline_cache_size = 4

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

let compile_mods modl mods insns =
  Stack.push (VM.FindVar modl) insns;
  List.iter begin fun modl ->
    Stack.push (VM.AccessVar modl) insns;
  end mods

let compile_class mods klass insns =
  begin match mods with
    | [] ->
      Stack.push (VM.FindVar klass) insns;
    | modl::mods ->
      compile_mods modl mods insns;
      Stack.push (VM.AccessVar klass) insns;
  end

let rec compile_pattern {Expr.pat_raw} =
  begin match pat_raw with
    | Expr.PatWildCard ->
      Pattern.Any
    | Expr.PatConst lit ->
      Pattern.Const lit
    | Expr.PatBind _ ->
      Pattern.Any
    | Expr.PatParams params ->
      Pattern.Params (compile_params params)
    | Expr.PatVariant (tag, params) ->
      Pattern.Variant (tag, compile_params params)
    | Expr.PatOr (lhs, rhs) ->
      Pattern.Or (compile_pattern lhs, compile_pattern rhs)
    | Expr.PatAs (pat, _) ->
      compile_pattern pat
  end
  
and compile_params {Expr.normal_params;Expr.rest_param;Expr.labeled_params} =
  let normal_params = List.map compile_pattern normal_params in
  let rest_param =
    begin match rest_param with
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
      Stack.push (VM.At pos) insns;
      Stack.push (VM.Push lit) insns
    | Expr.Get ([], Expr.Var x) ->
      Stack.push (VM.At pos) insns;
      Stack.push (VM.FindVar x) insns
    | Expr.Get ([], Expr.Method (mods_k, klass, sel)) ->
      Stack.push (VM.At pos) insns;
      compile_class mods_k klass insns;
      Stack.push (VM.FindMethod sel) insns
    | Expr.Get (modl::mods, Expr.Var x) ->
      Stack.push (VM.At pos) insns;
      compile_mods modl mods insns;
      Stack.push (VM.AccessVar x) insns
    | Expr.Get (modl::mods, Expr.Method (mods_k, klass, sel)) ->
      Stack.push (VM.At pos) insns;
      compile_mods modl mods insns;
      compile_class mods_k klass insns;
      Stack.push (VM.AccessMethod sel) insns
    | Expr.Let (pat, expr) ->
      compile_expr expr insns;
      Stack.push (VM.At pos) insns;
      Stack.push (VM.Check (compile_pattern pat)) insns;
      compile_bind pat insns;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Lambda (params, body) ->
      let body = compile_with begin fun insns ->
          Stack.push (VM.At pos) insns;
          Stack.push (VM.Check (Pattern.Params (compile_params params))) insns;
          compile_multiple_bind params insns;
          compile_body body insns;
          Stack.push VM.Return insns
        end
      in
      Stack.push (VM.At pos) insns;
      Stack.push (VM.MakeClosure body) insns
    | Expr.Call (func, args) ->
      compile_expr func insns;
      compile_args args insns;
      Stack.push (VM.At pos) insns;
      Stack.push VM.Call insns
    | Expr.Send (recv, sel, args) ->
      compile_expr recv insns;
      compile_args args insns;
      Stack.push (VM.At pos) insns;
      Stack.push (VM.Send (sel, Hashtbl.create initial_inline_cache_size)) insns
    | Expr.Args args ->
      compile_args args insns
    | Expr.And (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (VM.At pos) insns;
      Stack.push (VM.Branch (rhs, [VM.Push (Literal.Bool false)])) insns
    | Expr.Or (lhs, rhs) ->
      compile_expr lhs insns;
      let rhs = compile_with (compile_expr rhs) in
      Stack.push (VM.At pos) insns;
      Stack.push (VM.Branch ([VM.Push (Literal.Bool true)], rhs)) insns
    | Expr.Match (args, cases) ->
      compile_args args insns;
      Stack.push (VM.At pos) insns;
      compile_cases cases insns
    | Expr.Module (name, exprs) ->
      Stack.push (VM.At pos) insns;
      Stack.push (VM.BeginModule name) insns;
      List.iter begin fun expr ->
        compile_expr expr insns;
        Stack.push (VM.AssertEqual Literal.Unit) insns
      end exprs;
      Stack.push (VM.Push Literal.Unit) insns;
      Stack.push (VM.EndModule name) insns
    | Expr.Export voms ->
      Stack.push (VM.At pos) insns;
      List.iter begin fun vom ->
        begin match vom with
          | Expr.Var x ->
            Stack.push (VM.ExportVar x) insns
          | Expr.Method (mods_k, klass, sel) ->
            compile_class mods_k klass insns;
            Stack.push (VM.ExportMethod sel) insns
        end
      end voms;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Open expr ->
      compile_expr expr insns;
      Stack.push (VM.At pos) insns;
      Stack.push VM.Open insns;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Include expr ->
      compile_expr expr insns;
      Stack.push (VM.At pos) insns;
      Stack.push VM.Include insns;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Record (klass, ctor, fields) ->
      Stack.push (VM.At pos) insns;
      Stack.push (VM.MakeClass klass) insns;
      Stack.push (VM.AddVar klass) insns;
      Stack.push (VM.MakeRecordCtor (klass, List.map fst fields)) insns;
      Stack.push (VM.AddVar ctor) insns;
      List.iter begin fun (field, is_mutable) ->
        Stack.push (VM.MakeGetter (klass, field)) insns;
        Stack.push (VM.MakeClass klass) insns;
        Stack.push (VM.AddMethod (Selector.of_ident field)) insns;
        if is_mutable then begin
          Stack.push (VM.MakeSetter (klass, field)) insns;
          Stack.push (VM.MakeClass klass) insns;
          Stack.push (VM.AddMethod (Selector.of_op (sprintf "%s=" field))) insns;
        end
      end fields;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Variant (klass, ctors) ->
      Stack.push (VM.At pos) insns;
      Stack.push (VM.MakeClass klass) insns;
      Stack.push (VM.AddVar klass) insns;
      List.iter begin fun (ctor, params) ->
        let params = compile_params params in
        Stack.push (VM.MakeVariantCtor (klass, ctor, params)) insns;
        Stack.push (VM.AddVar ctor) insns
      end ctors;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Phantom klass ->
      Stack.push (VM.At pos) insns;
      Stack.push (VM.MakeClass klass) insns;
      Stack.push (VM.AddVar klass) insns;
      Stack.push (VM.Push Literal.Unit) insns
    | Expr.Trait (params, body) ->
      let body = compile_with begin fun insns ->
          Stack.push (VM.At pos) insns;
          Stack.push (VM.Check (Pattern.Params (compile_params params))) insns;
          compile_multiple_bind params insns;
          List.iter begin fun expr ->
            compile_expr expr insns;
            Stack.push (VM.AssertEqual Literal.Unit) insns
          end body;
          Stack.push VM.ReturnModule insns
        end
      in
      Stack.push (VM.At pos) insns;
      Stack.push (VM.MakeClosure body) insns
    | Expr.Except (expr, voms) ->
      compile_expr expr insns;
      Stack.push (VM.At pos) insns;
      List.iter begin fun vom ->
        begin match vom with
          | Expr.Var x ->
            Stack.push (VM.UnexportVar x) insns
          | Expr.Method (mods_k, klass, sel) ->
            compile_class mods_k klass insns;
            Stack.push (VM.UnexportMethod sel) insns
        end
      end voms;
    | Expr.TryCatch (body, catches) ->
      compile_expr body insns;
      let (pat, catches) = compile_catches catches in
      Stack.push (VM.At pos) insns;
      Stack.push (VM.TryCatch (pat, catches)) insns;
    | Expr.TryFinally (body, finally) ->
      compile_expr body insns;
      let finally = compile_with begin fun insns ->
          Stack.push VM.Pop insns;
          List.iter begin fun expr ->
            compile_expr expr insns;
            Stack.push (VM.AssertEqual Literal.Unit) insns
          end finally;
          Stack.push (VM.Push Literal.Unit) insns;
          Stack.push VM.Return insns;
        end
      in
      Stack.push (VM.MakeClosure finally) insns;
      Stack.push (VM.At pos) insns;
      Stack.push VM.TryFinally insns;
    | Expr.Throw expr ->
      compile_expr expr insns;
      Stack.push VM.Throw insns;
    | Expr.Exception (ctor, params) ->
      let params = compile_params params in
      Stack.push (VM.MakeExceptionCtor (ctor, params)) insns;
      Stack.push (VM.AddVar ctor) insns;
      Stack.push (VM.Push Literal.Unit) insns;
  end

and compile_bind {Expr.pat_pos;Expr.pat_raw} insns =
  Stack.push (VM.At pat_pos) insns;
  begin match pat_raw with
    | Expr.PatWildCard ->
      Stack.push VM.Pop insns
    | Expr.PatConst lit ->
      Stack.push (VM.AssertEqual lit) insns;
    | Expr.PatBind (Expr.Var x) ->
      Stack.push (VM.AddVar x) insns
    | Expr.PatBind (Expr.Method (mods, klass, sel)) ->
      compile_class mods klass insns;
      Stack.push (VM.AddMethod sel) insns;
    | Expr.PatParams params ->
      compile_multiple_bind params insns;
    | Expr.PatVariant (tag, params) ->
      Stack.push (VM.RemoveTag tag) insns;
      compile_multiple_bind params insns;
    | Expr.PatOr (lhs, rhs) ->
      let pattern = compile_pattern lhs in
      Stack.push (VM.Test pattern) insns;
      let then_insns = compile_with (compile_bind lhs) in
      let else_insns = compile_with (compile_bind rhs) in
      Stack.push (VM.Branch (then_insns, else_insns)) insns
    | Expr.PatAs (pat, x) ->
      Stack.push VM.Dup insns;
      compile_bind pat insns;
      Stack.push (VM.AddVar x) insns
  end

and compile_multiple_bind {Expr.normal_params;Expr.rest_param;Expr.labeled_params} insns =
  List.iteri begin fun i pat ->
    Stack.push VM.Split insns;
    compile_bind pat insns
  end normal_params;
  begin match rest_param with
    | None ->
      ()
    | Some rest_param ->
      Stack.push VM.Dup insns;
      compile_bind rest_param insns
  end;
  List.iter begin fun (label, (pat, default)) ->
    begin match default with
      | None ->
        Stack.push (VM.GetLabeled (label, None)) insns
      | Some expr ->
        let default = compile_with (compile_expr expr) in
        Stack.push (VM.GetLabeled (label, Some default)) insns
    end;
    compile_bind pat insns
  end labeled_params;
  Stack.push VM.Pop insns;

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
  Stack.push (VM.MakeArgs (count, has_rest, List.rev rev_labels)) insns

and compile_body exprs insns =
  List.iter begin fun expr ->
    compile_expr expr insns;
    Stack.push (VM.AssertEqual Literal.Unit) insns
  end exprs;
  if exprs = [] then
    Stack.push (VM.Push Literal.Unit) insns
  else
    ignore (Stack.pop insns)

and compile_cases cases insns =
  begin match cases with
    | [] ->
      Stack.push VM.Fail insns
    | (params, guard, body)::cases ->
      Stack.push (VM.Test (Pattern.Params (compile_params params))) insns;
      let else_insns = compile_with (compile_cases cases) in
      let then_insns = compile_with begin fun insns ->
          Stack.push VM.Begin insns;
          Stack.push VM.Dup insns;
          compile_multiple_bind params insns;
          begin match guard with
            | None ->
              Stack.push VM.Pop insns;
              compile_body body insns;
              Stack.push VM.End insns
            | Some guard ->
              compile_expr guard insns;
              let then_insns = compile_with begin fun insns ->
                  Stack.push VM.Pop insns;
                  compile_body body insns;
                  Stack.push VM.End insns
                end
              in
              let else_insns = VM.End::else_insns in
              Stack.push (VM.Branch (then_insns, else_insns)) insns
          end;
        end
      in
      Stack.push (VM.Branch (then_insns, else_insns)) insns
  end

and compile_catches catches =
  let rec loop catches =
    begin match catches with
      | [] ->
        assert false
      | [(pat, body)] ->
        let insns = Stack.create () in
        Stack.push VM.Begin insns;
        compile_bind pat insns;
        compile_body body insns;
        Stack.push VM.End insns;
        (compile_pattern pat, list_of_stack insns)
      | (pat, body)::catches ->
        let insns = Stack.create () in
        let lhs = compile_pattern pat in
        Stack.push (VM.Test lhs) insns;
        let (rhs, else_insns) = loop catches in
        let then_insns = compile_with begin fun insns ->
            Stack.push VM.Begin insns;
            compile_bind pat insns;
            compile_body body insns;
            Stack.push VM.End insns
          end
        in
        Stack.push (VM.Branch (then_insns, else_insns)) insns;
        (Pattern.Or (lhs, rhs), list_of_stack insns)
    end
  in
  let (pat, insns) = loop catches in
  (pat, insns @ [VM.Return])

let compile expr =
  compile_with begin fun insns ->
    compile_expr expr insns;
    Stack.push VM.Return insns;
  end
