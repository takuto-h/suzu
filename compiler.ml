
open Printf

type t = {
  insns : Insn.t Stack.t;
}

let create () = {
  insns = Stack.create ();
}

let push_insn comp insn =
  Stack.push insn comp.insns

let rec get_insns comp =
  let rec loop insns =
    begin try
        let insn = Stack.pop comp.insns in
        loop (insn::insns)
      with
      | Stack.Empty ->
        insns
    end
  in
  loop []

let rec compile comp {Expr.pos;Expr.raw} =
  begin match raw with
    | Expr.Const lit ->
      push_insn comp (Insn.Push lit)
    | Expr.Get (mods, VarOrMethod.Var x) ->
      push_insn comp (Insn.GetVar (mods, x))
    | Expr.Get (mods1, VarOrMethod.Method (mods2, klass, sel)) ->
      push_insn comp (Insn.GetMethod (mods1, mods2, klass, (Selector.string_of sel)))
    | Expr.Let (pat, expr) ->
      compile comp expr;
      push_insn comp (Insn.Bind pat)
    | Expr.Lambda (params, body) ->
      let insns = compile_exprs body in
      push_insn comp (Insn.PushClosure (params, insns))
    | Expr.FunCall (func, args) ->
      compile comp func;
      let (arg_count, keywords) = compile_args comp args in
      push_insn comp (Insn.Call (arg_count, keywords))
    | Expr.MethodCall (recv, sel, args) ->
      compile comp recv;
      let (arg_count, keywords) = compile_args comp args in
      push_insn comp (Insn.Send (Selector.string_of sel, arg_count, keywords))
    | Expr.And (lhs, rhs) ->
      compile comp lhs;
      let comp_rhs = create () in
      compile comp_rhs rhs;
      let rhs = get_insns comp_rhs in
      push_insn comp (Insn.And rhs)
    | Expr.Or (lhs, rhs) ->
      compile comp lhs;
      let comp_rhs = create () in
      compile comp_rhs rhs;
      let rhs = get_insns comp_rhs in
      push_insn comp (Insn.Or rhs)
    | Expr.Module (name, exprs) ->
      push_insn comp (Insn.Enter name);
      List.iter (compile comp) exprs;
      push_insn comp Insn.Leave
    | Expr.Export voms ->
      List.iter begin fun vom ->
        begin match vom with
          | VarOrMethod.Var x ->
            push_insn comp (Insn.ExportVar x)
          | VarOrMethod.Method (mods, klass, sel) ->
            push_insn comp (Insn.ExportMethod (mods, klass, (Selector.string_of sel)))
        end
      end voms
    | Expr.Open expr ->
      compile comp expr;
      push_insn comp Insn.Open
    | Expr.Include expr ->
      compile comp expr;
      push_insn comp Insn.Include
    | Expr.Record (klass_name, ctor_name, fields) ->
      push_insn comp (Insn.DefRecord (klass_name, ctor_name, fields))
    | Expr.Variant (klass_name, ctors) ->
      push_insn comp (Insn.DefVariant (klass_name, ctors))
    | Expr.Phantom klass_name ->
      push_insn comp (Insn.DefPhantom klass_name)
    | Expr.Trait (params, body) ->
      let insns = compile_exprs body in
      push_insn comp (Insn.PushTrait (params, insns))
    | Expr.Except (modl, voms) ->
      compile comp modl;
      List.iter begin fun vom ->
        begin match vom with
          | VarOrMethod.Var x ->
            push_insn comp (Insn.UnexportVar x)
          | VarOrMethod.Method (mods, klass, sel) ->
            push_insn comp (Insn.UnexportMethod (mods, klass, (Selector.string_of sel)))
        end
      end voms
    | Expr.Match (args, cases) ->
      let (arg_count, keywords) = compile_args comp args in
      let rev_cases = ref [] in
      List.iter begin fun (params, guard, body) ->
        let body = compile_exprs body in
        begin match guard with
          | None ->
            rev_cases := (params, None, body)::!rev_cases
          | Some guard ->
            let guard = compile_exprs [guard] in
            rev_cases := (params, Some guard, body)::!rev_cases
        end
      end cases;
      push_insn comp (Insn.Match (arg_count, keywords, List.rev !rev_cases))
    | Expr.Tuple args ->
      let (arg_count, keywords) = compile_args comp args in
      push_insn comp (Insn.PushTuple (arg_count, keywords))
  end

and compile_exprs exprs =
  let comp_exprs = create () in
  List.iter (compile comp_exprs) exprs;
  get_insns comp_exprs

and compile_args comp {Expr.normal_args;Expr.keyword_args} =
  let arg_count = List.length normal_args in
  List.iter (compile comp) normal_args;
  let rev_keywords = ref [] in
  List.iter begin fun (key, arg) ->
    rev_keywords := key::!rev_keywords;
    compile comp arg
  end keyword_args;
  (arg_count, List.rev !rev_keywords)

let rcpl () =
  Interp.rppl begin fun expr ->
    let insns = compile_exprs [expr] in
    List.iter begin fun insn ->
      printf "%s\n" (Insn.show insn)
    end insns
  end

let load name =
  SnPervasives.with_open_in name begin fun chan_in ->
    let source = Source.of_channel name chan_in in
    Interp.load_source begin fun expr ->
      let insns = compile_exprs [expr] in
      List.iter begin fun insn ->
        printf "%s\n" (Insn.show insn)
      end insns
    end source
  end
