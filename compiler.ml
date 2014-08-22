
let compile_expr insns {Expr.pos;Expr.raw} =
  Stack.push (Insn.At pos) insns;
  begin match raw with
    | Expr.Const lit ->
      Stack.push (Insn.Push lit) insns
    | Expr.Get (mods, vom) ->
      Stack.push (Insn.Push Literal.Unit) insns
    | Expr.Let (pat, expr) ->
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

let compile expr =
  let insns = Stack.create () in
  compile_expr insns expr;
  list_of_stack insns
