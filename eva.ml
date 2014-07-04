
let value_of_literal lit =
  begin match lit with
    | Literal.Unit ->
      Value.Unit
    | Literal.Int i ->
      Value.Int i
    | Literal.String s ->
      Value.String s
    | Literal.Char c ->
      Value.Char c
    | Literal.Bool b ->
      Value.Bool b
  end

let rec eval expr =
  begin match expr.Expr.raw with
    | Expr.Con lit ->
      value_of_literal lit
    | Expr.Var x ->
      Value.Unit
    | Expr.Abs (params, body) ->
      Value.Unit
    | Expr.App (func, args) ->
      Value.Unit
  end
