
open Printf

type t = {
  env : Env.t;
}

let create () = {
  env = Env.create_global ();
}

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

let rec eval eva {Expr.pos;Expr.raw;} =
  begin match raw with
    | Expr.Con lit ->
      value_of_literal lit
    | Expr.Var x ->
      begin try
        Env.find eva.env x
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "unbound variable: %s\n" x))
      end
    | Expr.Abs (params, body) ->
      Value.Unit
    | Expr.App (func, args) ->
      Value.Unit
  end
