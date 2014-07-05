
open Printf

module Env = Value.Env

type t = {
  env : Env.t;
  dummy : unit;
}

let create () = {
  env = Env.create_global ();
  dummy = ();
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
        Env.find_var eva.env x
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "undefined variable: %s\n" x))
      end
    | Expr.Abs (params, body) ->
      Value.Closure (eva.env, params, body)
    | Expr.App (func, args) ->
      let func = eval eva func in
      let args = List.map (eval eva) args in
      apply eva pos func args
    | Expr.Block exprs ->
      List.fold_left begin fun _ elem ->
        eval eva elem
      end Value.Unit exprs
    | Expr.Def (x, expr) ->
      let value = eval eva expr in
      begin
        Env.add_var eva.env x value;
        value
      end
    | Expr.MethodCall (recv, sel, args) ->
      let recv = eval eva recv in
      let klass = Value.class_of recv in
      begin try
        let meth = Env.find_method eva.env klass sel in
        let args = List.map (eval eva) args in
        apply eva pos meth args
      with
        | Not_found ->
          failwith (Pos.show_error pos (sprintf "undefined method: %s#%s\n" klass sel))
      end
  end

and apply eva pos func args =
  begin match func with
    | Value.Closure (env, params, body) ->
      let env = Env.create_local env in
      let eva = { eva with env = env } in
      begin try
        begin
          List.iter2 (Env.add_var env) params args;
          eval eva body
        end
      with
        | Invalid_argument _ ->
          let req_num = List.length params in
          let got_num = List.length args in
          let message = sprintf "wrong number of arguments: required %d, but got %d\n" req_num got_num in
          failwith (Pos.show_error pos message)
      end
    | _ ->
      failwith (Pos.show_error pos "lhs is not a function; it cannot be applied.\n")
  end
