
open Printf

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | WildCard
  | Const of Literal.t
  | Bind of VarOrMethod.t
  | Or of t * t
  | As of t * string

let at pos raw = {
  pos = pos;
  raw = raw;
}

let rec show {raw} =
  begin match raw with
    | WildCard ->
      "_"
    | Const lit ->
      Literal.show lit
    | Bind vom ->
      VarOrMethod.show vom
    | Or (lhs, rhs) ->
      sprintf "(Or %s %s)" (show lhs) (show rhs)
    | As (pat, x) ->
      sprintf "(As %s %s)" (show pat) x
  end
