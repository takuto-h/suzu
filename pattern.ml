
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
  | Variant of string * t list

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
      sprintf "(%s | %s)" (show lhs) (show rhs)
    | As (pat, x) ->
      sprintf "(%s as %s)" (show pat) x
    | Variant (ctor, params) ->
      sprintf "%s(%s)" ctor (SnString.concat_map " " show params)
  end
