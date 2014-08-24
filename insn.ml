
type has_default = bool

type pattern =
  | Any
  | Const of Literal.t
  | Params of params
  | Variant of string * params
  | Or of pattern * pattern

and params = {
  normal_params : pattern list;
  labeled_params : (string * (pattern * has_default)) list;
}

type t =
  | At of Pos.t
  | Push of Literal.t
  | FindVar of string
  | FindMethod of Selector.t
  | AccessVar of string
  | AccessMethod of Selector.t
  | Pop
  | AssertEqual of Literal.t
  | AddVar of string
  | AddMethod of Selector.t
  | GetNth of int
  | GetLabeled of string * (t list) option
  | RemoveTag of string
  | Dup
