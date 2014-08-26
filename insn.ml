
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
  | Pop
  | Dup
  | FindVar of string
  | FindMethod of Selector.t
  | AccessVar of string
  | AccessMethod of Selector.t
  | AddVar of string
  | AddMethod of Selector.t
  | AssertEqual of Literal.t
  | GetNth of int
  | GetLabeled of string * (t list) option
  | RemoveTag of string
  | Test of pattern
  | Branch of t list * t list
  | Call
  | Return
  | MakeArgs of int * string list
  | MakeClosure of t list

let make_params normal_params labeled_params = {
  normal_params = normal_params;
  labeled_params = labeled_params;
}
