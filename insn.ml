
open Printf

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
  | Check of pattern
  | Branch of t list * t list
  | Call
  | Send of Selector.t
  | Return
  | MakeArgs of int * string list
  | MakeClosure of t list
  | Fail
  | Begin
  | End

let make_params normal_params labeled_params = {
  normal_params = normal_params;
  labeled_params = labeled_params;
}

let rec show_pattern pat =
  begin match pat with
    | Any ->
      "_"
    | Const lit ->
      Literal.show lit
    | Params params ->
      show_params params
    | Variant (tag, params) ->
      sprintf "%s%s" tag (show_params params)
    | Or (lhs, rhs) ->
      sprintf "(%s | %s)" (show_pattern lhs) (show_pattern rhs)
  end

and show_params {normal_params;labeled_params} =
  let str_normal = SnString.concat_map ", " show_pattern normal_params in
  let str_labeled = SnString.concat_map ", " show_labeled_param labeled_params in
  if List.length normal_params <> 0 && List.length labeled_params <> 0 then
    sprintf "(%s, %s)" str_normal str_labeled
  else
    sprintf "(%s%s)" str_normal str_labeled

and show_labeled_param (label, (pat, has_default)) =
  if has_default then
    sprintf ":%s %s = <expr>" label (show_pattern pat)
  else
    sprintf ":%s %s" label (show_pattern pat)
