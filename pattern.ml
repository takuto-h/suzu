
open Printf

type has_default = bool

type t =
  | Any
  | Const of Literal.t
  | Params of params
  | Variant of string * params
  | Or of t * t

and params = {
  normal_params : t list;
  labeled_params : (string * (t * has_default)) list;
}

let make_params normal_params labeled_params = {
  normal_params = normal_params;
  labeled_params = labeled_params;
}

let rec show pat =
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
      sprintf "(%s | %s)" (show lhs) (show rhs)
  end

and show_params {normal_params;labeled_params} =
  let str_normal = SnString.concat_map ", " show normal_params in
  let str_labeled = SnString.concat_map ", " show_labeled_param labeled_params in
  if List.length normal_params <> 0 && List.length labeled_params <> 0 then
    sprintf "(%s, %s)" str_normal str_labeled
  else
    sprintf "(%s%s)" str_normal str_labeled

and show_labeled_param (label, (pat, has_default)) =
  if has_default then
    sprintf ":%s %s = <expr>" label (show pat)
  else
    sprintf ":%s %s" label (show pat)
