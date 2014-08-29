
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
  rest_param : t option;
  labeled_params : (string * (t * has_default)) list;
}

let make_params normal_params rest_param labeled_params = {
  normal_params = normal_params;
  rest_param = rest_param;
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

and show_params {normal_params;rest_param;labeled_params} =
  let str_normal = SnString.concat_map ", " show normal_params in
  let str_labeled = SnString.concat_map ", " show_labeled_param labeled_params in
  begin match rest_param with
    | Some rest_param ->
      let str_rest = sprintf "*%s" (show rest_param) in
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_rest; str_labeled])
    | None ->
      sprintf "(%s)" (SnString.concat ", " [str_normal; str_labeled])
  end

and show_labeled_param (label, (pat, has_default)) =
  if has_default then
    sprintf ":%s %s = <expr>" label (show pat)
  else
    sprintf ":%s %s" label (show pat)
