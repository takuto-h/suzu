
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

val make_params : t list -> t option -> (string * (t * has_default)) list -> params
val show : t -> string
val show_params : params -> string
