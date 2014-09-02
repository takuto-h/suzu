
type var_or_method =
  | Var of string
  | Method of string list * string * Selector.t

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Const of Literal.t
  | Get of string list * var_or_method
  | Let of pat * t
  | Lambda of params * t list
  | Call of t * args
  | Send of t * Selector.t * args
  | Args of args
  | And of t * t
  | Or of t * t
  | Match of args * (params * t option * t list) list
  | Module of string * t list
  | Export of var_or_method list
  | Open of t
  | Include of t
  | Record of string * string * (string * bool) list
  | Variant of string * (string * params) list
  | Phantom of string
  | Trait of params * t list
  | Except of t * var_or_method list
  | TryCatch of t * (pat * t list) list
  | TryFinally of t * t list
  | Throw of t
  | Exception of string * params

and pat = {
  pat_pos : Pos.t;
  pat_raw : pat_raw;
}

and pat_raw = 
  | PatWildCard
  | PatConst of Literal.t
  | PatBind of var_or_method
  | PatVariant of string * params
  | PatParams of params
  | PatOr of pat * pat
  | PatAs of pat * string

and params = {
  normal_params : pat list;
  rest_param : pat option;
  labeled_params : (string * (pat * t option)) list;
}

and args = {
  normal_args : t list;
  rest_arg : t option;
  labeled_args : (string * t) list;
}

val at : Pos.t -> raw -> t
val show : t -> string

module Pattern : sig
  val at : Pos.t -> pat_raw -> pat
  val show : pat -> string
end

module Params : sig
  val make : pat list -> pat option -> (string * (pat * t option)) list -> params
  val show : params -> string
end

module Args : sig
  val make : t list -> t option -> (string * t) list -> args
  val show : args -> string
end
