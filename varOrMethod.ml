
open Printf

type t =
  | Var of string
  | Method of string list * string * Selector.t

let show vom =
  begin match vom with
    | Var x ->
      sprintf "(Var %s)" x
    | Method (mods, klass, sel) ->
      sprintf "(Method (%s %s) %s)" (SnString.concat " " mods) klass (Selector.show sel)
  end
