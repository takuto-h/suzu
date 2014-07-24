
open Printf

type t =
  | Var of string
  | Method of string list * string * Selector.t

let show vom =
  begin match vom with
    | Var x ->
      x
    | Method (mods, klass, sel) ->
      if mods = [] then
        sprintf "%s#%s" klass (Selector.show sel)
      else
        sprintf "%s:%s#%s" (SnString.concat ":" mods) klass (Selector.show sel)
  end
