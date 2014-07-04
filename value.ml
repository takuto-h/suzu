
open Printf

type t =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Closure of env * string list * Expr.t 

and env =
  | Global of frame
  | Local of frame * env

and frame = (string, t) Hashtbl.t

let show value =
  begin match value with
    | Unit ->
      "()"
    | Int i ->
      sprintf "%d" i
    | String s ->
      sprintf "%S" s
    | Char c ->
      sprintf "%C" c
    | Bool b ->
      sprintf "%B" b
    | Closure (_,  _, _) ->
      "<closure>"
  end

module Env = struct
  type t = env

  let initial_global_table_size = 16
  let initial_local_table_size = 4
    
  let create_global () = Global (Hashtbl.create initial_global_table_size)
  let create_local outer = Local (Hashtbl.create initial_local_table_size, outer)
    
  let rec find env x =
    begin match env with
      | Global frame ->
        Hashtbl.find frame x
      | Local (frame, outer) ->
        begin try
          Hashtbl.find frame x
        with
          | Not_found ->
            find outer x
        end
    end

  let add env x v =
    begin match env with
      | Global frame ->
        Hashtbl.add frame x v
      | Local (frame, _) ->
        Hashtbl.add frame x v
    end
end
