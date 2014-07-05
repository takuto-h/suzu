
open Printf

type t =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Closure of env * string list * Expr.t
  | Class of string

and env =
  | Global of frame
  | Local of frame * env

and frame = {
  var_table : (string, t) Hashtbl.t;
  method_table : (string * string, t) Hashtbl.t;
}

let class_of value =
  begin match value with
    | Unit ->
      "Unit:C"
    | Int _ ->
      "Int:C"
    | String _ ->
      "String:C"
    | Char _ ->
      "Char:C"
    | Bool _ ->
      "Bool:C"
    | Closure (_,  _, _) ->
      "Closure:C"
    | Class _ ->
      "Class:C"
  end

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
    | Class klass ->
      sprintf "<class %s>" klass
  end

module Frame = struct
  type t = frame

  let create initial_table_size = {
    var_table = Hashtbl.create initial_table_size;
    method_table = Hashtbl.create initial_table_size;
  }

  let find_var {var_table;} x = Hashtbl.find var_table x
  let add_var {var_table;} x v = Hashtbl.add var_table x v

  let find_method {method_table;} klass sel = Hashtbl.find method_table (klass, sel)
  let add_method {method_table;} klass sel meth = Hashtbl.add method_table (klass, sel) meth
end

module Env = struct
  type t = env

  let initial_global_table_size = 16
  let initial_local_table_size = 4
    
  let create_global () = Global (Frame.create initial_global_table_size)
  let create_local outer = Local (Frame.create initial_local_table_size, outer)
    
  let rec find env proc =
    begin match env with
      | Global frame ->
        proc frame 
      | Local (frame, outer) ->
        begin try
          proc frame
        with
          | Not_found ->
            find outer proc
        end
    end

  let add env proc =
    begin match env with
      | Global frame ->
        proc frame
      | Local (frame, _) ->
        proc frame
    end

  let find_var env x = find env (fun frame -> Frame.find_var frame x)
  let add_var env x v = add env (fun frame -> Frame.add_var frame x v)    

  let find_method env klass sel = find env (fun frame -> Frame.find_method frame klass sel)
  let add_method env klass sel meth = find env (fun frame -> Frame.add_method frame klass sel meth)
end
