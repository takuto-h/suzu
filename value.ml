
open Printf

type t =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Closure of env * string list * Expr.t list
  | Class of string
  | Module of env

and env =
  | Global of frame
  | Local of frame * env

and frame = {
  var_table : (string, t) Hashtbl.t;
  method_table : (string * string, t) Hashtbl.t;
}

type value = t

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
    | Module _ ->
      "Module:C"
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
    | Module _ ->
      "<module>"
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

  exception Module_not_found of string
  exception Not_a_module of string * value

  let initial_global_table_size = 16
  let initial_local_table_size = 4
    
  let create_global () = Global (Frame.create initial_global_table_size)
  let create_local outer = Local (Frame.create initial_local_table_size, outer)
    
  let rec find_name proc env =
    begin match env with
      | Global frame ->
        proc frame 
      | Local (frame, outer) ->
        begin try
          proc frame
        with
          | Not_found ->
            find_name proc outer
        end
    end

  let rec find_path proc env mods =
    begin match mods with
      | [] ->
        find_name proc env
      | mod_name::mods ->
        let modl = begin try
          find_name (fun frame -> Frame.find_var frame mod_name) env
        with
          | Not_found ->
            raise (Module_not_found mod_name)
        end
        in
        begin match modl with
          | Module modl ->
            find_path proc modl mods
          | _ ->
            raise (Not_a_module (mod_name, modl))
        end
    end

  let add_name proc env =
    begin match env with
      | Global frame ->
        proc frame
      | Local (frame, _) ->
        proc frame
    end

  let rec find_var env mods x =
    find_path (fun frame -> Frame.find_var frame x) env mods

  let add_var env x v =
    add_name (fun frame -> Frame.add_var frame x v) env

  let find_method env mods klass sel =
    find_path (fun frame -> Frame.find_method frame klass sel) env mods

  let add_method env klass sel meth =
    add_name (fun frame -> Frame.add_method frame klass sel meth) env
end
