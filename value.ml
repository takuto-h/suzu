
open Printf

module VarSet = Set.Make(struct type t = string let compare = compare end)
module MethodSet = Set.Make(struct type t = string * string let compare = compare end)

type t =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool
  | Closure of env * string list * Expr.t list
  | Subr of int * (Pos.t -> t list -> t)
  | Module of env
  | Class of string
  | Record of string * (string, t) Hashtbl.t

and env =
  | Global of frame
  | Local of frame * env

and frame = {
  mutable exported_vars : VarSet.t;
  mutable exported_methods : MethodSet.t;
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
    | Subr (_, _) ->
      "Subr:C"
    | Module _ ->
      "Module:C"
    | Class _ ->
      "Class:C"
    | Record (klass, _) ->
      klass
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
    | Subr (_, _) ->
      "<subr>"
    | Module _ ->
      "<module>"
    | Class klass ->
      sprintf "<class %s>" klass
    | Record (klass, _) ->
      sprintf "<record %s>" klass
  end

module Frame = struct
  type t = frame

  type var_or_method =
    | Var of string
    | Method of string * string

  type find_from =
    | Inside
    | Outside

  let create initial_table_size = {
    exported_vars = VarSet.empty;
    exported_methods = MethodSet.empty;
    var_table = Hashtbl.create initial_table_size;
    method_table = Hashtbl.create initial_table_size;
  }

  let find_var {exported_vars;var_table;} x from =
    begin match from with
      | Outside when not (VarSet.mem x exported_vars) ->
        raise Not_found
      | _ ->
        Hashtbl.find var_table x
    end

  let find_method {exported_methods;method_table;} klass sel from =
    begin match from with
      | Outside when not (MethodSet.mem (klass, sel) exported_methods) ->
        raise Not_found
      | _ ->
        Hashtbl.find method_table (klass, sel)
    end

  let find_binding frame vom from =
    begin match vom with
      | Var x ->
        find_var frame x from
      | Method (klass, sel) ->
        find_method frame klass sel from
    end

  let add_var ({exported_vars;var_table;} as frame) x v export =
    begin
      begin if export then
        frame.exported_vars <- VarSet.add x exported_vars
      end;
      Hashtbl.add var_table x v
    end

  let add_method ({exported_methods;method_table;} as frame) klass sel meth export =
    begin
      begin if export then
        frame.exported_methods <- MethodSet.add (klass, sel) exported_methods
      end;
      Hashtbl.add method_table (klass, sel) meth
    end

  let add_binding frame vom value export =
    begin match vom with
      | Var x ->
        add_var frame x value export
      | Method (klass, sel) ->
        add_method frame klass sel value export
    end
end

module Env = struct
  type t = env

  exception Module_not_found of string
  exception Not_a_module of string * value

  let initial_global_table_size = 16
  let initial_local_table_size = 4
    
  let create_global () = Global (Frame.create initial_global_table_size)
  let create_local outer = Local (Frame.create initial_local_table_size, outer)
  
  let rec lookup env vom =
    begin match env with
      | Global frame ->
        Frame.find_binding frame vom Frame.Inside
      | Local (frame, outer) ->
        begin try
          Frame.find_binding frame vom Frame.Inside
        with
          | Not_found ->
            lookup outer vom
        end
    end

  let with_current_frame proc env =
    begin match env with
      | Global frame ->
        proc frame 
      | Local (frame, _) ->
        proc frame
    end

  let rec find_module_binding modl mods vom =
    begin match mods with
      | [] ->
        with_current_frame (fun frame -> Frame.find_binding frame vom Frame.Outside) modl
      | mod_name::mods ->
        let modl = begin try
          with_current_frame (fun frame -> Frame.find_var frame mod_name Frame.Outside) modl
        with
          | Not_found ->
            raise (Module_not_found mod_name)
        end
        in
        begin match modl with
          | Module modl ->
            find_module_binding modl mods vom
          | _ ->
            raise (Not_a_module (mod_name, modl))
        end
    end

  let find_binding env mods vom =
    begin match mods with
      | [] ->
        lookup env vom
      | mod_name::mods ->
        let modl = begin try
          lookup env (Frame.Var mod_name)
        with
          | Not_found ->
            raise (Module_not_found mod_name)
        end
        in
        begin match modl with
          | Module modl ->
            find_module_binding modl mods vom
          | _ ->
            raise (Not_a_module (mod_name, modl))
        end
    end

  let find_var env mods x =
    find_binding env mods (Frame.Var x)

  let find_method env mods klass sel =
    find_binding env mods (Frame.Method (klass, sel))

  let add_var ?(export=false) env x v =
    with_current_frame (fun frame -> Frame.add_binding frame (Frame.Var x) v export) env

  let add_method ?(export=false) env klass sel meth =
    with_current_frame (fun frame -> Frame.add_binding frame (Frame.Method (klass, sel)) meth export) env
end
