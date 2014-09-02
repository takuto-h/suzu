
type frame
type args

type t = {
  mutable insns : Insn.t list;
  mutable stack : value list;
  mutable env : env;
  mutable pos : Pos.t;
  mutable controls  : control list;
  mutable curr_mod_path : string list;
}

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string
  | Class of string
  | Module of frame
  | Args of args
  | Variant of string * string * args
  | Record of string * (string, value) Hashtbl.t
  | Closure of env * Insn.t list
  | Subr of int * bool * string list * (t -> args -> unit)
  | Cont of control list
  | Buffer of Buffer.t

and env = frame list

and control =
  | Dump of Insn.t list * value list * env * Pos.t
  | Catch of Pattern.t * Insn.t list * env * Pos.t
  | Finally of value
  | Reset

exception Error of Pos.t * string * Pos.t list
exception InternalError of string

val create : Insn.t list -> env -> t
val create_subr : int -> ?allows_rest:bool -> ?req_labels:string list -> (t -> args -> unit) -> value
val create_frame : unit -> frame

val make_args : value list -> (string * value) list -> args
val get_arg : args -> int -> value

val get_class : value -> string
val show_value : value -> string
val int_of_value : value -> int
val bool_of_value : value -> bool
val char_of_value : value -> char
val string_of_value : value -> string
val class_of_value : value -> string
val module_of_value : value -> frame
val args_of_value : value -> args
val buffer_of_value : value -> Buffer.t

val add_var : ?export:bool -> env -> string -> value -> unit

val push_value : t -> value -> unit
val call : t -> value -> value -> unit
val run : t -> value
