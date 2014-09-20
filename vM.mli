
type t
type args
type frame
type control

type env = frame list

type insn =
  | At of Pos.t
  | Push of Literal.t
  | Pop
  | Dup
  | Split
  | GetLabeled of string * (insn list) option
  | RemoveTag of string
  | AssertEqual of Literal.t
  | Test of Pattern.t
  | Check of Pattern.t
  | Branch of insn list * insn list
  | Call
  | Send of Selector.t * (string, value) Hashtbl.t
  | Return
  | ReturnModule
  | Fail
  | Begin
  | End
  | BeginModule of string
  | EndModule of string
  | FindVar of string
  | FindMethod of Selector.t
  | AccessVar of string
  | AccessMethod of Selector.t
  | AddVar of string
  | AddMethod of Selector.t
  | ExportVar of string
  | ExportMethod of Selector.t
  | UnexportVar of string
  | UnexportMethod of Selector.t
  | Open
  | Include
  | MakeArgs of int * bool * string list
  | MakeClosure of insn list
  | MakeClass of string
  | MakeRecordCtor of string * string list
  | MakeGetter of string * string
  | MakeSetter of string * string
  | MakeVariantCtor of string * string * Pattern.params
  | MakeExceptionCtor of string * Pattern.params
  | TryCatch of Pattern.t * insn list
  | TryFinally
  | Throw

and value =
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Class of string
  | Module of frame
  | Args of args
  | Variant of string * string * args
  | Record of string * (string, value) Hashtbl.t
  | Closure of env * insn list
  | Subr of int * bool * string list * (t -> args -> unit)
  | Cont of control list
  | Buffer of Buffer.t
  | Hash of (value, value) Hashtbl.t

exception Error of Pos.t * string * Pos.t list
exception InternalError of string

val create : insn list -> env -> t
val create_subr : int -> ?allows_rest:bool -> ?req_labels:string list -> (t -> args -> unit) -> value
val create_frame : unit -> frame

val make_args : value list -> (string * value) list -> args
val get_arg : args -> int -> value

val get_class : value -> string
val show_value : value -> string

val bool_of_value : value -> bool
val int_of_value : value -> int
val float_of_value : value -> float
val char_of_value : value -> char
val string_of_value : value -> string
val args_of_value : value -> args
val buffer_of_value : value -> Buffer.t
val hashtbl_of_value : value -> (value, value) Hashtbl.t

val push_value : t -> value -> unit
val unit : value

val add_var : ?export:bool -> env -> string -> value -> unit
val subr_reset : value
val subr_shift : value

val run : t -> value
val get_env : t -> env
