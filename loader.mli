
type t

val create : unit -> t
val load_string : t -> string -> string -> unit
val load_file : t -> string -> unit
val repl : t -> unit
val get_env : t -> VM.env
