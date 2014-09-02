
type source =
  | File
  | String of string

type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
  source : source;
}

val make : string -> int -> int -> int -> source -> t
val dummy : t
val show : t -> string
val show_source : t -> string
val show_message : t -> string -> string
