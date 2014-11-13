
type has_rest = bool

type t =
  | At of Pos.t
  | Push of Literal.t
  | Pop
  | Dup
  | Split
  | SplitLabeled of string * (t list) option
  | RemoveTag of string
  | AssertEqual of Literal.t
  | Test of Pattern.t
  | Check of Pattern.t
  | Branch of t list * t list
  | Call
  | Send of Selector.t
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
  | MakeArgs of int * has_rest * string list
  | MakeClosure of t list
  | MakeClass of string
  | MakeRecordCtor of string * string list
  | MakeGetter of string * string
  | MakeSetter of string * string
  | MakeVariantCtor of string * string * Pattern.params
  | MakeExceptionCtor of string * Pattern.params
  | TryCatch of Pattern.t * t list
  | TryFinally
  | Throw

val show : t -> string
