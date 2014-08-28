
type t =
  | At of Pos.t
  | Push of Literal.t
  | Pop
  | Dup
  | GetNth of int
  | GetLabeled of string * (t list) option
  | RemoveTag of string
  | AssertEqual of Literal.t
  | Test of Pattern.t
  | Check of Pattern.t
  | Branch of t list * t list
  | Call
  | Send of Selector.t
  | Return
  | ReturnModule
  | MakeArgs of int * string list
  | MakeClosure of t list
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
