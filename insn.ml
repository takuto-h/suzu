
type t =
  | At of Pos.t
  | Push of Literal.t
  | Pop
  | Dup
  | FindVar of string
  | FindMethod of Selector.t
  | AccessVar of string
  | AccessMethod of Selector.t
  | AddVar of string
  | AddMethod of Selector.t
  | AssertEqual of Literal.t
  | GetNth of int
  | GetLabeled of string * (t list) option
  | RemoveTag of string
  | Test of Pattern.t
  | Check of Pattern.t
  | Branch of t list * t list
  | Call
  | Send of Selector.t
  | Return
  | MakeArgs of int * string list
  | MakeClosure of t list
  | Fail
  | Begin
  | End
