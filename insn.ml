
open Printf

type has_rest = bool

type t =
  | At of Pos.t
  | Push of Literal.t
  | Pop
  | Dup
  | Split
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
  | TryFinally
  | TryCatch of Pattern.t * t list
  | Throw

let rec show insn =
  begin match insn with
    | At pos ->
      sprintf "(At %s)" (Pos.show pos)
    | Push lit ->
      sprintf "(Push %s)" (Literal.show lit)
    | Pop ->
      "Pop"
    | Dup ->
      "Dup"
    | Split ->
      "Split"
    | GetLabeled (label, None) ->
      sprintf "(GetLabeled %s)" label
    | GetLabeled (label, Some insns) ->
      sprintf "(GetLabeled %s (%s))" label (SnString.concat_map " " show insns)
    | RemoveTag tag ->
      sprintf "(RemoveTag %s)" tag
    | AssertEqual lit ->
      sprintf "(AssertEqual %s)" (Literal.show lit)
    | Test pat ->
      sprintf "(Test %s)" (Pattern.show pat)
    | Check pat ->
      sprintf "(Check %s)" (Pattern.show pat)
    | Branch (then_insns, else_insns) ->
      sprintf "(Branch (%s) (%s))" (SnString.concat_map " " show then_insns) (SnString.concat_map " " show else_insns)
    | Call ->
      "Call"
    | Send sel ->
      sprintf "(Send %s)" (Selector.show sel)
    | Return ->
      "Return"
    | ReturnModule ->
      "ReturnModule"
    | Fail ->
      "Fail"
    | Begin ->
      "Begin"
    | End ->
      "End"
    | BeginModule name ->
      sprintf "(BeginModule %s)" name
    | EndModule name ->
      sprintf "(EndModule %s)" name
    | FindVar x ->
      sprintf "(FindVar %s)" x
    | FindMethod sel ->
      sprintf "(FindMethod %s)" (Selector.show sel)
    | AccessVar x ->
      sprintf "(AccessVar %s)" x
    | AccessMethod sel ->
      sprintf "(AccessMethod %s)" (Selector.show sel)
    | AddVar x ->
      sprintf "(AddVar %s)" x
    | AddMethod sel ->
      sprintf "(AddMethod %s)" (Selector.show sel)
    | ExportVar x ->
      sprintf "(ExportVar %s)" x
    | ExportMethod sel ->
      sprintf "(ExportMethod %s)" (Selector.show sel)
    | UnexportVar x ->
      sprintf "(UnexportVar %s)" x
    | UnexportMethod sel ->
      sprintf "(UnexportMethod %s)" (Selector.show sel)
    | Open ->
      "Open"
    | Include ->
      "Include"
    | MakeArgs (count, has_rest, labels) ->
      sprintf "(MakeArgs %d %B (%s)))" count has_rest (SnString.concat " " labels)
    | MakeClosure insns ->
      sprintf "(MakeClosure (%s)))" (SnString.concat_map " " show insns)
    | MakeClass klass ->
      sprintf "(MakeClass %s)" klass
    | MakeRecordCtor (klass, fields) ->
      sprintf "(MakeRecordCtor %s (%s))" klass (SnString.concat " " fields)
    | MakeGetter (klass, field) ->
      sprintf "(MakeGetter %s %s)" klass field
    | MakeSetter (klass, field) ->
      sprintf "(MakeSetter %s %s)" klass field
    | MakeVariantCtor (klass, ctor, params) ->
      sprintf "(MakeVariantCtor %s %s %s)" klass ctor (Pattern.show_params params)
    | MakeExceptionCtor (ctor, params) ->
      sprintf "(MakeExceptionCtor %s %s)" ctor (Pattern.show_params params)
    | TryFinally ->
      "TryFinally"
    | TryCatch (pat, insns) ->
      sprintf "(TryCatch %s (%s))" (Pattern.show pat) (SnString.concat_map " " show insns)
    | Throw ->
      "Throw"
  end  
  
