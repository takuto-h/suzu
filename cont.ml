
open Printf

type t = {
  insns : Insn.t list;
  stack : value list;
  pos : Pos.t;
}

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | String of string

let create insns = {
  insns = insns;
  stack = [];
  pos = Pos.dummy;
}

let push_value cont value =
  {cont with stack = value::cont.stack}

let show_value value =
  begin match value with
    | Unit ->
      "()"
    | Int i ->
      sprintf "%d" i
    | Bool b ->
      sprintf "%B" b
    | Char c ->
      sprintf "%C" c
    | String s ->
      sprintf "%S" s
  end

let value_of_literal lit =
  begin match lit with
    | Literal.Unit ->
      Unit
    | Literal.Int i ->
      Int i
    | Literal.Bool b ->
      Bool b
    | Literal.Char c ->
      Char c
    | Literal.String s ->
      String s
  end

let execute cont insn =
  begin match insn with
    | Insn.At pos ->
      {cont with pos = pos}
    | Insn.Push lit ->
      push_value cont (value_of_literal lit)
  end

let rec run cont =
  begin match (cont.insns, cont.stack) with
    | ([], [top]) ->
      top
    | ([], _) ->
      assert false
    | (insn::insns, _) ->
      let cont = execute {cont with insns = insns} insn in
      run cont
  end
