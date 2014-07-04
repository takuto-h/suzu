
open Printf

type t =
  | Unit
  | Int of int
  | String of string
  | Char of char
  | Bool of bool

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
  end