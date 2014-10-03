
open Printf

type t = 
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string

let show lit =
  begin match lit with
    | Unit ->
      "()"
    | Int i ->
      sprintf "%d" i
    | Float f ->
      sprintf "%f" f
    | Bool b ->
      sprintf "%B" b
    | Char c ->
      sprintf "%C" c
    | String s ->
      sprintf "%S" s
  end
