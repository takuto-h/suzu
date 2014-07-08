
open Printf

type t = 
  | EOF
  | Newline
  | Undent
  | Int of int
  | String of string
  | Char of char
  | Ident of string
  | Reserved of string
  | OrOp of string
  | AndOp of string
  | CmpOp of string
  | AddOp of string
  | MulOp of string
  | PowOp of string


let string_of_operator token =
  begin match token with
    | OrOp str | AndOp str | CmpOp str | AddOp str | MulOp str | PowOp str ->
      Some str
    | _ ->
      None
  end

let show token =
  begin match token with
    | EOF ->
      "EOF"
    | Newline ->
      "newline"
    | Undent ->
      "undent"
    | Int _ ->
      "integer"
    | String _ ->
      "string"
    | Char _ ->
      "character"
    | Ident _ ->
      "identifier"
    | Reserved s | OrOp s | AndOp s | CmpOp s | AddOp s | MulOp s | PowOp s ->
      sprintf "'%s'" s
  end
