
open Printf

type t = 
  | EOF
  | Newline
  | Undent
  | Int of int
  | Char of char
  | String of string
  | Ident of string
  | Reserved of string
  | CmpOp of string
  | AddOp of string
  | MulOp of string
  | UnaryOp of string

let get_op token =
  begin match token with
    | CmpOp str | AddOp str | MulOp str | UnaryOp str ->
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
    | Char _ ->
      "character"
    | String _ ->
      "string"
    | Ident _ ->
      "identifier"
    | Reserved s | CmpOp s | AddOp s | MulOp s | UnaryOp s ->
      sprintf "'%s'" s
  end
