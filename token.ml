
open Printf

type t = 
  | EOF
  | Newline
  | Undent
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Ident of string
  | Reserved of string
  | CmpOp of string
  | OrOp of string
  | AndOp of string
  | AddOp of string
  | MulOp of string
  | PowOp of string
  | UnaryOp of string

let get_op token =
  begin match token with
    | CmpOp str | OrOp str | AndOp str | AddOp str | MulOp str | PowOp str | UnaryOp str ->
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
    | Float _ ->
      "float"
    | Char _ ->
      "character"
    | String _ ->
      "string"
    | Ident _ ->
      "identifier"
    | Reserved s | CmpOp s | OrOp s | AndOp s | AddOp s | MulOp s | PowOp s | UnaryOp s ->
      sprintf "'%s'" s
  end
