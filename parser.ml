
open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable pos : Pos.t;
}

let create lexer = {
  lexer = lexer;
  token = Token.EOF;
  pos = Pos.dummy;
}

let expected parser str_token =
  Pos.show_error parser.pos (sprintf "unexpected %s, expected %s\n" (Token.show parser.token) str_token)

let lookahead parser =
  begin match Lexer.next parser.lexer with
    | (None, pos) ->
      begin
        parser.token <- Token.EOF;
        parser.pos <- pos
      end
    | (Some token, pos) ->
      begin
        parser.token <- token;
        parser.pos <- pos
      end
  end

let skip parser token =
  begin if parser.token = token then
    lookahead parser
  else
    ()
  end

let parse_token parser token =
  begin if parser.token <> token then
    failwith (expected parser (Token.show token))
  else
     lookahead parser
  end

let parse_literal parser =
  begin match parser.token with
    | Token.Int n ->
      begin
        lookahead parser;
        Literal.Int n
      end
    | Token.String str ->
      begin
        lookahead parser;
        Literal.String str
      end
    | Token.Char c ->
      begin
        lookahead parser;
        Literal.Char c
      end
    | Token.Reserved "true" ->
      begin
        lookahead parser;
        Literal.Bool true
      end
    | Token.Reserved "false" ->
      begin
        lookahead parser;
        Literal.Bool false
      end
    | _ ->
      failwith (expected parser "literal")
  end

let parse_expr parser =
  Expr.at parser.pos (Expr.Con (parse_literal parser))

let parse_stmt parser =
  let expr = parse_expr parser in
  begin match parser.token with
    | Token.EOF | Token.Newline | Token.Reserved ";" ->
      expr
    | _ ->
      failwith (expected parser "newline or ';'")
  end

let parse parser =
  begin
    lookahead parser;
    begin match parser.token with
      | Token.EOF ->
        None
      | _ ->
        Some (parse_stmt parser)
    end
  end
