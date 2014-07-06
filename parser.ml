
open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable pos : Pos.t;
}

type sep_or_term = 
  | Sep
  | Term
  | Neither

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

let parse_elems parser sep_or_term parse_elem =
  let rec loop elems =
    begin match sep_or_term parser.token with
      | Term ->
        begin
          lookahead parser;
          List.rev elems
        end
      | _ ->
        let elem = parse_elem parser in
        begin match sep_or_term parser.token with
          | Term ->
            begin
              lookahead parser;
              List.rev (elem::elems)
            end
          | Sep ->
            begin
              lookahead parser;
              loop (elem::elems)
            end
          | Neither ->
            failwith (expected parser "separator or terminator")
        end
    end
  in
  loop []

let comma_or_rparen token =
  begin match token with
    | Token.Reserved "," ->
      Sep
    | Token.Reserved ")" ->
      Term
    | _ ->
      Neither
  end

let semi_or_newline_or_undent token =
  begin match token with
    | Token.Reserved ";" ->
      Sep
    | Token.Newline ->
      Sep
    | Token.Undent ->
      Term
    | _ ->
      Neither
  end

let semi_or_rbrace token =
  begin match token with
    | Token.Reserved ";" ->
      Sep
    | Token.Reserved "}" ->
      Term
    | _ ->
      Neither
  end

let parse_block_like_elems parser parse_elem =
  begin match parser.token with
    | Token.Reserved ":" ->
      begin
        Lexer.indent parser.lexer;
        lookahead parser;
        parse_elems parser semi_or_newline_or_undent parse_elem
      end
    | Token.Reserved "{" ->
      begin
        lookahead parser;
        parse_elems parser semi_or_rbrace parse_elem
      end
    | _ ->
      failwith (expected parser "':' or '{'")
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

let parse_ident parser =
  begin match parser.token with
    | Token.Ident str ->
      begin
        lookahead parser;
        str
      end
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_params parser =
  begin
    parse_token parser (Token.Reserved "(");
    parse_elems parser comma_or_rparen parse_ident
  end

let rec parse_expr parser =
  parse_def_expr parser

and parse_def_expr parser =
  begin match parser.token with
    | Token.Reserved "def" ->
      let pos = parser.pos in
      begin
        lookahead parser;
        let ident = parse_ident parser in
        parse_token parser (Token.CmpOp "=");
        let expr = parse_expr parser in
        Expr.at pos (Expr.Define (ident, expr))
      end
    | _ ->
      parse_dot_expr parser
  end

and parse_dot_expr parser =
  let expr = parse_prim_expr parser in
  let rec loop recv =
    begin match parser.token with
      | Token.Reserved "." ->
        let pos = parser.pos in
        begin
          lookahead parser;
          let sel = parse_ident parser in
          begin match parser.token with
            | Token.Reserved "(" ->
              begin
                lookahead parser;
                let args = parse_args parser in
                loop (Expr.at pos (Expr.MethodCall (recv, sel, args)))
              end
            | _ ->
              Expr.at pos (Expr.MethodCall (recv, sel, []))
          end
        end
      | _ ->
        recv
    end
  in
  loop expr

and parse_prim_expr parser =
  let expr = parse_atomic_expr parser in
  let rec loop func =
    begin match parser.token with
      | Token.Reserved "(" ->
        let pos = parser.pos in
        begin
          lookahead parser;
          let args = parse_args parser in
          loop (Expr.at pos (Expr.FunCall (func, args)))
        end
      | _ ->
        func
    end
  in
  loop expr

and parse_args parser =
  parse_elems parser comma_or_rparen parse_expr

and parse_atomic_expr parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Int _ | Token.String _ | Token.Char _ | Token.Reserved "true" | Token.Reserved "false" ->
      let lit = parse_literal parser in
      Expr.at pos (Expr.Const lit)
    | Token.Ident ident ->
      begin
        lookahead parser;
        parse_get_expr parser pos ident;
      end
    | Token.Reserved "(" ->
      begin
        lookahead parser;
        parse_parens parser pos
      end
    | Token.Reserved "^" ->
      begin
        lookahead parser;
        parse_lambda parser pos
      end
    | _ ->
      failwith (expected parser "expression")
  end

and parse_get_expr parser pos ident =
  begin match parser.token with
    | Token.Reserved "#" ->
      let pos = parser.pos in
      begin
        lookahead parser;
        let sel = parse_ident parser in
        Expr.at pos (Expr.Get (Expr.Method (ident, sel)))
      end
    | _ ->
      Expr.at pos (Expr.Get (Expr.Var ident))
  end

and parse_parens parser pos =
  if parser.token = Token.Reserved ")" then
    begin
      lookahead parser;
      Expr.at pos (Expr.Const Literal.Unit)
    end
  else
    let expr = parse_expr parser in
    begin
      parse_token parser (Token.Reserved ")");
      expr
    end

and parse_lambda parser pos =
  let params = parse_params parser in
  let body = parse_block_like_elems parser parse_expr in
  Expr.at pos (Expr.Lambda (params, body))

and parse_block parser =
  let pos = parser.pos in
  let exprs = parse_block_like_elems parser parse_expr in
  Expr.at pos (Expr.Block exprs)

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
