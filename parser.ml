
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

let parse_non_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.Op str in
      begin
        lookahead parser;
        let rhs = parse_lower parser in
        Expr.at pos (Expr.MethodCall (lhs, op, [rhs]))
      end
  end

let rec parse_right_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.Op str in
      begin
        lookahead parser;
        let rhs = parse_right_assoc parser get_op parse_lower in
        Expr.at pos (Expr.MethodCall (lhs, op, [rhs]))
      end
  end

let rec parse_left_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  let rec loop lhs =
    begin match get_op parser.token with
      | None ->
        lhs
      | Some str ->
        let pos = parser.pos in
        let op = Selector.Op str in
        begin
          lookahead parser;
          let rhs = parse_lower parser in
          loop (Expr.at pos (Expr.MethodCall (lhs, op, [rhs])))
        end
    end
  in
  loop lhs

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

let parse_selector parser =
  begin match parser.token with
    | Token.Reserved "(" ->
      begin
        lookahead parser;
        let sel = begin match Token.string_of_operator parser.token with
          | Some str ->
            Selector.Op str
          | None ->
            failwith (expected parser "operator")
        end
        in
        lookahead parser;
        parse_token parser (Token.Reserved ")");
        sel
      end
    | Token.Ident _ ->
      Selector.Ident (parse_ident parser)
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_var_or_method parser =
  let ident = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "#" ->
      begin
        lookahead parser;
        let sel = parse_selector parser in
        Expr.Method ([], ident, sel)
      end
    | Token.Reserved ":" ->
      begin
        lookahead parser;
        let rec loop rev_idents =
          let ident = parse_ident parser in
          begin match parser.token with
            | Token.Reserved "#" ->
              begin
                lookahead parser;
                let sel = parse_selector parser in
                Expr.Method (List.rev rev_idents, ident, sel)
              end
            | Token.Reserved ":" ->
              begin
                lookahead parser;
                loop (ident::rev_idents)
              end
            | _ ->
              failwith (expected parser "'#' or ':'")
          end
        in
        loop [ident]
      end
    | _ ->
      Expr.Var ident
  end

let rec parse_expr parser =
  parse_binding_expr parser

and parse_binding_expr parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Reserved "def" ->
      begin
        lookahead parser;
        parse_def_expr parser pos
      end
    | Token.Reserved "module" ->
      begin
        lookahead parser;
        parse_module parser pos
      end
    | _ ->
      parse_or_expr parser
  end

and parse_def_expr parser pos =
  let pos_lambda = parser.pos in
  let var_or_method = parse_var_or_method parser in
  begin match parser.token with
    | Token.Reserved "(" ->
      let func = parse_lambda parser pos_lambda in
      Expr.at pos (Expr.Def (var_or_method, func))
    | Token.Reserved "=" ->
      begin
        lookahead parser;
        let expr = parse_expr parser in
        Expr.at pos (Expr.Def (var_or_method, expr))
      end
    | _ ->
      failwith (expected parser "'=' or '('")
  end

and parse_module parser pos =
  let mod_name = parse_ident parser in
  let exprs = parse_block_like_elems parser parse_expr in
  Expr.at pos (Expr.Module (mod_name, exprs))

and parse_or_expr parser =
  let lhs = parse_and_expr parser in
  begin match parser.token with
    | Token.Reserved "||" ->
      let pos = parser.pos in
      begin
        lookahead parser;
        let rhs = parse_or_expr parser in
        Expr.at pos (Expr.Or (lhs, rhs))
      end
    | _ ->
      lhs
  end

and parse_and_expr parser =
  let lhs = parse_cmp_expr parser in
  begin match parser.token with
    | Token.Reserved "&&" ->
      let pos = parser.pos in
      begin
        lookahead parser;
        let rhs = parse_and_expr parser in
        Expr.at pos (Expr.And (lhs, rhs))
      end
    | _ ->
      lhs
  end

and parse_cmp_expr parser =
  let get_op token =
    begin match token with
      | Token.CmpOp str ->
        Some str
      | _ ->
        None
    end
  in
  parse_non_assoc parser get_op parse_add_expr

and parse_add_expr parser =
  let get_op token =
    begin match token with
      | Token.AddOp str ->
        Some str
      | _ ->
        None
    end
  in
  parse_left_assoc parser get_op parse_mul_expr

and parse_mul_expr parser =
  let get_op token =
    begin match token with
      | Token.MulOp str ->
        Some str
      | _ ->
        None
    end
  in
  parse_left_assoc parser get_op parse_pow_expr

and parse_pow_expr parser =
  let get_op token =
    begin match token with
      | Token.PowOp str ->
        Some str
      | _ ->
        None
    end
  in
  parse_right_assoc parser get_op parse_unary_expr

and parse_unary_expr parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.AddOp "-" ->
      begin
        lookahead parser;
        let expr = parse_unary_expr parser in
        Expr.at pos (Expr.MethodCall (expr, Selector.Op "~-", []))
      end
    | Token.AddOp "+" ->
      begin
        lookahead parser;
        let expr = parse_unary_expr parser in
        Expr.at pos (Expr.MethodCall (expr, Selector.Op "~+", []))
      end
    | Token.CmpOp "!" ->
      begin
        lookahead parser;
        let expr = parse_unary_expr parser in
        Expr.at pos (Expr.MethodCall (expr, Selector.Op "!", []))
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
          let sel = parse_selector parser in
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
    | Token.Ident _ ->
      let ident = parse_ident parser in
      parse_get_expr parser pos [ident];
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
    | Token.Reserved "if" ->
      begin
        lookahead parser;
        parse_if_expr parser pos
      end
    | _ ->
      failwith (expected parser "expression")
  end

and parse_get_expr parser pos rev_idents =
  begin match parser.token with
    | Token.Reserved "#" ->
      begin
        lookahead parser;
        let sel = parse_selector parser in
        Expr.at pos (Expr.Get ([], Expr.Method (List.rev (List.tl rev_idents), List.hd rev_idents, sel)))
      end
    | Token.Reserved ":" ->
      begin
        lookahead parser;
        begin match parser.token with
          | Token.Reserved "(" ->
            begin
              lookahead parser;
              let var_or_method = parse_var_or_method parser in
              parse_token parser (Token.Reserved ")");
              Expr.at pos (Expr.Get (List.rev rev_idents, var_or_method))
            end
          | Token.Ident _ ->
            let ident = parse_ident parser in
            parse_get_expr parser pos (ident::rev_idents);
          | _ ->
            failwith (expected parser "identifier or '('")
        end
      end
    | _ ->
      Expr.at pos (Expr.Get (List.rev (List.tl rev_idents), Expr.Var (List.hd rev_idents)))
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
  Expr.at pos (Expr.FunCall (Expr.at pos (Expr.Lambda ([], exprs)), []))

and parse_if_expr parser pos =
  begin
    parse_token parser (Token.Reserved "(");
    let cond_expr = parse_expr parser in
    parse_token parser (Token.Reserved ")");
    let then_expr = parse_block parser in
    skip parser Token.Newline;
    parse_token parser (Token.Reserved "else");
    let else_expr = parse_block parser in
    Expr.at pos (Expr.Or (Expr.at pos (Expr.And (cond_expr, then_expr)), else_expr))
  end

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
