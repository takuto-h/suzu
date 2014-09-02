
open Printf

exception Error of Pos.t * string

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

let expected parser str_expected =
  Error (parser.pos, sprintf "unexpected %s, expected %s\n" (Token.show parser.token) str_expected)

let expected_at pos str_unexpected str_expected =
  Error (pos, sprintf "unexpected %s, expected %s\n" str_unexpected str_expected)

let lookahead parser =
  begin try
      begin match Lexer.next parser.lexer with
        | (None, pos) ->
          parser.token <- Token.EOF;
          parser.pos <- pos;
        | (Some token, pos) ->
          parser.token <- token;
          parser.pos <- pos;
      end
    with
    | Lexer.Error (pos, message) ->
      raise (Error (pos, message))
  end

let skip parser token =
  if parser.token = token then
    lookahead parser
  else
    ()

let parse_token parser token =
  if parser.token = token then
    lookahead parser
  else
    raise (expected parser (Token.show token))

let parse_non_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.of_op str in
      lookahead parser;
      let rhs = parse_lower parser in
      Expr.at pos (Expr.Send (lhs, op, Expr.Args.make [rhs] None []))
  end

let rec parse_right_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.of_op str in
      lookahead parser;
      let rhs = parse_right_assoc parser get_op parse_lower in
      Expr.at pos (Expr.Send (lhs, op, Expr.Args.make [rhs] None []))
  end

let rec parse_left_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  let rec loop lhs =
    begin match get_op parser.token with
      | None ->
        lhs
      | Some str ->
        let pos = parser.pos in
        let op = Selector.of_op str in
        lookahead parser;
        let rhs = parse_lower parser in
        loop (Expr.at pos (Expr.Send (lhs, op, Expr.Args.make [rhs] None [])))
    end
  in
  loop lhs

let parse_elems parser sep_or_term parse_elem =
  let rec loop elems =
    begin match sep_or_term parser.token with
      | Term ->
        lookahead parser;
        List.rev elems
      | _ ->
        let elem = parse_elem parser in
        begin match sep_or_term parser.token with
          | Term ->
            lookahead parser;
            List.rev (elem::elems)
          | Sep ->
            lookahead parser;
            loop (elem::elems)
          | Neither ->
            raise (expected parser "separator or terminator")
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
      Lexer.indent parser.lexer;
      lookahead parser;
      parse_elems parser semi_or_newline_or_undent parse_elem
    | Token.Reserved "{" ->
      lookahead parser;
      parse_elems parser semi_or_rbrace parse_elem
    | _ ->
      raise (expected parser "':' or '{'")
  end

let parse_literal parser =
  begin match parser.token with
    | Token.Int n ->
      lookahead parser;
      Literal.Int n
    | Token.String str ->
      lookahead parser;
      Literal.String str
    | Token.Char c ->
      lookahead parser;
      Literal.Char c
    | Token.Reserved "true" ->
      lookahead parser;
      Literal.Bool true
    | Token.Reserved "false" ->
      lookahead parser;
      Literal.Bool false
    | _ ->
      raise (expected parser "literal")
  end

let parse_ident parser =
  begin match parser.token with
    | Token.Ident str ->
      lookahead parser;
      str
    | _ ->
      raise (expected parser "identifier")
  end

let parse_selector parser =
  begin match parser.token with
    | Token.Reserved "(" ->
      lookahead parser;
      let sel = begin match (parser.token, Token.get_op parser.token) with
        | (Token.Ident str, _) ->
          lookahead parser;
          if parser.token = Token.Reserved "=" then
            begin
              lookahead parser;
              Selector.of_op (sprintf "%s=" str)
            end
          else
            Selector.of_ident str
        | (Token.Reserved "[", _) ->
          lookahead parser;
          parse_token parser (Token.Reserved "]");
          if parser.token = Token.Reserved "=" then
            begin
              lookahead parser;
              Selector.of_op "[]="
            end
          else
            Selector.of_op "[]"
        | (_, Some str) ->
          lookahead parser;
          Selector.of_op str
        | (_, None) ->
          raise (expected parser "operator")
      end
      in
      parse_token parser (Token.Reserved ")");
      sel
    | Token.Ident _ ->
      Selector.of_ident (parse_ident parser)
    | _ ->
      raise (expected parser "identifier")
  end

let parse_var_or_method parser =
  let ident = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "#" ->
      lookahead parser;
      let sel = parse_selector parser in
      Expr.Method ([], ident, sel)
    | Token.Reserved "::" ->
      lookahead parser;
      let rec loop rev_idents =
        let ident = parse_ident parser in
        begin match parser.token with
          | Token.Reserved "#" ->
            lookahead parser;
            let sel = parse_selector parser in
            Expr.Method (List.rev rev_idents, ident, sel)
          | Token.Reserved "::" ->
            lookahead parser;
            loop (ident::rev_idents)
          | _ ->
            raise (expected parser "'#' or '::'")
        end
      in
      loop [ident]
    | _ ->
      Expr.Var ident
  end

let rec parse_var_or_methods parser rev_voms =
  let vom = parse_var_or_method parser in
  if parser.token <> Token.Reserved "," then
    List.rev (vom::rev_voms)
  else
    begin
      lookahead parser;
      parse_var_or_methods parser (vom::rev_voms)
    end

let parse_export_expr parser pos rev_voms =
  let voms = parse_var_or_methods parser [] in
  Expr.at pos (Expr.Export voms)

let rec parse_expr parser =
  parse_binding_expr parser

and parse_binding_expr parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Reserved "def" ->
      lookahead parser;
      parse_def_expr parser pos
    | Token.Reserved "let" ->
      lookahead parser;
      parse_let_expr parser pos
    | Token.Reserved "export" ->
      lookahead parser;
      parse_export_expr parser pos []
    | Token.Reserved "open" ->
      lookahead parser;
      parse_open_expr parser pos
    | Token.Reserved "include" ->
      lookahead parser;
      parse_include_expr parser pos
    | Token.Reserved "trait" ->
      lookahead parser;
      parse_trait parser pos
    | Token.Reserved "throw" ->
      lookahead parser;
      let expr = parse_expr parser in
      Expr.at pos (Expr.Throw expr)
    | Token.Reserved "exception" ->
      lookahead parser;
      let ctor = parse_ident parser in
      let params = parse_params parser in
      Expr.at pos (Expr.Exception (ctor, params))
    | _ ->
      parse_except_expr parser
  end

and parse_def_expr parser pos =
  let pos_vom = parser.pos in
  let vom = parse_var_or_method parser in
  let pat = Expr.Pattern.at pos_vom (Expr.PatBind vom) in
  begin match parser.token with
    | Token.Reserved "(" ->
      let func = parse_lambda parser pos_vom in
      skip parser Token.Newline;
      parse_token parser (Token.Reserved "end");
      Expr.at pos (Expr.Let (pat, func))
    | _ ->
      raise (expected parser "'('")
  end

and parse_let_expr parser pos =
  let pat = parse_pattern parser in
  begin match parser.token with
    | Token.Reserved "=" ->
      lookahead parser;
      let expr = parse_expr parser in
      Expr.at pos (Expr.Let (pat, expr))
    | _ ->
      raise (expected parser "'='")
  end

and parse_open_expr parser pos =
  let expr = parse_expr parser in
  Expr.at pos (Expr.Open expr)

and parse_include_expr parser pos =
  let expr = parse_expr parser in
  Expr.at pos (Expr.Include expr)

and parse_trait parser pos =
  let pos_vom = parser.pos in
  let vom = parse_var_or_method parser in
  let pat = Expr.Pattern.at pos_vom (Expr.PatBind vom) in
  let (params, body) = parse_function parser in
  skip parser Token.Newline;
  parse_token parser (Token.Reserved "end");
  Expr.at pos (Expr.Let (pat, Expr.at pos (Expr.Trait (params, body))))

and parse_except_expr parser =
  let expr = parse_or_expr parser in
  begin match parser.token with
    | Token.Reserved "except" ->
      let pos = parser.pos in
      lookahead parser;
      let voms = parse_var_or_methods parser [] in
      Expr.at pos (Expr.Except (expr, voms))
    | _ ->
      expr
  end

and parse_or_expr parser =
  let lhs = parse_and_expr parser in
  begin match parser.token with
    | Token.Reserved "||" ->
      let pos = parser.pos in
      lookahead parser;
      let rhs = parse_or_expr parser in
      Expr.at pos (Expr.Or (lhs, rhs))
    | _ ->
      lhs
  end

and parse_and_expr parser =
  let lhs = parse_cmp_expr parser in
  begin match parser.token with
    | Token.Reserved "&&" ->
      let pos = parser.pos in
      lookahead parser;
      let rhs = parse_and_expr parser in
      Expr.at pos (Expr.And (lhs, rhs))
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
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.at pos (Expr.Send (expr, Selector.of_op "~-", Expr.Args.make [] None []))
    | Token.AddOp "+" ->
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.at pos (Expr.Send (expr, Selector.of_op "~+", Expr.Args.make [] None []))
    | Token.CmpOp "!" ->
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.at pos (Expr.Send (expr, Selector.of_op "!", Expr.Args.make [] None []))
    | _ ->
      parse_prim_expr parser
  end

and parse_prim_expr parser =
  let expr = parse_atomic_expr parser in
  let rec loop expr =
    let pos = parser.pos in
    begin match parser.token with
      | Token.Reserved "(" | Token.Reserved "^" | Token.Reserved ":" | Token.Reserved "{" ->
        let args = parse_args parser in
        loop (Expr.at pos (Expr.Call (expr, args)))
      | Token.Reserved "." ->
        let pos = parser.pos in
        lookahead parser;
        let sel = parse_selector parser in
        begin match parser.token with
          | Token.Reserved "(" | Token.Reserved "^" | Token.Reserved ":" | Token.Reserved "{" ->
            let args = parse_args parser in
            loop (Expr.at pos (Expr.Send (expr, sel, args)))
          | Token.Reserved "=" ->
            lookahead parser;
            let value = parse_expr parser in
            let sel = Selector.of_op (sprintf "%s=" (Selector.string_of sel)) in
            Expr.at pos (Expr.Send (expr, sel, Expr.Args.make [value] None []))
          | _ ->
            loop (Expr.at pos (Expr.Send (expr, sel, Expr.Args.make [] None [])))
        end
      | Token.Reserved "[" ->
        lookahead parser;
        let key = parse_expr parser in
        parse_token parser (Token.Reserved "]");
        if parser.token = Token.Reserved "=" then
          begin
            lookahead parser;
            let value = parse_expr parser in
            let sel = Selector.of_op "[]=" in
            Expr.at pos (Expr.Send (expr, sel, Expr.Args.make [key;value] None []))
          end
        else
          let sel = Selector.of_op "[]" in
          loop (Expr.at pos (Expr.Send (expr, sel, Expr.Args.make [key] None [])))
      | _ ->
        expr
    end
  in
  loop expr

and parse_args parser =
  let (rev_normal, rest, rev_labeled) = if parser.token = Token.Reserved "(" then
      begin
        parse_rev_paren_args parser
      end
    else
      ([], None, [])
  in
  let (rev_normal, rev_labeled) = parse_extra_rev_args parser rev_normal rev_labeled in
  Expr.Args.make (List.rev rev_normal) rest (List.rev rev_labeled)

and parse_rev_paren_args parser =
  lookahead parser;
  let rec loop rev_normal =
    begin match parser.token with
      | Token.Reserved ")" ->
        lookahead parser;
        (rev_normal, None, [])
      | Token.MulOp "*" ->
        lookahead parser;
        let rest = parse_expr parser in
        let rev_labeled = begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            []
          | Token.Reserved "," ->
            lookahead parser;
            parse_rev_labeled_args parser
          | _ ->
            raise (expected parser "',' or ')'")
        end
        in
        (rev_normal, Some rest, rev_labeled)
      | Token.Reserved ":" ->
        let rev_labeled = parse_rev_labeled_args parser in
        (rev_normal, None, rev_labeled)
      | _ ->
        let arg = parse_expr parser in
        begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            (arg::rev_normal, None, [])
          | Token.Reserved "," ->
            lookahead parser;
            loop (arg::rev_normal)
          | _ ->
            raise (expected parser "',' or ')'")
        end
    end
  in
  loop []

and parse_rev_labeled_args parser =
  let rec loop rev_labeled =
    begin match parser.token with
      | Token.Reserved ")" ->
        lookahead parser;
        rev_labeled
      | Token.Reserved ":" ->
        let pos_label = parser.pos in
        lookahead parser;
        let label = parse_ident parser in
        begin if List.mem_assoc label rev_labeled then
            raise (expected_at pos_label (sprintf "'%s'" label) "other label")
        end;
        let value = parse_expr parser in
        begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            (label, value)::rev_labeled
          | Token.Reserved "," ->
            lookahead parser;
            loop ((label, value)::rev_labeled)
          | _ ->
            raise (expected parser "',' or ')'")
        end
      | _ ->
        raise (expected parser "':' or ')'")
    end
  in
  loop []

and parse_extra_rev_args parser rev_normal rev_labeled =
  begin match parser.token with
    | Token.Reserved "^" ->
      let pos_lambda = parser.pos in
      lookahead parser;
      let lambda = parse_lambda parser pos_lambda in
      (lambda::rev_normal, parse_extra_rev_labeled_args parser rev_labeled)
    | Token.Reserved ":" ->
      let pos_lambda = parser.pos in
      Lexer.indent parser.lexer;
      lookahead parser;
      if parser.token = Token.Newline then
        begin
          lookahead parser;
          (rev_normal, parse_extra_rev_labeled_args parser rev_labeled)
        end
      else
        let exprs = parse_elems parser semi_or_newline_or_undent parse_expr in
        let lambda = Expr.at pos_lambda (Expr.Lambda (Expr.Params.make [] None [], exprs)) in
        (lambda::rev_normal, parse_extra_rev_labeled_args parser rev_labeled)
    | Token.Reserved "{" ->
      let lambda = parse_block parser in
      (lambda::rev_normal, parse_extra_rev_labeled_args parser rev_labeled)
    | Token.Ident _ ->
      (rev_normal, parse_extra_rev_labeled_args parser rev_labeled)
    | _ ->
      (rev_normal, rev_labeled)
  end

and parse_extra_rev_labeled_args parser rev_labeled_args =
  skip parser Token.Newline;
  if parser.token = Token.Reserved "end" then
    begin
      lookahead parser;
      rev_labeled_args
    end
  else
    let pos_label = parser.pos in
    let (label, value) = parse_extra_labeled_arg parser in
    begin if List.mem_assoc label rev_labeled_args then
        raise (expected_at pos_label (sprintf "'%s'" label) "other label")
    end;
    parse_extra_rev_labeled_args parser ((label, value)::rev_labeled_args)

and parse_extra_labeled_arg parser =
  let label = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "(" ->
      lookahead parser;
      let value = parse_expr parser in
      parse_token parser (Token.Reserved ")");
      (label, value)
    | Token.Reserved "^" ->
      let pos_lambda = parser.pos in
      lookahead parser;
      let lambda = parse_lambda parser pos_lambda in
      (label, lambda)
    | Token.Reserved ":" ->
      let lambda = parse_block parser in
      (label, lambda)
    | Token.Reserved "{" ->
      let lambda = parse_block parser in
      (label, lambda)
    | _ ->
      raise (expected parser "'(' or '^' or ':'")
  end

and parse_atomic_expr parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Int _ | Token.String _ | Token.Char _ | Token.Reserved "true" | Token.Reserved "false" ->
      let lit = parse_literal parser in
      Expr.at pos (Expr.Const lit)
    | Token.Ident _ ->
      let ident = parse_ident parser in
      parse_get_expr parser pos [ident];
    | Token.Reserved "[" ->
      lookahead parser;
      parse_list parser pos
    | Token.Reserved "(" ->
      parse_parens parser
    | Token.Reserved "^" ->
      lookahead parser;
      parse_lambda parser pos
    | Token.Reserved "match" ->
      lookahead parser;
      parse_match_expr parser pos
    | Token.Reserved "try" ->
      lookahead parser;
      parse_try_expr parser pos
    | _ ->
      raise (expected parser "expression")
  end

and parse_get_expr parser pos rev_idents =
  begin match parser.token with
    | Token.Reserved "#" ->
      lookahead parser;
      let sel = parse_selector parser in
      Expr.at pos (Expr.Get ([], Expr.Method (List.rev (List.tl rev_idents), List.hd rev_idents, sel)))
    | Token.Reserved "::" ->
      lookahead parser;
      begin match parser.token with
        | Token.Reserved "(" ->
          lookahead parser;
          let var_or_method = parse_var_or_method parser in
          parse_token parser (Token.Reserved ")");
          Expr.at pos (Expr.Get (List.rev rev_idents, var_or_method))
        | Token.Ident _ ->
          let ident = parse_ident parser in
          parse_get_expr parser pos (ident::rev_idents);
        | _ ->
          raise (expected parser "identifier or '('")
      end
    | _ ->
      Expr.at pos (Expr.Get (List.rev (List.tl rev_idents), Expr.Var (List.hd rev_idents)))
  end

and parse_list parser pos =
  let nil = Expr.at pos (Expr.Get (["List"], Expr.Var "Nil")) in
  let cons head tail =
    Expr.at pos
      (Expr.Call
         (Expr.at pos (Expr.Get (["List"], Expr.Var "Cons")),
          Expr.Args.make [head; tail] None []))
  in    
  let rec loop () =
    begin match parser.token with
      | Token.Reserved "]" ->
        lookahead parser;
        nil
      | _ ->
        let head = parse_expr parser in
        begin match parser.token with
          | Token.Reserved "]" ->
            lookahead parser;
            cons head nil
          | Token.Reserved "," ->
            lookahead parser;
            cons head (loop ())
          | Token.Reserved "|" ->
            lookahead parser;
            let tail = parse_expr parser in
            parse_token parser (Token.Reserved "]");
            cons head tail
          | _ ->
            raise (expected parser "',' or '|' or ']'")
        end
    end
  in
  loop ()

and parse_parens parser =
  let pos = parser.pos in
  let (rev_normal, rest, rev_labeled) = parse_rev_paren_args parser in
  begin match (rev_normal, rest, rev_labeled) with
    | ([], None, []) ->
      Expr.at pos (Expr.Const Literal.Unit)
    | (expr::[], None, []) ->
      expr
    | _ ->
      Expr.at pos (Expr.Args (Expr.Args.make (List.rev rev_normal) rest (List.rev rev_labeled)))
  end

and parse_lambda parser pos =
  let (params, body) = parse_function parser in
  Expr.at pos (Expr.Lambda (params, body))

and parse_function parser =
  if parser.token = Token.Reserved "(" then
    let params = parse_params parser in
    let body = parse_block_like_elems parser parse_expr in
    (params, body)
  else
    let body = parse_block_like_elems parser parse_expr in
    (Expr.Params.make [] None [], body)

and parse_block parser =
  let pos = parser.pos in
  let exprs = parse_block_like_elems parser parse_expr in
  Expr.at pos (Expr.Lambda (Expr.Params.make [] None [], exprs))

and parse_match_expr parser pos =
  let (rev_normal, rest, rev_labeled) = parse_rev_paren_args parser in
  let args = Expr.Args.make (List.rev rev_normal) rest (List.rev rev_labeled) in
  skip parser (Token.Reserved ":");
  let rec loop rev_case_clauses =
    skip parser Token.Newline;
    if parser.token = Token.Reserved "end" then
      begin
        lookahead parser;
        Expr.at pos (Expr.Match (args, List.rev rev_case_clauses))
      end
    else
      let case_clause = parse_case_clause parser in
      loop (case_clause::rev_case_clauses)
  in
  loop []

and parse_case_clause parser =
  parse_token parser (Token.Reserved "case");
  let params = parse_params parser in
  let guard = if parser.token = Token.Reserved "when" then
    begin
      lookahead parser;
      parse_token parser (Token.Reserved "(");
      let cond = parse_expr parser in
      parse_token parser (Token.Reserved ")");
      Some cond
    end
  else
    None
  in
  let body = parse_block_like_elems parser parse_expr in
  (params, guard, body)

and parse_try_expr parser pos =
  let body = parse_block parser in
  skip parser Token.Newline;
  begin match parser.token with
    | Token.Reserved "catch" ->
      let rec loop rev_catches =
        skip parser Token.Newline;
        begin match parser.token with
          | Token.Reserved "end" ->
            lookahead parser;
            Expr.at pos (Expr.TryCatch (body, List.rev rev_catches))
          | Token.Reserved "catch" ->
            lookahead parser;
            parse_token parser (Token.Reserved "(");
            let pat = parse_pattern parser in
            parse_token parser (Token.Reserved ")");
            let exprs = parse_block_like_elems parser parse_expr in
            loop ((pat, exprs)::rev_catches)
          | Token.Reserved "finally" ->
            let body = Expr.at pos (Expr.TryCatch (body, List.rev rev_catches)) in
            let body = Expr.at pos (Expr.Lambda (Expr.Params.make [] None [], [body])) in
            parse_try_finally_expr parser pos body
          | _ ->
            raise (expected parser "'catch' or 'finally' or 'end'")
        end
      in
      loop []
    | Token.Reserved "finally" ->
      parse_try_finally_expr parser pos body
    | _ ->
      raise (expected parser "'catch' or 'finally'")
  end

and parse_try_finally_expr parser pos body =
  lookahead parser;
  let finally = parse_block_like_elems parser parse_expr in
  skip parser Token.Newline;
  parse_token parser (Token.Reserved "end");
  Expr.at pos (Expr.TryFinally (body, finally))

and parse_pattern parser =
  parse_as_pattern parser

and parse_as_pattern parser =
  let pat = parse_or_pattern parser in
  if parser.token = Token.Reserved "as" then
    let pos = parser.pos in
    lookahead parser;
    let ident = parse_ident parser in
    Expr.Pattern.at pos (Expr.PatAs (pat, ident))
  else
    pat

and parse_or_pattern parser =
  let lhs = parse_atomic_pattern parser in
  if parser.token = Token.Reserved "or" then
    let pos = parser.pos in
    lookahead parser;
    let rhs = parse_or_pattern parser in
    Expr.Pattern.at pos (Expr.PatOr (lhs, rhs))
  else
    lhs

and parse_atomic_pattern parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Int _ | Token.String _ | Token.Char _ | Token.Reserved "true" | Token.Reserved "false" ->
      let lit = parse_literal parser in
      Expr.Pattern.at pos (Expr.PatConst lit)
    | Token.Ident "_" ->
      lookahead parser;
      Expr.Pattern.at pos Expr.PatWildCard
    | Token.Ident _ ->
      let pos = parser.pos in
      let vom = parse_var_or_method parser in
      if parser.token = Token.Reserved "(" then
        parse_variant_pattern parser pos vom
      else
        Expr.Pattern.at pos (Expr.PatBind vom)
    | Token.Reserved "[" ->
      lookahead parser;
      parse_list_pattern parser pos
    | Token.Reserved "(" ->
      parse_parens_pattern parser
    | _ ->
      raise (expected parser "pattern")
  end

and parse_variant_pattern parser pos_vom vom =
  begin match vom with
    | Expr.Var ctor ->
      let pos = parser.pos in
      let pats = parse_params parser in
      Expr.Pattern.at pos (Expr.PatVariant (ctor, pats))
    | Expr.Method (_, _, _) ->
      raise (expected_at pos_vom "method pattern" "variant constructor")
  end

and parse_list_pattern parser pos =
  let nil = Expr.Pattern.at pos (Expr.PatVariant ("Nil", Expr.Params.make [] None [])) in
  let cons head tail =
    Expr.Pattern.at pos (Expr.PatVariant ("Cons", Expr.Params.make [head; tail] None []))
  in
  let rec loop () =
    begin match parser.token with
      | Token.Reserved "]" ->
        lookahead parser;
        nil
      | _ ->
        let head = parse_pattern parser in
        begin match parser.token with
          | Token.Reserved "]" ->
            lookahead parser;
            cons head nil
          | Token.Reserved "," ->
            lookahead parser;
            cons head (loop ())
          | Token.Reserved "|" ->
            lookahead parser;
            let tail = parse_pattern parser in
            parse_token parser (Token.Reserved "]");
            cons head tail
          | _ ->
            raise (expected parser "',' or '|' or ']'")
        end
    end
  in
  loop ()

and parse_parens_pattern parser =
  let pos = parser.pos in
  let params = parse_params parser in
  begin match (params.Expr.normal_params, params.Expr.rest_param, params.Expr.labeled_params) with
    | ([], None, []) ->
      Expr.Pattern.at pos (Expr.PatConst Literal.Unit)
    | (pat::[], None, []) ->
      pat
    | _ ->
      Expr.Pattern.at pos (Expr.PatParams params)
  end

and parse_params parser =
  lookahead parser;
  let rec loop rev_normal =
    begin match parser.token with
      | Token.Reserved ")" ->
        lookahead parser;
        (List.rev rev_normal, None, [])
      | Token.MulOp "*" ->
        lookahead parser;
        let rest = parse_pattern parser in
        let labeled = begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            []
          | Token.Reserved "," ->
            lookahead parser;
            parse_labeled_params parser
          | _ ->
            raise (expected parser "',' or ')'")
        end
        in
        (List.rev rev_normal, Some rest, labeled)
      | Token.Reserved ":" ->
        let labeled = parse_labeled_params parser in
        (List.rev rev_normal, None, labeled)
      | _ ->
        let pat = parse_pattern parser in
        begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            (List.rev (pat::rev_normal), None, [])
          | Token.Reserved "," ->
            lookahead parser;
            loop (pat::rev_normal)
          | _ ->
            raise (expected parser "',' or ')'")
        end
    end
  in
  let (normal, rest, labeled) = loop [] in
  Expr.Params.make normal rest labeled

and parse_labeled_params parser =
  let rec loop rev_labeled =
    begin match parser.token with
      | Token.Reserved ")" ->
        lookahead parser;
        List.rev rev_labeled
      | Token.Reserved ":" ->
        lookahead parser;
        let label = parse_ident parser in
        let pattern_and_default = parse_pattern_and_default parser in
        begin match parser.token with
          | Token.Reserved ")" ->
            lookahead parser;
            List.rev ((label, pattern_and_default)::rev_labeled)
          | Token.Reserved "," ->
            lookahead parser;
            loop ((label, pattern_and_default)::rev_labeled)
          | _ ->
            raise (expected parser "',' or ')'")
        end
      | _ ->
        raise (expected parser "':' or ')'")
    end
  in
  loop []

and parse_pattern_and_default parser =
  let pat = parse_pattern parser in
  if parser.token = Token.Reserved "=" then
    begin
      lookahead parser;
      (pat, Some (parse_expr parser))
    end
  else
    (pat, None)

let parse_field_decl parser =
  let mutabl = (parser.token = Token.Reserved "mutable") in
  begin if mutabl then
      lookahead parser
  end;   
  let field = parse_ident parser in
  (field, mutabl)

let parse_ctor_decl parser =
  parse_token parser (Token.Reserved "def");
  let ctor = parse_ident parser in
  let params = parse_params parser in
  (ctor, params)

let parse_class parser pos =
  let klass = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "=" ->
      lookahead parser;
      let ctor = parse_ident parser in
      let fields = parse_block_like_elems parser parse_field_decl in
      skip parser Token.Newline;
      parse_token parser (Token.Reserved "end");
      Expr.at pos (Expr.Record (klass, ctor, fields))
    | Token.Reserved ":" | Token.Reserved "{" ->
      let ctors = parse_block_like_elems parser parse_ctor_decl in
      skip parser Token.Newline;
      parse_token parser (Token.Reserved "end");
      Expr.at pos (Expr.Variant (klass, ctors))
    | _ ->
      Expr.at pos (Expr.Phantom klass)
  end

let rec parse_toplevel parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Reserved "module" ->
      lookahead parser;
      parse_module parser pos
    | Token.Reserved "class" ->
      lookahead parser;
      parse_class parser pos
    | _ ->
      parse_expr parser
  end

and parse_module parser pos =
  let mod_name = parse_ident parser in
  let exprs = parse_block_like_elems parser parse_toplevel in
  skip parser Token.Newline;
  parse_token parser (Token.Reserved "end");
  Expr.at pos (Expr.Module (mod_name, exprs))

let parse_stmt parser =
  let expr = parse_toplevel parser in
  begin match parser.token with
    | Token.EOF | Token.Newline | Token.Reserved ";" ->
      expr
    | _ ->
      raise (expected parser "newline or ';'")
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
