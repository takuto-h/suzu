
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

let expected_at pos str_unexpected str_expected =
  Pos.show_error pos (sprintf "unexpected %s, expected %s\n" str_unexpected str_expected)

let lookahead parser =
  begin match Lexer.next parser.lexer with
    | (None, pos) ->
      parser.token <- Token.EOF;
      parser.pos <- pos
    | (Some token, pos) ->
      parser.token <- token;
      parser.pos <- pos
  end

let skip parser token =
  if parser.token = token then
    lookahead parser
  else
    ()

let parse_token parser token =
  if parser.token <> token then
    failwith (expected parser (Token.show token))
  else
    lookahead parser

let parse_non_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.Op str in
      lookahead parser;
      let rhs = parse_lower parser in
      Expr.at pos (Expr.MethodCall (lhs, op, Expr.Args.make [rhs] []))
  end

let rec parse_right_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  begin match get_op parser.token with
    | None ->
      lhs
    | Some str ->
      let pos = parser.pos in
      let op = Selector.Op str in
      lookahead parser;
      let rhs = parse_right_assoc parser get_op parse_lower in
      Expr.at pos (Expr.MethodCall (lhs, op, Expr.Args.make [rhs] []))
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
        lookahead parser;
        let rhs = parse_lower parser in
        loop (Expr.at pos (Expr.MethodCall (lhs, op, Expr.Args.make [rhs] [])))
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
      Lexer.indent parser.lexer;
      lookahead parser;
      parse_elems parser semi_or_newline_or_undent parse_elem
    | Token.Reserved "{" ->
      lookahead parser;
      parse_elems parser semi_or_rbrace parse_elem
    | _ ->
      failwith (expected parser "':' or '{'")
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
      failwith (expected parser "literal")
  end

let parse_ident parser =
  begin match parser.token with
    | Token.Ident str ->
      lookahead parser;
      str
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_selector parser =
  begin match parser.token with
    | Token.Reserved "(" ->
      lookahead parser;
      let sel = begin match (parser.token, Token.string_of_operator parser.token) with
        | (Token.Ident str, _) ->
          lookahead parser;
          if parser.token = Token.Reserved "=" then
            begin
              lookahead parser;
              Selector.Op (sprintf "%s=" str)
            end
          else
            Selector.Ident str
        | (_, Some str) ->
          lookahead parser;
          Selector.Op str
        | (_, None) ->
          failwith (expected parser "operator")
      end
      in
      parse_token parser (Token.Reserved ")");
      sel
    | Token.Ident _ ->
      Selector.Ident (parse_ident parser)
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_var_or_method parser =
  let ident = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "#" ->
      lookahead parser;
      let sel = parse_selector parser in
      VarOrMethod.Method ([], ident, sel)
    | Token.Reserved "::" ->
      lookahead parser;
      let rec loop rev_idents =
        let ident = parse_ident parser in
        begin match parser.token with
          | Token.Reserved "#" ->
            lookahead parser;
            let sel = parse_selector parser in
            VarOrMethod.Method (List.rev rev_idents, ident, sel)
          | Token.Reserved "::" ->
            lookahead parser;
            loop (ident::rev_idents)
          | _ ->
            failwith (expected parser "'#' or '::'")
        end
      in
      loop [ident]
    | _ ->
      VarOrMethod.Var ident
  end

let rec parse_pattern parser =
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
  let rec loop lhs =
    if parser.token = Token.Reserved "|" then
      let pos = parser.pos in
      lookahead parser;
      let rhs = parse_atomic_pattern parser in
      loop (Expr.Pattern.at pos (Expr.PatOr (lhs, rhs)))
    else
      lhs
  in
  loop lhs

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
    | Token.Reserved "(" ->
      parse_parens_pattern parser
    | _ ->
      failwith (expected parser "pattern")
  end

and parse_variant_pattern parser pos_vom vom =
  begin match vom with
    | VarOrMethod.Var ctor ->
      let pos = parser.pos in
      let pats = parse_params parser in
      Expr.Pattern.at pos (Expr.PatVariant (ctor, pats))
    | VarOrMethod.Method (_, _, _) ->
      failwith (expected_at pos_vom "method pattern" "variant constructor")
  end

and parse_parens_pattern parser =
  let pos = parser.pos in
  let params = parse_params parser in
  begin match (params.Expr.normal_params, params.Expr.keyword_params) with
    | ([], []) ->
      Expr.Pattern.at pos (Expr.PatConst Literal.Unit)
    | (pat::[], []) ->
      pat
    | _ ->
      failwith "TODO: Implement tuple pattern"
  end

and parse_params parser =
  parse_token parser (Token.Reserved "(");
  let normals = parse_elems parser comma_or_rparen parse_pattern in
  Expr.Params.make normals []

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
    | Token.Reserved "trait" ->
      lookahead parser;
      parse_trait parser pos
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
      failwith (expected parser "'('")
  end

and parse_let_expr parser pos =
  let pat = parse_pattern parser in
  begin match parser.token with
    | Token.Reserved "=" ->
      lookahead parser;
      let expr = parse_expr parser in
      Expr.at pos (Expr.Let (pat, expr))
    | _ ->
      failwith (expected parser "'='")
  end

and parse_open_expr parser pos =
  let expr = parse_expr parser in
  Expr.at pos (Expr.Open expr)

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
      Expr.at pos (Expr.MethodCall (expr, Selector.Op "~-", Expr.Args.make [] []))
    | Token.AddOp "+" ->
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.at pos (Expr.MethodCall (expr, Selector.Op "~+", Expr.Args.make [] []))
    | Token.CmpOp "!" ->
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.at pos (Expr.MethodCall (expr, Selector.Op "!", Expr.Args.make [] []))
    | _ ->
      parse_dot_expr parser
  end

and parse_dot_expr parser =
  let expr = parse_prim_expr parser in
  let rec loop recv =
    begin match parser.token with
      | Token.Reserved "." ->
        let pos = parser.pos in
        lookahead parser;
        let sel = parse_selector parser in
        begin match parser.token with
          | Token.Reserved "(" | Token.Reserved "^" | Token.Reserved ":" | Token.Reserved "{" ->
            let args = parse_args parser in
            loop (Expr.at pos (Expr.MethodCall (recv, sel, args)))
          | Token.Reserved "=" ->
            lookahead parser;
            let expr = parse_expr parser in
            Expr.at pos (Expr.MethodCall (recv, Selector.Op (sprintf "%s=" (Selector.string_of sel)), Expr.Args.make [expr] []))
          | _ ->
            Expr.at pos (Expr.MethodCall (recv, sel, Expr.Args.make [] []))
        end
      | _ ->
        recv
    end
  in
  loop expr

and parse_prim_expr parser =
  let expr = parse_atomic_expr parser in
  let rec loop func =
    let pos = parser.pos in
    begin match parser.token with
      | Token.Reserved "(" | Token.Reserved "^" | Token.Reserved ":" | Token.Reserved "{" ->
        let args = parse_args parser in
        loop (Expr.at pos (Expr.FunCall (func, args)))
      | _ ->
        func
    end
  in
  loop expr

and parse_args parser =
  let normals = if parser.token = Token.Reserved "(" then
      begin
        lookahead parser;
        parse_elems parser comma_or_rparen parse_expr
      end
    else
      []
  in
  begin match parser.token with
    | Token.Reserved "^" ->
      let pos_lambda = parser.pos in
      lookahead parser;
      let lambda = parse_lambda parser pos_lambda in
      Expr.Args.make (normals @ [lambda]) (parse_keyword_args parser [])
    | Token.Reserved ":" ->
      let pos_lambda = parser.pos in
      Lexer.indent parser.lexer;
      lookahead parser;
      if parser.token = Token.Newline then
        begin
          lookahead parser;
          Expr.Args.make normals (parse_keyword_args parser [])
        end
      else
        let exprs = parse_elems parser semi_or_newline_or_undent parse_expr in
        let lambda = Expr.at pos_lambda (Expr.Lambda (Expr.Params.make [] [], exprs)) in
        Expr.Args.make (normals @ [lambda]) (parse_keyword_args parser [])
    | Token.Reserved "{" ->
      let lambda = parse_block parser in
      Expr.Args.make (normals @ [lambda]) (parse_keyword_args parser [])
    | Token.Ident _ ->
      Expr.Args.make normals (parse_keyword_args parser [])
    | _ ->
      Expr.Args.make normals []
  end

and parse_keyword_args parser rev_keyword_args =
  skip parser Token.Newline;
  if parser.token = Token.Reserved "end" then
    begin
      lookahead parser;
      List.rev rev_keyword_args
    end
  else
    let keyword_arg = parse_keyword_arg parser in
    parse_keyword_args parser (keyword_arg::rev_keyword_args)

and parse_keyword_arg parser =
  let key = parse_ident parser in
  begin match parser.token with
    | Token.Reserved "(" ->
      lookahead parser;
      let value = parse_expr parser in
      parse_token parser (Token.Reserved ")");
      (key, value)
    | Token.Reserved "^" ->
      let pos_lambda = parser.pos in
      lookahead parser;
      let lambda = parse_lambda parser pos_lambda in
      (key, lambda)
    | Token.Reserved ":" ->
      let lambda = parse_block parser in
      (key, lambda)
    | Token.Reserved "{" ->
      let lambda = parse_block parser in
      (key, lambda)
    | _ ->
      failwith (expected parser "'(' or '^' or ':'")
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
    | Token.Reserved "(" ->
      lookahead parser;
      parse_parens parser pos
    | Token.Reserved "^" ->
      lookahead parser;
      parse_lambda parser pos
    | Token.Reserved "match" ->
      lookahead parser;
      parse_match_expr parser pos
    | _ ->
      failwith (expected parser "expression")
  end

and parse_get_expr parser pos rev_idents =
  begin match parser.token with
    | Token.Reserved "#" ->
      lookahead parser;
      let sel = parse_selector parser in
      Expr.at pos (Expr.Get ([], VarOrMethod.Method (List.rev (List.tl rev_idents), List.hd rev_idents, sel)))
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
          failwith (expected parser "identifier or '('")
      end
    | _ ->
      Expr.at pos (Expr.Get (List.rev (List.tl rev_idents), VarOrMethod.Var (List.hd rev_idents)))
  end

and parse_parens parser pos =
  if parser.token = Token.Reserved ")" then
    begin
      lookahead parser;
      Expr.at pos (Expr.Const Literal.Unit)
    end
  else
    let expr = parse_expr parser in
    parse_token parser (Token.Reserved ")");
    expr

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
    (Expr.Params.make [] [], body)

and parse_block parser =
  let pos = parser.pos in
  let exprs = parse_block_like_elems parser parse_expr in
  Expr.at pos (Expr.Lambda (Expr.Params.make [] [], exprs))

and parse_match_expr parser pos =
  parse_token parser (Token.Reserved "(");
  let target_expr = parse_expr parser in
  parse_token parser (Token.Reserved ")");
  skip parser (Token.Reserved ":");
  let rec loop rev_case_clauses =
    skip parser Token.Newline;
    if parser.token = Token.Reserved "end" then
      begin
        lookahead parser;
        Expr.at pos (Expr.Match (target_expr, List.rev rev_case_clauses))
      end
    else
      let case_clause = parse_case_clause parser in
      loop (case_clause::rev_case_clauses)
  in
  loop []

and parse_case_clause parser =
  parse_token parser (Token.Reserved "case");
  parse_token parser (Token.Reserved "(");
  let pat = parse_pattern parser in
  parse_token parser (Token.Reserved ")");
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
  (pat, guard, body)

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
      failwith (expected parser "'=' or ':' or '{'")
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
