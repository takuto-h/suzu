
open Printf

type t = {
  source : Source.t;
  parens : string Stack.t;
  offside_lines : int Stack.t;
  mutable is_bol : bool;  (* beginning of line *)
  mutable is_bob : bool;  (* beginning of block *)
}

let initial_buffer_size = 16

let reserved = [
  "case";
  "class";
  "def";
  "else";
  "export";
  "false";
  "if";
  "match";
  "module";
  "mutable";
  "open";
  "try";
  "true";
  "with"
]

let create source = {
  source = source;
  parens = Stack.create ();
  offside_lines = Stack.create ();
  is_bol = true;
  is_bob = true;
}

let indent lexer =
  begin if Stack.is_empty lexer.parens then
    lexer.is_bob <- true
  else
    let pos = Source.pos lexer.source in
    failwith (Pos.show_error pos "layout inside parentheses\n")
  end

let is_digit c = String.contains "0123456789" c

let is_ident_start c = String.contains "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_" c

let is_ident_part c = is_ident_start c || is_digit c

let is_op_part c = String.contains "=<>|&+-*/%" c

let is_whitespace c = String.contains " \t\r\n" c

let int_of_digit c = Char.code c - Char.code '0'

let lex_close_paren lexer pos open_paren close_paren =
  begin if Stack.is_empty lexer.parens || Stack.top lexer.parens <> open_paren then
    failwith (Pos.show_error pos (sprintf "unmatched parenthesis: '%s'\n" close_paren))
  else
    begin
      ignore (Stack.pop lexer.parens);
      Token.Reserved close_paren
    end
  end

let rec lex_op lexer buf =
  begin match Source.peek lexer.source with
    | Some c when is_op_part c ->
      begin
        Buffer.add_char buf c;
        Source.junk lexer.source;
        lex_op lexer buf
      end
    | Some _ | None ->
      Buffer.contents buf
  end

let rec lex_int lexer n =
  begin match Source.peek lexer.source with
    | Some c when is_digit c ->
      begin
        Source.junk lexer.source;
        lex_int lexer (n * 10 + int_of_digit c)
      end
    | Some _ | None ->
      Token.Int n
  end

let lex_escape_sequence pos c =
  begin match c with
    | '\\' | '"' | '\'' ->
      c
    | 'n' ->
      '\n'
    | 'r' ->
      '\r'
    | 't' ->
      '\t'
    | 'b' ->
      '\b'
    | _ ->
      failwith (Pos.show_error pos "invalid escape sequence\n")
  end

let rec lex_string lexer buf =
  begin match Source.peek lexer.source with
    | Some '"' ->
      begin
        Source.junk lexer.source;
        Token.String (Buffer.contents buf)
      end
    | Some '\\' ->
      begin
        Source.junk lexer.source;
        begin match Source.peek lexer.source with
          | Some c ->
            let pos = Source.pos lexer.source in
            begin
              Source.junk lexer.source;
              Buffer.add_char buf (lex_escape_sequence pos c);
              lex_string lexer buf
            end
          | None ->
            let pos_eof = Source.pos lexer.source in
            failwith (Pos.show_error pos_eof "EOF inside a string literal\n")
        end
      end
    | Some c ->
      begin
        Source.junk lexer.source;
        Buffer.add_char buf c;
        lex_string lexer buf
      end
    | None ->
      let pos_eof = Source.pos lexer.source in
      failwith (Pos.show_error pos_eof "EOF inside a string literal\n")
  end

let lex_char lexer =
  let res = begin match Source.peek lexer.source with
    | Some '\\' ->
      begin
        Source.junk lexer.source;
        begin match Source.peek lexer.source with
          | Some c ->
            let pos = Source.pos lexer.source in
            begin
              Source.junk lexer.source;
              lex_escape_sequence pos c
            end
          | None ->
            let pos_eof = (Source.pos lexer.source) in
            failwith (Pos.show_error pos_eof "EOF inside a character literal\n")
        end
      end
    | Some c ->
      begin
        Source.junk lexer.source;
        c
      end
    | None ->
      let pos_eof = (Source.pos lexer.source) in
      failwith (Pos.show_error pos_eof "EOF inside a character literal\n")
  end in
  begin if Source.peek lexer.source = Some '\'' then
    begin
      Source.junk lexer.source;
      Token.Char res
    end
  else
    let pos = Source.pos lexer.source in
    failwith (Pos.show_error pos "invalid character literal\n")
  end

let ident_or_reserved str =
  begin if List.mem str reserved then
    Token.Reserved str
  else
    Token.Ident str
  end

let rec lex_ident lexer buf =
  begin match Source.peek lexer.source with
    | Some c when is_ident_part c ->
      begin
        Source.junk lexer.source;
        Buffer.add_char buf c;
        lex_ident lexer buf
      end
    | Some _ | None ->
      ident_or_reserved (Buffer.contents buf)
  end

let lex_visible_token lexer pos c =
  begin
    Source.junk lexer.source;
    begin match c with
      | ';' | ',' | '^' | '.' | ':' | '#' ->
        Token.Reserved (sprintf "%c" c)
      | '(' | '{' | '[' ->
        begin
          Stack.push (sprintf "%c" c) lexer.parens;
          Token.Reserved (sprintf "%c" c)
        end
      | ')' ->
        lex_close_paren lexer pos "(" ")"
      | '}' ->
        lex_close_paren lexer pos "{" "}"
      | ']' ->
        lex_close_paren lexer pos "[" "]"
      | '=' ->
        begin match Source.peek lexer.source with
          | Some c2 when is_op_part c2 ->
            let buf = Buffer.create initial_buffer_size in
            begin
              Source.junk lexer.source;
              Buffer.add_char buf c;
              Buffer.add_char buf c2;
              Token.CmpOp (lex_op lexer buf)
            end
          | _ ->
            Token.Reserved "="
        end
      | '|' ->
        begin match Source.peek lexer.source with
          | Some '|' ->
            begin
              Source.junk lexer.source;
              Token.Reserved "||"
            end
          | Some _ | None ->
            let buf = Buffer.create initial_buffer_size in
            begin
              Buffer.add_char buf c;
              Token.CmpOp (lex_op lexer buf)
            end
        end
      | '&' ->
        begin match Source.peek lexer.source with
          | Some '&' ->
            begin
              Source.junk lexer.source;
              Token.Reserved "&&"
            end
          | Some _ | None ->
            let buf = Buffer.create initial_buffer_size in
            begin
              Buffer.add_char buf c;
              Token.CmpOp (lex_op lexer buf)
            end
        end
      | '<' | '>' | '!' ->
        let buf = Buffer.create initial_buffer_size in
        begin
          Buffer.add_char buf c;
          Token.CmpOp (lex_op lexer buf)
        end
      | '+' | '-' ->
        let buf = Buffer.create initial_buffer_size in
        begin
          Buffer.add_char buf c;
          Token.AddOp (lex_op lexer buf)
        end
      | '%' ->
        let buf = Buffer.create initial_buffer_size in
        begin
          Buffer.add_char buf c;
          Token.MulOp (lex_op lexer buf)
        end
      | '*' ->
        begin match Source.peek lexer.source with
          | Some '*' ->
            begin
              Source.junk lexer.source;
              Token.PowOp "**"
            end
          | Some _ | None ->
            let buf = Buffer.create initial_buffer_size in
            begin
              Buffer.add_char buf c;
              Token.MulOp (lex_op lexer buf)
            end
        end
      | '"' ->
        let buf = Buffer.create initial_buffer_size in
        lex_string lexer buf
      | '\'' ->
        lex_char lexer
      | _ when is_digit c ->
        lex_int lexer (int_of_digit c)
      | _ when is_ident_start c ->
        let buf = Buffer.create initial_buffer_size in
        begin
          Buffer.add_char buf c;
          lex_ident lexer buf
        end
      | _ ->
        failwith (Pos.show_error pos (sprintf "unknown character: '%c'\n" c))
    end
  end

let lex_token lexer pos c =
  let offset = pos.Pos.cnum - pos.Pos.bol in
  begin if lexer.is_bob then
    begin
      lexer.is_bob <- false;
      lexer.is_bol <- false;
      Stack.push offset lexer.offside_lines;
      lex_visible_token lexer pos c
    end
  else
    begin if lexer.is_bol then
      let offside_line = Stack.top lexer.offside_lines in
      begin if offset < offside_line then
        begin
          ignore (Stack.pop lexer.offside_lines);
          Token.Undent
        end
      else if offset = offside_line then
        begin
          lexer.is_bol <- false;
          Token.Newline
        end
      else
        begin
          lexer.is_bol <- false;
          lex_visible_token lexer pos c
        end
      end
    else
      lex_visible_token lexer pos c
    end
  end

let rec skip_single_line_comment lexer =
  begin match Source.peek lexer.source with
    | None | Some '\n' ->
      ()
    | Some _ ->
      begin
        Source.junk lexer.source;
        skip_single_line_comment lexer
      end
  end

let rec next lexer =
  let pos = Source.pos lexer.source in
  begin match Source.peek lexer.source with
    | None when Stack.length lexer.offside_lines <= 1 ->
      None, pos
    | None ->
      begin
        ignore (Stack.pop lexer.offside_lines);
        (Some Token.Undent, pos)
      end
    | Some '\n' when Stack.is_empty lexer.parens ->
      begin
        lexer.is_bol <- true;
        Source.junk lexer.source;
        next lexer
      end
    | Some c when is_whitespace c ->
      begin
        Source.junk lexer.source;
        next lexer
      end
    | Some '/' ->
      begin
        Source.junk lexer.source;
        begin match Source.peek lexer.source with
          | Some '/' ->
            begin
              Source.junk lexer.source;
              skip_single_line_comment lexer;
              next lexer
            end
          | Some _ | None ->
            let buf = Buffer.create initial_buffer_size in
            begin
              Buffer.add_char buf '/';
              (Some (Token.MulOp (lex_op lexer buf)), pos)
            end
        end
      end
    | Some c ->
      (Some (lex_token lexer pos c), pos)
  end

