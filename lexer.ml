
open Printf

exception Error of Pos.t * string

type t = {
  source : Source.t;
  parens : string Stack.t;
  offside_lines : int Stack.t;
  mutable is_bol : bool;  (* beginning of line *)
  mutable is_bob : bool;  (* beginning of block *)
}

let initial_buffer_size = 16

let reserved = [
  "as";
  "case";
  "class";
  "def";
  "end";
  "except";
  "export";
  "false";
  "finally";
  "include";
  "let";
  "match";
  "module";
  "mutable";
  "open";
  "or";
  "throw";
  "trait";
  "true";
  "try";
  "when";
]

let create source =
  let offside_lines = Stack.create () in
  Stack.push (-1) offside_lines;
  {
    source = source;
    parens = Stack.create ();
    offside_lines = offside_lines;
    is_bol = true;
    is_bob = true;
  }

let indent lexer =
  if Stack.is_empty lexer.parens then
    lexer.is_bob <- true
  else
    let pos = Source.pos lexer.source in
    raise (Error (pos, "layout inside parentheses\n"))

let is_ident_start c = String.contains "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_" c

let is_ident_part c = is_ident_start c || SnChar.is_digit c

let is_ident_suffix c = String.contains "!?" c

let is_op_part c = String.contains "=<>|&+-*/%" c

let is_whitespace c = String.contains " \t\r\n" c

let lex_close_paren lexer pos open_paren close_paren =
  if Stack.is_empty lexer.parens || Stack.top lexer.parens <> open_paren then
    raise (Error (pos, sprintf "unmatched parenthesis: '%s'\n" close_paren))
  else
    begin
      ignore (Stack.pop lexer.parens);
      Token.Reserved close_paren
    end

let rec lex_op lexer buf =
  begin match Source.peek lexer.source with
    | Some c when is_op_part c ->
      Buffer.add_char buf c;
      Source.junk lexer.source;
      lex_op lexer buf
    | Some _ | None ->
      Buffer.contents buf
  end

let rec lex_int lexer n =
  begin match Source.peek lexer.source with
    | Some c when SnChar.is_digit c ->
      Source.junk lexer.source;
      lex_int lexer (n * 10 + SnChar.int_of_digit c)
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
      raise (Error (pos, "invalid escape sequence\n"))
  end

let rec lex_string lexer buf =
  begin match Source.peek lexer.source with
    | Some '"' ->
      Source.junk lexer.source;
      Token.String (Buffer.contents buf)
    | Some '\\' ->
      Source.junk lexer.source;
      begin match Source.peek lexer.source with
        | Some c ->
          let pos = Source.pos lexer.source in
          Source.junk lexer.source;
          Buffer.add_char buf (lex_escape_sequence pos c);
          lex_string lexer buf
        | None ->
          let pos_eof = Source.pos lexer.source in
          raise (Error (pos_eof, "EOF inside a string literal\n"))
      end
    | Some c ->
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_string lexer buf
    | None ->
      let pos_eof = Source.pos lexer.source in
      raise (Error (pos_eof, "EOF inside a string literal\n"))
  end

let lex_char lexer =
  let res = begin match Source.peek lexer.source with
    | Some '\\' ->
      Source.junk lexer.source;
      begin match Source.peek lexer.source with
        | Some c ->
          let pos = Source.pos lexer.source in
          Source.junk lexer.source;
          lex_escape_sequence pos c
        | None ->
          let pos_eof = (Source.pos lexer.source) in
          raise (Error (pos_eof, "EOF inside a character literal\n"))
      end
    | Some c ->
      Source.junk lexer.source;
      c
    | None ->
      let pos_eof = (Source.pos lexer.source) in
      raise (Error (pos_eof, "EOF inside a character literal\n"))
  end in
  if Source.peek lexer.source = Some '\'' then
    begin
      Source.junk lexer.source;
      Token.Char res
    end
  else
    let pos = Source.pos lexer.source in
    raise (Error (pos, "invalid character literal\n"))

let ident_or_reserved str =
  if List.mem str reserved then
    Token.Reserved str
  else
    Token.Ident str

let rec lex_ident lexer buf =
  begin match Source.peek lexer.source with
    | Some c when is_ident_part c ->
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_ident lexer buf
    | Some c when is_ident_suffix c ->
      Source.junk lexer.source;
      Buffer.add_char buf c;
      ident_or_reserved (Buffer.contents buf)
    | Some _ | None ->
      ident_or_reserved (Buffer.contents buf)
  end

let lex_visible_token lexer pos c =
  Source.junk lexer.source;
  begin match c with
    | ';' | ',' | '^' | '.' | '#' ->
      Token.Reserved (sprintf "%c" c)
    | '(' | '{' | '[' ->
      Stack.push (sprintf "%c" c) lexer.parens;
      Token.Reserved (sprintf "%c" c)
    | ')' ->
      lex_close_paren lexer pos "(" ")"
    | '}' ->
      lex_close_paren lexer pos "{" "}"
    | ']' ->
      lex_close_paren lexer pos "[" "]"
    | ':' ->
      begin match Source.peek lexer.source with
        | Some ':' ->
          Source.junk lexer.source;
          Token.Reserved "::"
        | Some _ | None ->
          Token.Reserved ":"
      end
    | '=' ->
      begin match Source.peek lexer.source with
        | Some c2 when is_op_part c2 ->
          let buf = Buffer.create initial_buffer_size in
          Source.junk lexer.source;
          Buffer.add_char buf c;
          Buffer.add_char buf c2;
          Token.CmpOp (lex_op lexer buf)
        | _ ->
          Token.Reserved "="
      end
    | '~' ->
      begin match Source.peek lexer.source with
        | Some c when is_op_part c ->
          let buf = Buffer.create initial_buffer_size in
          Buffer.add_char buf '~';
          Token.UnaryOp (lex_op lexer buf)
        | Some _ | None ->
          Token.Reserved "~"
      end
    | '|' ->
      begin match Source.peek lexer.source with
        | Some '|' ->
          Source.junk lexer.source;
          Token.Reserved "||"
        | Some c when is_op_part c ->
          let buf = Buffer.create initial_buffer_size in
          Buffer.add_char buf '|';
          Token.CmpOp (lex_op lexer buf)
        | Some _ | None ->
          Token.Reserved "|"
      end
    | '&' ->
      begin match Source.peek lexer.source with
        | Some '&' ->
          Source.junk lexer.source;
          Token.Reserved "&&"
        | Some c when is_op_part c ->
          let buf = Buffer.create initial_buffer_size in
          Buffer.add_char buf c;
          Token.CmpOp (lex_op lexer buf)
        | Some _ | None ->
          Token.Reserved "&"
      end
    | '<' | '>' | '!' ->
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.CmpOp (lex_op lexer buf)
    | '+' | '-' ->
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.AddOp (lex_op lexer buf)
    | '%' ->
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.MulOp (lex_op lexer buf)
    | '*' ->
      begin match Source.peek lexer.source with
        | Some '*' ->
          Source.junk lexer.source;
          Token.PowOp "**"
        | Some _ | None ->
          let buf = Buffer.create initial_buffer_size in
          Buffer.add_char buf c;
          Token.MulOp (lex_op lexer buf)
      end
    | '"' ->
      let buf = Buffer.create initial_buffer_size in
      lex_string lexer buf
    | '\'' ->
      lex_char lexer
    | _ when SnChar.is_digit c ->
      lex_int lexer (SnChar.int_of_digit c)
    | _ when is_ident_start c ->
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      lex_ident lexer buf
    | _ ->
      raise (Error (pos, sprintf "unknown character: '%c'\n" c))
  end

let lex_token lexer pos c =
  let offset = pos.Pos.cnum - pos.Pos.bol in
  let offside_line = Stack.top lexer.offside_lines in
  if lexer.is_bob then
    begin if offset > offside_line then
        begin
          lexer.is_bob <- false;
          lexer.is_bol <- false;
          Stack.push offset lexer.offside_lines;
          lex_visible_token lexer pos c
        end
      else
        begin
          lexer.is_bob <- false;
          lexer.is_bol <- false;
          Token.Newline
        end
    end
  else if lexer.is_bol then
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

let rec skip_single_line_comment lexer =
  begin match Source.peek lexer.source with
    | None | Some '\n' ->
      ()
    | Some _ ->
      Source.junk lexer.source;
      skip_single_line_comment lexer
  end

let rec next lexer =
  let pos = Source.pos lexer.source in
  begin match Source.peek lexer.source with
    | None when Stack.length lexer.offside_lines <= 2 ->
      None, pos
    | None ->
      ignore (Stack.pop lexer.offside_lines);
      (Some Token.Undent, pos)
    | Some '\n' when Stack.is_empty lexer.parens ->
      lexer.is_bol <- true;
      Source.junk lexer.source;
      next lexer
    | Some c when is_whitespace c ->
      Source.junk lexer.source;
      next lexer
    | Some '/' ->
      Source.junk lexer.source;
      begin match Source.peek lexer.source with
        | Some '/' ->
          Source.junk lexer.source;
          skip_single_line_comment lexer;
          next lexer
        | Some _ | None ->
          let buf = Buffer.create initial_buffer_size in
          Buffer.add_char buf '/';
          (Some (Token.MulOp (lex_op lexer buf)), pos)
      end
    | Some c ->
      (Some (lex_token lexer pos c), pos)
  end

