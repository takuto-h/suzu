
open Printf

let initial_buffer_size = 64

let eval proc str =
  let source = Source.of_string "<stdin>" str in
  let lexer = Lexer.create source in
  let parser = Parser.create lexer in
  begin try
    let rec loop () =
      begin match Parser.parse parser with
        | None ->
          ()
        | Some expr ->
          begin
            printf "%s\n" (proc expr);
            loop ()
          end
      end
    in
    loop ()
  with
    | Failure message ->
      begin
        eprintf "%s" message;
        flush stderr;
        ()
      end
  end

let rec read_multiple_lines buf =
  begin
    printf "... ";
    let line = read_line () in
    begin if line = "" then
      Buffer.contents buf
    else
      begin
        Buffer.add_string buf (sprintf "%s\n" line);
        read_multiple_lines buf
      end
    end
  end

let read () =
  let line = read_line () in
  let len = String.length line in
  begin if len <> 0 && (line.[len - 1] = ':' || line.[len - 1] = '{') then
    let buf = Buffer.create initial_buffer_size in
    begin
      Buffer.add_string buf (sprintf "%s\n" line);
      read_multiple_lines buf
    end
  else
    line
  end

let rec repl proc =
  begin try
    begin
      printf ">>> ";
      let str = read () in
      eval proc str;
      repl proc
    end
  with
    | End_of_file ->
      ()
  end

let () =
  let env = Value.Env.create_global () in
  begin
    ModInt.initialize env;
    ModBool.initialize env;
    let eva = Eva.create env in
    repl (fun expr -> Value.show (Eva.eval eva expr))
  end
