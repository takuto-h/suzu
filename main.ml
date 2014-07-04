
open Printf

let initial_buffer_size = 256

let eval str =
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
            printf "%s\n" (Value.show (Eva.eval expr));
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

let rec read buf =
  let line = read_line () in
  if line = "" then
    Buffer.contents buf
  else
    begin
      Buffer.add_string buf (sprintf "%s\n" line);
      printf "... ";
      read buf
    end

let rec repl () =
  begin try
    begin
      printf ">>> ";
      let str = read (Buffer.create initial_buffer_size) in
      eval str;
      repl ()
    end
  with
    | End_of_file ->
      ()
  end

let () = repl ()
