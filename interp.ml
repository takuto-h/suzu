
open SnPervasives
open Printf

type t = {
  eva : Eva.t;
}

let initial_buffer_size = 64

let create () =
  let env = Value.Env.create_global () in
  ModPervasives.initialize env;
  ModInt.initialize env;
  ModBool.initialize env;
  { eva = Eva.create env; }

let parse_string proc str =
  let source = Source.of_string "<stdin>" str in
  let lexer = Lexer.create source in
  let parser = Parser.create lexer in
  begin try
      let rec loop () =
        begin match Parser.parse parser with
          | None ->
            ()
          | Some expr ->
            printf "%s\n" (proc expr);
            loop ()
        end
      in
      loop ()
    with
    | Failure message ->
      printf "%s" message
  end

let rec read_multiple_lines buf =
  printf "... ";
  let line = read_line () in
  if line = "" then
    Buffer.contents buf
  else
    begin
      Buffer.add_string buf (sprintf "%s\n" line);
      read_multiple_lines buf
    end

let read () =
  let line = read_line () in
  let len = String.length line in
  if len <> 0 && (line.[len - 1] = ':' || line.[len - 1] = '{') then
    let buf = Buffer.create initial_buffer_size in
    Buffer.add_string buf (sprintf "%s\n" line);
    read_multiple_lines buf
  else
    line

let rec rppl proc =
  begin try
      printf ">>> ";
      let str = read () in
      parse_string proc str;
      rppl proc
    with
    | End_of_file ->
      ()
  end

let repl {eva;} =
  rppl (fun expr -> Value.show (Eva.eval eva expr))

let load_file {eva;} name =
  with_open_in name begin fun chan_in ->
    let source = Source.of_channel name chan_in in
    let lexer = Lexer.create source in
    let parser = Parser.create lexer in
    begin try
        let rec loop () =
          begin match Parser.parse parser with
            | None ->
              ()
            | Some expr ->
              ignore (Eva.eval eva expr);
              loop ()
          end
        in
        loop ()
      with
      | Failure message ->
        printf "%s" message
    end
  end
