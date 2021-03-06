
open SnPervasives
open Printf
open Scanf

type t = {
  mutable env : VM.env;
}

let initial_buffer_size = 64

let create () = {
  env = [VM.create_frame ()];
}

let compile_and_run loader expr =
  let insns = Compiler.compile expr in
  let vm = VM.create insns loader.env in
  let value = VM.run vm in
  loader.env <- VM.get_env vm;
  value

let load_source proc loader source =
  let lexer = Lexer.create source in
  let parser = Parser.create lexer in
  begin try
      let rec loop () =
        begin match Parser.parse parser with
          | None ->
            ()
          | Some expr ->
            proc (compile_and_run loader expr);
            loop ()
        end
      in
      loop ()
    with
    | Parser.Error (pos, message) ->
      eprintf "%s" (Pos.show_message pos (sprintf "syntax error: %s" message))
    | VM.Error (pos, message, trace) ->
      eprintf "%s" (Pos.show_message pos (sprintf "error: %s" message));
      List.iter begin fun pos ->
        eprintf "%s" (Pos.show_message pos "note: error from here\n")
      end trace
  end

let load_string loader name str =
  let source = Source.of_string name str in
  load_source (fun _ -> ()) loader source

let load_file loader name =
  with_open_in begin fun chan_in ->
    let source = Source.of_channel name chan_in in
    load_source (fun _ -> ()) loader source
  end name

let rec read_multiple_lines buf =
  eprintf "... ";
  flush stderr;
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

let parse_directive loader str =
  begin try
      sscanf str "#use %S" (load_file loader)
    with
    | Scan_failure _ | End_of_file ->
      eprintf "invalid directive: %s\n" str
  end

let rec repl loader =
  let rec loop () =
    begin try
        eprintf ">>> ";
        flush stderr;
        let str = read () in
        begin if String.length str <> 0 && str.[0] = '#' then
            parse_directive loader str
          else
            let source = Source.of_string "<stdin>" str in
            load_source (fun value -> eprintf "%s\n" (VM.show_value value)) loader source
        end;
        loop ()
      with
      | End_of_file ->
        ()
    end
  in
  loop ()

let get_env {env} =
  env
