
open SnPervasives
open Printf

type t = {
  mutable env : VM.env;
}

let initial_buffer_size = 64

let create () = {
  env = [VM.create_frame ()];
}

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

let compile_and_run loader expr =
  let insns = Compiler.compile expr in
  let vm = VM.create insns loader.env in
  let value = VM.run vm in
  loader.env <- vm.VM.env;
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
      printf "%s" (Pos.show_message pos (sprintf "syntax error: %s" message))
    | VM.Error (pos, message, rev_stack_trace) ->
      printf "%s" (Pos.show_message pos (sprintf "error: %s" message));
      List.iter begin fun pos ->
        printf "%s" (Pos.show_message pos "note: error from here\n")
      end (List.rev rev_stack_trace)
  end

let load_string loader name str =
  let source = Source.of_string name str in
  load_source (fun _ -> ()) loader source

let load_file loader name =
  with_open_in name begin fun chan_in ->
    let source = Source.of_channel name chan_in in
    load_source (fun _ -> ()) loader source
  end

let rec repl loader =
  let rec loop () =
    begin try
        printf ">>> ";
        let str = read () in
        let source = Source.of_string "<stdin>" str in
        load_source (fun value -> printf "%s\n" (VM.show_value value)) loader source;
        loop ()
      with
      | End_of_file ->
        ()
    end
  in
  loop ()
