
open Printf

let usage_msg = sprintf "usage: %s [source_files]" Sys.argv.(0)

let main () =
  let loader = Loader.create () in
  Builtin.initialize loader;
  Prelude.initialize loader;
  Arg.parse_argv ~current:(ref 0) Sys.argv [] begin fun file_name ->
    Loader.load_file loader file_name
  end usage_msg;
  Loader.repl loader

let () = main ()
