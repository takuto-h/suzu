
open Printf

let usage_msg = sprintf "usage: %s [source_files]" Sys.argv.(0)

let main () =
  let loader = Loader.create () in
  Builtin.initialize loader;
  Prelude.initialize loader;
  Arg.parse_argv ~current:(ref 0) Sys.argv [] begin fun fname ->
    Loader.load_file loader fname
  end usage_msg;
  Loader.repl loader

let () = main ()
