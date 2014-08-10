
open Printf

let usage_msg = sprintf "usage: %s [source_files]" Sys.argv.(0)

let main () =
  let interp = Interp.create () in
  ModPervasives.initialize interp;
  ModInt.initialize interp;
  ModUnit.initialize interp;
  ModBool.initialize interp;
  ModChar.initialize interp;
  ModString.initialize interp;
  ModTuple.initialize interp;
  ModProc.initialize interp;
  ModModule.initialize interp;
  ModClass.initialize interp;
  ModList.initialize interp;
  Arg.parse_argv ~current:(ref 0) Sys.argv [] begin fun file_name ->
    Interp.load_file interp file_name
  end usage_msg;
  Interp.repl interp

let () = main ()
