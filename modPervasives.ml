
let write_line =
  Eva.Subr begin 1, true, fun eva pos args ->
      let str = Eva.call_fun eva pos ModString.subr_string_format args in
      print_endline (Eva.string_of_value pos str);
      Eva.Unit
  end

let read_line =
  Eva.Subr begin 0, false, fun eva pos args ->
      Eva.String (read_line ())
  end

let initialize env =
  let mod_pervasives = Eva.Env.create_local env in
  Eva.Env.add_var env "Pervasives" (Eva.Module mod_pervasives);
  Eva.Env.add_var mod_pervasives "write_line" write_line ~export:true;
  Eva.Env.add_var mod_pervasives "read_line" read_line ~export:true;
  Eva.Env.open_module env mod_pervasives
