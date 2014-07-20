
let write_line =
  Value.Subr begin 1, fun pos args ->
    let str = List.nth args 0 in
    print_endline (Eva.string_of_value pos str);
    Value.Unit
  end

let read_line =
  Value.Subr begin 0, fun pos args ->
    Value.String (read_line ())
  end

let initialize env =
  let mod_pervasives = Value.Env.create_local env in
  Value.Env.add_var env "Pervasives" (Value.Module mod_pervasives);
  Value.Env.add_var mod_pervasives "write_line" write_line ~export:true;
  Value.Env.add_var mod_pervasives "read_line" read_line ~export:true;
  Value.Env.open_module env mod_pervasives
