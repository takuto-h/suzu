
let subr_char_to_string = Eva.subr_show

let initialize {Interp.eva={Eva.env}} =
  let mod_char = Eva.Env.create_local env in
  let mod_char_open = Eva.Env.create_local mod_char in
  Eva.Env.add_var env "Char" (Eva.Module mod_char);
  Eva.Env.add_var mod_char "C" (Eva.Class "Char::C") ~export:true;
  Eva.Env.add_var mod_char "Open" (Eva.Module mod_char_open) ~export:true;
  Eva.Env.add_method mod_char_open "Char::C" "to_string" subr_char_to_string ~export:true;
  Eva.Env.open_module env mod_char_open
