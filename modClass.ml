
let subr_class_to_string = Eva.subr_value_to_string

let initialize {Interp.eva={Eva.env}} =
  let mod_class = Eva.Env.create_local env in
  let mod_class_open = Eva.Env.create_local mod_class in
  Eva.Env.add_var env "Class" (Eva.Module mod_class);
  Eva.Env.add_var mod_class "C" (Eva.Class "Class::C") ~export:true;
  Eva.Env.add_var mod_class "Open" (Eva.Module mod_class_open) ~export:true;
  Eva.Env.add_method mod_class_open "Class::C" "to_string" subr_class_to_string ~export:true;
  Eva.Env.open_module env mod_class_open
