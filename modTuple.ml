
let subr_tuple_to_string = Eva.subr_show

let initialize {Interp.eva={Eva.env}} =
  let mod_tuple = Eva.Env.create_local env in
  let mod_tuple_open = Eva.Env.create_local mod_tuple in
  Eva.Env.add_var env "Tuple" (Eva.Module mod_tuple);
  Eva.Env.add_var mod_tuple "C" (Eva.Class "Tuple::C") ~export:true;
  Eva.Env.add_var mod_tuple "Open" (Eva.Module mod_tuple_open) ~export:true;
  Eva.Env.add_method mod_tuple_open "Tuple::C" "==" Eva.subr_eq ~export:true;
  Eva.Env.add_method mod_tuple_open "Tuple::C" "!=" Eva.subr_ne ~export:true;
  Eva.Env.add_method mod_tuple_open "Tuple::C" "to_string" subr_tuple_to_string ~export:true;
  Eva.Env.open_module env mod_tuple_open
