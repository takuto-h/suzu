
let subr_unit_to_string = Eva.subr_show

let initialize {Interp.eva={Eva.env}} =
  let mod_unit = Eva.Env.create_local env in
  let mod_unit_open = Eva.Env.create_local mod_unit in
  Eva.Env.add_var env "Unit" (Eva.Module mod_unit);
  Eva.Env.add_var mod_unit "C" (Eva.Class "Unit::C") ~export:true;
  Eva.Env.add_var mod_unit "Open" (Eva.Module mod_unit_open) ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" ">" Eva.subr_gt ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "<" Eva.subr_lt ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" ">=" Eva.subr_ge ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "<=" Eva.subr_le ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "==" Eva.subr_eq ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "!=" Eva.subr_ne ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "<=>" Eva.subr_compare ~export:true;
  Eva.Env.add_method mod_unit_open "Unit::C" "to_string" subr_unit_to_string ~export:true;
  Eva.Env.open_module env mod_unit_open
