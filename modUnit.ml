
let subr_unit_to_string = Eva.make_unary_subr Eva.value_of_string (fun () -> "()") Eva.unit_of_value

let initialize {Eva.env} =
  let mod_unit = Eva.Env.create_local env in
  let mod_unit_open = Eva.Env.create_local mod_unit in
  Eva.Env.add_var env "Unit" (Eva.Module mod_unit);
  Eva.Env.add_var mod_unit "C" (Eva.Class "Unit:C") ~export:true;
  Eva.Env.add_var mod_unit "Open" (Eva.Module mod_unit_open) ~export:true;
  Eva.Env.add_method mod_unit_open "Unit:C" "to_string" subr_unit_to_string ~export:true;
  Eva.Env.open_module env mod_unit_open
