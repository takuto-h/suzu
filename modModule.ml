
let subr_module_to_string = Eva.subr_value_to_string

let initialize {Interp.eva={Eva.env}} =
  let mod_module = Eva.Env.create_local env in
  let mod_module_open = Eva.Env.create_local mod_module in
  Eva.Env.add_var env "Module" (Eva.Module mod_module);
  Eva.Env.add_var mod_module "C" (Eva.Class "Module::C") ~export:true;
  Eva.Env.add_var mod_module "Open" (Eva.Module mod_module_open) ~export:true;
  Eva.Env.add_method mod_module_open "Module::C" "to_string" subr_module_to_string ~export:true;
  Eva.Env.open_module env mod_module_open
