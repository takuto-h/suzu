
let unary_op proc =
  Eva.Subr begin 1, false, fun eva pos args ->
      let self = List.nth args 0 in
      Eva.Bool (proc (Eva.bool_of_value pos self))
  end

let initialize env =
  let mod_bool = Eva.Env.create_local env in
  let mod_bool_open = Eva.Env.create_local mod_bool in
  Eva.Env.add_var env "Bool" (Eva.Module mod_bool);
  Eva.Env.add_var mod_bool "C" (Eva.Class "Bool:C") ~export:true;
  Eva.Env.add_var mod_bool "Open" (Eva.Module mod_bool_open) ~export:true;
  Eva.Env.add_method mod_bool_open "Bool:C" "!" (unary_op not) ~export:true;
  Eva.Env.open_module env mod_bool_open
