
open Printf

let make_unary_op proc = Eva.make_unary_subr Eva.value_of_bool proc Eva.bool_of_value

let subr_bool_to_string = Eva.make_unary_subr Eva.value_of_string (sprintf "%B") Eva.bool_of_value

let initialize {Interp.eva={Eva.env}} =
  let mod_bool = Eva.Env.create_local env in
  let mod_bool_open = Eva.Env.create_local mod_bool in
  Eva.Env.add_var env "Bool" (Eva.Module mod_bool);
  Eva.Env.add_var mod_bool "C" (Eva.Class "Bool::C") ~export:true;
  Eva.Env.add_var mod_bool "Open" (Eva.Module mod_bool_open) ~export:true;
  Eva.Env.add_method mod_bool_open "Bool::C" "!" (make_unary_op not) ~export:true;
  Eva.Env.add_method mod_bool_open "Bool::C" "to_string" subr_bool_to_string ~export:true;
  Eva.Env.open_module env mod_bool_open
