
let make_binary_cmp_op proc = Eva.make_binary_subr Eva.value_of_bool proc Eva.int_of_value
let make_binary_arith_op proc = Eva.make_binary_subr Eva.value_of_int proc Eva.int_of_value

let make_unary_op proc = Eva.make_unary_subr Eva.value_of_int proc Eva.int_of_value

let subr_int_to_string = Eva.subr_value_to_string

let initialize {Interp.eva={Eva.env}} =
  let mod_int = Eva.Env.create_local env in
  let mod_int_open = Eva.Env.create_local mod_int in
  Eva.Env.add_var env "Int" (Eva.Module mod_int);
  Eva.Env.add_var mod_int "answer" (Eva.Int 42) ~export:true;
  Eva.Env.add_var mod_int "C" (Eva.Class "Int::C") ~export:true;
  Eva.Env.add_var mod_int "Open" (Eva.Module mod_int_open) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "<" (make_binary_cmp_op ( < )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" ">" (make_binary_cmp_op ( > )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "<=" (make_binary_cmp_op ( <= )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" ">=" (make_binary_cmp_op ( >= )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "==" (make_binary_cmp_op ( = )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "!=" (make_binary_cmp_op ( <> )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "+" (make_binary_arith_op ( + )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "-" (make_binary_arith_op ( - )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "*" (make_binary_arith_op ( * )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "/" (make_binary_arith_op ( / )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "%" (make_binary_arith_op ( mod )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "~+" (make_unary_op ( ~+ )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "~-" (make_unary_op ( ~- )) ~export:true;
  Eva.Env.add_method mod_int_open "Int::C" "to_string" subr_int_to_string ~export:true;
  Eva.Env.open_module env mod_int_open
