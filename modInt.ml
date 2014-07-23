
let binary_cmp_op proc =
  Eva.Subr begin 2, false, fun eva pos args ->
      let self = List.nth args 0 in
      let i = List.nth args 1 in
      Eva.Bool (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let binary_arith_op proc =
  Eva.Subr begin 2, false, fun eva pos args ->
      let self = List.nth args 0 in
      let i = List.nth args 1 in
      Eva.Int (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let unary_arith_op proc =
  Eva.Subr begin 1, false, fun eva pos args ->
      let self = List.nth args 0 in
      Eva.Int (proc (Eva.int_of_value pos self))
  end

let initialize env =
  let mod_int = Eva.Env.create_local env in
  let mod_int_open = Eva.Env.create_local mod_int in
  Eva.Env.add_var env "Int" (Eva.Module mod_int);
  Eva.Env.add_var mod_int "answer" (Eva.Int 42) ~export:true;
  Eva.Env.add_var mod_int "C" (Eva.Class "Int:C") ~export:true;
  Eva.Env.add_var mod_int "Open" (Eva.Module mod_int_open) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "<" (binary_cmp_op ( < )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" ">" (binary_cmp_op ( > )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "<=" (binary_cmp_op ( <= )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" ">=" (binary_cmp_op ( >= )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "==" (binary_cmp_op ( = )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "!=" (binary_cmp_op ( <> )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "+" (binary_arith_op ( + )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "-" (binary_arith_op ( - )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "*" (binary_arith_op ( * )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "/" (binary_arith_op ( / )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "%" (binary_arith_op ( mod )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "~+" (unary_arith_op ( ~+ )) ~export:true;
  Eva.Env.add_method mod_int_open "Int:C" "~-" (unary_arith_op ( ~- )) ~export:true;
  Eva.Env.open_module env mod_int_open
