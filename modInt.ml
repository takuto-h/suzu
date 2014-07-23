
let binary_cmp_op proc =
  Value.Subr begin 2, false, fun pos args ->
      let self = List.nth args 0 in
      let i = List.nth args 1 in
      Value.Bool (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let binary_arith_op proc =
  Value.Subr begin 2, false, fun pos args ->
      let self = List.nth args 0 in
      let i = List.nth args 1 in
      Value.Int (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let unary_arith_op proc =
  Value.Subr begin 1, false, fun pos args ->
      let self = List.nth args 0 in
      Value.Int (proc (Eva.int_of_value pos self))
  end

let initialize env =
  let mod_int = Value.Env.create_local env in
  let mod_int_open = Value.Env.create_local mod_int in
  Value.Env.add_var env "Int" (Value.Module mod_int);
  Value.Env.add_var mod_int "answer" (Value.Int 42) ~export:true;
  Value.Env.add_var mod_int "C" (Value.Class "Int:C") ~export:true;
  Value.Env.add_var mod_int "Open" (Value.Module mod_int_open) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "<" (binary_cmp_op ( < )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" ">" (binary_cmp_op ( > )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "<=" (binary_cmp_op ( <= )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" ">=" (binary_cmp_op ( >= )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "==" (binary_cmp_op ( = )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "!=" (binary_cmp_op ( <> )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "+" (binary_arith_op ( + )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "-" (binary_arith_op ( - )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "*" (binary_arith_op ( * )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "/" (binary_arith_op ( / )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "%" (binary_arith_op ( mod )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "~+" (unary_arith_op ( ~+ )) ~export:true;
  Value.Env.add_method mod_int_open "Int:C" "~-" (unary_arith_op ( ~- )) ~export:true;
  Value.Env.open_module env mod_int_open
