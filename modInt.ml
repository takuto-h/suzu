
let binary_cmp_op proc =
  Value.Subr begin 2, fun pos args ->
    let self = List.nth args 0 in
    let i = List.nth args 1 in
    Value.Bool (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let binary_arith_op proc =
  Value.Subr begin 2, fun pos args ->
    let self = List.nth args 0 in
    let i = List.nth args 1 in
    Value.Int (proc (Eva.int_of_value pos self) (Eva.int_of_value pos i))
  end

let unary_arith_op proc =
  Value.Subr begin 1, fun pos args ->
    let self = List.nth args 0 in
    Value.Int (proc (Eva.int_of_value pos self))
  end

let initialize env =
  let mod_int = Value.Env.create_local env in
  begin
    Value.Env.add_var env "Int" (Value.Module mod_int);
    Value.Env.add_var mod_int "answer" (Value.Int 42) ~export:true;
    Value.Env.add_var mod_int "C" (Value.Class "Int:C") ~export:true;
    Value.Env.add_method env "Int:C" "<" (binary_cmp_op ( < ));
    Value.Env.add_method env "Int:C" ">" (binary_cmp_op ( > ));
    Value.Env.add_method env "Int:C" "<=" (binary_cmp_op ( <= ));
    Value.Env.add_method env "Int:C" ">=" (binary_cmp_op ( >= ));
    Value.Env.add_method env "Int:C" "==" (binary_cmp_op ( = ));
    Value.Env.add_method env "Int:C" "!=" (binary_cmp_op ( <> ));
    Value.Env.add_method env "Int:C" "+" (binary_arith_op ( + ));
    Value.Env.add_method env "Int:C" "-" (binary_arith_op ( - ));
    Value.Env.add_method env "Int:C" "*" (binary_arith_op ( * ));
    Value.Env.add_method env "Int:C" "/" (binary_arith_op ( / ));
    Value.Env.add_method env "Int:C" "%" (binary_arith_op ( mod ));
    Value.Env.add_method env "Int:C" "~+" (unary_arith_op ( ~+ ));
    Value.Env.add_method env "Int:C" "~-" (unary_arith_op ( ~- ));
  end
