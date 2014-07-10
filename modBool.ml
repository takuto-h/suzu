
let unary_op proc =
  Value.Subr begin 1, fun pos args ->
    let self = List.nth args 0 in
    Value.Bool (proc (Eva.bool_of_value pos self))
  end

let initialize env =
  let mod_bool = Value.Env.create_local env in
  begin
    Value.Env.add_var env "Bool" (Value.Module mod_bool);
    Value.Env.add_var mod_bool "C" (Value.Class "Bool:C") ~export:true;
    Value.Env.add_method env "Bool:C" "!" (unary_op not);
  end
