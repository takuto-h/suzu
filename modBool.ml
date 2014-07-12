
let unary_op proc =
  Value.Subr begin 1, fun pos args ->
    let self = List.nth args 0 in
    Value.Bool (proc (Eva.bool_of_value pos self))
  end

let initialize env =
  let mod_bool = Value.Env.create_local env in
  let mod_bool_open = Value.Env.create_local mod_bool in
  begin
    Value.Env.add_var env "Bool" (Value.Module mod_bool);
    Value.Env.add_var mod_bool "C" (Value.Class "Bool:C") ~export:true;
    Value.Env.add_var mod_bool "Open" (Value.Module mod_bool_open) ~export:true;
    Value.Env.add_method mod_bool_open "Bool:C" "!" (unary_op not) ~export:true;
    Value.Env.open_module env mod_bool_open
  end
