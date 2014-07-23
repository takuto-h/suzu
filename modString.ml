
let string_format =
  Value.Subr begin 1, true, fun pos args ->
      let self = List.nth args 0 in
      self
  end

let initialize env =
  let mod_string = Value.Env.create_local env in
  let mod_string_open = Value.Env.create_local mod_string in
  Value.Env.add_var env "String" (Value.Module mod_string);
  Value.Env.add_var mod_string "C" (Value.Class "String:C") ~export:true;
  Value.Env.add_var mod_string "format" string_format ~export:true;
  Value.Env.add_var mod_string "Open" (Value.Module mod_string_open) ~export:true;
