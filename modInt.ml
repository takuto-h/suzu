
let initialize env =
  let mod_int = Value.Env.create_local env in
  begin
    Value.Env.add_var env "Int" (Value.Module mod_int);
    Value.Env.add_var mod_int "answer" (Value.Int 42);
    Value.Env.add_var mod_int "C" (Value.Class "Int:C");
    Value.Env.add_method env "Int:C" "+" begin
      Value.Subr begin 2, fun pos args ->
        let self = List.nth args 0 in
        let i = List.nth args 1 in
        Value.Int (Eva.int_of_value pos self + Eva.int_of_value pos i)
      end
    end
  end
