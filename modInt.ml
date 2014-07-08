
let initialize env =
  let mod_int = Value.Env.create_local env in
  begin
    Value.Env.add_var env "Int" (Value.Module mod_int);
    Value.Env.add_var mod_int "answer" (Value.Int 42);
    Value.Env.add_var mod_int "C" (Value.Class "Int:C");
    Value.Env.add_method env "Int:C" "+" begin
      Value.Subr begin fun pos args ->
        begin match args with
          | [self; i] -> Value.Int (Eva.int_of_value pos self + Eva.int_of_value pos i)
          | _ -> failwith (Eva.wrong_number_of_arguments pos 2 (List.length args))
        end
      end
    end
  end
