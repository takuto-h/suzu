
let initialize env =
  let modl = Value.Env.create_local env in
  begin
    Value.Env.add_var env "Int" (Value.Module modl);
    Value.Env.add_var modl "answer" (Value.Int 42)
  end
