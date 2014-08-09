
let subr_if =
  Eva.create_subr 2 ~opt_keys:["else"] begin fun eva pos args ->
    let cond = Eva.Args.nth args 0 in
    let then_thunk = Eva.Args.nth args 1 in
    let else_thunk = Eva.Args.get args "else" in
    begin match else_thunk with
      | None when Eva.bool_of_value pos cond ->
        ignore (Eva.call_fun eva pos then_thunk (Eva.Args.make [] []));
        Eva.Unit
      | Some _ when Eva.bool_of_value pos cond ->
        Eva.call_fun eva pos then_thunk (Eva.Args.make [] [])
      | None ->
        Eva.Unit
      | Some else_thunk ->
        Eva.call_fun eva pos else_thunk (Eva.Args.make [] [])
    end
  end

let subr_while =
  Eva.create_subr 1 ~req_keys:["do"] begin fun eva pos args ->
    let test_thunk = Eva.Args.nth args 0 in
    let body_thunk = Eva.Args.find args "do" in
    let rec loop () =
      let cond = Eva.call_fun eva pos test_thunk (Eva.Args.make [] []) in
      if Eva.bool_of_value pos cond then
        begin
          ignore (Eva.call_fun eva pos body_thunk (Eva.Args.make [] []));
          loop ();
        end
      else
        Eva.Unit
    in
    loop ()
  end

let subr_write_line =
  Eva.create_subr 1 ~allows_rest:true begin fun eva pos args ->
    let str = Eva.call_fun eva pos ModString.subr_string_format args in
    print_endline (Eva.string_of_value pos str);
    Eva.Unit
  end

let subr_read_line =
  Eva.create_subr 0 begin fun eva pos args ->
      Eva.String (read_line ())
  end

let initialize env =
  let mod_pervasives = Eva.Env.create_local env in
  Eva.Env.add_var env "Pervasives" (Eva.Module mod_pervasives);
  Eva.Env.add_var mod_pervasives "if" subr_if ~export:true;
  Eva.Env.add_var mod_pervasives "while" subr_while ~export:true;
  Eva.Env.add_var mod_pervasives "write_line" subr_write_line ~export:true;
  Eva.Env.add_var mod_pervasives "read_line" subr_read_line ~export:true;
  Eva.Env.open_module env mod_pervasives
