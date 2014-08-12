
let subr_class_of =
  Eva.create_subr 1 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    Eva.Class (Eva.Value.class_of arg0)
  end

let subr_class_to_string = Eva.subr_show

let initialize {Interp.eva={Eva.env}} =
  let mod_class = Eva.Env.create_local env in
  let mod_class_open = Eva.Env.create_local mod_class in
  Eva.Env.add_var env "Class" (Eva.Module mod_class);
  Eva.Env.add_var mod_class "C" (Eva.Class "Class::C") ~export:true;
  Eva.Env.add_var mod_class "of" subr_class_of ~export:true;
  Eva.Env.add_var mod_class "Open" (Eva.Module mod_class_open) ~export:true;
  Eva.Env.add_method mod_class_open "Class::C" "==" Eva.subr_eq ~export:true;
  Eva.Env.add_method mod_class_open "Class::C" "!=" Eva.subr_ne ~export:true;
  Eva.Env.add_method mod_class_open "Class::C" "to_string" subr_class_to_string ~export:true;
  Eva.Env.open_module env mod_class_open
