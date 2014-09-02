
open Printf

let make_binary_subr proc_out proc_body proc_in =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    let arg1 = VM.nth args 1 in
    VM.push_value vm (proc_out (proc_body (proc_in arg0) (proc_in arg1)))
  end

let make_unary_subr proc_out proc_body proc_in =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    VM.push_value vm (proc_out (proc_body (proc_in arg0)))
  end

let make_binary_cmp_subr proc =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    let arg1 = VM.nth args 1 in
    VM.push_value vm (VM.Bool (proc arg0 arg1))
  end

let make_binary_arith_subr proc =
  make_binary_subr (fun i -> VM.Int i) proc VM.int_of_value

let make_unary_arith_subr proc =
  make_unary_subr (fun i -> VM.Int i) proc VM.int_of_value

let subr_reset =
  VM.create_subr 1 begin fun vm args ->
    let func = VM.nth args 0 in
    VM.call vm func (VM.Args (VM.make_args [] []));
    vm.VM.controls <- VM.Reset::vm.VM.controls;
  end

let subr_shift =
  VM.create_subr 1 begin fun vm args ->
    let func = VM.nth args 0 in
    let rec loop rev_left right =
      begin match right with
        | [] ->
          (List.rev rev_left, right)
        | VM.Reset::right ->
          (List.rev (VM.Reset::rev_left), VM.Reset::right)
        | control::right ->
          loop (control::rev_left) right
      end
    in
    let (left, right) = loop [] vm.VM.controls in
    let left = VM.Dump (vm.VM.insns, vm.VM.stack, vm.VM.env, vm.VM.pos)::left in
    VM.call vm func (VM.Args (VM.make_args [VM.Cont left] []));
    vm.VM.controls <- right;
  end

let subr_write_line =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    print_endline (VM.string_of_value arg0);
    VM.push_value vm VM.Unit
  end

let subr_read_line =
  VM.create_subr 0 begin fun vm args ->
    VM.push_value vm (VM.String (read_line ()))
  end

let subr_eq = make_binary_cmp_subr ( = )
let subr_ne = make_binary_cmp_subr ( <> )
let subr_gt = make_binary_cmp_subr ( > )
let subr_ge = make_binary_cmp_subr ( >= )
let subr_lt = make_binary_cmp_subr ( < )
let subr_le = make_binary_cmp_subr ( <= )

let subr_compare =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    let arg1 = VM.nth args 1 in
    VM.push_value vm (VM.Int (compare arg0 arg1))
  end

let subr_show =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    VM.push_value vm (VM.String (VM.show_value arg0))
  end

let subr_class_of =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    VM.push_value vm (VM.Class (VM.get_class arg0))
  end

let subr_not =
  make_unary_subr (fun b -> VM.Bool b) not VM.bool_of_value

let subr_char_code =
  VM.create_subr 1 begin fun vm args ->
    let c = VM.char_of_value (VM.nth args 0) in
    VM.push_value vm (VM.Int (Char.code c))
  end

let subr_char_to_string =
  make_unary_subr (fun str -> VM.String str) (sprintf "%c") VM.char_of_value

let subr_string_get =
  VM.create_subr 2 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    let index = VM.int_of_value (VM.nth args 1) in
    begin try
        VM.push_value vm (VM.Char (String.get str index))
      with
      | Invalid_argument _ ->
        raise (VM.InternalError (sprintf "index %d out of bounds of %S\n" index str))
    end
  end

let subr_string_length =
  VM.create_subr 1 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    VM.push_value vm (VM.Int (String.length str))
  end

let subr_string_contain_p =
  VM.create_subr 2 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    let c = VM.char_of_value (VM.nth args 1) in
    VM.push_value vm (VM.Bool (String.contains str c))
  end

let subr_args_get =
  VM.create_subr 2 begin fun vm args ->
    let arg = VM.args_of_value (VM.nth args 0) in
    let index = VM.int_of_value (VM.nth args 1) in
    begin try
        VM.push_value vm (VM.some (VM.nth arg index))
      with
      | Failure "nth" ->
        VM.push_value vm (VM.none)
    end
  end

let subr_buffer_create =
  VM.create_subr 1 begin fun vm args ->
    let initial_buffer_size = VM.int_of_value (VM.nth args 0) in
    VM.push_value vm (VM.Buffer (Buffer.create initial_buffer_size))
  end

let subr_buffer_add_string =
  VM.create_subr 2 begin fun vm args ->
    let buffer = VM.buffer_of_value (VM.nth args 0) in
    let str = VM.string_of_value (VM.nth args 1) in
    Buffer.add_string buffer str;
    VM.push_value vm VM.Unit
  end

let subr_buffer_contents =
  VM.create_subr 1 begin fun vm args ->
    let buffer = VM.buffer_of_value (VM.nth args 0) in
    VM.push_value vm (VM.String (Buffer.contents buffer))
  end

let initialize loader =
  let env = [VM.create_frame ()] in
  VM.add_var env "reset" subr_reset ~export:true;
  VM.add_var env "shift" subr_shift ~export:true;
  VM.add_var env "write_line" subr_write_line ~export:true;
  VM.add_var env "read_line" subr_read_line ~export:true;
  VM.add_var env "gt" subr_gt ~export:true;
  VM.add_var env "lt" subr_lt ~export:true;
  VM.add_var env "ge" subr_ge ~export:true;
  VM.add_var env "le" subr_le ~export:true;
  VM.add_var env "eq" subr_eq ~export:true;
  VM.add_var env "ne" subr_ne ~export:true;
  VM.add_var env "compare" subr_compare ~export:true;
  VM.add_var env "show" subr_show ~export:true;
  VM.add_var env "class_of" subr_class_of ~export:true;
  VM.add_var env "add" (make_binary_arith_subr ( + )) ~export:true;
  VM.add_var env "sub" (make_binary_arith_subr ( - )) ~export:true;
  VM.add_var env "mul" (make_binary_arith_subr ( * )) ~export:true;
  VM.add_var env "div" (make_binary_arith_subr ( / )) ~export:true;
  VM.add_var env "mod" (make_binary_arith_subr ( mod )) ~export:true;
  VM.add_var env "plus" (make_unary_arith_subr ( ~+ )) ~export:true;
  VM.add_var env "minus" (make_unary_arith_subr ( ~- )) ~export:true;
  VM.add_var env "not" subr_not ~export:true;
  VM.add_var env "char_code" subr_char_code ~export:true;
  VM.add_var env "char_to_string" subr_char_to_string ~export:true;
  VM.add_var env "string_get" subr_string_get ~export:true;
  VM.add_var env "string_length" subr_string_length ~export:true;
  VM.add_var env "string_contain?" subr_string_contain_p ~export:true;
  VM.add_var env "args_get" subr_args_get ~export:true;
  VM.add_var env "buffer_create" subr_buffer_create ~export:true;
  VM.add_var env "buffer_add_string" subr_buffer_add_string ~export:true;
  VM.add_var env "buffer_contents" subr_buffer_contents ~export:true;
  VM.add_var loader.Loader.env "Builtin" (VM.Module (List.hd env));
  VM.add_var loader.Loader.env "debug" VM.subr_debug;
