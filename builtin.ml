
open Printf

let some x = VM.Variant ("Option::C", "Some", VM.make_args [x] [])
let none = VM.Variant ("Option::C", "None", VM.make_args [] [])

let make_binary_subr proc =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.get_arg args 0 in
    let arg1 = VM.get_arg args 1 in
    VM.push_value vm (proc arg0 arg1)
  end

let make_unary_subr proc =
  VM.create_subr 1 begin fun vm args ->
    let arg = VM.get_arg args 0 in
    VM.push_value vm (proc arg)
  end

let make_nullary_subr proc =
  VM.create_subr 0 begin fun vm args ->
    VM.push_value vm (proc ())
  end

let make_binary_cmp_subr proc =
  make_binary_subr (fun arg0 arg1 -> VM.Bool (proc arg0 arg1))

let make_binary_arith_subr proc =
  make_binary_subr begin fun arg0 arg1 ->
    let i0 = VM.int_of_value arg0 in
    let i1 = VM.int_of_value arg1 in
    VM.Int (proc i0 i1)
  end

let make_unary_arith_subr proc =
  make_unary_subr (fun arg -> VM.Int (proc (VM.int_of_value arg)))

let subr_reset =
  VM.create_subr 1 begin fun vm args ->
    let func = VM.get_arg args 0 in
    VM.call vm func (VM.Args (VM.make_args [] []));
    vm.VM.controls <- VM.Reset::vm.VM.controls;
  end

let subr_shift =
  VM.create_subr 1 begin fun vm args ->
    let func = VM.get_arg args 0 in
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
  make_unary_subr begin fun arg ->
    let str = VM.string_of_value arg in
    print_endline str;
    VM.Unit
  end

let subr_read_line =
  make_nullary_subr begin fun () ->
    VM.String (read_line ())
  end

let subr_eq = make_binary_cmp_subr ( = )
let subr_ne = make_binary_cmp_subr ( <> )
let subr_gt = make_binary_cmp_subr ( > )
let subr_ge = make_binary_cmp_subr ( >= )
let subr_lt = make_binary_cmp_subr ( < )
let subr_le = make_binary_cmp_subr ( <= )

let subr_compare =
  make_binary_subr (fun arg0 arg1 -> VM.Int (compare arg0 arg1))

let subr_show =
  make_unary_subr (fun arg -> VM.String (VM.show_value arg))

let subr_class_of =
  make_unary_subr (fun arg -> VM.Class (VM.get_class arg))

let subr_not =
  make_unary_subr (fun arg -> VM.Bool (not (VM.bool_of_value arg)))

let subr_char_code =
  make_unary_subr (fun arg -> VM.Int (Char.code (VM.char_of_value arg)))

let subr_char_to_string =
  make_unary_subr (fun arg -> VM.String (sprintf "%c" (VM.char_of_value arg)))

let subr_string_get =
  make_binary_subr begin fun arg0 arg1 ->
    let str = VM.string_of_value arg0 in
    let index = VM.int_of_value arg1 in
    begin try
        VM.Char (String.get str index)
      with
      | Invalid_argument _ ->
        raise (VM.InternalError (sprintf "index %d out of bounds of %S\n" index str))
    end
  end

let subr_string_length =
  make_unary_subr (fun arg -> VM.Int (String.length (VM.string_of_value arg)))

let subr_string_contain_p =
  make_binary_subr begin fun arg0 arg1 ->
    let str = VM.string_of_value arg0 in
    let c = VM.char_of_value arg1 in
    VM.Bool (String.contains str c)
  end

let subr_args_get =
  make_binary_subr begin fun arg0 arg1 ->
    let args = VM.args_of_value arg0 in
    let index = VM.int_of_value arg1 in
    begin try
        some (VM.get_arg args index)
      with
      | Failure "nth" ->
        none
    end
  end

let subr_buffer_create =
  make_unary_subr begin fun arg ->
    let initial_buffer_size = VM.int_of_value arg in
    VM.Buffer (Buffer.create initial_buffer_size)
  end

let subr_buffer_add_string =
  make_binary_subr begin fun arg0 arg1 ->
    let buffer = VM.buffer_of_value arg0 in
    let str = VM.string_of_value arg1 in
    Buffer.add_string buffer str;
    VM.Unit;
  end

let subr_buffer_contents =
  make_unary_subr begin fun arg ->
    let buffer = VM.buffer_of_value arg in
    VM.String (Buffer.contents buffer)
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
