
open Printf

let some x = VM.Variant ("Option::C", "Some", VM.make_args [x] [])
let none = VM.Variant ("Option::C", "None", VM.make_args [] [])

let cons x xs = VM.Variant ("List::C", "Cons", VM.make_args [x;xs] [])
let nil = VM.Variant ("List::C", "Nil", VM.make_args [] [])

let create_nullary_subr proc =
  VM.create_subr 0 begin fun vm args ->
    VM.push_value vm (proc ())
  end

let create_unary_subr proc =
  VM.create_subr 1 begin fun vm args ->
    let arg = VM.get_arg args 0 in
    VM.push_value vm (proc arg)
  end

let create_binary_subr proc =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.get_arg args 0 in
    let arg1 = VM.get_arg args 1 in
    VM.push_value vm (proc arg0 arg1)
  end

let create_ternary_subr proc =
  VM.create_subr 3 begin fun vm args ->
    let arg0 = VM.get_arg args 0 in
    let arg1 = VM.get_arg args 1 in
    let arg2 = VM.get_arg args 2 in
    VM.push_value vm (proc arg0 arg1 arg2)
  end

let create_binary_cmp_subr proc =
  create_binary_subr (fun arg0 arg1 -> VM.Bool (proc arg0 arg1))

let create_unary_int_subr proc =
  create_unary_subr (fun arg -> VM.Int (proc (VM.int_of_value arg)))

let create_binary_int_subr proc =
  create_binary_subr begin fun arg0 arg1 ->
    let i0 = VM.int_of_value arg0 in
    let i1 = VM.int_of_value arg1 in
    VM.Int (proc i0 i1)
  end

let create_unary_float_subr proc =
  create_unary_subr (fun arg -> VM.Float (proc (VM.float_of_value arg)))

let create_binary_float_subr proc =
  create_binary_subr begin fun arg0 arg1 ->
    let f0 = VM.float_of_value arg0 in
    let f1 = VM.float_of_value arg1 in
    VM.Float (proc f0 f1)
  end


let subr_write_line =
  create_unary_subr begin fun arg ->
    let str = VM.string_of_value arg in
    print_endline str;
    VM.unit
  end

let subr_read_line =
  create_nullary_subr begin fun () ->
    VM.String (read_line ())
  end

let subr_compare =
  create_binary_subr (fun arg0 arg1 -> VM.Int (compare arg0 arg1))

let subr_show =
  create_unary_subr (fun arg -> VM.String (VM.show_value arg))

let subr_bool_not =
  create_unary_subr (fun arg -> VM.Bool (not (VM.bool_of_value arg)))

let subr_float_from_string =
  create_unary_subr begin fun arg0 ->
    let str = VM.string_of_value arg0 in
    begin try
        VM.Float (float_of_string str)
      with
      | Failure "float_of_string" ->
        raise (VM.InternalError "invalid floating point number\n")
    end
  end

let subr_float_from_int =
  create_unary_subr (fun arg0 -> VM.Float (float_of_int (VM.int_of_value arg0)))

let subr_char_code =
  create_unary_subr (fun arg -> VM.Int (Char.code (VM.char_of_value arg)))

let subr_char_to_string =
  create_unary_subr (fun arg -> VM.String (sprintf "%c" (VM.char_of_value arg)))

let subr_string_get =
  create_binary_subr begin fun arg0 arg1 ->
    let str = VM.string_of_value arg0 in
    let index = VM.int_of_value arg1 in
    begin try
        some (VM.Char (String.get str index))
      with
      | Invalid_argument _ ->
        none
    end
  end

let subr_string_length =
  create_unary_subr (fun arg -> VM.Int (String.length (VM.string_of_value arg)))

let subr_string_contain_p =
  create_binary_subr begin fun arg0 arg1 ->
    let str = VM.string_of_value arg0 in
    let c = VM.char_of_value arg1 in
    VM.Bool (String.contains str c)
  end

let subr_args_get =
  create_binary_subr begin fun arg0 arg1 ->
    let args = VM.args_of_value arg0 in
    let index = VM.int_of_value arg1 in
    begin try
        some (VM.get_arg args index)
      with
      | Failure "nth" ->
        none
    end
  end

let subr_class_of =
  create_unary_subr (fun arg -> VM.Class (VM.get_class arg))

let subr_buffer_create =
  create_unary_subr begin fun arg ->
    let initial_buffer_size = VM.int_of_value arg in
    VM.Buffer (Buffer.create initial_buffer_size)
  end

let subr_buffer_add_string =
  create_binary_subr begin fun arg0 arg1 ->
    let buffer = VM.buffer_of_value arg0 in
    let str = VM.string_of_value arg1 in
    Buffer.add_string buffer str;
    VM.unit;
  end

let subr_buffer_contents =
  create_unary_subr begin fun arg ->
    let buffer = VM.buffer_of_value arg in
    VM.String (Buffer.contents buffer)
  end

let subr_hash_create =
  create_unary_subr begin fun arg ->
    let initial_table_size = VM.int_of_value arg in
    VM.Hash (Hashtbl.create initial_table_size)
  end

let subr_hash_get =
  create_binary_subr begin fun arg0 arg1 ->
    let table = VM.hashtbl_of_value arg0 in
    let key = arg1 in
    begin try
        some (Hashtbl.find table key)
      with
      | Not_found ->
        none
    end
  end

let subr_hash_add =
  create_ternary_subr begin fun arg0 arg1 arg2 ->
    let table = VM.hashtbl_of_value arg0 in
    let key = arg1 in
    let value = arg2 in
    Hashtbl.add table key value;
    VM.unit
  end

let subr_hash_remove =
  create_binary_subr begin fun arg0 arg1 ->
    let table = VM.hashtbl_of_value arg0 in
    let key = arg1 in
    Hashtbl.remove table key;
    VM.unit
  end

let subr_hash_keys =
  create_unary_subr begin fun arg0 ->
    let table = VM.hashtbl_of_value arg0 in
    Hashtbl.fold begin fun key value acc ->
      cons key acc
    end table nil
  end

let initialize loader =
  let env = [VM.create_frame ()] in
  VM.add_var env "reset" VM.subr_reset ~export:true;
  VM.add_var env "shift" VM.subr_shift ~export:true;
  VM.add_var env "write_line" subr_write_line ~export:true;
  VM.add_var env "read_line" subr_read_line ~export:true;
  VM.add_var env "gt" (create_binary_cmp_subr ( > )) ~export:true;
  VM.add_var env "lt" (create_binary_cmp_subr ( < )) ~export:true;
  VM.add_var env "ge" (create_binary_cmp_subr ( >= )) ~export:true;
  VM.add_var env "le" (create_binary_cmp_subr ( <= )) ~export:true;
  VM.add_var env "eq" (create_binary_cmp_subr ( = )) ~export:true;
  VM.add_var env "ne" (create_binary_cmp_subr ( <> )) ~export:true;
  VM.add_var env "compare" subr_compare ~export:true;
  VM.add_var env "show" subr_show ~export:true;
  VM.add_var env "bool_not" subr_bool_not ~export:true;
  VM.add_var env "int_neg" (create_unary_int_subr ( ~- )) ~export:true;
  VM.add_var env "int_pos" (create_unary_int_subr ( ~+ )) ~export:true;
  VM.add_var env "int_add" (create_binary_int_subr ( + )) ~export:true;
  VM.add_var env "int_sub" (create_binary_int_subr ( - )) ~export:true;
  VM.add_var env "int_mul" (create_binary_int_subr ( * )) ~export:true;
  VM.add_var env "int_div" (create_binary_int_subr ( / )) ~export:true;
  VM.add_var env "int_mod" (create_binary_int_subr ( mod )) ~export:true;
  VM.add_var env "float_from_string" subr_float_from_string ~export:true;
  VM.add_var env "float_from_int" subr_float_from_int ~export:true;
  VM.add_var env "float_neg" (create_unary_float_subr ( ~-. )) ~export:true;
  VM.add_var env "float_pos" (create_unary_float_subr ( ~+. )) ~export:true;
  VM.add_var env "float_add" (create_binary_float_subr ( +. )) ~export:true;
  VM.add_var env "float_sub" (create_binary_float_subr ( -. )) ~export:true;
  VM.add_var env "float_mul" (create_binary_float_subr ( *. )) ~export:true;
  VM.add_var env "float_div" (create_binary_float_subr ( /. )) ~export:true;
  VM.add_var env "float_pow" (create_binary_float_subr ( ** )) ~export:true;
  VM.add_var env "float_sqrt" (create_unary_float_subr sqrt) ~export:true;
  VM.add_var env "float_exp" (create_unary_float_subr exp) ~export:true;
  VM.add_var env "float_log" (create_unary_float_subr log) ~export:true;
  VM.add_var env "float_log10" (create_unary_float_subr log10) ~export:true;
  VM.add_var env "float_cos" (create_unary_float_subr cos) ~export:true;
  VM.add_var env "float_sin" (create_unary_float_subr sin) ~export:true;
  VM.add_var env "float_tan" (create_unary_float_subr tan) ~export:true;
  VM.add_var env "float_acos" (create_unary_float_subr acos) ~export:true;
  VM.add_var env "float_asin" (create_unary_float_subr asin) ~export:true;
  VM.add_var env "float_atan" (create_unary_float_subr atan) ~export:true;
  VM.add_var env "float_atan2" (create_binary_float_subr atan2) ~export:true;
  VM.add_var env "char_code" subr_char_code ~export:true;
  VM.add_var env "char_to_string" subr_char_to_string ~export:true;
  VM.add_var env "string_get" subr_string_get ~export:true;
  VM.add_var env "string_length" subr_string_length ~export:true;
  VM.add_var env "string_contain?" subr_string_contain_p ~export:true;
  VM.add_var env "args_get" subr_args_get ~export:true;
  VM.add_var env "class_of" subr_class_of ~export:true;
  VM.add_var env "buffer_create" subr_buffer_create ~export:true;
  VM.add_var env "buffer_add_string" subr_buffer_add_string ~export:true;
  VM.add_var env "buffer_contents" subr_buffer_contents ~export:true;
  VM.add_var env "hash_create" subr_hash_create ~export:true;
  VM.add_var env "hash_get" subr_hash_get ~export:true;
  VM.add_var env "hash_add" subr_hash_add ~export:true;
  VM.add_var env "hash_remove" subr_hash_remove ~export:true;
  VM.add_var env "hash_keys" subr_hash_keys ~export:true;
  VM.add_var (Loader.get_env loader) "Builtin" (VM.Module (List.hd env));
