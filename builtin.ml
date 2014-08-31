
open Printf

let make_binary_subr proc_out proc_body proc_in =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    let arg1 = VM.nth args 1 in
    proc_out (proc_body (proc_in arg0) (proc_in arg1))
  end

let make_unary_subr proc_out proc_body proc_in =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    proc_out (proc_body (proc_in arg0))
  end

let make_binary_cmp_subr proc =
  VM.create_subr 2 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    let arg1 = VM.nth args 1 in
    VM.Bool (proc arg0 arg1)
  end

let make_binary_arith_subr proc =
  make_binary_subr (fun i -> VM.Int i) proc VM.int_of_value

let make_unary_arith_subr proc =
  make_unary_subr (fun i -> VM.Int i) proc VM.int_of_value

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
    VM.Int (compare arg0 arg1)
  end

let subr_show =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    VM.String (VM.show_value arg0)
  end

let subr_not =
  make_unary_subr (fun b -> VM.Bool b) not VM.bool_of_value

let subr_char_code =
  VM.create_subr 1 begin fun vm args ->
    let c = VM.char_of_value (VM.nth args 0) in
    VM.Int (Char.code c)
  end

let subr_char_to_string =
  make_unary_subr (fun str -> VM.String str) (sprintf "%c") VM.char_of_value

let subr_string_get =
  VM.create_subr 2 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    let index = VM.int_of_value (VM.nth args 1) in
    begin try
        VM.Char (String.get str index)
      with
      | Invalid_argument _ ->
        raise (VM.InternalError (sprintf "index %d out of bounds of %S\n" index str))
    end
  end

let subr_string_length =
  VM.create_subr 1 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    VM.Int (String.length str)
  end

let subr_string_contain_p =
  VM.create_subr 2 begin fun vm args ->
    let str = VM.string_of_value (VM.nth args 0) in
    let c = VM.char_of_value (VM.nth args 1) in
    VM.Bool (String.contains str c)
  end

let subr_args_get =
  VM.create_subr 2 begin fun vm args ->
    let arg = VM.args_of_value (VM.nth args 0) in
    let index = VM.int_of_value (VM.nth args 1) in
    begin try
        VM.some (VM.nth arg index)
      with
      | Failure "nth" ->
        VM.none
    end
  end

let subr_buffer_create =
  VM.create_subr 1 begin fun vm args ->
    let initial_buffer_size = VM.int_of_value (VM.nth args 0) in
    VM.Buffer (Buffer.create initial_buffer_size)
  end

let subr_buffer_add_string =
  VM.create_subr 2 begin fun vm args ->
    let buffer = VM.buffer_of_value (VM.nth args 0) in
    let str = VM.string_of_value (VM.nth args 1) in
    Buffer.add_string buffer str;
    VM.Unit
  end

let subr_buffer_contents =
  VM.create_subr 1 begin fun vm args ->
    let buffer = VM.buffer_of_value (VM.nth args 0) in
    VM.String (Buffer.contents buffer)
  end
    
(*module Format = struct

  type format_insn =
    | String of string
    | Placeholder of int

  exception Illigal_format

  let initial_unformatted_buffer_size = 16
  let initial_formatted_buffer_size = 256

  let rec parse_format_insns strm rev_insns =
    begin match Stream.peek strm with
      | Some '{' ->
        Stream.junk strm;
        if Stream.peek strm = Some '{' then
          parse_string strm '{' rev_insns
        else
          parse_placeholder strm rev_insns
      | Some '}' ->
        Stream.junk strm;
        if Stream.peek strm = Some '}' then
          parse_string strm '}' rev_insns
        else
          raise Illigal_format
      | Some c ->
        parse_string strm c rev_insns
      | None ->
        List.rev rev_insns
    end

  and parse_placeholder strm rev_insns =
    let rec loop n =
      begin match Stream.peek strm with
        | Some c when SnChar.is_digit c ->
          let i = SnChar.int_of_digit c in
          Stream.junk strm;
          loop (n * 10 + i)
        | Some '}' ->
          Stream.junk strm;
          parse_format_insns strm (Placeholder n::rev_insns)
        | Some _ | None ->
          raise Illigal_format
      end
    in
    loop 0

  and parse_string strm c rev_insns =
    let buf = Buffer.create initial_unformatted_buffer_size in
    Buffer.add_char buf c;
    Stream.junk strm;
    let rec loop () =
      begin match Stream.peek strm with
        | Some '{' ->
          Stream.junk strm;
          if Stream.peek strm = Some '{' then
            begin
              Buffer.add_char buf '{';
              Stream.junk strm;
              loop ()
            end
          else
            parse_placeholder strm (String (Buffer.contents buf)::rev_insns)
        | Some '}' ->
          Stream.junk strm;
          if Stream.peek strm = Some '}' then
            begin
              Buffer.add_char buf '}';
              Stream.junk strm;
              loop ()
            end
          else
            raise Illigal_format
        | Some c ->
          Buffer.add_char buf c;
          Stream.junk strm;
          loop ()
        | None ->
          List.rev (String (Buffer.contents buf)::rev_insns)
      end
    in
    loop ()

  let rec execute_format_insns vm insns args buf =
    begin match insns with
      | [] ->
        Buffer.contents buf
      | (String str)::insns ->
        Buffer.add_string buf str;
        execute_format_insns vm insns args buf
      | (Placeholder n)::insns ->
        let str = VM.string_of_value vm begin try
              VM.send vm (List.nth args n) (Selector.Ident "to_string") (VM.make_args [] [])
            with
            | Failure "nth" ->
              raise (VM.InternalError (vm, sprintf "argument not supplied: {%d}\n" n))
          end
        in
        Buffer.add_string buf str;
        execute_format_insns vm insns args buf
    end

  let subr =
    VM.create_subr 1 ~allows_rest:true begin fun vm args ->
      let self = VM.nth args 0 in
      let insns = begin try
          parse_format_insns (Stream.of_string (VM.string_of_value vm self)) []
        with
        | Illigal_format ->
          raise (VM.InternalError (vm, sprintf "illegal format string: %s\n" (VM.show_value self)))
      end
      in
      let buf = Buffer.create initial_formatted_buffer_size in
      VM.value_of_string (execute_format_insns vm insns (List.tl args.VM.normal_args) buf)
    end
end*)

let subr_class_of =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    VM.Class (VM.get_class arg0)
  end

let subr_write_line =
  VM.create_subr 1 begin fun vm args ->
    let arg0 = VM.nth args 0 in
    print_endline (VM.string_of_value arg0);
    VM.Unit
  end

let subr_read_line =
  VM.create_subr 0 begin fun vm args ->
    VM.String (read_line ())
  end

let initialize loader =
  let env = [VM.create_frame ()] in
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
  VM.add_var loader.Loader.env "debug" begin
    VM.create_subr 0 begin fun vm args ->
      List.iter begin fun {VM.vars;VM.methods} ->
        Hashtbl.iter begin fun x value ->
          printf "%s = %s\n" x (VM.show_value value)
        end vars;
        Hashtbl.iter begin fun (klass, sel) value ->
          printf "%s#%s = %s\n" klass sel (VM.show_value value)
        end methods
      end vm.VM.env;
      VM.Unit
    end
  end
