
open Printf

let make_binary_subr proc_out proc_body proc_in =
  Eva.create_subr 2 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    let arg1 = Eva.Args.nth args 1 in
    proc_out (proc_body (proc_in pos arg0) (proc_in pos arg1))
  end

let make_unary_subr proc_out proc_body proc_in =
  Eva.create_subr 1 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    proc_out (proc_body (proc_in pos arg0))
  end

let make_binary_cmp_subr proc =
  Eva.create_subr 2 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    let arg1 = Eva.Args.nth args 1 in
    Eva.value_of_bool (proc arg0 arg1)
  end

let make_binary_arith_subr proc =
  make_binary_subr Eva.value_of_int proc Eva.int_of_value

let make_unary_arith_subr proc =
  make_unary_subr Eva.value_of_int proc Eva.int_of_value

let subr_eq = make_binary_cmp_subr ( = )
let subr_ne = make_binary_cmp_subr ( <> )
let subr_gt = make_binary_cmp_subr ( > )
let subr_ge = make_binary_cmp_subr ( >= )
let subr_lt = make_binary_cmp_subr ( < )
let subr_le = make_binary_cmp_subr ( <= )

let subr_compare =
  Eva.create_subr 2 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    let arg1 = Eva.Args.nth args 1 in
    Eva.value_of_int (compare arg0 arg1)
  end

let subr_show =
  Eva.create_subr 1 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    Eva.value_of_string (Eva.Value.show arg0)
  end

let subr_not =
  make_unary_subr Eva.value_of_bool not Eva.bool_of_value

let subr_char_to_string =
  make_unary_subr Eva.value_of_string (sprintf "%c") Eva.char_of_value

module Format = struct

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

  let rec execute_format_insns eva pos insns args buf =
    begin match insns with
      | [] ->
        Buffer.contents buf
      | (String str)::insns ->
        Buffer.add_string buf str;
        execute_format_insns eva pos insns args buf
      | (Placeholder n)::insns ->
        let str = Eva.string_of_value pos begin try
              Eva.call_method eva pos (List.nth args n) (Selector.Ident "to_string") (Eva.Args.make [] [])
            with
            | Failure "nth" ->
              raise (Eva.Error (pos, sprintf "argument not supplied: {%d}\n" n, []))
          end
        in
        Buffer.add_string buf str;
        execute_format_insns eva pos insns args buf
    end

  let subr =
    Eva.create_subr 1 ~allows_rest:true begin fun eva pos args ->
      let self = Eva.Args.nth args 0 in
      let insns = begin try
          parse_format_insns (Stream.of_string (Eva.string_of_value pos self)) []
        with
        | Illigal_format ->
          raise (Eva.Error (pos, sprintf "illegal format string: %s\n" (Eva.Value.show self), []))
      end
      in
      let buf = Buffer.create initial_formatted_buffer_size in
      Eva.String (execute_format_insns eva pos insns (List.tl args.Eva.normal_args) buf)
    end
end

let subr_class_of =
  Eva.create_subr 1 begin fun eva pos args ->
    let arg0 = Eva.Args.nth args 0 in
    Eva.Class (Eva.Value.class_of arg0)
  end

let subr_write_line =
  Eva.create_subr 1 ~allows_rest:true begin fun eva pos args ->
    let str = Eva.call_fun eva pos Format.subr args in
    print_endline (Eva.string_of_value pos str);
    Eva.Unit
  end

let subr_read_line =
  Eva.create_subr 0 begin fun eva pos args ->
      Eva.String (read_line ())
  end

let initialize {Interp.eva={Eva.env}} =
  let mod_builtin = Eva.Env.create_local env in
  Eva.Env.add_var env "Builtin" (Eva.Module mod_builtin);
  Eva.Env.add_var mod_builtin "gt" subr_gt ~export:true;
  Eva.Env.add_var mod_builtin "lt" subr_lt ~export:true;
  Eva.Env.add_var mod_builtin "ge" subr_ge ~export:true;
  Eva.Env.add_var mod_builtin "le" subr_le ~export:true;
  Eva.Env.add_var mod_builtin "eq" subr_eq ~export:true;
  Eva.Env.add_var mod_builtin "ne" subr_ne ~export:true;
  Eva.Env.add_var mod_builtin "compare" subr_compare ~export:true;
  Eva.Env.add_var mod_builtin "show" subr_show ~export:true;
  Eva.Env.add_var mod_builtin "add" (make_binary_arith_subr ( + )) ~export:true;
  Eva.Env.add_var mod_builtin "sub" (make_binary_arith_subr ( - )) ~export:true;
  Eva.Env.add_var mod_builtin "mul" (make_binary_arith_subr ( * )) ~export:true;
  Eva.Env.add_var mod_builtin "div" (make_binary_arith_subr ( / )) ~export:true;
  Eva.Env.add_var mod_builtin "mod" (make_binary_arith_subr ( mod )) ~export:true;
  Eva.Env.add_var mod_builtin "plus" (make_unary_arith_subr ( ~+ )) ~export:true;
  Eva.Env.add_var mod_builtin "minus" (make_unary_arith_subr ( ~- )) ~export:true;
  Eva.Env.add_var mod_builtin "not" subr_not ~export:true;
  Eva.Env.add_var mod_builtin "char_to_string" subr_char_to_string ~export:true;
  Eva.Env.add_var mod_builtin "format" Format.subr ~export:true;
  Eva.Env.add_var mod_builtin "class_of" subr_class_of ~export:true;
  Eva.Env.add_var mod_builtin "write_line" subr_write_line ~export:true;
  Eva.Env.add_var mod_builtin "read_line" subr_read_line ~export:true;
