
open Printf

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

let rec execute_format_insns insns args buf =
  begin match insns with
    | [] ->
      Buffer.contents buf
    | (String str)::insns ->
      Buffer.add_string buf str;
      execute_format_insns insns args buf
    | (Placeholder n)::insns ->
      Buffer.add_string buf (string_of_int n);
      execute_format_insns insns args buf
  end

let subr_string_format =
  Eva.Subr begin 1, true, fun eva pos args ->
      let self = List.nth args 0 in
      let insns = begin try
          parse_format_insns (Stream.of_string (Eva.string_of_value pos self)) []
        with
        | Illigal_format ->
          failwith (Pos.show_error pos (sprintf "illegal format string: %s\n" (Eva.Value.show self)))
      end
      in
      Eva.String (execute_format_insns insns (List.tl args) (Buffer.create initial_formatted_buffer_size))
  end

let initialize env =
  let mod_string = Eva.Env.create_local env in
  let mod_string_open = Eva.Env.create_local mod_string in
  Eva.Env.add_var env "String" (Eva.Module mod_string);
  Eva.Env.add_var mod_string "C" (Eva.Class "String:C") ~export:true;
  Eva.Env.add_var mod_string "format" subr_string_format ~export:true;
  Eva.Env.add_var mod_string "Open" (Eva.Module mod_string_open) ~export:true;
