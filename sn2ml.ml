
open SnPervasives
open Printf

let initial_buffer_size = 1024

let usage_msg = sprintf "usage: %s [source_files]" Sys.argv.(0)

let main () =
  Arg.parse_argv ~current:(ref 0) Sys.argv [] begin fun file_name_in ->
    let file_name_out = sprintf "%s.ml" (Filename.chop_suffix file_name_in ".sn") in
    with_open_in begin fun chan_in ->
      with_open_out begin fun chan_out ->
        let buf = Buffer.create initial_buffer_size in
        let str = begin try
            while true do
              let line = input_line chan_in in
              Buffer.add_string buf (sprintf "%s\n" line)
            done;
            ""
          with
          | End_of_file ->
            Buffer.contents buf
        end
        in
        output_string chan_out (sprintf "\nlet str = %S\n" str);
        output_string chan_out (sprintf "\nlet initialize loader =\n  Loader.load_string loader %S str\n" file_name_in);
      end file_name_out
    end file_name_in
  end usage_msg

let () = main ()
