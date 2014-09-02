
open SnPervasives
open Printf

let initial_output_buffer_size = 1024

let usage_msg = sprintf "usage: %s [source_files]" Sys.argv.(0)

let main () =
  Arg.parse_argv ~current:(ref 0) Sys.argv [] begin fun fname_sn ->
    let fname = Filename.chop_suffix fname_sn ".sn" in
    let fname_ml = sprintf "%s.ml" fname in
    let fname_mli = sprintf "%s.mli" fname in
    with_open_in begin fun chan_sn ->
      with_open_out begin fun chan_ml ->
        let buf = Buffer.create initial_output_buffer_size in
        begin try
            while true do
              let line = input_line chan_sn in
              Buffer.add_string buf (sprintf "%s\n" line)
            done
          with
          | End_of_file ->
            ()
        end;
        output_string chan_ml (sprintf "\nlet str = %S\n" (Buffer.contents buf));
        output_string chan_ml (sprintf "\nlet initialize loader =");
        output_string chan_ml (sprintf "\n  Loader.load_string loader %S str\n" fname_sn);
      end fname_ml
    end fname_sn;
    with_open_out begin fun chan_mli ->
      output_string chan_mli "\nval initialize : Loader.t -> unit\n"
    end fname_mli
  end usage_msg

let () = main ()
