
open Printf

let initial_buffer_size = 256

let rec read buf =
  let line = read_line () in
  if line = "" then
    Buffer.contents buf
  else
    begin
      Buffer.add_string buf (sprintf "%s\n" line);
      printf "... ";
      read buf
    end

let rec loop () =
  begin try
    begin
      printf ">>> ";
      let str = read (Buffer.create initial_buffer_size) in
      printf "%s\n" str;
      loop ()
    end
  with
    | End_of_file ->
      ()
  end

let () = loop ()
