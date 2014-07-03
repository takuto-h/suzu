
open SnPervasives
open Printf

type source =
  | File
  | String of string

type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
  source : source;
}

let make fname lnum cnum bol source = {
  fname = fname;
  lnum = lnum;
  cnum = cnum;
  bol = bol;
  source = source;
}

let show {fname;lnum;cnum;bol;} =
  let offset = cnum - bol in
  sprintf "%s:%d:%d" fname lnum offset

let show_source {fname;lnum;cnum;bol;source;} =
  let offset = cnum - bol in
  let str_anchor = String.make (offset + 1) ' ' in
  begin
    String.set str_anchor offset '^';
    begin match source with
      | File ->
        with_open_in fname begin fun chan_in ->
          begin try
            begin
              seek_in chan_in bol;
              let str_line = input_line chan_in in
              sprintf "%s\n%s\n" str_line str_anchor
            end
          with
            | End_of_file -> ""
          end
        end
    | String str ->
      let str = String.sub str bol (String.length str - bol) in
      let str_line = begin try
        String.sub str 0 (String.index str '\n')
      with
        | Not_found -> str
      end in
      sprintf "%s\n%s\n" str_line str_anchor
    end
  end

let show_message pos message =
  sprintf "%s: %s%s" (show pos) message (show_source pos)

let show_error pos message =
  show_message pos (sprintf "error: %s" message)