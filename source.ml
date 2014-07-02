
type t = {
  fname : string;
  mutable lnum : int;
  mutable cnum : int;
  mutable bol : int;
  strm : char Stream.t;
  source : Pos.source;
}

let create fname strm source = {
  fname = fname;
  lnum = 1;
  cnum = 0;
  bol = 0;
  strm = strm;
  source = source;
}

let of_channel fname chan_in = create fname (Stream.of_channel chan_in) Pos.File
let of_string fname str = create fname (Stream.of_string str) (Pos.String str)

let pos {fname;lnum;cnum;bol;source;} =
  Pos.make fname lnum cnum bol source

let peek {strm;} =
  Stream.peek strm

let junk src =
  begin match peek src with
    | None ->
      ()
    | Some '\n' ->
      begin
        src.lnum <- src.lnum + 1;
        src.cnum <- src.cnum + 1;
        src.bol <- src.cnum;
        Stream.junk src.strm
      end
    | Some _ ->
      begin
        src.cnum <- src.cnum + 1;
        Stream.junk src.strm
      end
  end

