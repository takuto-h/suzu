
type t = {
  fname : string;
  mutable lnum : int;
  mutable cnum : int;
  mutable bol : int;
  strm : char Stream.t;
  source : Pos.source;
  mutable unread : char option;
  mutable prev_lnum : int;
  mutable prev_cnum : int;
  mutable prev_bol : int;
}

let create fname strm source = {
  fname = fname;
  lnum = 1;
  cnum = 0;
  bol = 0;
  strm = strm;
  source = source;
  unread = None;
  prev_lnum = 1;
  prev_cnum = 0;
  prev_bol = 0;
}

let of_channel fname chan_in = create fname (Stream.of_channel chan_in) Pos.File
let of_string fname str = create fname (Stream.of_string str) (Pos.String str)

let pos {fname;lnum;cnum;bol;source} =
  Pos.make fname lnum cnum bol source

let peek {strm;unread} =
  begin match unread with
    | None ->
      Stream.peek strm
    | Some _ ->
      unread
  end

let junk src =
  src.prev_lnum <- src.lnum;
  src.prev_cnum <- src.cnum;
  src.prev_bol <- src.bol;
  begin match (src.unread, peek src) with
    | (Some _, _) ->
      src.unread <- None
    | (None, None) ->
      ()
    | (None, Some '\n') ->
      src.lnum <- src.lnum + 1;
      src.cnum <- src.cnum + 1;
      src.bol <- src.cnum;
      Stream.junk src.strm;
    | (None, Some '\t') ->
      src.cnum <- src.cnum + 8;
      Stream.junk src.strm;
    | (None, Some _) ->
      src.cnum <- src.cnum + 1;
      Stream.junk src.strm;
  end

let unread src c =
  src.lnum <- src.prev_lnum;
  src.cnum <- src.prev_cnum;
  src.bol <- src.prev_bol;
  begin match src.unread with
    | None ->
      src.unread <- Some c
    | Some _ ->
      assert false
  end
