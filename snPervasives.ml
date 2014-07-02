
let with_open_in fname_in func = 
  let chan_in = open_in fname_in in
  begin try
    let x = func chan_in in
    begin
      (close_in chan_in);
      x
    end
  with
    | exn ->
      begin
        close_in_noerr chan_in;
        raise exn
      end
  end

let with_open_out fname_out func =
  let chan_out = open_out fname_out in
  begin try
    let x = func chan_out in
    begin
      close_out chan_out;
      x
    end
  with
    | exn ->
      begin
        close_out_noerr chan_out;
        raise exn
      end
  end
