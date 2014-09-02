
let with_open_in proc fname_in = 
  let chan_in = open_in fname_in in
  try
    let x = proc chan_in in
    close_in chan_in;
    x
  with
  | exn ->
    close_in_noerr chan_in;
    raise exn


let with_open_out proc fname_out =
  let chan_out = open_out fname_out in
  try
    let x = proc chan_out in
    close_out chan_out;
    x
  with
  | exn ->
    close_out_noerr chan_out;
    raise exn
