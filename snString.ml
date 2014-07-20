
open Printf

let concat_map sep proc lst =
  begin match lst with
  | [] ->
    ""
  | x::xs ->
    List.fold_left begin fun acc elem ->
      sprintf "%s%s%s" acc sep (proc elem)
    end (proc x) xs
  end

let concat sep strs =
  concat_map sep (fun x -> x) strs
