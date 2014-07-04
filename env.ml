
type frame = (string, Value.t) Hashtbl.t

type t =
  | Global of frame
  | Local of frame * t

let initial_global_table_size = 16
let initial_local_table_size = 4

let create_global = Global (Hashtbl.create initial_global_table_size)
let create_local outer = Local (Hashtbl.create initial_local_table_size, outer)

let rec find env x =
  begin match env with
    | Global frame ->
      Hashtbl.find frame x
    | Local (frame, outer) ->
      begin try
        Hashtbl.find frame x
      with
        | Not_found ->
          find outer x
      end
  end
