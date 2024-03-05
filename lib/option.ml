let get_or_else value = function Some v -> v | None -> value
let or_else alternate = function Some v -> Some v | None -> alternate
let filter fn = function Some v when fn v -> Some v | _ -> None
let flatten = function Some (Some v) -> Some v | _ -> None
let contains value = function Some v -> v == value | _ -> false

let zip l_opt r_opt =
  match (l_opt, r_opt) with
  | Some l_val, Some r_val -> Some (l_val, r_val)
  | _ -> None

let tap fn = function
  | Some v ->
      fn v;
      Some v
  | _ -> None

let tap_none fn = function
  | Some v -> v
  | _ ->
      fn ();
      None
