type 'a t = 'a * 'a list

let one a = (a, [])

let from_list list =
  match list with head :: tail -> Some (head, tail) | [] -> None

let map fn nel = match nel with head, rest -> (fn head, List.map fn rest)

let filter fn nel =
  match nel with
  | head, rest ->
      from_list ((if fn head then [ head ] else []) @ List.filter fn rest)

let find fn nel =
  match nel with
  | head, rest -> if fn head then Some head else List.find_opt fn rest

let exists fn nel =
  match nel with head, rest -> fn head || List.exists fn rest

let for_all fn nel =
  match nel with head, rest -> fn head && List.for_all fn rest

let zip l_nel r_nel =
  match (l_nel, r_nel) with
  | (l, rest_l), (r, rest_r) -> ((l, r), List.combine rest_l rest_r)

let reverse nel =
  match nel with
  | head, rest ->
      if List.length rest > 0 then
        let revRest = List.rev rest in
        (List.hd revRest, List.tl revRest @ [ head ])
      else (head, [])

let ( <+> ) l_nel r_nel =
  match (l_nel, r_nel) with
  | (l, rest_l), (r, rest_r) -> (l, rest_l @ [ r ] @ rest_r)

let ( <+ ) nel value = match nel with head, rest -> (head, rest @ [ value ])
let ( +> ) value nel = match nel with head, rest -> (value, [ head ] @ rest)
let size nel = match nel with _, rest -> List.length rest + 1
let to_list nel = match nel with head, rest -> head :: rest
let to_option nel = match nel with head, rest -> Some (head, rest)
let to_result nel = match nel with head, rest -> Ok (head, rest)
