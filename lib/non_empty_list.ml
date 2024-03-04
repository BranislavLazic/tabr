type 'a non_empty_list = 'a * 'a list

let one a = (a, [])

let from_list list =
  match list with head :: tail -> Some (head, tail) | [] -> None

let map fn nel = match nel with a, rest -> (fn a, List.map fn rest)

let filter fn nel =
  match nel with a, rest -> (if fn a then [ a ] else []) @ List.filter fn rest

let exists fn nel = match nel with a, rest -> fn a || List.exists fn rest
let for_all fn nel = match nel with a, rest -> fn a && List.for_all fn rest

let ( <+> ) l_nel r_nel =
  match (l_nel, r_nel) with
  | (l, rest_l), (r, rest_r) -> (l, rest_l @ [ r ] @ rest_r)

let concat l_nel r_nel = l_nel <+> r_nel
