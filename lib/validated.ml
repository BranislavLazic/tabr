type ('a, 'b) validated = Valid of 'a | Invalid of 'b

let valid a = Valid a
let invalid b = Invalid b
let invalid_nel b = Invalid b
let is_valid = function Valid _ -> true | Invalid _ -> false
let is_invalid = function Valid _ -> false | Invalid _ -> true
let map fn = function Valid a -> Valid (fn a) | Invalid b -> Invalid b

let bimap valid_fn invalid_fn = function
  | Valid a -> Valid (valid_fn a)
  | Invalid b -> Invalid (invalid_fn b)

let fold valid_fn invalid_fn = function
  | Valid a -> valid_fn a
  | Invalid b -> invalid_fn b

let to_option = function Valid a -> Some a | Invalid _ -> None
let to_result = function Valid a -> Ok a | Invalid b -> Error b
