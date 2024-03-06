type ('a, 'b) t = Left of 'a | Right of 'b | Both of 'a * 'b

let right a = Right a
let left a = Left a
let both a b = Both (a, b)

let map fn ior =
  match ior with
  | Left a -> Left a
  | Right a -> Right (fn a)
  | Both (a, b) -> Both (a, fn b)

let left_map fn ior =
  match ior with
  | Left a -> Left (fn a)
  | Right a -> Right a
  | Both (a, b) -> Both (fn a, b)

let swap ior =
  match ior with
  | Left a -> Right a
  | Right a -> Left a
  | Both (a, b) -> Both (b, a)

let fold left_fn right_fn both_fn ior =
  match ior with
  | Left a -> left_fn a
  | Right a -> right_fn a
  | Both (a, b) -> both_fn a b

let to_option ior =
  match ior with Left _ -> None | Right a -> Some a | Both (_, a) -> Some a

let to_result ior =
  match ior with Right a -> Ok a | Left a -> Error a | Both (_, b) -> Ok b

let get_or_else alternate ior =
  match ior with Right a -> a | Left _ -> alternate | Both (_, a) -> a

let is_left ior = match ior with Left _ -> true | _ -> false
let is_right ior = match ior with Right _ -> true | _ -> false
let is_both ior = match ior with Both _ -> true | _ -> false
