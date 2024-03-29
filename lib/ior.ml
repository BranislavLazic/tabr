type ('a, 'b) t = Left of 'a | Right of 'b | Both of 'a * 'b

let right a = Right a
let left a = Left a
let both a b = Both (a, b)

let fold left_fn right_fn both_fn = function
  | Left a -> left_fn a
  | Right a -> right_fn a
  | Both (a, b) -> both_fn a b

let map fn = function
  | Left a -> Left a
  | Right a -> Right (fn a)
  | Both (a, b) -> Both (a, fn b)

let bimap left_fn right_fn ior =
  fold
    (fun a -> Left (left_fn a))
    (fun a -> Right (right_fn a))
    (fun a b -> Both (left_fn a, right_fn b))
    ior

let left_map fn = function
  | Left a -> Left (fn a)
  | Right a -> Right a
  | Both (a, b) -> Both (fn a, b)

let swap ior =
  fold (fun a -> Right a) (fun a -> Left a) (fun a b -> Both (b, a)) ior

let to_option = function
  | Left _ -> None
  | Right a -> Some a
  | Both (_, a) -> Some a

let to_result = function
  | Right a -> Ok a
  | Left a -> Error a
  | Both (_, b) -> Ok b

let get_or_else alternate = function
  | Right a -> a
  | Left _ -> alternate
  | Both (_, a) -> a

let is_left = function Left _ -> true | _ -> false
let is_right = function Right _ -> true | _ -> false
let is_both = function Both _ -> true | _ -> false
