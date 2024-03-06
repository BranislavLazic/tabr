open Alcotest
open Tabr

let test_right () =
  let result = Ior.right "Hi" in
  match result with
  | Right v -> Alcotest.(check @@ string) "same values" "Hi" v
  | _ -> Alcotest.fail "Bad value"

let test_left () =
  let result = Ior.left "Hi" in
  match result with
  | Left v -> Alcotest.(check @@ string) "same values" "Hi" v
  | _ -> Alcotest.fail "Bad value"

let test_both () =
  let result = Ior.both "Hello" "world" in
  match result with
  | Both (l, r) ->
      Alcotest.(check @@ string) "same values" "Hello" l;
      Alcotest.(check @@ string) "same values" "world" r
  | _ -> Alcotest.fail "Bad values"

let test_map_on_right () =
  let result = Ior.right 5 |> Ior.map (fun v -> v * 2) in
  match result with
  | Right v -> Alcotest.(check @@ int) "same values" 10 v
  | _ -> Alcotest.fail "Bad values"

let test_map_on_both () =
  let result = Ior.both "Hello" 5 |> Ior.map (fun v -> v * 2) in
  match result with
  | Both (l, r) ->
      Alcotest.(check @@ int) "same values" 10 r;
      Alcotest.(check @@ string) "same values" "Hello" l
  | _ -> Alcotest.fail "Bad values"

let test_left_map () =
  let result = Ior.left 5 |> Ior.left_map (fun v -> v * 2) in
  match result with
  | Left v -> Alcotest.(check @@ int) "same values" 10 v
  | _ -> Alcotest.fail "Bad values"

let test_swap_right_to_left () =
  let result = Ior.right "Hello" |> Ior.swap in
  match result with
  | Left v -> Alcotest.(check @@ string) "same values" "Hello" v
  | _ -> Alcotest.fail "Bad values"

let test_swap_left_to_right () =
  let result = Ior.left "Hello" |> Ior.swap in
  match result with
  | Right v -> Alcotest.(check @@ string) "same values" "Hello" v
  | _ -> Alcotest.fail "Bad values"

let test_swap_both () =
  let result = Ior.both "Hello" 1 |> Ior.swap in
  match result with
  | Both (l, r) ->
      Alcotest.(check @@ string) "same values" "Hello" r;
      Alcotest.(check @@ int) "same values" 1 l
  | _ -> Alcotest.fail "Bad values"

let test_fold_on_right () =
  let result =
    Ior.right 1
    |> Ior.fold
         (fun l -> Ior.Left l)
         (fun r -> Ior.Right (r + 1))
         (fun l r -> Ior.Both (l, r + 1))
  in
  match result with
  | Right r -> Alcotest.(check @@ int) "same values" 2 r
  | _ -> Alcotest.fail "Bad values"

let test_fold_on_left () =
  let result =
    Ior.left 1
    |> Ior.fold
         (fun l -> Ior.Left (l + 1))
         (fun r -> Ior.Right r)
         (fun l r -> Ior.Both (l + 1, r))
  in
  match result with
  | Left l -> Alcotest.(check @@ int) "same values" 2 l
  | _ -> Alcotest.fail "Bad values"

let test_fold_on_both () =
  let result =
    Ior.both 1 "Hello"
    |> Ior.fold
         (fun l -> Ior.Left l)
         (fun r -> Ior.Right r)
         (fun l r -> Ior.Both (l + 1, r))
  in
  match result with
  | Both (l, r) ->
      Alcotest.(check @@ int) "same values" 2 l;
      Alcotest.(check @@ string) "same values" "Hello" r
  | _ -> Alcotest.fail "Bad values"

let test_to_option_right () =
  let result = Ior.right 5 |> Ior.to_option in
  match result with
  | Some v -> Alcotest.(check @@ int) "same values" 5 v
  | _ -> Alcotest.fail "Bad values"

let test_to_option_left () =
  let result = Ior.left 5 |> Ior.to_option in
  match result with
  | None -> Alcotest.(check pass) "Absent value" () ()
  | _ -> Alcotest.fail "Bad values"

let test_to_option_both () =
  let result = Ior.both 5 "Hello" |> Ior.to_option in
  match result with
  | Some v -> Alcotest.(check @@ string) "same values" "Hello" v
  | _ -> Alcotest.fail "Bad values"

let test_to_result_right () =
  let result = Ior.right 5 |> Ior.to_result in
  match result with
  | Ok v -> Alcotest.(check @@ int) "same values" 5 v
  | _ -> Alcotest.fail "Bad values"

let test_to_result_left () =
  let result = Ior.left 5 |> Ior.to_result in
  match result with
  | Error v -> Alcotest.(check @@ int) "same values" 5 v
  | _ -> Alcotest.fail "Bad values"

let test_to_result_both () =
  let result = Ior.both 5 "Hello" |> Ior.to_result in
  match result with
  | Ok v -> Alcotest.(check @@ string) "same values" "Hello" v
  | _ -> Alcotest.fail "Bad values"

let test_get_or_else_right () =
  let result = Ior.right "Hello" |> Ior.get_or_else "World" in
  Alcotest.(check @@ string) "same values" "Hello" result

let test_get_or_else_left () =
  let result = Ior.left "Hello" |> Ior.get_or_else "World" in
  Alcotest.(check @@ string) "same values" "World" result

let test_get_or_else_both () =
  let result = Ior.both 1 "Hello" |> Ior.get_or_else "World" in
  Alcotest.(check @@ string) "same values" "Hello" result

let test_is_right () =
  let result = Ior.right 1 |> Ior.is_right in
  Alcotest.(check @@ bool) "same values" true result

let test_is_left () =
  let result = Ior.left 1 |> Ior.is_left in
  Alcotest.(check @@ bool) "same values" true result

let test_is_both () =
  let result = Ior.both 1 2 |> Ior.is_both in
  Alcotest.(check @@ bool) "same values" true result

let () =
  run "Ior"
    [
      ("right", [ test_case "should return Ior as Right" `Quick test_right ]);
      ("left", [ test_case "should return Ior as Left" `Quick test_left ]);
      ("both", [ test_case "should return Ior as Both" `Quick test_both ]);
      ( "map",
        [
          test_case "should map a value on Right" `Quick test_map_on_right;
          test_case "should map a value on Both" `Quick test_map_on_both;
        ] );
      ( "left_map",
        [ test_case "should map a value on Left" `Quick test_left_map ] );
      ( "swap",
        [
          test_case "should swap Right to Left" `Quick test_swap_right_to_left;
          test_case "should swap Left to Right" `Quick test_swap_left_to_right;
          test_case "should swap Both" `Quick test_swap_both;
        ] );
      ( "fold",
        [
          test_case "should fold on Right" `Quick test_fold_on_right;
          test_case "should fold on Left" `Quick test_fold_on_left;
          test_case "should fold on Both" `Quick test_fold_on_both;
        ] );
      ( "to_option",
        [
          test_case "should return Some for Right" `Quick test_to_option_right;
          test_case "should return None for Left" `Quick test_to_option_left;
          test_case "should return Some for Both" `Quick test_to_option_both;
        ] );
      ( "to_result",
        [
          test_case "should return Ok for Right" `Quick test_to_result_right;
          test_case "should return Error for Left" `Quick test_to_result_left;
          test_case "should return Ok for Both" `Quick test_to_result_both;
        ] );
      ( "get_or_else",
        [
          test_case "should return a Right value" `Quick test_get_or_else_right;
          test_case "should return an alternate value for Left" `Quick
            test_get_or_else_left;
          test_case "should return a value for Both" `Quick
            test_get_or_else_both;
        ] );
      ( "is_right",
        [ test_case "should return true for Right" `Quick test_is_right ] );
      ( "is_left",
        [ test_case "should return true for Left" `Quick test_is_left ] );
      ( "is_both",
        [ test_case "should return true for Both" `Quick test_is_both ] );
    ]
