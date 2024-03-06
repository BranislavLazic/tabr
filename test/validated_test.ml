open Alcotest
open Tabr.Validated

let test_validated_valid () =
  let result = Valid 5 in
  match result with
  | Valid v -> Alcotest.(check @@ int) "same values" 5 v
  | _ -> Alcotest.fail "Bad value"

let test_validated_invalid () =
  let result = Invalid "Error" in
  match result with
  | Invalid v -> Alcotest.(check @@ string) "same values" "Error" v
  | _ -> Alcotest.fail "Bad value"

let test_validated_is_valid () =
  let result = Valid 5 in
  Alcotest.(check bool) "same values" true (is_valid result)

let test_validated_is_invalid () =
  let result = Invalid "Error" in
  Alcotest.(check bool) "same values" true (is_invalid result)

let test_validated_map () =
  let result = Valid 5 |> map (fun v -> v + 1) in
  match result with
  | Valid v -> Alcotest.(check @@ int) "same values" 6 v
  | _ -> Alcotest.fail "Bad value"

let test_validated_bimap_valid () =
  let result = Valid 5 |> bimap (fun v -> v + 1) (fun v -> v + 1) in
  match result with
  | Valid v -> Alcotest.(check @@ int) "same values" 6 v
  | _ -> Alcotest.fail "Bad value"

let test_validated_bimap_invalid () =
  let result = Invalid 2 |> bimap (fun v -> v + 1) (fun v -> v + 4) in
  match result with
  | Invalid v -> Alcotest.(check @@ int) "same values" 6 v
  | _ -> Alcotest.fail "Bad value"

let () =
  run "Validated"
    [
      ("valid", [ test_case "should be valid" `Quick test_validated_valid ]);
      ( "invalid",
        [ test_case "should be invalid" `Quick test_validated_invalid ] );
      ("is_valid", [ test_case "should be true" `Quick test_validated_is_valid ]);
      ( "is_invalid",
        [ test_case "should be true" `Quick test_validated_is_invalid ] );
      ( "map",
        [ test_case "should map a value on Valid" `Quick test_validated_map ] );
      ( "bimap",
        [
          test_case "should map a value on Valid" `Quick
            test_validated_bimap_valid;
          test_case "should map a value on Invalid" `Quick
            test_validated_bimap_invalid;
        ] );
    ]
