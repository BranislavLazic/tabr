open Alcotest

let test_get_or_else () =
  let result = Tabr.Option.get_or_else "Hi" (Some "Hello") in
  Alcotest.(check @@ string) "same values" "Hello" result

let test_get_or_else_alternate () =
  let result = Tabr.Option.get_or_else "Hi" None in
  Alcotest.(check @@ string) "same values" "Hi" result

let test_or_else () =
  let result = Tabr.Option.or_else (Some "Hi") (Some "Hello") in
  Alcotest.(check @@ option string) "same values" (Some "Hello") result

let test_or_else_alternate () =
  let result = Tabr.Option.or_else (Some "Hi") None in
  Alcotest.(check @@ option string) "same values" (Some "Hi") result

let test_filter () =
  let result = Tabr.Option.filter (fun v -> v > 122) (Some 123) in
  Alcotest.(check @@ option int) "same values" (Some 123) result

let test_filter_condition_not_satisfied () =
  let result = Tabr.Option.filter (fun v -> v > 123) (Some 123) in
  Alcotest.(check @@ option int) "same values" None result

let test_filter_absent () =
  let result = Tabr.Option.filter (fun v -> v > 122) None in
  Alcotest.(check @@ option int) "same values" None result

let test_flatten () =
  let result = Tabr.Option.flatten (Some (Some "Hello")) in
  Alcotest.(check @@ option string) "same values" (Some "Hello") result

let test_flatten_none () =
  let result = Tabr.Option.flatten None in
  Alcotest.(check @@ option string) "same values" None result

let test_contains () =
  let result = Tabr.Option.contains "Hello" (Some "Hello") in
  Alcotest.(check @@ bool) "same values" true result

let test_contains_does_not_contain () =
  let result = Tabr.Option.contains "Hi" (Some "Hello") in
  Alcotest.(check @@ bool) "same values" false result

let test_zip () =
  let result = Tabr.Option.zip (Some "Hello") (Some "world") in
  match result with
  | Some (l, r) ->
      Alcotest.(check @@ string) "same values" "Hello" l;
      Alcotest.(check @@ string) "same values" "world" r
  | _ -> Alcotest.fail "Bad values"

let test_tap () =
  let result = Tabr.Option.tap (fun v -> print_endline v) (Some "Hello") in
  Alcotest.(check @@ option string) "same values" (Some "Hello") result

let test_tap_none () =
  let result =
    Tabr.Option.tap_none (fun () -> print_endline "It's none") None
  in
  Alcotest.(check @@ option string) "same values" None result

let () =
  run "Option"
    [
      ( "get_or_else",
        [
          test_case "should return a value when present" `Quick test_get_or_else;
          test_case "should return an alternate value when absent" `Quick
            test_get_or_else_alternate;
        ] );
      ( "or_else",
        [
          test_case "should return an original option when present" `Quick
            test_or_else;
          test_case "should return an alternate option when absent" `Quick
            test_or_else_alternate;
        ] );
      ( "filter",
        [
          test_case
            "should filter a value when present and condition is satisfied"
            `Quick test_filter;
          test_case
            "should filter a value when present and condition is not satisfied"
            `Quick test_filter_condition_not_satisfied;
          test_case
            "should filter a value when absent and condition is satisfied"
            `Quick test_filter_absent;
        ] );
      ( "flatten",
        [
          test_case "should flatten some value" `Quick test_flatten;
          test_case "should flatten none value" `Quick test_flatten_none;
        ] );
      ( "contains",
        [
          test_case
            "should return true when the option contains a required value"
            `Quick test_contains;
          test_case
            "should return false when the option does not contains a required \
             value"
            `Quick test_contains_does_not_contain;
        ] );
      ( "tap",
        [
          test_case "should not change the value when present" `Quick test_tap;
          test_case "should not change the value when absent" `Quick
            test_tap_none;
        ] );
      ("zip", [ test_case "should combine two options" `Quick test_zip ]);
    ]
