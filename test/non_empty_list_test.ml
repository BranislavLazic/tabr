open Alcotest
open Tabr
open Tabr.Non_empty_list

let test_non_empty_list () =
  let result : int Non_empty_list.non_empty_list = (1, [ 2; 3 ]) in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3 ]

let test_non_empty_list_single_element () =
  let result : int Non_empty_list.non_empty_list = Non_empty_list.one 99 in
  match result with head, _ -> Alcotest.(check @@ int) "same values" head 99

let test_non_empty_list_from_list_present () =
  let result : int Non_empty_list.non_empty_list option =
    Tabr.Non_empty_list.from_list [ 1; 2; 3 ]
  in
  match result with
  | Some (head, rest) ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3 ]
  | None -> Alcotest.fail "Bad values"

let test_non_empty_list_from_list_absent () =
  let result : int Non_empty_list.non_empty_list option =
    Tabr.Non_empty_list.from_list []
  in
  match result with
  | None -> Alcotest.(check pass) "Absent value" () ()
  | _ -> Alcotest.fail "Bad values"

let test_non_empty_list_map () =
  let result : int Non_empty_list.non_empty_list =
    (1, [ 2; 3 ]) |> Non_empty_list.map (fun v -> v * v)
  in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 4; 9 ]

let test_non_empty_list_filter () =
  let result = Non_empty_list.filter (fun v -> v != 2) (1, [ 2; 3 ]) in
  match result with
  | Some (head, rest) ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 3 ]
  | _ -> Alcotest.fail "Bad values"

let test_non_empty_list_exists () =
  let result = Non_empty_list.exists (fun v -> v == 2) (1, [ 2; 3 ]) in
  Alcotest.(check @@ bool) "same values" true result

let test_non_empty_list_exists_not_satisfied () =
  let result = Non_empty_list.exists (fun v -> v == 4) (1, [ 2; 3 ]) in
  Alcotest.(check @@ bool) "same values" false result

let test_non_empty_list_for_all () =
  let result = Non_empty_list.for_all (fun v -> v == 2) (2, [ 2; 2 ]) in
  Alcotest.(check @@ bool) "same values" true result

let test_non_empty_list_reverse () =
  let nel = (1, [ 2; 3 ]) in
  let result = Non_empty_list.reverse nel in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 3;
      Alcotest.(check @@ list int) "same values" rest [ 2; 1 ]

let test_non_empty_list_for_all_not_satisfied () =
  let result = Non_empty_list.for_all (fun v -> v == 2) (1, [ 2; 3 ]) in
  Alcotest.(check @@ bool) "same values" false result

let test_non_empty_list_concat () =
  let left_nel = (1, [ 2; 3 ]) in
  let right_nel = (4, [ 5; 6 ]) in
  let result = left_nel <+> right_nel in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3; 4; 5; 6 ]

let test_non_empty_list_append () =
  let nel : int Non_empty_list.non_empty_list = (1, [ 2; 3 ]) in
  let result : int Non_empty_list.non_empty_list = nel <+ 4 in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3; 4 ]

let test_non_empty_list_prepend () =
  let nel : int Non_empty_list.non_empty_list = (1, [ 2; 3 ]) in
  let result : int Non_empty_list.non_empty_list = 4 +> nel in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 4;
      Alcotest.(check @@ list int) "same values" rest [ 1; 2; 3 ]

let () =
  run "Non empty list"
    [
      ( "init",
        [
          test_case "should init a non-empty list with three elements" `Quick
            test_non_empty_list;
        ] );
      ( "one",
        [
          test_case "should init a non-empty list with a single element" `Quick
            test_non_empty_list_single_element;
        ] );
      ( "from_list",
        [
          test_case
            "should init a non-empty list from a list as an option - present"
            `Quick test_non_empty_list_from_list_present;
          test_case
            "should init a non-empty list from a list as an option - absent"
            `Quick test_non_empty_list_from_list_absent;
        ] );
      ("map", [ test_case "should map values" `Quick test_non_empty_list_map ]);
      ( "filter",
        [ test_case "should filter values" `Quick test_non_empty_list_filter ]
      );
      ( "exists",
        [
          test_case
            "should return true if at least one value satisfies a condition"
            `Quick test_non_empty_list_exists;
          test_case
            "should return false if at least one value does not satisfy a \
             condition"
            `Quick test_non_empty_list_exists_not_satisfied;
        ] );
      ( "for_all",
        [
          test_case "should return true if all values satisfy a condition"
            `Quick test_non_empty_list_for_all;
          test_case
            "should return false if all values do not satisfy a condition"
            `Quick test_non_empty_list_for_all_not_satisfied;
        ] );
      ( "reverse",
        [
          test_case "should reverse a non-empty list" `Quick
            test_non_empty_list_reverse;
        ] );
      ( "<+> - concat",
        [
          test_case "should concatenate non-empty lists" `Quick
            test_non_empty_list_concat;
        ] );
      ( "<+ - append",
        [
          test_case "should append a value to a non-empty list" `Quick
            test_non_empty_list_append;
        ] );
      ( "+> - prepend",
        [
          test_case "should prepend a value to a non-empty list" `Quick
            test_non_empty_list_prepend;
        ] );
    ]
