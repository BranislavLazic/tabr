open Alcotest
open Tabr
open Tabr.Non_empty_list

let test_non_empty_list () =
  let result = (1, [ 2; 3 ]) in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3 ]

let test_non_empty_list_single_element () =
  let result = Non_empty_list.one 99 in
  match result with head, _ -> Alcotest.(check @@ int) "same values" head 99

let test_non_empty_list_from_list_present () =
  let result = Non_empty_list.from_list [ 1; 2; 3 ] in
  match result with
  | Some (head, rest) ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3 ]
  | None -> Alcotest.fail "Bad values"

let test_non_empty_list_from_list_absent () =
  let result = Non_empty_list.from_list [] in
  match result with
  | None -> Alcotest.(check pass) "Absent value" () ()
  | _ -> Alcotest.fail "Bad values"

let test_non_empty_list_map () =
  let result = (1, [ 2; 3 ]) |> Non_empty_list.map (fun v -> v * v) in
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

let test_non_empty_list_find () =
  let result = Non_empty_list.find (fun v -> v == 2) (1, [ 2; 3 ]) in
  Alcotest.(check @@ option int) "same values" (Some 2) result

let test_non_empty_list_find_not_found () =
  let result = Non_empty_list.find (fun v -> v == 4) (1, [ 2; 3 ]) in
  Alcotest.(check @@ option int) "same values" None result

let test_non_empty_list_exists () =
  let result = Non_empty_list.exists (fun v -> v == 2) (1, [ 2; 3 ]) in
  Alcotest.(check @@ bool) "same values" true result

let test_non_empty_list_exists_not_satisfied () =
  let result = Non_empty_list.exists (fun v -> v == 4) (1, [ 2; 3 ]) in
  Alcotest.(check @@ bool) "same values" false result

let test_non_empty_list_for_all () =
  let result = Non_empty_list.for_all (fun v -> v == 2) (2, [ 2; 2 ]) in
  Alcotest.(check @@ bool) "same values" true result

let test_non_empty_list_zip () =
  let result = Non_empty_list.zip (1, [ 2; 3 ]) (4, [ 5; 6 ]) in
  match result with
  | head, rest ->
      Alcotest.(check @@ pair int int) "same values" head (1, 4);
      Alcotest.(check @@ list (pair int int))
        "same values" rest
        [ (2, 5); (3, 6) ]

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
  let nel = (1, [ 2; 3 ]) in
  let result = nel <+ 4 in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 1;
      Alcotest.(check @@ list int) "same values" rest [ 2; 3; 4 ]

let test_non_empty_list_prepend () =
  let nel : int Non_empty_list.t = (1, [ 2; 3 ]) in
  let result : int Non_empty_list.t = 4 +> nel in
  match result with
  | head, rest ->
      Alcotest.(check @@ int) "same values" head 4;
      Alcotest.(check @@ list int) "same values" rest [ 1; 2; 3 ]

let test_non_empty_list_size () =
  let size = (1, [ 2; 3 ]) |> Non_empty_list.size in
  Alcotest.(check @@ int) "same values" size 3

let test_non_empty_list_to_list () =
  let list = (1, [ 2; 3 ]) |> Non_empty_list.to_list in
  Alcotest.(check @@ list int) "same values" list [ 1; 2; 3 ]

let test_non_empty_list_to_option () =
  let option = (1, [ 2; 3 ]) |> Non_empty_list.to_option in
  Alcotest.(check @@ option (pair int (list int)))
    "same values" option
    (Some (1, [ 2; 3 ]))

let test_non_empty_list_result () =
  let result = (1, [ 2; 3 ]) |> Non_empty_list.to_result in
  Alcotest.(check @@ result (pair int (list int)) string)
    "same values" result
    (Ok (1, [ 2; 3 ]))

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
      ( "find",
        [
          test_case "should find a value" `Quick test_non_empty_list_find;
          test_case "should not find a value" `Quick
            test_non_empty_list_find_not_found;
        ] );
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
      ( "zip",
        [
          test_case "should zip two non-empty lists" `Quick
            test_non_empty_list_zip;
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
      ( "size",
        [
          test_case "should return the size of a non-empty list" `Quick
            test_non_empty_list_size;
        ] );
      ( "to_list",
        [
          test_case "should return a list from a non-empty list" `Quick
            test_non_empty_list_to_list;
        ] );
      ( "to_option",
        [
          test_case "should return an option from a non-empty list" `Quick
            test_non_empty_list_to_option;
        ] );
      ( "to_result",
        [
          test_case "should return a result from a non-empty list" `Quick
            test_non_empty_list_result;
        ] );
    ]
