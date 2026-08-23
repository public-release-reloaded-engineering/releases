open! Core

let%expect_test "sort behaves the same as List.sort and maintains stability" =
  let%quick_test prop (items : (int * string) list) =
    let list_result = List.sort items ~compare:(fun (x, _) (y, _) -> Int.compare x y) in
    let (Fixed_list.T fixed_result) = Fixed_list.of_list items in
    let fixed_result =
      Fixed_list.sort fixed_result ~compare:(fun (x, _) (y, _) -> Int.compare x y)
    in
    [%test_result: (int * string) list]
      (Fixed_list.to_list fixed_result)
      ~expect:list_result
  in
  ()
;;

let%expect_test "map behaves the same as List.map" =
  let%quick_test prop (items : int list) =
    let list_result = List.map items ~f:(fun x -> x * 2) in
    let (Fixed_list.T fixed_result) = Fixed_list.of_list items in
    let fixed_result = Fixed_list.map fixed_result ~f:(fun x -> x * 2) in
    [%test_result: int list] (Fixed_list.to_list fixed_result) ~expect:list_result
  in
  ()
;;

let%expect_test "mapi behaves the same as List.mapi" =
  let%quick_test prop (items : int list) =
    let list_result = List.mapi items ~f:(fun i x -> i + x) in
    let (Fixed_list.T fixed_result) = Fixed_list.of_list items in
    let fixed_result = Fixed_list.mapi fixed_result ~f:(fun i x -> i + x) in
    [%test_result: int list] (Fixed_list.to_list fixed_result) ~expect:list_result
  in
  ()
;;

let%expect_test "partition_mapi behaves the same as List.partition_mapi" =
  let%quick_test prop (items : int list) =
    let f i x : (int, int) Either.t = if i mod 2 = 0 then First x else Second (x * 10) in
    let list_result = List.partition_mapi items ~f in
    let (Fixed_list.T fixed_result) = Fixed_list.of_list items in
    let%tydi (T
               { first = first_fixed_result
               ; second = second_fixed_result
               ; first_sum = _
               ; second_sum = _
               })
      =
      Fixed_list.partition_mapi fixed_result ~f
    in
    [%test_result: int list * int list]
      (Fixed_list.to_list first_fixed_result, Fixed_list.to_list second_fixed_result)
      ~expect:list_result
  in
  ()
;;

let%expect_test "rev behaves the same as List.rev" =
  let%quick_test prop (items : int list) =
    let list_result = List.rev items in
    let (Fixed_list.T fixed_result) = Fixed_list.of_list items in
    let fixed_result = Fixed_list.rev fixed_result in
    [%test_result: int list] (Fixed_list.to_list fixed_result) ~expect:list_result
  in
  ()
;;

let%expect_test "unzip behaves the same as List.unzip" =
  let%quick_test prop (items : (int * string) list) =
    let list_result = List.unzip items in
    let (Fixed_list.T fixed) = Fixed_list.of_list items in
    let fixed_result = Fixed_list.unzip fixed in
    [%test_result: int list * string list]
      (Fixed_list.to_list (fst fixed_result), Fixed_list.to_list (snd fixed_result))
      ~expect:list_result
  in
  ()
;;

let%expect_test "compare matches List.compare" =
  let%quick_test prop (ls : (int * int) list) =
    let xs, ys = List.unzip ls in
    let (Fixed_list.T fixed) = Fixed_list.of_list ls in
    let fixed_xs, fixed_ys = Fixed_list.unzip fixed in
    [%test_result: int]
      ([%compare: (int, _) Fixed_list.t] fixed_xs fixed_ys)
      ~expect:([%compare: int list] xs ys)
  in
  ()
;;
