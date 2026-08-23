open! Core
open Fixed_list

let%expect_test "basic list operations" =
  let empty = [] in
  let l1 = 1 :: empty in
  let l2 = 2 :: l1 in
  let l3 = 3 :: l2 in
  print_s [%sexp (to_list l3 : int list)];
  [%expect {| (3 2 1) |}];
  print_s [%sexp (length l3 : int)];
  [%expect {| 3 |}];
  print_s [%sexp (hd l3 : int)];
  [%expect {| 3 |}];
  print_s [%sexp (to_list (tl l3) : int list)];
  [%expect {| (2 1) |}]
;;

let%expect_test "init" =
  let (T fixed) = init 3 ~f:Fn.id in
  print_s [%sexp (to_list fixed : int list)];
  [%expect {| (0 1 2) |}]
;;

let%expect_test "conversion functions" =
  let (T fixed) = of_list [ 1; 2; 3 ] in
  print_s [%sexp (to_list fixed : int list)];
  [%expect {| (1 2 3) |}]
;;

let%expect_test "map operations" =
  let l = [ 1; 2; 3 ] in
  let doubled = map l ~f:(fun x -> x * 2) in
  print_s [%sexp (to_list doubled : int list)];
  [%expect {| (2 4 6) |}];
  let indexed = mapi l ~f:(fun i x -> i + x) in
  print_s [%sexp (to_list indexed : int list)];
  [%expect {| (1 3 5) |}]
;;

let%expect_test "rev" =
  let l = [ 1; 2; 3; 5; 10 ] in
  let reversed = rev l in
  print_s [%sexp (to_list reversed : int list)];
  [%expect {| (10 5 3 2 1) |}]
;;

let%expect_test "map2" =
  let l1 = [ 1; 2 ] in
  let l2 = [ 10; 20 ] in
  let sums = map2 l1 l2 ~f:( + ) in
  print_s [%sexp (to_list sums : int list)];
  [%expect {| (11 22) |}]
;;

let%expect_test "map3" =
  let l1 = [ 1; 2 ] in
  let l2 = [ 10; 20 ] in
  let l3 = [ 100; 200 ] in
  let sums = map3 l1 l2 l3 ~f:(fun a b c -> a + b + c) in
  print_s [%sexp (to_list sums : int list)];
  [%expect {| (111 222) |}]
;;

let%expect_test "iter2" =
  let l1 = [ 1; 2 ] in
  let l2 = [ 10; 20 ] in
  let pairs = ref ([] : _ List.t) in
  iter2 l1 l2 ~f:(fun a b -> pairs := (a, b) :: !pairs);
  print_s [%sexp (List.rev !pairs : (int * int) list)];
  [%expect {| ((1 10) (2 20)) |}]
;;

let%expect_test "fold2" =
  let l1 = [ 1; 2 ] in
  let l2 = [ 10; 20 ] in
  let sum = fold2 l1 l2 ~init:0 ~f:(fun acc a b -> acc + a + b) in
  print_s [%sexp (sum : int)];
  [%expect {| 33 |}]
;;

let%expect_test "Rev.map" =
  let (T fixed) = init 5 ~f:Fn.id in
  let result = Rev.map fixed ~f:(fun x -> x * 2) in
  print_s [%sexp (result : (int, _) t)];
  [%expect {| (8 6 4 2 0) |}]
;;

let%expect_test "Rev.mapi" =
  let (Fixed_list.T fixed) = Fixed_list.init 5 ~f:Fn.id in
  let result = Fixed_list.Rev.mapi fixed ~f:(fun i x -> i + x) in
  print_s [%sexp (Fixed_list.to_list result : int list)];
  [%expect {| (8 6 4 2 0) |}]
;;

let%expect_test "transpose" =
  let row1 = [ 1; 2; 3 ] in
  let row2 = [ 4; 5; 6 ] in
  let matrix = [ row1; row2 ] in
  let result = transpose matrix in
  print_s [%sexp (result : ((int, _) t, _) t)];
  [%expect {| ((1 4) (2 5) (3 6)) |}]
;;

let%expect_test "sort" =
  let fixed = [ 3; 1; 4; 1; 5; 9; 2; 6 ] in
  let result = Fixed_list.sort fixed ~compare:Int.compare in
  print_s [%sexp (result : (int, _) t)];
  [%expect {| (1 1 2 3 4 5 6 9) |}]
;;

let%expect_test "last" =
  let l = [ 1; 2; 3; 4; 5 ] in
  print_s [%sexp (last l : int)];
  [%expect {| 5 |}]
;;

let%expect_test "sequence conversion" =
  let seq = Sequence.range 0 5 in
  let (T fixed) = of_sequence seq in
  print_s [%sexp (to_list fixed : int list)];
  [%expect {| (0 1 2 3 4) |}];
  let seq_back = to_sequence fixed in
  print_s [%sexp (Sequence.to_list seq_back : int list)];
  [%expect {| (0 1 2 3 4) |}]
;;

let%expect_test "unzip" =
  let pairs = [ 1, "one"; 2, "two"; 3, "three" ] in
  let nums, strings = unzip pairs in
  print_s [%sexp ((to_list nums, to_list strings) : int list * string list)];
  [%expect {| ((1 2 3) (one two three)) |}]
;;

let%expect_test "append" =
  let (xs : (int, n2) t) = [ 1; 2 ] in
  let (ys : (int, n3) t) = [ 3; 4; 5 ] in
  let result = append xs ys ~proof:(Sums.left3 ()) in
  print_s [%sexp (to_list result : int list)];
  [%expect {| (1 2 3 4 5) |}]
;;

let%expect_test "map2_uneven" =
  let shorter = [ 1; 2 ] in
  let longer = [ 10; 20; 30 ] in
  let mapped, remaining = map2_uneven ~shorter ~longer ~proof:(Sums.left1 ()) ~f:( + ) in
  print_s [%sexp ((to_list mapped, to_list remaining) : int list * int list)];
  [%expect {| ((11 22) (30)) |}]
;;

let%expect_test "partition_mapi" =
  let (T fixed) = init 5 ~f:Fn.id in
  let (Partition.T { first; second; first_sum = _; second_sum = _ }) =
    partition_mapi fixed ~f:(fun i x -> if i mod 2 = 0 then First x else Second (x * 10))
  in
  print_s [%sexp ((to_list first, to_list second) : int list * int list)];
  [%expect {| ((0 2 4) (10 30)) |}]
;;

let%expect_test "partition_mapi should provide indices in correct order" =
  let (T fixed) = init 5 ~f:Fn.id in
  let (Partition.T { first; second; first_sum = _; second_sum = _ }) =
    partition_mapi fixed ~f:(fun i x -> if i < 2 then First x else Second (x * 10))
  in
  print_s [%sexp ((to_list first, to_list second) : int list * int list)];
  [%expect {| ((0 1) (20 30 40)) |}]
;;

let%expect_test "compare_lengths" =
  let l1 = [ 1; 2 ] in
  let l2 = [ 'a'; 'b' ] in
  let l3 = [ true; false; true ] in
  print_s [%sexp (compare_lengths l1 l2 : (_, _) Length_ordering.t)];
  [%expect {| (Equal <opaque>) |}];
  print_s [%sexp (compare_lengths l1 l3 : (_, _) Length_ordering.t)];
  [%expect {| (Less <opaque>) |}];
  print_s [%sexp (compare_lengths l3 l1 : (_, _) Length_ordering.t)];
  [%expect {| (Greater <opaque>) |}]
;;

let%expect_test "take and drop" =
  let longer = [ 1; 2; 3; 4; 5 ] in
  let template = [ 'a'; 'b' ] in
  let taken = take longer ~template ~proof:(Sums.left3 ()) in
  let dropped = drop longer ~template ~proof:(Sums.left3 ()) in
  print_s [%sexp ((to_list taken, to_list dropped) : int list * int list)];
  [%expect {| ((1 2) (3 4 5)) |}]
;;

let%expect_test "Rev.append" =
  let xs = [ 1; 2 ] in
  let ys = [ 3; 4; 5 ] in
  let result = Rev.append xs ys ~proof:(Sums.left3 ()) in
  print_s [%sexp (to_list result : int list)];
  [%expect {| (2 1 3 4 5) |}]
;;

let%expect_test "Rev.partition_mapi" =
  let (T fixed) = init 5 ~f:Fn.id in
  let (Partition.T { first; second; first_sum = _; second_sum = _ }) =
    Rev.partition_mapi fixed ~f:(fun i x ->
      if i mod 2 = 0 then First x else Second (x * 10))
  in
  print_s [%sexp ((to_list first, to_list second) : int list * int list)];
  [%expect {| ((4 2 0) (30 10)) |}]
;;

let%expect_test "Rev.partition_mapi should provide indices in correct order" =
  let (T fixed) = init 5 ~f:Fn.id in
  let (Partition.T { first; second; first_sum = _; second_sum = _ }) =
    Rev.partition_mapi fixed ~f:(fun i x -> if i < 2 then First x else Second (x * 10))
  in
  print_s [%sexp ((to_list first, to_list second) : int list * int list)];
  [%expect {| ((1 0) (40 30 20)) |}]
;;

let%expect_test "sequential vs parallel monad operations" =
  let module Sequential = Monad_sequence.Sequential.Make (Or_error) in
  let module Parallel = Monad_sequence.Parallel.Make (Or_error) in
  let l = [ 1; 2; 3 ] in
  (* Test successful case *)
  let f x = Or_error.return (x * 10) in
  let sequential = Sequential.map l ~f in
  let parallel = Parallel.map l ~f in
  print_s [%sexp ((sequential, parallel) : (int, _) t Or_error.t * (int, _) t Or_error.t)];
  [%expect {| ((Ok (10 20 30)) (Ok (10 20 30))) |}];
  (* Test multiple errors - sequential should report first error, parallel should report
     all *)
  let f x =
    match x with
    | 2 -> Or_error.return (x * 10)
    | i -> Or_error.error_s [%message "not 2" (i : int)]
  in
  let sequential = Sequential.map l ~f in
  let parallel = Parallel.map l ~f in
  print_s [%sexp ((sequential, parallel) : (int, _) t Or_error.t * (int, _) t Or_error.t)];
  [%expect {| ((Error ("not 2" (i 1))) (Error (("not 2" (i 1)) ("not 2" (i 3))))) |}]
;;

let%expect_test "sequential vs parallel monad operations (two-parameter)" =
  (* Like Or_error but with a phantom type parameter *)
  let module Phantom_or_error = struct
    type ('a, 'e) t = 'a Or_error.t [@@deriving sexp_of]

    let return x = Or_error.return x
    let bind t ~f = Or_error.bind t ~f
    let map2 t1 t2 ~f = Or_error.map2 t1 t2 ~f
  end
  in
  let module Sequential = Monad_sequence.Sequential.Make2 (Phantom_or_error) in
  let module Parallel = Monad_sequence.Parallel.Make2 (Phantom_or_error) in
  let l = [ 1; 2; 3 ] in
  (* Test successful case *)
  let f x = Or_error.return (x * 10) in
  let sequential = Sequential.map l ~f in
  let parallel = Parallel.map l ~f in
  print_s
    [%sexp
      ((sequential, parallel)
       : ((int, _) t, unit) Phantom_or_error.t * ((int, _) t, unit) Phantom_or_error.t)];
  [%expect {| ((Ok (10 20 30)) (Ok (10 20 30))) |}];
  (* Test multiple errors - sequential should report first error, parallel should report
     all *)
  let f x =
    match x with
    | 2 -> Or_error.return (x * 10)
    | i -> Or_error.error_s [%message "not 2" (i : int)]
  in
  let sequential = Sequential.map l ~f in
  let parallel = Parallel.map l ~f in
  print_s
    [%sexp
      ((sequential, parallel)
       : ((int, _) t, unit) Phantom_or_error.t * ((int, _) t, unit) Phantom_or_error.t)];
  [%expect {| ((Error ("not 2" (i 1))) (Error (("not 2" (i 1)) ("not 2" (i 3))))) |}]
;;

let%expect_test "distribute_monad with Option" =
  let module Distribute = Monad_sequence.Distribute.Make (Option) in
  (* Test Some case: Some list gets distributed to list of Some values *)
  let template = [ (); (); () ] in
  let input = Some [ 1; 2; 3 ] in
  let result = Distribute.distribute_monad input ~template in
  print_s [%sexp (to_list result : int option list)];
  [%expect {| ((1) (2) (3)) |}];
  (* Test None case: None gets distributed to list of None values *)
  let input_none = None in
  let result_none = Distribute.distribute_monad input_none ~template in
  print_s [%sexp (to_list result_none : int option list)];
  [%expect {| (() () ()) |}]
;;

let%expect_test "distribute_monad with Or_error" =
  let module Distribute = Monad_sequence.Distribute.Make (Or_error) in
  let template = [ 1; 2; 3 ] in
  (* Test successful case *)
  let input_ok = Or_error.return [ "a"; "b"; "c" ] in
  let result_ok = Distribute.distribute_monad input_ok ~template in
  print_s [%sexp (to_list result_ok : string Or_error.t list)];
  [%expect {| ((Ok a) (Ok b) (Ok c)) |}];
  (* Test error case *)
  let input_error = Or_error.error_string "we're distributing the error" in
  let result_error = Distribute.distribute_monad input_error ~template in
  print_s [%sexp (to_list result_error : string Or_error.t list)];
  [%expect
    {|
    ((Error "we're distributing the error")
     (Error "we're distributing the error")
     (Error "we're distributing the error"))
    |}]
;;

let%expect_test "sort stability" =
  let items = [ "a", 1; "b", 1; "c", 2; "d", 2; "e", 1; "f", 1; "g", 1; "h", 2 ] in
  let result = sort items ~compare:(fun (_, x) (_, y) -> Int.compare x y) in
  print_s [%sexp (to_list result : (string * int) list)];
  [%expect {| ((a 1) (b 1) (e 1) (f 1) (g 1) (c 2) (d 2) (h 2)) |}]
;;

let%expect_test "zip" =
  let l1 = [ 1; 2; 3 ] in
  let l2 = [ "one"; "two"; "three" ] in
  let result = Fixed_list.zip l1 l2 in
  print_s [%sexp (result : (int * string, _) t)];
  [%expect {| ((1 one) (2 two) (3 three)) |}]
;;

let%expect_test "reduce" =
  let fixed = [ 1; 2; 3; 4 ] in
  let sum = Fixed_list.reduce fixed ~f:( + ) in
  let max = Fixed_list.reduce fixed ~f:Int.max in
  print_s [%sexp (sum : int)];
  [%expect {| 10 |}];
  print_s [%sexp (max : int)];
  [%expect {| 4 |}]
;;

let%expect_test "fold_map" =
  let fixed = [ 1; 2; 3 ] in
  let sum, doubled =
    Fixed_list.fold_map fixed ~init:0 ~f:(fun acc x ->
      let doubled = x * 2 in
      acc + x, doubled)
  in
  print_s [%sexp ((sum, doubled) : int * (int, _) t)];
  [%expect {| (6 (2 4 6)) |}]
;;

let%expect_test "fold_mapi" =
  let (Fixed_list.T fixed) = Fixed_list.of_list [ 10; 20; 30 ] in
  let sum, with_indices =
    Fixed_list.fold_mapi fixed ~init:0 ~f:(fun i acc x ->
      let result = x + i in
      acc + x, result)
  in
  print_s [%sexp ((sum, Fixed_list.to_list with_indices) : int * int list)];
  [%expect {| (60 (10 21 32)) |}]
;;

let%expect_test "nonempty list conversion" =
  let (nl : _ Nonempty_list.t) = [ 1; 2; 3 ] in
  let (Fixed_list.T_succ fixed) = Fixed_list.of_nonempty nl in
  print_s [%sexp (fixed : (int, _) t)];
  [%expect {| (1 2 3) |}];
  let nl_back = Fixed_list.to_nonempty fixed in
  print_s [%sexp (nl_back : int Nonempty_list.t)];
  [%expect {| (1 2 3) |}]
;;
