open! Core

let ns = [ 10; 1000; 100_000 ]

(* Conversion benchmarks *)
let%bench_fun ("of_list [Fixed_list]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> Fixed_list.of_list xs
;;

let%bench_fun ("to_list [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.to_list fixed
;;

(* Init benchmarks *)
let%bench_fun ("init [List]" [@indexed n = ns]) = fun () -> List.init n ~f:Fn.id

let%bench_fun ("init [Fixed_list]" [@indexed n = ns]) =
  fun () -> Fixed_list.init n ~f:Fn.id
;;

(* Basic operations benchmarks *)
let%bench_fun ("length [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.length xs
;;

let%bench_fun ("length [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.length fixed
;;

(* Map benchmarks *)
let%bench_fun ("map [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.map xs ~f:(fun x -> x + 1)
;;

let%bench_fun ("map [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.map fixed ~f:(fun x -> x + 1))
;;

(* Mapi benchmarks *)
let%bench_fun ("mapi [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.mapi xs ~f:(fun i x -> i + x)
;;

let%bench_fun ("mapi [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.mapi fixed ~f:(fun i x -> i + x))
;;

(* Rev benchmarks *)
let%bench_fun ("rev [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.rev xs
;;

let%bench_fun ("rev [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.rev fixed)
;;

(* Map2 benchmarks *)
let%bench_fun ("map2 [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.map2_exn xs xs ~f:( + )
;;

let%bench_fun ("map2 [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.map2 fixed fixed ~f:( + ))
;;

(* Map3 benchmarks *)
let add3 a b c = a + b + c

let%bench_fun ("map3 [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.map3_exn xs xs xs ~f:add3
;;

let%bench_fun ("map3 [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.map3 fixed fixed fixed ~f:add3)
;;

(* Iter2 benchmarks *)
let%bench_fun ("iter2 [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  let sum = ref 0 in
  fun () -> List.iter2_exn xs xs ~f:(fun x y -> sum := !sum + x + y)
;;

let%bench_fun ("iter2 [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  let sum = ref 0 in
  fun () -> Fixed_list.iter2 fixed fixed ~f:(fun x y -> sum := !sum + x + y)
;;

(* Fold2 benchmarks *)
let%bench_fun ("fold2 [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.fold2_exn xs xs ~init:0 ~f:(fun acc x y -> acc + x + y)
;;

let%bench_fun ("fold2 [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.fold2 fixed fixed ~init:0 ~f:(fun acc x y -> acc + x + y)
;;

(* Rev_map benchmarks *)
let%bench_fun ("rev_map [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.rev_map xs ~f:(fun x -> x + 1)
;;

let%bench_fun ("rev_map [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.Rev.map fixed ~f:(fun x -> x + 1))
;;

(* Rev_mapi benchmarks *)
let%bench_fun ("rev_mapi [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.rev_mapi xs ~f:(fun i x -> i + x)
;;

let%bench_fun ("rev_mapi [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.T (Fixed_list.Rev.mapi fixed ~f:(fun i x -> i + x))
;;

(* Transpose benchmarks *)
let matrix_sizes = [ 10; 100; 1000 ]

let%bench_fun ("transpose [List]" [@indexed n = matrix_sizes]) =
  let matrix = List.init n ~f:(fun _ -> List.init n ~f:Fn.id) in
  fun () -> List.transpose_exn matrix
;;

let%bench_fun ("transpose [Fixed_list]" [@indexed n = matrix_sizes]) =
  let (Fixed_list.T row) = Fixed_list.init n ~f:Fn.id in
  let (Fixed_list.T matrix) = Fixed_list.init n ~f:(fun _ -> row) in
  let x, Fixed_list.T xs =
    match matrix with
    | [] -> failwith "unexpected"
    | x :: xs -> x, Fixed_list.T xs
  in
  let matrix = Fixed_list.(x :: xs) in
  fun () ->
    let _thing = Fixed_list.transpose matrix |> Sys.opaque_identity in
    ()
;;

(* Sort benchmarks *)
let%bench_fun ("sort [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:(fun i -> n - i) in
  (* worst case: reversed *)
  fun () -> List.sort xs ~compare:Int.compare
;;

let%bench_fun ("sort [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:(fun i -> n - i) in
  fun () -> Fixed_list.T (Fixed_list.sort fixed ~compare:Int.compare)
;;

module Packed_proof = struct
  type ('shorter, 'longer) t =
    | T : ('diff, 'shorter, 'longer) Fixed_list.Sum.t -> ('shorter, 'longer) t
end

(* Helper to get proof that shorter + diff = longer *)
let get_length_proof shorter longer =
  match Fixed_list.compare_lengths shorter longer with
  | Equal _ -> failwith "unexpected: lengths should differ"
  | Greater _ -> failwith "unexpected: first list should be shorter"
  | Less proof -> Packed_proof.T proof
;;

(* Take/Drop benchmarks *)
let%bench_fun ("take [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  let half = n / 2 in
  fun () -> List.take xs half
;;

let%bench_fun ("take [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T longer) = Fixed_list.init n ~f:Fn.id in
  let half = n / 2 in
  let (Fixed_list.T template) = Fixed_list.init half ~f:Fn.id in
  let (Packed_proof.T proof) = get_length_proof template longer in
  fun () -> Fixed_list.T (Fixed_list.take longer ~template ~proof)
;;

let%bench_fun ("drop [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  let half = n / 2 in
  fun () -> List.drop xs half
;;

let%bench_fun ("drop [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T longer) = Fixed_list.init n ~f:Fn.id in
  let half = n / 2 in
  let (Fixed_list.T template) = Fixed_list.init half ~f:Fn.id in
  let (Packed_proof.T proof) = get_length_proof template longer in
  fun () -> Fixed_list.T (Fixed_list.drop longer ~template ~proof)
;;

(* Map2_uneven benchmarks *)
let%bench_fun ("map2_uneven [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T longer) = Fixed_list.init n ~f:Fn.id in
  let half = n / 2 in
  let (Fixed_list.T shorter) = Fixed_list.init half ~f:Fn.id in
  let (Packed_proof.T proof) = get_length_proof shorter longer in
  fun () ->
    let _thing =
      Fixed_list.map2_uneven ~shorter ~longer ~proof ~f:( + ) |> Sys.opaque_identity
    in
    ()
;;

(* Append benchmarks *)
let%bench_fun ("append [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  let ys = List.init n ~f:(fun i -> i + n) in
  fun () -> List.append xs ys
;;

let%bench_fun ("append [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init (2 * n) ~f:Fn.id in
  let (Fixed_list.Partition.T { first; second; first_sum = _; second_sum }) =
    Fixed_list.partition_mapi fixed ~f:(fun i x -> if i < n then First x else Second x)
  in
  fun () -> Fixed_list.T (Fixed_list.append first second ~proof:second_sum)
;;

let%bench_fun ("Rev.append [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init (2 * n) ~f:Fn.id in
  let (Fixed_list.Partition.T { first; second; first_sum = _; second_sum }) =
    Fixed_list.partition_mapi fixed ~f:(fun i x -> if i < n then First x else Second x)
  in
  fun () -> Fixed_list.T (Fixed_list.Rev.append first second ~proof:second_sum)
;;

let%bench_fun ("zip [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.zip_exn xs xs
;;

let%bench_fun ("zip [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.zip fixed fixed |> Fixed_list.T
;;

let%bench_fun ("reduce [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.reduce_exn (0 :: xs) ~f:( + )
;;

let%bench_fun ("reduce [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () -> Fixed_list.reduce (0 :: fixed) ~f:( + )
;;

let%bench_fun ("fold_map [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.fold_map xs ~init:0 ~f:(fun acc x -> acc + x, x * 2)
;;

let%bench_fun ("fold_map [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () ->
    let _thing =
      Fixed_list.fold_map fixed ~init:0 ~f:(fun acc x -> acc + x, x * 2)
      |> Sys.opaque_identity
    in
    ()
;;

let%bench_fun ("fold_mapi [List]" [@indexed n = ns]) =
  let xs = List.init n ~f:Fn.id in
  fun () -> List.fold_mapi xs ~init:0 ~f:(fun i acc x -> acc + x, i + x)
;;

let%bench_fun ("fold_mapi [Fixed_list]" [@indexed n = ns]) =
  let (Fixed_list.T fixed) = Fixed_list.init n ~f:Fn.id in
  fun () ->
    let _thing =
      Fixed_list.fold_mapi fixed ~init:0 ~f:(fun i acc x -> acc + x, i + x)
      |> Sys.opaque_identity
    in
    ()
;;

(* Distribute_monad benchmarks *)
let%bench_fun ("distribute_monad [Option]" [@indexed n = ns]) =
  let (Fixed_list.T data) = Fixed_list.init n ~f:Fn.id in
  let input = Some data in
  let module Distribute = Fixed_list.Monad_sequence.Distribute.Make (Option) in
  fun () ->
    let _thing =
      Distribute.distribute_monad input ~template:data |> Sys.opaque_identity
    in
    ()
;;

let%bench_fun ("distribute_monad [Or_error]" [@indexed n = ns]) =
  let (Fixed_list.T data) = Fixed_list.init n ~f:Fn.id in
  let input = Or_error.return data in
  let module Distribute = Fixed_list.Monad_sequence.Distribute.Make (Or_error) in
  fun () ->
    let _thing =
      Distribute.distribute_monad input ~template:data |> Sys.opaque_identity
    in
    ()
;;
