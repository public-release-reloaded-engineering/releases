open! Core

(* so that odoc displays Arithmetic as a collapsed section *)
module type Arithmetic = Arithmetic_intf.S

include Arithmetic

type ('a, 'n) t =
  | [] : ('a, zero) t
  | ( :: ) : 'a * ('a, 'n) t -> ('a, 'n succ) t

type ('a, 'n) fixed_list = ('a, 'n) t
type 'a packed = T : ('a, 'n) t -> 'a packed

let%template[@mode m = (local, global)] rec compare
  : type n. _ -> _ -> (_, n) t -> (_, n) t -> int
  =
  fun base_a base_n t1 t2 ->
  match t1, t2 with
  | [], [] -> Ordering.to_int Equal
  | x :: xs, y :: ys ->
    let result = base_a x y in
    (match Ordering.of_int result with
     | Less | Greater -> result
     | Equal -> (compare [@mode m]) base_a base_n xs ys)
;;

let of_list lst =
  let rec loop acc : _ list -> _ = function
    | [] -> acc
    | x :: xs ->
      let (T t) = acc in
      loop (T (x :: t)) xs
  in
  loop (T []) (List.rev lst)
;;

let[@tail_mod_cons] rec to_list : type n. (_, n) t -> _ list = function
  | [] -> []
  | hd :: tl -> hd :: to_list tl
;;

let sexp_of_t (type a) sexp_of_a _ (t : (a, _) t) = to_list t |> [%sexp_of: a list]

let init n ~f =
  let rec loop i =
    if i >= n
    then T []
    else (
      let (T tl) = loop (i + 1) in
      T (f i :: tl))
  in
  loop 0
;;

let to_sequence t =
  Sequence.unfold_step ~init:(T t) ~f:(function
    | T [] -> Done
    | T (hd :: tl) -> Yield { value = hd; state = T tl })
;;

let rec length_aux : type n. int -> (_, n) t -> int =
  fun len -> function
  | [] -> len
  | _ :: tl -> length_aux (len + 1) tl
;;

let length xs = length_aux 0 xs

let hd : type n. (_, n succ) t -> _ = function
  | hd :: _ -> hd
;;

let tl : type n. (_, n succ) t -> (_, n) t = function
  | _ :: tl -> tl
;;

let rec last : type n. (_, n succ) t -> _ = function
  | [ x ] -> x
  | _ :: (_ :: _ as tl) -> last tl
;;

let[@tail_mod_cons] rec map : type n. (_, n) t -> f:_ -> (_, n) t =
  fun t ~f ->
  match t with
  | [] -> []
  | hd :: tl -> f hd :: map tl ~f
;;

let mapi : type n. (_, n) t -> f:(int -> _ -> _) -> (_, n) t =
  fun t ~f ->
  let[@tail_mod_cons] rec loop : type n. int -> (_, n) t -> (_, n) t =
    fun i -> function
    | [] -> []
    | hd :: tl -> f i hd :: loop (i + 1) tl
  in
  loop 0 t
;;

module Indexed_fold (M : sig
    type item
    type 'n t

    val f : 'n t -> item -> 'n succ t
  end) : sig
  val fold : (M.item, 'n) t -> init:zero M.t -> 'n M.t
end = struct
  let fold t ~init =
    let rec loop : type a b sum. (_, b) t -> a M.t -> (a, b, sum) Sum.t -> sum M.t =
      fun t acc counter ->
      match t with
      | [] ->
        let T = Sum.finalize counter in
        acc
      | hd :: tl -> loop tl (M.f acc hd) (Sum.shift counter)
    in
    loop t init Sum.empty
  ;;
end

module Indexed_foldi (M : sig
    type item
    type 'n t

    val f : int -> 'n t -> item -> 'n succ t
  end) : sig
  val fold : (M.item, 'n) t -> init:zero M.t -> 'n M.t
end = struct
  let fold t ~init =
    let rec loop : type a b sum. (_, b) t -> int -> a M.t -> (a, b, sum) Sum.t -> sum M.t =
      fun t i acc counter ->
      match t with
      | [] ->
        let T = Sum.finalize counter in
        acc
      | hd :: tl -> loop tl (i + 1) (M.f i acc hd) (Sum.shift counter)
    in
    loop t 0 init Sum.empty
  ;;
end

let rec rev_append
  : type a b sum. (_, b) t -> (_, a) t -> proof:(a, b, sum) Sum.t -> (_, sum) t
  =
  fun t acc ~proof ->
  match t with
  | [] ->
    let T = Sum.finalize proof in
    acc
  | hd :: tl -> rev_append tl (hd :: acc) ~proof:(Sum.shift proof)
;;

let rev t = rev_append t [] ~proof:Sum.empty
let append xs ys ~proof = rev_append (rev xs) ys ~proof

let rev_map (type a b) t ~f =
  let module F =
    Indexed_fold (struct
      type item = a
      type 'n t = (b, 'n) fixed_list

      let f acc item = f item :: acc
    end)
  in
  F.fold t ~init:[]
;;

let rev_mapi (type a b) t ~f =
  let module F =
    Indexed_fold (struct
      type item = a
      type 'n t = (b, 'n) fixed_list * int

      let f (acc, i) item = f i item :: acc, i + 1
    end)
  in
  fst (F.fold t ~init:([], 0))
;;

let of_sequence seq =
  let rec loop acc seq =
    match Sequence.next seq with
    | None -> acc
    | Some (x, xs) ->
      let (T t) = acc in
      loop (T (x :: t)) xs
  in
  let (T t) = loop (T []) seq in
  T (rev t)
;;

let[@tail_mod_cons] rec map2 : type n. (_, n) t -> (_, n) t -> f:_ -> (_, n) t =
  fun t1 t2 ~f ->
  match t1, t2 with
  | [], [] -> []
  | x :: xs, y :: ys -> f x y :: map2 xs ys ~f
;;

let rec iter2 : type n. (_, n) t -> (_, n) t -> f:_ -> unit =
  fun t1 t2 ~f ->
  match t1, t2 with
  | [], [] -> ()
  | x :: xs, y :: ys ->
    f x y;
    iter2 xs ys ~f
;;

let rec fold2 : type n. (_, n) t -> (_, n) t -> init:'acc -> f:_ -> 'acc =
  fun t1 t2 ~init ~f ->
  match t1, t2 with
  | [], [] -> init
  | x :: xs, y :: ys -> fold2 xs ys ~init:(f init x y) ~f
;;

let[@tail_mod_cons] rec map3 : type n. (_, n) t -> (_, n) t -> (_, n) t -> f:_ -> (_, n) t
  =
  fun t1 t2 t3 ~f ->
  match t1, t2, t3 with
  | [], [], [] -> []
  | x :: xs, y :: ys, z :: zs -> f x y z :: map3 xs ys zs ~f
;;

let map2_uneven ~shorter ~longer ~proof ~f =
  let rec loop
    : type shorter longer diff processed result.
      (_, shorter) t
      -> (_, longer) t
      -> (_, processed) t
      -> (processed, shorter, result) Sum.t
         (* proves processed + shorter_remaining = shorter *)
      -> (diff, shorter, longer) Sum.t
      -> (_, result) t * (_, diff) t
    =
    fun shorter longer acc processed_counter diff_counter ->
    match shorter, longer with
    | [], remaining ->
      let T = Sum.finalize processed_counter in
      let T = Sum.finalize diff_counter in
      acc, remaining
    | x :: xs, y :: ys ->
      loop
        xs
        ys
        (f x y :: acc)
        (Sum.shift processed_counter)
        (Sum.decr_second diff_counter)
    | _ :: _, [] -> Sum.absurd diff_counter
  in
  let shorter_result, diff = loop shorter longer [] Sum.empty proof in
  rev shorter_result, diff
;;

let take
  : type a b longer n remaining.
    (a, longer) t -> template:(b, n) t -> proof:(remaining, n, longer) Sum.t -> (a, n) t
  =
  fun t ~template ~proof ->
  let shorter, _ = map2_uneven ~shorter:template ~longer:t ~proof ~f:(fun _ x -> x) in
  shorter
;;

let drop
  : type a b longer n remaining.
    (a, longer) t
    -> template:(b, n) t
    -> proof:(remaining, n, longer) Sum.t
    -> (a, remaining) t
  =
  fun t ~template ~proof ->
  let _, remaining = map2_uneven ~shorter:template ~longer:t ~proof ~f:(fun _ x -> x) in
  remaining
;;

let rec transpose : type n m. ((_, n) t, m succ) t -> ((_, m succ) t, n) t = function
  | [ xs ] -> map xs ~f:(fun x -> [ x ])
  | xs :: (_ :: _ as xss) -> map2 xs (transpose xss) ~f:(fun x rest -> x :: rest)
;;

module Partition = struct
  type ('item1, 'item2, 'first, 'second, 'total) t =
    { first : ('item1, 'first) fixed_list
    ; second : ('item2, 'second) fixed_list
    ; first_sum : ('first, 'second, 'total) Sum.t
    ; second_sum : ('second, 'first, 'total) Sum.t
    }

  let empty = { first = []; second = []; first_sum = Sum.empty; second_sum = Sum.empty }

  let push_first { first; second; first_sum; second_sum } x =
    { first = x :: first
    ; second
    ; first_sum = Sum.incr_first first_sum
    ; second_sum = Sum.incr_second second_sum
    }
  ;;

  let push_second { first; second; first_sum; second_sum } y =
    { first
    ; second = y :: second
    ; first_sum = Sum.incr_second first_sum
    ; second_sum = Sum.incr_first second_sum
    }
  ;;

  type ('item1, 'item2, 'total) packed =
    | T : ('item1, 'item2, 'first, 'second, 'total) t -> ('item1, 'item2, 'total) packed
end

let rev_partition_mapi (type a b c) t ~f =
  let module Indexed_fold =
    Indexed_fold (struct
      type item = a
      type 'k t = (b, c, 'k) Partition.packed * int

      let f (Partition.T partition, i) item =
        match f i item with
        | First x -> Partition.T (Partition.push_first partition x), i + 1
        | Second y -> Partition.T (Partition.push_second partition y), i + 1
      ;;
    end)
  in
  Indexed_fold.fold t ~init:(T Partition.empty, 0) |> fst
;;

let partition_mapi t ~f =
  let%tydi (T { first; second; first_sum; second_sum }) = rev_partition_mapi t ~f in
  Partition.T { first = rev first; second = rev second; first_sum; second_sum }
;;

let rev_unzip (type a b n) (t : (a * b, n) t) =
  let module Indexed_fold =
    Indexed_fold (struct
      type item = a * b
      type 'k t = (a, 'k) fixed_list * (b, 'k) fixed_list

      let f (xs, ys) (x, y) = x :: xs, y :: ys
    end)
  in
  Indexed_fold.fold t ~init:([], [])
;;

let unzip t = rev t |> rev_unzip

module Length_ordering = struct
  type ('first, 'second) t =
    | Equal : (('first, 'second) Type_equal.t[@sexp.opaque]) -> ('first, 'second) t
    | Less : (('diff succ, 'first, 'second) Sum.t[@sexp.opaque]) -> ('first, 'second) t
    | Greater : (('diff succ, 'second, 'first) Sum.t[@sexp.opaque]) -> ('first, 'second) t
  [@@deriving sexp_of]
end

let flip_sum : type a b sum. (_, b) t -> (a, b, sum) Sum.t -> (b, a, sum) Sum.t =
  fun t sum ->
  let rec loop
    : type i remaining_b processed.
      (_, remaining_b) t
      -> (i, remaining_b, b) Sum.t
      -> (i, a, processed) Sum.t
      -> (processed, remaining_b, sum) Sum.t
      -> (b, a, sum) Sum.t
    =
    fun t b processed sum ->
    match t with
    | [] ->
      let T, T = Sum.finalize b, Sum.finalize sum in
      processed
    | _ :: t -> loop t (Sum.shift b) (Sum.incr_first processed) (Sum.shift sum)
  in
  loop t Sum.empty Sum.empty sum
;;

let compare_lengths first second =
  let rec loop
    : type k l processed total_k total_l.
      (_, k) t
      -> (_, l) t
      -> (processed, k, total_k) Sum.t
      -> (processed, l, total_l) Sum.t
      -> (total_k, total_l) Length_ordering.t
    =
    fun first second first_counter second_counter ->
    match first, second with
    | [], [] ->
      let T = Sum.finalize first_counter in
      let T = Sum.finalize second_counter in
      Equal T
    | _ :: _, [] ->
      let T = Sum.finalize second_counter in
      Greater (flip_sum first first_counter)
    | [], _ :: _ ->
      let T = Sum.finalize first_counter in
      Less (flip_sum second second_counter)
    | _ :: xs, _ :: ys -> loop xs ys (Sum.shift first_counter) (Sum.shift second_counter)
  in
  loop first second Sum.empty Sum.empty
;;

let zip xs ys = map2 xs ys ~f:(fun x y -> x, y)

let reduce : type n. ('a, n succ) t -> f:('a -> 'a -> 'a) -> 'a =
  fun t ~f ->
  let rec loop : type n. 'a -> ('a, n) t -> 'a =
    fun acc -> function
    | [] -> acc
    | x :: xs -> loop (f acc x) xs
  in
  match t with
  | x :: xs -> loop x xs
;;

let fold_map
  : type n a b acc. (a, n) t -> init:acc -> f:(acc -> a -> acc * b) -> acc * (b, n) t
  =
  fun t ~init ~f ->
  let module F =
    Indexed_fold (struct
      type item = a
      type 'k t = acc * (b, 'k) fixed_list

      let f (acc, xs) item =
        let acc', x = f acc item in
        acc', x :: xs
      ;;
    end)
  in
  let acc, reversed = F.fold t ~init:(init, []) in
  acc, rev reversed
;;

let fold_mapi
  : type n a b acc.
    (a, n) t -> init:acc -> f:(int -> acc -> a -> acc * b) -> acc * (b, n) t
  =
  fun t ~init ~f ->
  let module F =
    Indexed_foldi (struct
      type item = a
      type 'k t = acc * (b, 'k) fixed_list

      let f i (acc, xs) item =
        let acc', x = f i acc item in
        acc', x :: xs
      ;;
    end)
  in
  let acc, reversed = F.fold t ~init:(init, []) in
  acc, rev reversed
;;

let to_nonempty : type n. ('a, n succ) t -> 'a Nonempty_list.t =
  fun t ->
  match t with
  | x :: xs -> Nonempty_list.create x (to_list xs)
;;

type 'a packed_succ = T_succ : ('a, 'n succ) t -> 'a packed_succ

let of_nonempty (t : _ Nonempty_list.t) : _ packed_succ =
  let hd = Nonempty_list.hd t in
  let (T tl) = of_list (Nonempty_list.tl t) in
  T_succ (hd :: tl)
;;

let rev_merge { Partition.first; second; first_sum; second_sum } ~compare ~reversed =
  let rec loop
    : type k l processed processed_k processed_l total.
      (_, k) t
      -> (_, l) t
      -> (_, processed) t
      -> (processed, k, processed_k) Sum.t
      -> (processed, l, processed_l) Sum.t
      -> (processed_l, k, total) Sum.t
      -> (processed_k, l, total) Sum.t
      -> (_, total) t
    =
    fun first second acc first_processed second_processed first_total second_total ->
    match first, second with
    | [], second ->
      let T = Sum.finalize first_processed in
      rev_append second acc ~proof:second_total
    | first, [] ->
      let T = Sum.finalize second_processed in
      rev_append first acc ~proof:first_total
    | x :: xs, y :: ys ->
      (match compare x y |> Ordering.of_int, reversed with
       | Less, false | Greater, true ->
         loop
           xs
           (y :: ys)
           (x :: acc)
           (Sum.shift first_processed)
           (Sum.incr_first second_processed)
           (Sum.shift first_total)
           second_total
       | (Less | Equal), true | (Greater | Equal), false ->
         loop
           (x :: xs)
           ys
           (y :: acc)
           (Sum.incr_first first_processed)
           (Sum.shift second_processed)
           first_total
           (Sum.shift second_total))
  in
  loop first second [] Sum.empty Sum.empty second_sum first_sum
;;

let sort t ~compare =
  let rec sort : type n. (_, n) t -> len:int -> reverse:bool -> (_, n) t =
    fun t ~len ~reverse ->
    match t with
    | [] -> []
    | [ x ] -> [ x ]
    | [ x1; x2 ] ->
      (match compare x1 x2 |> Ordering.of_int, reverse with
       | Less, false | Equal, _ | Greater, true -> [ x1; x2 ]
       | Less, true | Greater, false -> [ x2; x1 ])
    | xs ->
      let half = len / 2 in
      let (T { first; second; first_sum; second_sum }) =
        rev_partition_mapi xs ~f:(fun i x -> if i < half then First x else Second x)
      in
      let first = sort first ~len:half ~reverse:(not reverse) in
      let second = sort second ~len:(len - half) ~reverse:(not reverse) in
      { first; second; first_sum; second_sum }
      |> rev_merge ~compare ~reversed:(not reverse)
  in
  sort t ~len:(length t) ~reverse:false
;;

module Monad_sequence = struct
  module Distribute = struct
    module Make (M : sig
        type 'a t

        val map : 'a t -> f:('a -> 'b) -> 'b t
      end) =
    struct
      let[@tail_mod_cons] rec distribute_monad
        : type a n. (a, n) t M.t -> template:(_, n) t -> (a M.t, n) t
        =
        fun xs ~template ->
        match template with
        | [] -> []
        | _ :: template -> M.map xs ~f:hd :: distribute_monad (M.map xs ~f:tl) ~template
      ;;
    end
  end

  module Sequential = struct
    module Make (M : sig
        type 'a t

        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
      end) =
    struct
      let map (type a b n) (t : (a, n) t) ~(f : a -> b M.t) =
        let module F =
          Indexed_fold (struct
            type item = a
            type 'k t = (b, 'k) fixed_list M.t

            let f t item =
              M.bind t ~f:(fun t -> M.bind (f item) ~f:(fun b -> M.return (b :: t)))
            ;;
          end)
        in
        M.bind (F.fold t ~init:(M.return [])) ~f:(fun t -> M.return (rev t))
      ;;
    end

    module Make2 (M : sig
        type ('a, 'e) t

        val return : 'a -> ('a, 'e) t
        val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      end) =
    struct
      let map (type a b e n) (t : (a, n) t) ~(f : a -> (b, e) M.t) =
        let module F =
          Indexed_fold (struct
            type item = a
            type 'k t = ((b, 'k) fixed_list, e) M.t

            let f t item =
              M.bind t ~f:(fun t -> M.bind (f item) ~f:(fun b -> M.return (b :: t)))
            ;;
          end)
        in
        M.bind (F.fold t ~init:(M.return [])) ~f:(fun t -> M.return (rev t))
      ;;
    end
  end

  module Parallel = struct
    module Make (M : sig
        type 'a t

        val return : 'a -> 'a t
        val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      end) =
    struct
      let map (type a b n) (t : (a, n) t) ~(f : a -> b M.t) =
        let module F =
          Indexed_fold (struct
            type item = a
            type 'k t = (b, 'k) fixed_list M.t

            let f t item = M.map2 t (f item) ~f:(fun t b -> b :: t)
          end)
        in
        M.map2 (F.fold t ~init:(M.return [])) (M.return ()) ~f:(fun t () -> rev t)
      ;;
    end

    module Make2 (M : sig
        type ('a, 'e) t

        val return : 'a -> ('a, 'e) t
        val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
      end) =
    struct
      let map (type a b e n) (t : (a, n) t) ~(f : a -> (b, e) M.t) =
        let module F =
          Indexed_fold (struct
            type item = a
            type 'k t = ((b, 'k) fixed_list, e) M.t

            let f t item = M.map2 t (f item) ~f:(fun t b -> b :: t)
          end)
        in
        M.map2 (F.fold t ~init:(M.return [])) (M.return ()) ~f:(fun t () -> rev t)
      ;;
    end
  end
end

module Rev = struct
  let map = rev_map
  let mapi = rev_mapi
  let append = rev_append
  let partition_mapi = rev_partition_mapi
end
