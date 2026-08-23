open! Core

(** {1 Arithmetic} *)

module type Arithmetic = Arithmetic_intf.S

include Arithmetic

(** {1 Fixed list} *)

(** A length-indexed list module that provides type-safe list operations.

    This module provides list operations where the length is tracked in the type system
    using Peano numbers (zero and successor). This allows for compile-time guarantees
    about length-sensitive operations like [map2], which can only be called on lists of
    equal length.

    The module includes both regular operations that preserve length (like [map]) and
    operations that combine lists of different lengths with explicit proofs of their
    length relationships (like [append] and [map2_uneven]).

    For example, you can have a function fetching data for each key
    {[
      let fetch_data keys =
        let module M = Fixed_list.Monad_sequence.Sequential.Make (Deferred) in
        M.map keys ~f:(fun key -> Db.fetch_by_key key)
      ;;
    ]}
    such that the caller doesn't need to do awkward unpacking when they need just one or
    two keys!
    {[
      let%bind [ data1; data2 ] = fetch_data [ key1; key2 ] in
      process data1 data2
    ]}
    (there are many more use cases) *)

(** Type of a list with elements of type ['a] and length ['n]. The length is represented
    using type-level natural numbers:
    - [zero] for empty lists
    - ['n succ] for lists one element longer than ['n] *)
type ('a, 'n) t =
  | [] : ('a, zero) t
  | ( :: ) : 'a * ('a, 'n) t -> ('a, 'n succ) t
[@@deriving compare ~localize, sexp_of]

type ('a, 'n) fixed_list = ('a, 'n) t
type 'a packed = T : ('a, 'n) t -> 'a packed

(** {2 Basic operations} *)

(** {3 Single list} *)

val length : ('a, _) t -> int
val init : int -> f:(int -> 'a) -> 'a packed
val of_list : 'a list -> 'a packed
val to_list : ('a, _) t -> 'a list
val of_sequence : 'a Sequence.t -> 'a packed
val to_sequence : ('a, _) t -> 'a Sequence.t
val rev : ('a, 'n) t -> ('a, 'n) t
val map : ('a, 'n) t -> f:('a -> 'b) -> ('b, 'n) t
val mapi : ('a, 'n) t -> f:(int -> 'a -> 'b) -> ('b, 'n) t
val unzip : ('a * 'b, 'n) t -> ('a, 'n) t * ('b, 'n) t

(** The sort is stable: the order of equal elements is preserved. *)
val sort : ('a, 'n) t -> compare:('a -> 'a -> int) -> ('a, 'n) t

(** [fold_map t ~init ~f] folds over [t] with accumulator [init] while simultaneously
    mapping the elements. For each element [x], [f] returns both the new accumulator and
    the mapped value. *)
val fold_map : ('a, 'n) t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * ('b, 'n) t

val fold_mapi
  :  ('a, 'n) t
  -> init:'acc
  -> f:(int -> 'acc -> 'a -> 'acc * 'b)
  -> 'acc * ('b, 'n) t

(** {3 Nonempty list} *)

val hd : ('a, _ succ) t -> 'a
val tl : ('a, 'n succ) t -> ('a, 'n) t
val last : ('a, _ succ) t -> 'a

type 'a packed_succ = T_succ : ('a, 'n succ) t -> 'a packed_succ

val to_nonempty : ('a, _ succ) t -> 'a Nonempty_list.t
val of_nonempty : 'a Nonempty_list.t -> 'a packed_succ

(** [reduce t ~f] reduces [t] to a single value by repeatedly applying [f]. Since [t] is
    non-empty, no initial value is needed. *)
val reduce : ('a, 'n succ) t -> f:('a -> 'a -> 'a) -> 'a

(** {3 Multi-list} *)

val zip : ('a, 'n) t -> ('b, 'n) t -> ('a * 'b, 'n) t
val map2 : ('a, 'n) t -> ('b, 'n) t -> f:('a -> 'b -> 'c) -> ('c, 'n) t
val iter2 : ('a, 'n) t -> ('b, 'n) t -> f:('a -> 'b -> unit) -> unit
val fold2 : ('a, 'n) t -> ('b, 'n) t -> init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'acc

val map3
  :  ('a, 'n) t
  -> ('b, 'n) t
  -> ('c, 'n) t
  -> f:('a -> 'b -> 'c -> 'd)
  -> ('d, 'n) t

(** [transpose t] transposes a matrix. The outer list is required to be nonempty; the
    result is not well-defined for an empty list *)
val transpose : (('a, 'n) t, 'm succ) t -> (('a, 'm succ) t, 'n) t

(** {2 Advanced} *)

(** [append t1 t2 ~proof] concatenates [t1] and [t2]. The [proof] argument witnesses that
    the length of the result is the sum of the input lengths. *)
val append
  :  ('a, 'first) t
  -> ('a, 'second) t
  -> proof:('second, 'first, 'sum) Sum.t
  -> ('a, 'sum) t

(** A partition of a list into two parts, along with proofs that their lengths sum to the
    original length in both orders (i.e., first + second = total and second + first =
    total). This is so that they could be put together back to a list of its original
    length. *)
module Partition : sig
  type ('item1, 'item2, 'first, 'second, 'total) t =
    { first : ('item1, 'first) fixed_list
    ; second : ('item2, 'second) fixed_list
    ; first_sum : ('first, 'second, 'total) Sum.t
    ; second_sum : ('second, 'first, 'total) Sum.t
    }

  type ('item1, 'item2, 'total) packed =
    | T : ('item1, 'item2, 'first, 'second, 'total) t -> ('item1, 'item2, 'total) packed
end

val partition_mapi
  :  ('a, 'n) t
  -> f:(int -> 'a -> ('b, 'c) Either.t)
  -> ('b, 'c, 'n) Partition.packed

module Length_ordering : sig
  type ('first, 'second) t =
    | Equal : ('first, 'second) Type_equal.t -> ('first, 'second) t
    | Less : ('diff succ, 'first, 'second) Sum.t -> ('first, 'second) t
    | Greater : ('diff succ, 'second, 'first) Sum.t -> ('first, 'second) t
  [@@deriving sexp_of]
end

(** [compare_lengths t1 t2] compares the lengths of two lists, returning either:
    - [Equal] with proof that the lengths are equal
    - [Less] with proof that [t1] is shorter than [t2] by [diff succ]
    - [Greater] with proof that [t1] is longer than [t2] by [diff succ] *)
val compare_lengths
  :  ('a, 'first) t
  -> ('b, 'second) t
  -> ('first, 'second) Length_ordering.t

(** It's possible to flip a [Sum.t], provided you have a list of the length of the second
    summand *)
val flip_sum : (_, 'l) t -> ('k, 'l, 'sum) Sum.t -> ('l, 'k, 'sum) Sum.t

(** [map2_uneven ~shorter ~longer ~proof ~f] applies [f] to corresponding elements of
    [shorter] and [longer], returning both the mapped elements and the remaining suffix of
    [longer]. The [proof] argument witnesses that [shorter] is indeed shorter than
    [longer], with their difference captured in ['diff]. *)
val map2_uneven
  :  shorter:('a, 'shorter) t
  -> longer:('b, 'longer) t
  -> proof:('diff, 'shorter, 'longer) Sum.t
  -> f:('a -> 'b -> 'c)
  -> ('c, 'shorter) t * ('b, 'diff) t

(** [take t ~template ~proof] returns the first [n] elements of [t], where [n] is the
    length of [template]. The [proof] witnesses that [t] is indeed longer than [template]
    by [remaining] elements. *)
val take
  :  ('a, 'longer) t
  -> template:('b, 'n) t
  -> proof:('remaining, 'n, 'longer) Sum.t
  -> ('a, 'n) t

(** [drop t ~template ~proof] returns all but the first [n] elements of [t], where [n] is
    the length of [template]. The [proof] witnesses that [t] is indeed longer than
    [template] by [remaining] elements. *)
val drop
  :  ('a, 'longer) t
  -> template:('b, 'n) t
  -> proof:('remaining, 'n, 'longer) Sum.t
  -> ('a, 'remaining) t

(** {2 Indexed fold} *)

(** A functor for building length-preserving fold operations. The accumulator type ['n t]
    is indexed by the length of the processed portion of the input list, starting from
    [zero] and increasing by one for each element processed.

    This is useful for implementing operations that need to maintain length information
    while building up a result, such as [rev_map] or [fold_map]. *)
module Indexed_fold (M : sig
    type item
    type 'n t

    val f : 'n t -> item -> 'n succ t
  end) : sig
  val fold : (M.item, 'n) t -> init:zero M.t -> 'n M.t
end

module Indexed_foldi (M : sig
    type item
    type 'n t

    val f : int -> 'n t -> item -> 'n succ t
  end) : sig
  val fold : (M.item, 'n) t -> init:zero M.t -> 'n M.t
end

(** {2 Monadic operations} *)

(** Monadic operations on fixed-length lists. Provides both sequential and parallel
    variants for mapping over lists with effects. *)
module Monad_sequence : sig
  module Distribute : sig
    module Make (M : sig
        type 'a t

        val map : 'a t -> f:('a -> 'b) -> 'b t
      end) : sig
      val distribute_monad : ('a, 'n) t M.t -> template:('b, 'n) t -> ('a M.t, 'n) t
    end
  end

  module Sequential : sig
    module Make (M : sig
        type 'a t

        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
      end) : sig
      val map : ('a, 'n) t -> f:('a -> 'b M.t) -> ('b, 'n) t M.t
    end

    module Make2 (M : sig
        type ('a, 'e) t

        val return : 'a -> ('a, 'e) t
        val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      end) : sig
      val map : ('a, 'n) t -> f:('a -> ('b, 'e) M.t) -> (('b, 'n) t, 'e) M.t
    end
  end

  module Parallel : sig
    module Make (M : sig
        type 'a t

        val return : 'a -> 'a t
        val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      end) : sig
      val map : ('a, 'n) t -> f:('a -> 'b M.t) -> ('b, 'n) t M.t
    end

    module Make2 (M : sig
        type ('a, 'e) t

        val return : 'a -> ('a, 'e) t
        val map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
      end) : sig
      val map : ('a, 'n) t -> f:('a -> ('b, 'e) M.t) -> (('b, 'n) t, 'e) M.t
    end
  end
end

(** {2 Reversed} *)

(** Implementations that return their results in reverse order. These functions are more
    efficient as they avoid an extra reversal, but the caller must handle the reversed
    output. *)
module Rev : sig
  val map : ('a, 'n) t -> f:('a -> 'b) -> ('b, 'n) t
  val mapi : ('a, 'n) t -> f:(int -> 'a -> 'b) -> ('b, 'n) t

  val append
    :  ('a, 'first) t
    -> ('a, 'second) t
    -> proof:('second, 'first, 'total) Sum.t
    -> ('a, 'total) t

  val partition_mapi
    :  ('a, 'n) t
    -> f:(int -> 'a -> ('b, 'c) Either.t)
    -> ('b, 'c, 'n) Partition.packed
end
