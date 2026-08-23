open! Core

module Defs = struct
  type zero = Zero
  type 'a succ = Succ
end

module Aliases = struct
  open Defs

  type 'a plus0 = 'a
  type 'a plus1 = 'a plus0 succ
  type 'a plus2 = 'a plus1 succ
  type 'a plus3 = 'a plus2 succ
  type 'a plus4 = 'a plus3 succ
  type 'a plus5 = 'a plus4 succ
  type 'a plus6 = 'a plus5 succ
  type 'a plus7 = 'a plus6 succ
  type 'a plus8 = 'a plus7 succ
  type 'a plus9 = 'a plus8 succ
  type 'a plus10 = 'a plus9 succ
  type n0 = zero plus0
  type n1 = zero plus1
  type n2 = zero plus2
  type n3 = zero plus3
  type n4 = zero plus4
  type n5 = zero plus5
  type n6 = zero plus6
  type n7 = zero plus7
  type n8 = zero plus8
  type n9 = zero plus9
  type n10 = zero plus10
end

module type Aliases_S = module type of Aliases

module type S = sig
  type zero = Defs.zero = Zero
  type 'a succ = 'a Defs.succ = Succ

  module Sum : sig
    (** Represents a proof that a + b = sum. Due to limitations of the typesystem, when
        manipulating this proof we can only increase ['a], not decrease.

        (See the implementations inside [Fixed_list] for how this is useful) *)
    type ('a, 'b, 'sum) t

    val empty : (zero, 'sum, 'sum) t

    (** Moves one element from b to a, preserving their sum *)
    val shift : ('a, 'b succ, 'sum) t -> ('a succ, 'b, 'sum) t

    val incr_first : ('a, 'b, 'sum) t -> ('a succ, 'b, 'sum succ) t
    val incr_second : ('a, 'b, 'sum) t -> ('a, 'b succ, 'sum succ) t
    val decr_second : ('a, 'b succ, 'sum succ) t -> ('a, 'b, 'sum) t

    (** When b is zero, proves that a equals sum *)
    val finalize : ('a, zero, 'sum) t -> ('a, 'sum) Type_equal.t

    val absurd : (_, _ succ, zero) t -> _
  end

  module type Aliases_S = Aliases_S

  include Aliases_S

  module Sums : sig
    val left0 : (n0, 'a, 'a plus0) Sum.t
    val left1 : unit -> (n1, 'a, 'a plus1) Sum.t
    val left2 : unit -> (n2, 'a, 'a plus2) Sum.t
    val left3 : unit -> (n3, 'a, 'a plus3) Sum.t
    val left4 : unit -> (n4, 'a, 'a plus4) Sum.t
    val left5 : unit -> (n5, 'a, 'a plus5) Sum.t
    val left6 : unit -> (n6, 'a, 'a plus6) Sum.t
    val left7 : unit -> (n7, 'a, 'a plus7) Sum.t
    val left8 : unit -> (n8, 'a, 'a plus8) Sum.t
    val left9 : unit -> (n9, 'a, 'a plus9) Sum.t
    val left10 : unit -> (n10, 'a, 'a plus10) Sum.t
  end
end
