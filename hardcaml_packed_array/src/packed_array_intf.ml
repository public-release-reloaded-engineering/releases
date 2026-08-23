open Base
open Hardcaml_kernel

module type S = sig
  type 'a unpacked

  include Interface.S with type 'a t = 'a array

  val to_packed_array : (module Comb.S with type t = 'a) -> 'a unpacked -> 'a t

  (** Latches array values on a read. This works for zero and non-zero latency reads.

      If [`Latch_all] is passed, the latching occurs for the entire array when the lowest
      index is read. This enables an atomic snapshot of the entire packed array to be
      read.

      If [`Latch_by_field] is passed, latching occurs on a per-field basis when a field's
      lowest array index is read. This enables each field to be read atomically, without
      reading the full packed array. *)
  val to_packed_array_latch_on_read
    :  [ `Latch_all | `Latch_by_field ]
    -> Signal.Reg_spec.t
    -> Signal.t unpacked
    -> read_enable:Signal.t t
    -> Signal.t t

  val of_packed_array : (module Comb.S with type t = 'a) -> 'a t -> 'a unpacked

  val of_packed_array_with_valid
    :  (module Comb.S with type t = 'a)
    -> 'a With_valid.t t
    -> 'a With_valid.t unpacked

  (* Extract fields *)
  val extract_field_as_int : (int t -> int) unpacked
  val extract_field_as_int_trunc : (int t -> int) unpacked
  val extract_field_as_int64 : (int t -> int64) unpacked
  val extract_field_as_bytes : (int t -> Bytes.t -> unit) unpacked
  val extract_field_as_string : (int t -> String.t) unpacked

  (* Set fields *)
  val set_field_as_int : (int t -> int -> unit) unpacked
  val set_field_as_int64 : (int t -> int64 -> unit) unpacked
  val set_field_as_bytes : (int t -> Bytes.t -> unit) unpacked
  val set_field_as_string : (int t -> String.t -> unit) unpacked
  val empty_packed_int_array : unit -> int t

  (* Specialized conversions for ints *)
  val of_packed_int_array : int t -> int unpacked
  val of_packed_int_array_trunc : int t -> int unpacked
  val of_packed_int_array_to_int64 : int t -> int64 unpacked
  val to_packed_int_array : int unpacked -> int t

  (* Read fields by providing a function which takes a packed array index and returns the
     corresponding integer value. This is useful for getting individual fields of
     [unpacked] without needing to read the full packed array. *)
  val read_field_as_int : ((index:int -> int) -> int) unpacked
  val read_field_as_int_trunc : ((index:int -> int) -> int) unpacked
  val read_field_as_int64 : ((index:int -> int) -> int64) unpacked
  val read_field_as_bytes : ((index:int -> int) -> Bytes.t -> unit) unpacked
  val read_field_as_string : ((index:int -> int) -> String.t) unpacked
end

(** Packed arrays are a flattened version of [X.t] represented as an array of 32 bit
    vectors.

    They may be used as fields within a register interface to encode larger or grouped
    values. *)
module type Packed_array = sig
  module type S = S

  module Make (X : sig
      include Interface.S

      val name : string
    end) : S with type 'a unpacked = 'a X.t

  (** Convenient interface to create [module Packed = ...] using the [include functor]
      extension. *)
  module Include : sig
    module type S = sig
      type 'a unpacked

      module Packed : S with type 'a unpacked = 'a unpacked
    end

    module type F = functor (X : Interface.S) -> S with type 'a unpacked := 'a X.t

    module Make (X : sig
        include Interface.S

        val name : string
      end) : S with type 'a unpacked := 'a X.t
  end

  module Int63 : Include.S with type 'a unpacked := 'a
  module Int64 : Include.S with type 'a unpacked := 'a
end
