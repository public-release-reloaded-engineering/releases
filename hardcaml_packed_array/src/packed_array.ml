open Base
open Hardcaml_kernel

module type S = Packed_array_intf.S

module Make (X : sig
    include Interface.S

    val name : string
  end) =
struct
  type 'a unpacked = 'a X.t

  module Pre = struct
    type 'a t = 'a array [@@deriving equal ~localize, compare ~localize, sexp_of]

    let word_size = 32
    let num_words = (X.sum_of_port_widths + word_size - 1) / word_size

    let port_names_and_widths =
      Array.init num_words ~f:(fun i -> X.name ^ "_" ^ Int.to_string i, 32)
    ;;

    let map = Array.map
    let map2 = Array.map2_exn
    let iter = Array.iter
    let iter2 = Array.iter2_exn
    let to_list = Array.to_list
  end

  include Pre
  include Interface.Make (Pre)

  let to_packed_array (type a) (module Comb : Comb.S with type t = a) (x : a X.t) =
    let module C = X.Make_comb (Comb) in
    C.pack x
    |> Comb.split_lsb ~part_width:32 ~exact:false
    |> List.map ~f:(fun x -> Comb.uresize x ~width:32)
    |> Array.of_list
  ;;

  let to_packed_array_latch_on_read
    how_to_latch
    spec
    (x : Signal.t X.t)
    ~(read_enable : Signal.t t)
    =
    let open Signal in
    match how_to_latch with
    | `Latch_all ->
      to_packed_array (module Signal) x
      |> Array.mapi ~f:(fun i d ->
        (* The lowest index must be cut through for read latencies of 0 and latched for
           nonzero read latencies. the upper indices are latched when the lowest index is
           read. *)
        let enable = read_enable.(0) in
        if i = 0 then cut_through_reg spec ~enable d else reg spec ~enable d)
    | `Latch_by_field ->
      X.map3 x (X.offsets ()) X.port_widths ~f:(fun d bit_offset width ->
        (* Similar to above, the bits of the field that are contained in the lowest array
           index must be cut through for read latencies of 0, and latched for nonzero read
           latencies. The remaining bits can be latched. *)
        let base_address = bit_offset / word_size in
        let lowest_index_bits = word_size - (bit_offset % word_size) in
        let enable = read_enable.(base_address) in
        if lowest_index_bits >= width
        then cut_through_reg spec ~enable d
        else (
          let d_hi, d_lo = split_in_half_lsb ~lsbs:lowest_index_bits d in
          reg spec ~enable d_hi @: cut_through_reg spec ~enable d_lo))
      |> to_packed_array (module Signal)
  ;;

  let of_packed_array (type a) (module Comb : Comb.S with type t = a) (t : a t) : a X.t =
    let module C = X.Make_comb (Comb) in
    Comb.uresize (Comb.concat_lsb (Array.to_list t)) ~width:X.sum_of_port_widths
    |> C.unpack
  ;;

  let bit_positions = X.map2 (X.offsets ()) X.port_widths ~f:(fun o w -> o, w)

  let of_packed_array_with_valid
    (type a)
    (module Comb : Comb.S with type t = a)
    (t : a With_valid.t t)
    : a With_valid.t X.t
    =
    let x = of_packed_array (module Comb) (Array.map t ~f:(fun v -> v.value)) in
    X.map2 x bit_positions ~f:(fun value (offset, width) ->
      { With_valid.valid = t.((offset + width - 1) / 32).valid; value })
  ;;

  module Set_in (I : Int.S) = struct
    let set_in_int ~(orig : int) ~(offset : int) ~(width : int) ~(v : I.t) =
      assert (offset >= 0 && offset < Int.num_bits);
      assert (width > 0 && offset + width < Int.num_bits);
      (* compute the piece from [v] that we will insert into [orig] *)
      let width_mask =
        if width = I.(num_bits |> to_int_exn)
        then I.minus_one
        else I.((one lsl width) - one)
      in
      let v_mask = I.((v land width_mask) lsl offset |> to_int_exn) in
      (* compute the mask to unset the (width, offset) region of [orig] *)
      let unset_region_mask =
        let width_mask =
          if width = Int.num_bits then Int.minus_one else Int.((one lsl width) - one)
        in
        lnot (width_mask lsl offset)
      in
      (* set the region in [orig] *)
      orig land unset_region_mask lor v_mask
    ;;
  end

  module Extract_field_as (I : Int.S) = struct
    let () = assert (I.(num_bits |> to_int_exn) >= 32)

    open Set_in (I)

    let rec loop f (offset, width) (out_offset, out_word) =
      let word = offset / 32 in
      let bit_offset = offset land 31 in
      let bits_left = 32 - bit_offset in
      let out_word = f word bit_offset bits_left (out_offset, out_word) in
      if bits_left >= width
      then out_word
      else
        loop f (offset + bits_left, width - bits_left) (out_offset + bits_left, out_word)
    ;;

    let loop_set (offset, width) (out_offset, out_word) packed =
      let f word bit_offset bits_left (set_offset, set_word) =
        packed.(word)
        <- set_in_int
             ~orig:packed.(word)
             ~offset:bit_offset
             ~width:bits_left
             ~v:I.(set_word lsr set_offset);
        set_word
      in
      ignore (loop f (offset, width) (out_offset, out_word) : I.t)
    ;;

    let loop_extract get =
      let f word bit_offset _bits_left (out_offset, out_word) =
        I.(out_word lor ((I.of_int_exn (get ~index:word) lsr bit_offset) lsl out_offset))
      in
      loop f [@nontail]
    ;;

    let read_trunc (offset, width) get =
      let clamped_width = Int.min width I.(num_bits |> to_int_exn) in
      let mask =
        if clamped_width = I.(num_bits |> to_int_exn)
        then I.minus_one
        else I.((one lsl clamped_width) - one)
      in
      let out_word = loop_extract get (offset, clamped_width) (0, I.zero) in
      I.(out_word land mask)
    ;;

    let read (offset, width) get =
      if width > I.(num_bits |> to_int_exn)
      then
        raise_s
          [%message
            "Cannot extract field as int -  too wide" (width : int) (I.num_bits : I.t)];
      read_trunc (offset, width) get
    ;;

    let set (offset, width) packed set_word =
      if width > I.(num_bits |> to_int_exn)
      then
        raise_s
          [%message "Cannot set field as int - too wide" (width : int) (I.num_bits : I.t)];
      loop_set (offset, width) (0, set_word) packed
    ;;
  end

  (* The extract_field_as_* functions are implemented using the read_field_as_* functions
     and using [Array.get] as the getting function. *)
  let convert_read_field_to_extract_field read_field unpacked =
    read_field (fun ~index -> Array.get unpacked index) [@nontail]
  ;;

  module E_int = Extract_field_as (Int)

  let read_field_as_int = X.map bit_positions ~f:E_int.read
  let read_field_as_int_trunc = X.map bit_positions ~f:E_int.read_trunc

  let extract_field_as_int =
    X.map read_field_as_int ~f:convert_read_field_to_extract_field
  ;;

  let extract_field_as_int_trunc =
    X.map read_field_as_int_trunc ~f:convert_read_field_to_extract_field
  ;;

  let set_field_as_int = X.map bit_positions ~f:E_int.set

  module E_int64 = Extract_field_as (Int64)

  let read_field_as_int64 = X.map bit_positions ~f:E_int64.read

  let extract_field_as_int64 =
    X.map read_field_as_int64 ~f:convert_read_field_to_extract_field
  ;;

  let set_field_as_int64 = X.map bit_positions ~f:E_int64.set

  let set_field_as_bytes =
    let module Set_in = Set_in (Int) in
    let set_in_int = Set_in.set_in_int in
    let rec loop (offset, width) (byte_pos, bytes) packed =
      if width <= 0
      then ()
      else (
        let cur_width = Int.min width 8 in
        let start_word = offset / 32 in
        let start_bit_offset = offset land 31 in
        let end_word = (offset + cur_width - 1) / 32 in
        let byte = Bytes.get bytes byte_pos |> Char.to_int in
        if start_word = end_word
        then
          packed.(start_word)
          <- set_in_int
               ~orig:packed.(start_word)
               ~offset:start_bit_offset
               ~width:cur_width
               ~v:byte
        else (
          assert (end_word = start_word + 1);
          let start_byte_bits = 32 - start_bit_offset in
          packed.(start_word)
          <- set_in_int
               ~orig:packed.(start_word)
               ~offset:start_bit_offset
               ~width:start_byte_bits
               ~v:byte;
          let end_byte_bits = cur_width - start_byte_bits in
          packed.(end_word)
          <- set_in_int
               ~orig:packed.(end_word)
               ~offset:0
               ~width:end_byte_bits
               ~v:(byte lsr start_byte_bits));
        loop (offset + 8, width - 8) (byte_pos + 1, bytes) packed)
    in
    X.map bit_positions ~f:(fun (offset, width) packed bytes ->
      assert (Bytes.length bytes = Int.round_up ~to_multiple_of:8 width / 8);
      loop (offset, width) (0, bytes) packed)
  ;;

  let read_field_as_bytes =
    let rec loop (offset, width) b get cur_byte =
      if width <= 0
      then ()
      else (
        let cur_width = Int.min width 8 in
        let cur_bitmask = (1 lsl cur_width) - 1 in
        let start_word = offset / 32 in
        let start_bit_offset = offset land 31 in
        let end_word = (offset + cur_width - 1) / 32 in
        let c =
          (if start_word = end_word
           then
             (* byte is in one word; just pull it out *)
             (get ~index:start_word lsr start_bit_offset) land cur_bitmask
           else (
             assert (end_word = start_word + 1);
             let start_byte_bits = 32 - start_bit_offset in
             let start_byte_bitmask = (1 lsl start_byte_bits) - 1 in
             let start_piece =
               (get ~index:start_word lsr start_bit_offset) land start_byte_bitmask
             in
             let end_byte_bits = cur_width - start_byte_bits in
             let end_byte_bitmask = (1 lsl end_byte_bits) - 1 in
             let end_piece = get ~index:end_word land end_byte_bitmask in
             (end_piece lsl start_byte_bits) lor start_piece))
          |> Char.of_int_exn
        in
        Bytes.set b cur_byte c;
        loop (offset + 8, width - 8) b get (cur_byte + 1))
    in
    X.map bit_positions ~f:(fun (offset, width) get b ->
      assert (8 * Bytes.length b >= width);
      loop (offset, width) b get 0)
  ;;

  (* Cannot use [convert_read_field_to_extract_field] here becuase of [b]. *)
  let extract_field_as_bytes =
    X.map read_field_as_bytes ~f:(fun read unpacked b ->
      read (fun ~index -> unpacked.(index)) b [@nontail])
  ;;

  let read_field_as_string =
    X.map2 X.port_widths read_field_as_bytes ~f:(fun bit_width f ->
      let byte_width = Int.round_up ~to_multiple_of:8 bit_width / 8 in
      let b = Bytes.create byte_width in
      fun i ->
        f i b;
        Bytes.to_string b)
  ;;

  let extract_field_as_string =
    X.map read_field_as_string ~f:convert_read_field_to_extract_field
  ;;

  let set_field_as_string =
    X.map set_field_as_bytes ~f:(fun f i s ->
      let b = Bytes.of_string s in
      f i b)
  ;;

  let of_packed_int_array t =
    X.map extract_field_as_int ~f:(fun extract_fn -> extract_fn t) [@nontail]
  ;;

  let of_packed_int_array_trunc t =
    X.map extract_field_as_int_trunc ~f:(fun extract_fn -> extract_fn t) [@nontail]
  ;;

  let of_packed_int_array_to_int64 t =
    X.map extract_field_as_int64 ~f:(fun extract_fn -> extract_fn t) [@nontail]
  ;;

  let empty_packed_int_array () = Array.create ~len:num_words 0

  let to_packed_int_array unpacked =
    let packed = empty_packed_int_array () in
    X.iter2 set_field_as_int unpacked ~f:(fun set_fn field -> set_fn packed field);
    packed
  ;;
end

module Include = struct
  module type S = sig
    type 'a unpacked

    module Packed : S with type 'a unpacked = 'a unpacked
  end

  module type F = functor (X : Interface.S) -> S with type 'a unpacked := 'a X.t

  module Make (X : sig
      include Interface.S

      val name : string
    end) =
  struct
    module Packed = Make (X)
  end
end

module type Packed_int = Include.S with type 'a unpacked := 'a

let make_int width =
  let module M = struct
    module T = struct
      let name = [%string "int%{width#Int}"]

      include (val Types.scalar ~wave_format:Int ~name width)
    end

    include T

    (* Cannot use include functor here. *)
    module Packed = Make (T)
  end
  in
  (module M : Packed_int)
;;

module Int63 = (val make_int 63)
module Int64 = (val make_int 64)
