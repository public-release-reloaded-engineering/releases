open! Core
include Arithmetic_intf
include Defs
include Aliases

module Sum = struct
  module type S = sig
    type a
    type 'p a1
    type b
    type sum

    val a1 : (a, zero a1) Type_equal.t
    val exchange : ('p succ a1, 'p a1 succ) Type_equal.t
    val sum : (b a1, sum) Type_equal.t
  end

  type ('a, 'b, 'sum) t = (module S with type a = 'a and type b = 'b and type sum = 'sum)

  let empty (type sum) : (zero, sum, sum) t =
    (module struct
      type a = zero
      type 'p a1 = 'p
      type b = sum
      type nonrec sum = sum

      let (a1 : (a, zero a1) Type_equal.t) = T
      let exchange (type p) : (p succ a1, p a1 succ) Type_equal.t = T
      let (sum : (b a1, sum) Type_equal.t) = T
    end)
  ;;

  module Lift_succ = Type_equal.Lift (struct
      type 'a t = 'a succ
    end)

  let shift (type a b sum) ((module M) : (a, b succ, sum) t) : (a succ, b, sum) t =
    (module struct
      type 'p a1 = 'p succ M.a1
      type a = M.a succ
      type nonrec b = b
      type nonrec sum = sum

      let (a1 : (a, zero a1) Type_equal.t) =
        Type_equal.trans (Lift_succ.lift M.a1) (Type_equal.sym M.exchange)
      ;;

      let exchange (type p) : (p succ a1, p a1 succ) Type_equal.t = M.exchange
      let (sum : (b a1, sum) Type_equal.t) = M.sum
    end)
  ;;

  let incr_first (type a b sum) ((module M) : (a, b, sum) t) : (a succ, b, sum succ) t =
    (module struct
      type 'p a1 = 'p succ M.a1
      type nonrec a = a succ
      type nonrec b = b
      type nonrec sum = sum succ

      let (a1 : (a, zero a1) Type_equal.t) =
        Type_equal.trans (Lift_succ.lift M.a1) (Type_equal.sym M.exchange)
      ;;

      let exchange (type p) : (p succ a1, p a1 succ) Type_equal.t = M.exchange

      let (sum : (b a1, sum) Type_equal.t) =
        Type_equal.trans M.exchange (Lift_succ.lift M.sum)
      ;;
    end)
  ;;

  let incr_second (type a b sum) ((module M) : (a, b, sum) t) : (a, b succ, sum succ) t =
    (module struct
      type 'p a1 = 'p M.a1
      type nonrec a = a
      type nonrec b = b succ
      type nonrec sum = sum succ

      let (a1 : (a, zero a1) Type_equal.t) = M.a1
      let exchange (type p) : (p succ a1, p a1 succ) Type_equal.t = M.exchange

      let (sum : (b a1, sum) Type_equal.t) =
        Type_equal.trans M.exchange (Lift_succ.lift M.sum)
      ;;
    end)
  ;;

  let decr_second (type a b sum) ((module M) : (a, b succ, sum succ) t) : (a, b, sum) t =
    (module struct
      type 'p a1 = 'p M.a1
      type nonrec a = a
      type nonrec b = b
      type nonrec sum = sum

      let (a1 : (a, zero a1) Type_equal.t) = M.a1
      let exchange (type p) : (p succ a1, p a1 succ) Type_equal.t = M.exchange

      let (sum' : (b a1 succ, sum succ) Type_equal.t) =
        Type_equal.trans (Type_equal.sym M.exchange) M.sum
      ;;

      let (sum : (b a1, sum) Type_equal.t) =
        let T = sum' in
        T
      ;;
    end)
  ;;

  let finalize (type a sum) ((module M) : (a, zero, sum) t) : (a, sum) Type_equal.t =
    Type_equal.trans M.a1 M.sum
  ;;

  let absurd (type a b) ((module M) : (a, b succ, zero) t) =
    match Type_equal.trans (Type_equal.sym M.exchange) M.sum with
    | _ -> .
  ;;
end

module Sums = struct
  open Sum

  let left0 = empty
  let left1 () = incr_first left0
  let left2 () = left1 () |> incr_first
  let left3 () = left2 () |> incr_first
  let left4 () = left3 () |> incr_first
  let left5 () = left4 () |> incr_first
  let left6 () = left5 () |> incr_first
  let left7 () = left6 () |> incr_first
  let left8 () = left7 () |> incr_first
  let left9 () = left8 () |> incr_first
  let left10 () = left9 () |> incr_first
end
