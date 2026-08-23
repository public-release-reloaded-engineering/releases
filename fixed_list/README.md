# Fixed_list

`Fixed_list` provides lists whose length is tracked in the OCaml type system. This lets APIs express and preserve length relationships statically: for example, `map2` and `zip` only accept lists of the same length, and mapping over a fixed list returns a fixed list of the same length.

## Overview

A `('a, 'n) Fixed_list.t` represents a list of elements of type `'a`, of length `'n`. The length parameter is encoded using Peano numbers; for example `[]` is of type `('a, zero) Fixed_list.t`, and `[1; 2]` is of type `(int, zero succ succ) Fixed_list.t`. `_ Fixed_list.t` overrides list constructors, such that we can construct it like any other list:

```ocaml
Fixed_list.map [1; 2] ~f:Int.to_string;;
(* ["1"; "2"] : (string, zero succ succ) Fixed_list.t *)
```

Common operations that preserve the list length are marked as so in the type system:

```ocaml
val rev : ('a, 'n) t -> ('a, 'n) t
val map : ('a, 'n) t -> f:('a -> 'b) -> ('b, 'n) t
val sort : ('a, 'n) t -> compare:('a -> 'a -> int) -> ('a, 'n) t
```

Operations on non-empty lists take a `(_, _ succ) Fixed_list.t`:

```ocaml
val hd : ('a, _ succ) t -> 'a
val tl : ('a, 'n succ) t -> ('a, 'n) t
val reduce : ('a, 'n succ) t -> f:('a -> 'a -> 'a) -> 'a
```

If it's known that the two lists are of equal length, we can zip them safely:

```ocaml
val zip : ('a, 'n) t -> ('b, 'n) t -> ('a * 'b, 'n) t
val map2 : ('a, 'n) t -> ('b, 'n) t -> f:('a -> 'b -> 'c) -> ('c, 'n) t
```

For more advanced use, one can carry around a `_ Sum.t` value that is a witness for two lengths adding up to another; see the library interface for more details.

```ocaml
val append
  :  ('a, 'first) t
  -> ('a, 'second) t
  -> proof:('second, 'first, 'sum) Sum.t
  -> ('a, 'sum) t
```

Support for persisting the list length across monads is included too in the `Monad_sequence` module:

```ocaml
module Monad_sequence : sig
  module Sequential : sig
    module Make (M : sig
        type 'a t

        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
      end) : sig
      val map : ('a, 'n) t -> f:('a -> 'b M.t) -> ('b, 'n) t M.t
    end
  end
end
```

## Examples

Preserving the correspondence between a batch of inputs and a batch of outputs:

```ocaml
let resolve_all (queries : (Query.t, 'n) Fixed_list.t)
  : (Answer.t, 'n) Fixed_list.t Deferred.t
  =
  let module M = Fixed_list.Monad_sequence.Parallel.Make (Deferred) in
  M.map queries ~f:resolve_one
;;

let%map answers = resolve_all queries in
Fixed_list.zip queries answers
```
