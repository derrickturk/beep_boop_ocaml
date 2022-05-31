open Base

module type Comparable = sig
  type t
  val compare: t -> t -> int
end

module Make_interval (Endpoint: Comparable) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  let create lo hi =
    if Endpoint.compare lo hi > 0
      then Empty
      else Interval (lo, hi)

  let contains i x = match i with
    | Empty -> false
    | Interval (lo, hi) ->
        Endpoint.compare lo x <= 0 && Endpoint.compare x hi <= 0

  (* nitpick: is this really "intersection"? I'd expect that to be
   *   the overlap of the two intervals
   * ohhhhhh wait create is smart and rejects malformed intervals
   * so actually...
   *)
  let intersect i1 i2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match i1, i2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (lo1, hi1), Interval (lo2, hi2) ->
          create (max lo1 lo2) (min hi1 hi2)
end

(* or = Make_interval (Int), using Base.Int *)
module Int_interval = Make_interval (struct
  type t = int
  let compare = Int.compare
end)

(* nope - this functor is "applicative": invoke it twice on the same named
 *   module, get compatible modules back
 *)
module Newtype_Int_interval_1 = Make_interval (Int)
module Newtype_Int_interval_2 = Make_interval (Int)

let naughty: Newtype_Int_interval_2.t = Newtype_Int_interval_1.create 23 45

(* however, applied to an anonymous structure (like the first Int_interval),
 *   it's "generative" - you get a newtype back, more or less
 *)

module Newtype_Int_interval_for_real = Make_interval (struct
  type t = int
  let compare = Int.compare
end)

(* this won't typecheck 
let forbidden: Newtype_Int_interval_for_real.t = Int_interval.create 23 45
*)
