#require "ppx_jane";; (* what's the equivalent of this for a compiled file? *)

open Base

module type Comparable = sig
  type t
  val compare: t -> t -> int
end

module type Interval = sig
  type t
  type endpoint
  val create: endpoint -> endpoint -> t
  val is_empty: t -> bool
  val contains: t -> endpoint -> bool
  val intersect: t -> t -> t
end

module Make_interval (Endpoint: sig
  (* this is a common idiom for, er, "multiple inheritance":
   *   combine inclusion with destructive substitution
   *)
  type t
  include Comparable with type t := t
  include Sexpable.S with type t := t
  end)
  : (sig (* this one's a little tricky, because of the double substitution *)
       type t
       include Interval with type t := t
       include Sexpable.S with type t := t
    end with type endpoint := Endpoint.t) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty
  [@@deriving sexp]

  let create lo hi =
    if Endpoint.compare lo hi > 0
      then Empty
      else Interval (lo, hi)

  (* recheck invariant *)
  let t_of_sexp sexp = match t_of_sexp sexp with
    | Empty -> Empty
    | Interval (lo, hi) -> create lo hi

  let is_empty = function
    | Empty -> true
    | _ -> false

  let contains i x = match i with
    | Empty -> false
    | Interval (lo, hi) ->
        Endpoint.compare lo x <= 0 && Endpoint.compare x hi <= 0

  let intersect i1 i2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match i1, i2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (lo1, hi1), Interval (lo2, hi2) ->
          create (max lo1 lo2) (min hi1 hi2)
end

module Int_interval = Make_interval (Int)
module String_interval = Make_interval (String)
