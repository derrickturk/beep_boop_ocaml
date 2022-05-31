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

module Make_interval (Endpoint: Comparable)
  (* sharing constraint: expose equality between types in interface *)
  : (Interval with type endpoint = Endpoint.t) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  type endpoint = Endpoint.t

  let create lo hi =
    if Endpoint.compare lo hi > 0
      then Empty
      else Interval (lo, hi)

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

module Make_interval_2 (Endpoint: Comparable)
  (* destructive substitution: replace definition in interface,
   *   "un-naming" it
   *)
  : (Interval with type endpoint := Endpoint.t) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  let create lo hi =
    if Endpoint.compare lo hi > 0
      then Empty
      else Interval (lo, hi)

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

module Int_interval_2 = Make_interval_2 (Int)
module String_interval_2 = Make_interval_2 (String)
