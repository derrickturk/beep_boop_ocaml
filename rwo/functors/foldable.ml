open Base

(* "S" for "Signature" - this is a (Base? OCaml?) convention *)
module type S = sig
  type 'a t
  val fold: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module type FoldableExt = sig
  type 'a t
  val iter: 'a t -> f:('a -> unit) -> unit
  val length: 'a t -> int
  val count: 'a t -> f:('a -> bool) -> int
  val for_all: 'a t -> f:('a -> bool) -> bool
  val exists: 'a t -> f:('a -> bool) -> bool
end

module MakeFoldable (F: S): FoldableExt with type 'a t := 'a F.t = struct
  open F
  let iter t ~f = fold t ~init:() ~f:(fun _ x -> f x)
  let length t = fold t ~init:0 ~f:(fun n _ -> n + 1)
  let count t ~f = fold t ~init:0 ~f:(fun n x -> if f x then n + 1 else n)

  exception Short_circuit (* clever... I guess :/ *)

  let for_all t ~f =
    try iter t ~f:(fun x -> if not (f x) then raise Short_circuit); true
    with Short_circuit -> false

  let exists t ~f =
    try iter t ~f:(fun x -> if f x then raise Short_circuit); false
    with Short_circuit -> true
end
