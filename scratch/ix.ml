type zero = |
type 'n succ = |

type _ fin =
  | Fzero: ('n succ) fin
  | Fsucc: 'n fin -> 'n succ fin

type (_, _) vec =
  | VNil: (zero, 'a) vec
  | VCons: 'a * ('n, 'a) vec -> ('n succ, 'a) vec

(* a guaranteed version of list indexing - the index must be less than the
 *   length of the list!
 *)
(* nth: 'n fin -> ('n, 'a) vec -> 'a *)
let rec nth: type n. n fin -> (n, 'a) vec -> 'a = fun n xs -> match n, xs with
  | Fzero, VCons(hd, _) -> hd
  | Fsucc n, VCons(_, tl) -> nth n tl
  (* "refutation case" - we're saying this is impossible! (can you see why?)
   *   the compiler is actually smart enough to realize this match is
   *   already exhaustive, but this way we'll catch it if we ever break
   *   that invariant.
   *)
  | _, _ -> . 

let ex = nth (Fsucc (Fsucc Fzero)) (VCons (3, VCons (2, VCons (1, VNil))))
(* won't typecheck!
let bad = nth (Fsucc (Fsucc (Fsucc Fzero))) (VCons (3, VCons (2, VCons (1, VNil))))
*)

(* left to the reader: how the hell would we write `take`? *)

module type Mnat = sig
  type n
end

let rec to_nat: type n. n fin -> (module Mnat) = function
  | Fzero -> (module struct type n = zero end: Mnat)
  | Fsucc n ->
      let (module N: Mnat) = to_nat n in
      (module struct type n = N.n succ end: Mnat)

(* existential types, the hard way *)
module type SomeVec = sig
  type n
  type a
  val v: (n, a) vec
end

let rec take: type n b.
              (n succ) fin
           -> (n, b) vec
           -> (module SomeVec with type a = b) =
  fun n xs -> match n, xs with
    | Fzero, _ -> (module struct
          type n = zero
          type a = b
          let v = VNil
        end: SomeVec with type a = b)
    | Fsucc n, VCons (x, xs) ->
        let (module M: SomeVec with type a = b) = take n xs in
        (module struct
          type n = M.n succ
          type a = b
          let v = VCons (x, M.v)
        end: SomeVec with type a = b)
    | Fsucc _, VNil -> . (* impossible! *)

(* so we can "show" an arbitrary (n, a) vec *)
let rec to_list: type n. (n, 'a) vec -> 'a list = function
  | VNil -> []
  | VCons (hd, tl) -> hd::to_list tl

let good =
  let (module M: SomeVec with type a = int) =
    take (Fsucc (Fsucc Fzero)) (VCons (1, VCons (2, VCons (3, VNil))))
  in
    (* inside here, we know that M.v: (zero succ succ, int) vec *)
  to_list M.v

(* won't compile - not enough to take *)
let bad =
  let (module M: SomeVec with type a = int) =
    take (Fsucc (Fsucc Fzero)) (VCons (1, VNil))
  in
  to_list M.v
