(* solutions to exercises from Chapter 7, "When Things Go Wrong", of
 *   "OCaml from the Very Beginning".
 *)

let smallest xs =
  let rec go smallest vals = match smallest, vals with
    | None, [] -> raise Not_found
    | Some n, [] -> n
    | None, x::xs -> if x > 0 then go (Some x) xs else go None xs
    | Some n, x::xs -> if x >0 && x < n then go (Some x) xs else go (Some n) xs
   in go None xs

let smallest_or_zero xs = try smallest xs with Not_found -> 0

exception Sqrt_of_negative of int

(* you can write this with f (g (h x)), or with f @@ g @@ h x,
 *   or define your own composition operator, but this seems
 *   to be a popular and attractive style, even if it's a little
 *   "point"-y to my Haskellian eyes
 *
 * btw this is the answer to #4, not #3 - int_of_float nan = 0,
 *   which is a Choice
 *)
let int_sqrt x = x |> float_of_int |> sqrt |> int_of_float

(* this, then, is the answer to #3 *)
let checked_int_sqrt x = if x < 0
  then raise (Sqrt_of_negative x)
  else int_sqrt x

(* so, #5, open-ended. exceptions suck because they're non-local control flow
 *   (like GOTO - or really more like "COME FROM"!) and because they generally
 *   exist outside the type system, or at least in some parallel "shadow" type
 *   system.
 * old-school sentinel values suck because they steal an element of the range
 *   to signal error, but sometimes you need the whole range! arbitrary
 *   sentinels are ugly and easy to mis-check - does -1 mean an error, or any
 *   negative return value? etc etc.
 * the solution to the above is to use algebraic types - using something like
 *   `option` or `Either` to augment the range without sacrificing
 *   usable values.
 * that said, some errors are so infrequent or intrinsically "un-handleable"
 *   (think: out-of-memory, CPU on fire) that it doesn't make much sense
 *   to force the programmer to explicitly handle them (e.g. by returning
 *   an option) every time. for those cases, exceptions are better suited,
 *   although I lean toward "crash-only" handling in those cases (like Rust's
 *   panic handling) rather than complex webs of exception handlers.
 * as a final note, OCaml's exception type is really cool, in that it's an open
 *   sum type.
 *)
