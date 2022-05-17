(* solutions to exercises from Chapter 14, "The Other Numbers", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack:
 *   - imperative features make this `plot` function easy to write,
 *       but that's not really a good thing - I'd prefer to write it more
 *       functionally rather than intertwingle I/O and layout calculation.
 *   - Float.pi is a thing
 *)

(* I'm lazy and rounding is subtle *)
let round = Float.round

let midpoint (x0, y0) (x1, y1) =
  (x0 + (x1 - x0) / 2, y0 + (y1 - y0) / 2)

(* or whatever, this is probably not ideal for negative values,
 *   just use the built-in *)
let parts x = let whole = floor x in (whole, x -. whole)

(* no, this is dumb *)
let star_unit_50 x =
  let n = int_of_float (x *. 49.) in
    print_string (String.make n ' ');
    print_endline "*"

let plot f l u step =
  let x = ref l in
    while !x <= u do
      star_unit_50 (f !x);
      x := !x +. step
    done
