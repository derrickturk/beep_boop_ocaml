(* solutions to exercises from Chapter 2, "Names and Functions", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack
 *   - notice the difference between let ... in ... and a top-level let
 *   - when *exactly* does OCaml need parens around negative integer literals?
 *       23 mod -5 parses correctly
 *       neg -30 parses as neg - 30 -> type error
 *   - they're back with the ‘’ quotes, better not copy and paste
 *   - I really want to talk about the <<< mysterious secret of
 *       multi-argument function types >>>, but I'll wait for the book
 *       to make the big reveal, I guess
 *   - you can load the definitions from a file like this into utop
 *       with #use "ch2.ml";;
 *)

(* this is a weird way to write these, because it's uncommon in OCaml to
 *   annotate function defintions with types, but I want them to sort-of
 *   "prove" that I've done the exercise correctly ("what is its type?").
 * this struck me as odd coming from Haskell, but the OCamlites aren't total
 *   barbarians: we'll see their idiom for the same purpose when we get to
 *   those odd .mli files.
 * anyway, there are lots of ways to skin this cat, even if they're not
 *   idiomatic - I've chosen one I think feels clean (because it preserves
 *   the definition in terms of one pattern match)...
 *)
let times_ten (x: int): int = x * 10

(* this would work too, and maybe I even like it better
 *   (btw no //-style line comments!): *)
let times_ten_also: int -> int = fun x -> x * 10

(* btw in Haskell this could be (using "operator sections" i.e. partial
 *   application for operators):
 *   times_ten :: Int -> Int
 *   times_ten = ( * 10) -- space so OCaml doesn't see a nested comment!
 *)

let both_nz (x: int) (y: int): bool = x <> 0 && y <> 0

let rec sum_up_to (x: int) : int =
  if x <= 0
    then 0
    else x + sum_up_to (x - 1)

(* OK, I am jumping ahead, but I want to do something intelligent here for
 *   the negative-power case.
 * honestly, taking a natural / non-negative integer argument would be better,
 *   but practically we would probably either throw an exception (under the
 *   logic that this probably reflects programmer error, and thus should blow up
 *   so we find and fix the error quickly), or use a return type like
 *   int option (if we expect to frequently get negative inputs as part of
 *     "normal" operation).
 * BTW OCaml exceptions are really interesting from a type systems viewpoint
 *   (more on that later, I hope!)
 *)
exception Negative_power_of_int

let rec power (x: int) (n: int): int =
  if n < 0
    then raise Negative_power_of_int
    else if n == 0
      then 1
      else x * power x (n - 1)

(* this is a really, really dumb joke, don't write code like this *)
let is_consonant c =
  let _ = Random.self_init ()
  and sometimes = Random.full_int 10 = 0
   in not (
     c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' || sometimes && c = 'y'
   )

(* #6: lexical scoping is real! (and it's nice to see a shadowing warning in
*   the form of "you never used the outer x")
*)

(* same discussion as for power, above. I know the answer they want at this
 *   point, which is to sort of smudge the domain:
 *)
let rec kinder_gentler_factorial n =
  if n <= 0 then 1 else n * kinder_gentler_factorial (n - 1)

(* but I'd probably do it like this under "practical" constraints *)
exception Factorial_of_negative

let rec harsher_stricter_factorial n =
  if n < 0 then raise Factorial_of_negative
   else if n == 0
     then 1
     else n * harsher_stricter_factorial (n - 1)
