(* solutions to exercises from Chapter 1, "Starting Off", of
 *   "OCaml from the Very Beginning".
 *)

(* fun notes to share on Slack:
 *   - by convention, we use the .ml file extension for Caml source
 *   - (there'll be .mli files later)
 *   - so there's an infix `mod` operator. funnily, languages disagree on what
 *       this should do for negative operands.
 *       OCaml:
 *         23 mod 5 -> 3
 *         23 mod -5 -> 3
 *         -23 mod -5 -> -3
 *       Haskell
 *         23 `mod` 5 -> 3
 *         23 `mod` (-5) -> -2
 *         (-23) `mod` (-5) -> -3
 *       Python
 *         same as Haskell
 *   - comparison operators are polymorphic in OCaml: they work for any two
 *       expressions of the same type, even where it's not obvious what that
 *       should mean (like for user-defined variant types):
 *       ```
 *       # type 'a thing = One of 'a | Two of 'a * 'a;;
 *       # Two (23, 45) < One 17;;
 *       - : bool = false
 *       ```
 *     in Haskell, we get polymorphism subject to constraints with type classes,
 *       so that types can opt-in to having things like < and >, and define what
 *       they mean - basically, the same effect as operator overloading in
 *       OO languages.
 *   - if/then/else is an expression, not a statement - like Rust etc, not like
 *       C and Python. it's equivalent-ish to C's x ? y : z or Python's horrible
 *       y if x else z Yoda-speak.
 *   - Question 1 is arguably a trick question, because they used the horrible
 *       ‘’ quotes instead of God's honest '', so the last couple expressions
 *       don't even parse.
 *)

let () = print_string "no code to write here!\n"
