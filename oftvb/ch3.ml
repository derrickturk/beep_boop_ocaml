(* solutions to exercises from Chapter 3, "Case by Case", of
 *   "OCaml from the Very Beginning".
 *)

(* Slack notes:
 *   - lots of ways to skin several cats: we have a few function syntaxes
 *       (direct definition by pattern match e.g. f x = expr, "lambda" with
 *       `fun`, anonymous function value with `function` which permits
 *       pattern matching against implicit arguments)
 *)

let not = function
  | true -> false
  | false -> true

(* in the absence of pattern guards, I refuse to use pattern-matching here
 *   for philosophical reasons: I can't write a pattern for "positive integers",
 *   only for specific values
 *)
let sum_up_to n = if n <= 0 then 0 else n + sum_up_to (n - 1)

exception Negative_power_of_int

let power x n =
  let rec real_power x = function
    | 0 -> 1
    | n -> x * real_power x (n - 1)
   in if n < 0 then raise Negative_power_of_int else real_power x n

(* I have chosen to write them in the most readable way, no further comment. *)

(* No, I won't contribute to American cultural chauvinism like that,
 *   I'm very woke. I have renamed the offending functions.
 *)
let is_ascii_lowercase = function
  | 'a'..'z' -> true
  | _ -> false

let is_ascii_uppercase = function
  | 'A'..'Z' -> true
  | _ -> false
