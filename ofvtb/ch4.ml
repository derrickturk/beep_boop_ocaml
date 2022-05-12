(* solutions to exercises from Chapter 4, "Making Lists", of
 *   "OCaml from the Very Beginning".
 *)

(* Slack notes:
 *   - Haskell's attack on my brain has left me scarred and deformed
 *   - I love tail recursion
 *   - really we should write some of these things as folds i.e. use
 *       library (higher-order) functions
 *   - I also hate naming arguments I don't have to name
 *   - I'm holding back on the true power of point-free style
 *   - seriously at some point I will define a function composition operator
 *   - I'm also not deploying the pipe/application and right-associative
 *       application operators I know OCaml has
 *   - you're welcome
 *   - I need to read about weak type variables, because I accidentally made one
 *       with my original (more point-free) definition of all_but_last
 *)

let rec odds = function
  | [] -> []
  | x::xs -> x::evens xs
    and evens = function
  | [] -> []
  | _::xs -> odds xs

let count_true =
  let rec go n = function
    | [] -> n
    | true::xs -> go (n + 1) xs
    | _::xs -> go n xs
   in go 0

(* bro, I literally CAN'T assume `rev`, at least as a global *)
let make_palindromic xs = xs @ List.rev xs

let is_palindromic xs = xs = List.rev xs

(* tail recursive but still not super efficient, we accumulate in reverse:
 *   a common pattern in this kind of code
 *)
let all_but_last xs =
  let rec go acc = function
    | [] -> List.rev acc
    | _::[] -> List.rev acc
    | x::xs -> go (x::acc) xs
   in go [] xs

(* this was my original take, which ends up with a weak type variable *)
let all_but_last_weak_hmm =
  let rec go acc = function
    | [] -> List.rev acc
    | _::[] -> List.rev acc
    | x::xs -> go (x::acc) xs
   in go []

let rec member x = function
  | [] -> false
  | y::ys -> if x = y then true else member x ys

let unique xs =
  let rec go seen = function
    | [] -> seen
    | x::xs -> if member x seen then go seen xs else go (x::seen) xs
   in go [] xs

(* rev sucks because it's quadratic in time, because it calls @, and also
 *   because it's linear-ish in space, because it's not tail-recursive.
 * same weak-type-variable thing here if I go point-free, btw
 *)
let better_rev xs =
  let rec go acc = function
    | [] -> acc
    | x::xs -> go (x::acc) xs
   in go [] xs
