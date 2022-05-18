(* solutions to exercises from Chapter 9, "More With Functions", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack
 *   - the process of turning functions of multiple arguments into functions
 *     returning functions, taking one argument each, is called "currying",
 *     after Haskell Curry (hey, I know that name!)
 *   - we can do it automatically:
 *   ```
 *   # let curry f x y = f (x, y);;
 *   val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
 *   ```
 *   - the reverse operation, turning a "curried" function back into one
 *     taking multiple arguments as a tuple, is called "uncurrying":
 *   ```
 *   # let uncurry f (x, y) = f x y;;
 *   val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun> 
 *   ```
 *)

(* #1: g: 'a -> 'b -> 'c -> 'd = 'a -> ('b -> ('c -> 'd)) - need I go on? *)

let member: 'a -> 'a list -> bool =
  fun x -> List.fold_left (fun b y -> b || x = y) false

let member_3: int list -> bool = member 3

(* no interesting reason, just argument order - (/) x y = x / y
 * in Haskell I'd spell this `flip div`
 * where flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
 *)
let divided_by denom num = num / denom

let half_em = List.map (divided_by 2)

(* #4: I wrote the function yesterday and I don't understand what the hell the
 *   second part means. my interpretation: no because types. (you either take a
 *   'a list, an 'a list list, etc etc - if you want me to write a rose tree
 *   type and a map over it, I can do that...?)
 *)

(* frankly I'd call the per-list one truncate and wouldn't name the mapped
 *   one at all...
 *)
let rec truncate n xs =
  if n < 0
    then raise (Invalid_argument "negative length")
    else match n, xs with
      | 0, _ -> []
      | _, [] -> []
      | _, x::xs -> x::truncate (n - 1) xs

let truncate_all n = List.map (truncate n)

let heads_or_default (n: int) = List.map (function [] -> n | x::_ -> x)
