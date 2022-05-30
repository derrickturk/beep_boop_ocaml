(* solutions to exercises from Chapter 10, "New Kinds of Data", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack
 *   - insert <finally, some good fucking food> here
 *   - OCaml doesn't have significant whitespace, unlike Haskell (!)
 *       so there's an little ambiguity when you nest matches - AFAICT,
 *       the solution is to enclose the inner match in parens
 *   - fun fact: begin x y z end is exactly the same as (x y z) so you can
 *       use whichever feels better for the situation
 *   - option is hard to use without some "applicative" and "monadic"
 *       combinators e.g. liftA2 (+) x y adds x and y if both are Some, and
 *       produces None if either is None. in Haskell these are important enough
 *       we give them operators and even special syntax (for monadic bind).
 *       OCaml's type system doesn't let us easily abstract over "all
 *       applicative functors", so I'm not sure if these exist in type-specific
 *       forms or not at all in the standard library - in any case, I couldn't
 *       find a "lifted" function application for 'a option, so I wrote one.
 *)

type rect =
  | Square of int
  | Rect of int * int

let area = function
  | Square n -> n * n
  | Rect (h, w) -> h * w

let orient = function
  | Square _ as s -> s
  | Rect (h, w) as r -> if h < w then Rect (w, h) else r

let height = function
  | Square n -> n
  | Rect (h, _) -> h

let width = function
  | Square n -> n
  | Rect (_, w) -> w

let uhhh_can_I_get_uhhh_borger rects = rects
  |> List.map orient
  |> List.sort (fun r1 r2 -> width r1 - width r2)

type 'a seq =
  | Nil
  | Cons of 'a * 'a seq

let rec take n xs =
  if n < 0
    then raise (Invalid_argument "negative count")
    else match n, xs with
      | _, Nil -> Nil
      | 0, _ -> Nil
      | _, Cons (x, xs) -> Cons (x, take (n - 1) xs)

let rec drop n xs =
  if n < 0
    then raise (Invalid_argument "negative count")
    else match n, xs with
      | _, Nil -> Nil
      | 0, _ -> xs
      | _, Cons (_, xs) -> drop (n - 1) xs

let rec map f = function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

type expr =
  | Lit of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

(* is this in the stdlib? *)
let liftO2 f o1 o2 = match o1, o2 with
  | None, _ -> None
  | _, None -> None
  | Some v1, Some v2 -> Some (f v1 v2)

(* from before *)
let rec power x n =
  if n < 0
    then raise (Invalid_argument "negative power")
    else if n = 0
      then 1
      else x * power x (n - 1)

let rec eval = function
  | Lit n -> Some n
  | Add (x, y) -> liftO2 (+) (eval x) (eval y)
  | Sub (x, y) -> liftO2 (-) (eval x) (eval y)
  | Mul (x, y) -> liftO2 ( * ) (eval x) (eval y)
  | Div (x, y) -> begin
      match eval y with (* here's that ambiguity, remember, begin/end = (/) *)
        | None -> None
        | Some 0 -> None
        | y' -> liftO2 (/) (eval x) y'
      end
  | Pow (x, y) ->
      match eval y with
        | Some y' when y' < 0 -> None
        | y' -> liftO2 power (eval x) y'
