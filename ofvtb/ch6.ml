(* solutions to exercises from Chapter 6, "Functions...", of
 *   "OCaml from the Very Beginning".
 *)

let rec calm: char list -> char list = function
  | [] -> []
  | '!'::rest -> '.'::calm rest
  | c::rest -> c::calm rest

let rec calm_map: char list -> char list =
  List.map (fun c -> if c = '!' then '.' else c)

let clip x =
  if x < 1
    then 1
    else if x > 10
      then 10
      else x

let cliplist = List.map clip

let clipanon = List.map (fun x -> x |> max 1 |> min 10)

let rec apply f n x =
  if n <= 0
    then x
    else f (apply f (n - 1) x)

let rec sort cmp =
  let rec insert cmp x = function
      | [] -> [x]
      | y::ys -> if cmp x y then x::y::ys else y::insert cmp x ys
   in function
     | [] -> []
     | x::xs -> insert cmp x (sort cmp xs)

let rec filter p = function
  | [] -> []
  | x::xs -> if p x then x::filter p xs else filter p xs

(* this is useful when you want to know if a condition holds for all
 *   elements of a list... lots of examples... say, typechecking
 *   a list literal by checking the inferred type of every element
 *   matches the declared type, or whatever.
 *)
let rec for_all p = function
  | [] -> true
  | x::xs -> p x && for_all p xs

(* in Haskell we have great fun by composing the `fmap` function to
 *   map over arbitrary levels of functorial structure - this guy would
 *   be fmap . fmap
 *)
let mapl f = List.map (List.map f);;
