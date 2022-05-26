(* solutions to exercises from Chapter 6, "Functions...", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack
 *   we need to talk about folds!
 *   folds, catamorphisms if you want to be fancy (please don't be fancy),
 *     are higher-order functions which recurse over a structure and
 *     "condense" it to some new value
 *   sometimes "fold" is spelled "reduce"
 *   I like to think of the natural fold for a data structure that plugs in
 *       arbitrary functions instead of the data constructors
 *   like the "canonical" right fold on a list:
 *   ```
 *   let rec foldr f xs init = match xs with
 *     | [] -> init
 *     | x::xs -> f x (foldr f xs init)
 *   ```
 *   from this fold, we can recover the original list, by using [] and ::
 *     as init and f:
 *   ``` 
 *   utop # foldr (fun x xs -> x::xs) [1;2;3;] [];;
 *   - : int list = [1; 2; 3]*
 *   ```
 *   we can get a sum, by using 0 and (+):
 *   ```
 *   utop # foldr (+) [1;2;3;] 0;;
 *   - : int = 6
 *   ```
 *   and so on. in fact, pretty much any function which processes a list
 *     recursively and makes a result can be written as a fold.
 *   there's also a "left fold", which builds up results from the other
 *     direction, and can be more efficient, because it can be tail recursive.
 *   ```
 *   let rec foldl f init xs = match xs with
 *     | [] -> init
 *     | x::xs -> foldl f (f init x) xs
 *   ```
 *   amusingly, foldl can be written in terms of foldr, and vice versa. (because
 *     foldr is in a sense the "canonical" fold on lists, it can do "anything").
 *   I tend to use foldl more than foldr, in practice.
 *   you can write folds ("catamorphisms") for all kinds of types, it's a whole
 *     thing. keywords: Bird and Meertens, bananas, lenses, barbed wire,
 *     catamorphism, anamorphism, histomorphism, zygomorphism,
 *     recursion schemes - there's a whole deranged cult of people who think
 *     all software should be written with fold (and unfold, and pals)
 *     combinators.
 *   anyway I mention all this because I got tired of writing "the same"
 *     recursive list processing in these exercises, and in the future I'll
 *     probably just reach for List.left_fold or List.right_fold when I see
 *     an opportunity. (I'll probably also include some examples later in my
 *     answers here which use folds to do the same thing.)
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

(* fun with the pipe! (personally I prefer composition operators, but OCaml
 *   doesn't have a standard one, and there are some subtle reasons I don't
 *   understand yet that it causes mild type havoc)
 *)
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

let filter_fold p =
  List.fold_left (fun ys y -> if p y then y::ys else ys) []

(* this is useful when you want to know if a condition holds for all
 *   elements of a list... lots of examples... say, typechecking
 *   a list literal by checking the inferred type of every element
 *   matches the declared type, or whatever.
 *)
let rec for_all p = function
  | [] -> true
  | x::xs -> p x && for_all p xs

let forall_fold p = List.fold_left (fun b x -> b && p x) true

(* in Haskell we have great fun by composing the `fmap` function to
 *   map over arbitrary levels of functorial structure - this guy would
 *   be fmap . fmap
 *)
let mapl f = List.map (List.map f)
