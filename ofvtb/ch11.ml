(* solutions to exercises from Chapter 11, "Growing Trees", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack:
 *   - I could just smell the endless folds coming, so I wrote a generic
 *       fold for trees and wrote the other functions using it
 *   - I love question #2, because it's the same thing that bitch-ass "homebrew"
 *       programmer melted down over on twitter - how dare some interviewer
 *       ask he, a Programming Fancyman, to perform such an esoteric task as
 *       """inverting""" a binary tree. what a fucking dweeb. sometimes you have
 *       impostor syndrome because you're an imposter. sus.
 *   - The last question is about "rose trees", which I thought might be the
 *       secret answer to a previous exercise.
 *)

type 'a tree =
  | Br of 'a * 'a tree * 'a tree
  | Lf

let rec map f = function
  | Lf -> Lf
  | Br (x, left, right) -> Br (f x, map f left, map f right)

let rec fold f leaf = function
  | Lf -> leaf
  | Br (x, left, right) -> f x (fold f leaf left) (fold f leaf right)

let size tr = fold (fun _ l r -> 1 + l + r) 0 tr
let total tr = fold (fun x l r -> x + l + r) 0 tr
let maxdepth tr = fold (fun _ l r -> 1 + if l > r then l else r) 0 tr
let list_of_tree tr = fold (fun x l r -> l @ [x] @ r) [] tr

(* I could write this with fold, but in a strict language it'd be inefficient:
 *   we want to stop when we find the thing, but fold wants to process the whole
 *   tree - in a lazy language we'd get the "best of both worlds" at the
 *   expense of general confusion.
 *)
let rec bst_lookup k = function
  | Lf -> None
  | Br ((k', v), left, right) ->
      if k = k'
        then Some v
        else if k < k'
          then bst_lookup k left
          else bst_lookup k right

let rec bst_insert k v = function
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v') as entry, left, right) ->
      if k = k'
        then Br ((k, v), left, right)
        else if k < k'
          then Br (entry, bst_insert k v left, right)
          else Br (entry, left, bst_insert k v right)

(* another could-be-a-fold-but-without-laziness-it's-inefficient
 * TODO: read about laziness in OCaml
 *)
let rec member x = function
  | Lf -> false
  | Br (y, left, right) -> x = y || member x left || member x right

(* how hard was that bro *)
let rec mirror tr = fold (fun x l r -> Br (x, r, l)) Lf tr

(* here's a silly way to do it - convert both to () tree ["erase" the data]
 *   and use OCaml's built-in structural equality!
 *)
let same_shape t1 t2 =
  let erase tr = map (fun _ -> ()) tr
   in erase t1 = erase t2

(* here's a more efficient way - again, we need to be lazy! *)
let rec same_shape' t1 t2 = match t1, t2 with
  | Lf, Lf -> true
  | Br (_, l1, r1), Br (_, l2, r2) -> same_shape' l1 l2 && same_shape' r1 r2
  | _, _ -> false

(* have I mentioned I love folds *)
let rec tree_of_dict d = List.fold_left (fun t (k, v) -> bst_insert k v t) Lf d

(* this is called a "rose tree", because have you ever seen a rosebush? one ate
 *   my in-laws backyard and I can vouch that this is its type.
 * there are a few variations on this type, but I like this one.
 *)
type 'a rose_tree =
  RoseBr of 'a * 'a rose_tree list

let rec rose_map f (RoseBr (x, brs)) = RoseBr (f x, List.map (rose_map f) brs)
let rec rose_fold f (RoseBr (x, brs)) = f x (List.map (rose_fold f) brs)

let sum xs = List.fold_left (+) 0 xs

let rose_size r = rose_fold (fun _ brs -> 1 + sum brs)
let rose_total r = rose_fold (fun x brs -> x + sum brs)
