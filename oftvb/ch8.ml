(* solutions to exercises from Chapter 8, "Looking Things Up", of
 *   "OCaml from the Very Beginning".
 *)

(* Slack notes:
 * - hey, remember my answer to the last question in Chapter 7? this is one
 *     one of those cases where I would never use exceptions!
 * - obviously this is a super inefficient structure, but it does get used in
 *     practice, especially for prototype or quick-n-dirty code (I use them a
 *     lot in Advent of Code). it goes back at least to Lisp, where the
 *     community calls them "alists" (association lists).
 * - I love `as`-patterns, which let me [partially] destructure a thing but
 *     also bind a name to the whole thing. it comes up more than you'd guess.
 *)

(* we're using options here *)
let rec lookup x = function
  | [] -> None
  | (k, v)::rest -> if k == x then Some v else lookup x rest

(* the grammar says I can have my beloved x' "x prime" style names from Haskell
 *   so idiom be damned I'm using them. (in Haskell we tend to use them for
 *   things like "updated x", "other key and other value" (like below).
 *)
let rec insert k v = function
  | [] -> [(k, v)]
  | (k', _) as head::rest ->
      if k = k' then (k', v)::rest else head::insert k v rest

let rec delete k = function
  | [] -> []
  | (k', _) as head::rest ->
      if k = k' then rest else head::delete k rest

(* OK, I went maximum effort on rewriting the chapter code, so expect short
 * cheeky answers. We haven't introduced modules yet, so we can't enforce this,
 * but I'm assuming the invariant that all "lookup dictionaries" are 
 * created by repeated application of `insert` and `delete`. *)
let dictionary_size = List.length

(* inefficient, but so are alists in general, so who cares *)
let replace k v d = match lookup k d with
  | None -> raise Not_found
  | Some _ -> insert k v d

(* oh wow, there's no List.zip? bollocks. *)
let rec strict_zip ks vs = match ks, vs with
  | [], [] -> []
  | x::xs, y::ys -> (x, y)::strict_zip xs ys
  | _, [] -> raise (Invalid_argument "more keys than values")
  | [], _ -> raise (Invalid_argument "more values than keys")

let rec unzip = function
  | [] -> ([], [])
  | (x, y)::rest -> let xs, ys = unzip rest in (x::xs, y::ys)

(* did I talk about folds yet? it gets real damn old writing the same
 *   recursive function 17,000 times.
 * since I chose a left fold, rightmost v for the same k wins.
 *)
let dictify xs = List.fold_left (fun d (k, v) -> insert k v d) [] xs

(* yeah pretty much the same thing in a way. gotta fold right to get the
 *   left-preference thing
 *)
let union a b = List.fold_right (fun (k, v) d -> insert k v d) a b
