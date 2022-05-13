(* solutions to exercises from Chapter 5, "Sorting Things", of
 *   "OCaml from the Very Beginning".
 *)

(* bro, just avoid sorting linked lists!
 * lazy "answers":
 * #1: let binding
 * #2: do they want me to prove that this sorts (no thanks),
 *   or that l / 2 <= l for all l? (QED?)
 * #3: O(n^2) + O(n) = O(n^2), so I'll take fun xs -> List.rev (insertion_sort xs), LOL
 * #4: cheeky mode: fun xs -> xs = sort xs, but I'll write a "real" one
 * #5: it's lexicographic order, but god knows what that means for things like
 *   trees
 * #6: no, LOL (is this just using a let/in for insert?)
 *)

let rec is_sorted = function
  | [] -> true
  | _::[] -> true
  | x::(y::_ as rest) -> x <= y && is_sorted rest
