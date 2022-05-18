(* solutions to exercises from Chapter 15, "The OCaml Standard Library", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack:
 *   - finally.
 *   - I've been using the stdlib the whole time
 *)

let rec concat = function
  | [] -> []
  | xs::rest -> xs @ concat rest

(* tail recursive: yes, but it's horrible and amounts to making the
 *   stack explicit - you're not actually saving anything
 *)

let all_religions_have_some_truth = List.for_all (List.mem true)

let excitement = String.fold_left (fun n c -> if c = '!' then n + 1 else n) 0

let calm_down = String.map (function '!' -> '.' | c -> c)

let concat0 = String.concat ""

let concat0' xs =
  let buf = Buffer.create 1024 in
    List.iter (Buffer.add_string buf) xs;
    Buffer.contents buf

(* yeah we're not using String
 * to use this from the toplevel we have to #require "str";;
 *)
let count_substring needle haystack =
  let pat = Str.regexp_string needle in
  let len = String.length needle in
  let rec go i n =
    try
      let i' = Str.search_forward pat haystack i + len in
      go i' (n + 1)
    with
      Not_found -> n
  in go 0 0
