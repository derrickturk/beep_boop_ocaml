(* a faster implementation of the Counter interface *)

open Base

type t = int Map.M(String).t

let empty = Map.empty (module String)

let see_line cs l =
  let new_count = match Map.find cs l with
  | None -> 1
  | Some c -> c + 1
  in Map.set cs ~key:l ~data:new_count

let to_list cs = Map.to_alist cs

type median =
  | Median of string
  | Before_and_after of string * string

let median cs =
  let sorted = List.sort (to_list cs)
    ~compare:(fun (_, a) (_, b) -> Int.descending a b) in
  let len = List.length sorted in
  let nth n = fst (List.nth_exn sorted n) in
  if len = 0
    then None
    else Some begin
      if len % 2 = 1
        then Median (nth (len / 2))
        else Before_and_after (nth (len / 2 - 1), nth (len / 2))
    end
