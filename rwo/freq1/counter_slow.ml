(* the original, slow, implementation of the interface in counter.mli *)

open Base

type t = (string * int) list

let empty = []

let see_line cs l =
  let new_count = match List.Assoc.find ~equal:String.equal cs l with
  | None -> 1
  | Some c -> c + 1
  in List.Assoc.add ~equal:String.equal cs l new_count

let to_list cs = cs
