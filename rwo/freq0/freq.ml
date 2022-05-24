open Base
open Stdio

let read_counts () =
  let see_line cs l =
    let new_count = match List.Assoc.find ~equal:String.equal cs l with
    | None -> 1
    | Some c -> c + 1
    in List.Assoc.add ~equal:String.equal cs l new_count
in In_channel.fold_lines In_channel.stdin ~init:[] ~f:see_line

let () =
  read_counts ()
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.descending a b)
  |> (fun ls -> List.take ls 10)
  |> List.iter ~f:(fun (l, c) -> printf "%3d: %s\n" c l)
