open Base
open Stdio

let read_counts () =
  In_channel.fold_lines In_channel.stdin ~init:Counter.empty ~f:Counter.see_line

let () =
  read_counts ()
  |> Counter.to_list
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.descending a b)
  |> (fun ls -> List.take ls 10)
  |> List.iter ~f:(fun (l, c) -> printf "%3d: %s\n" c l)
