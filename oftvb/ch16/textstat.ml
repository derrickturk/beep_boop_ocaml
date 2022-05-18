(* mostly duplicated from Chapter 13 *)

type stats = {
  mutable lines: int;
  mutable chars: int;
  mutable words: int;
  mutable sentences: int;
  histogram: int array;
}

let init_stats () = {
  lines = 0;
  chars = 0;
  words = 0;
  sentences = 0;
  histogram = Array.make 256 0;
}

let lines s = s.lines
let characters s = s.chars
let words s = s.words
let sentences s = s.sentences
let histogram s = s.histogram

type line_stats = {
  mutable words: int;
  mutable sentences: int;
}

let line_stats line =
  let stats = { words = 0; sentences = 0 } in
  let see_char = function
    | '.' | '?' | '!' -> stats.sentences <- stats.sentences + 1
    | ' ' -> stats.words <- stats.words + 1
    | _ -> ()
  in
    String.iter see_char line;
    stats

let update_counts hist line =
  let see_char c =
    let i = int_of_char c in hist.(i) <- hist.(i) + 1
  in String.iter see_char line

let channel_stats chan =
  let stats = init_stats () in
    try
      while true do
        let line = input_line chan in
        let ls = line_stats line in
          stats.lines <- stats.lines + 1;
          stats.chars <- stats.chars + String.length line;
          stats.words <- stats.words + ls.words;
          stats.sentences <- stats.sentences + ls.sentences;
          update_counts stats.histogram line
      done;
      stats (* unreachable! *)
    with End_of_file -> stats

let file_stats path =
  let chan = open_in path in
    Fun.protect (fun () -> channel_stats chan)
      ~finally:(fun () -> close_in_noerr chan)

let dump_histogram hist =
  let max = float_of_int @@
    Array.fold_left (fun x y -> if y > x then y else x) 0 hist in
  let counts = Array.map
    (fun x -> int_of_float @@ float_of_int x /. max *. 76.0) hist in
  let row i c =
    if c <> 0 then begin
      Printf.printf "%03d|" i;
      print_endline @@ String.make c '#'
    end
  in Array.iteri row counts

let print_file_stats path =
  let dump_stats chan =
    let stats = channel_stats chan in
      print_string "We had ";
      print_int stats.lines;
      print_string " lines, ";
      print_int stats.chars;
      print_string " characters, ";
      print_int stats.words;
      print_string " words, and ";
      print_int stats.sentences;
      print_endline " sentences in the file.";
      dump_histogram stats.histogram
  in
  let chan = open_in path in
    Fun.protect (fun () -> dump_stats chan)
      ~finally:(fun () -> close_in_noerr chan)
