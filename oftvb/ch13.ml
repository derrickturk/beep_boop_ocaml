(* solutions to exercises from Chapter 13, "Putting Things in Boxes", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack:
 *  - I had to re-read that bit about the binding of if-then-else without the
 *      else a couple of times; the upshot - I think! - is that when the
 *      `then` expression is of type `()`, then it needs to be parenthesized
 *      if it's a compound (`;`-separated) expression; otherwise, the first
 *      semicolon will be parsed as terminating the whole if-then-else
 *      expression (after an implicit `else ()`). I need to fact-check this.
 *  - while loops in a functional language make me Real Nervous; I think I'm
 *      correct in believing that the only way one can possibly end is through
 *      reference shenanigans. (well, mutable state or I/O, I suppose - like
 *      waiting on End_of_file).
 *  - have I mentioned how gross and inadequate I find waiting on an End_of_file
 *      exception every time we process a file? I have got to find a nice
 *      streaming I/O library. this is making me more excited to read RWO.
 *  - this mfer doesn't close his files in the event of exceptions. see: gross
 *      and inadequate, above. C++-style destructors (like Rust's Drop) are a
 *      way better solution here. even Python-style context managers are better.
 *      `Fun.protect` is ehhh. what's worse is that "you must close this"
 *      doesn't appear anywhere in the type.
 *  - OCaml refs are actually records with a single mutable field called
 *      contents - the operators just provide the barest syntactic sugar.
 *      OCaml programmers seem to take their syntactic coffee black, for the
 *      most part.
 *  - I'm going to use this knowledge to alter the infuriating stats function.
 *  - ahhhhh how I hate the ASCII assumption! that stupid 256-histogram. YES,
 *      OCaml's char is really an "octet"/byte. but NO, that doesn't mean that
 *      every "character" fits in that range. the name is a historical relic.
 *      we're really making a byte histogram.
 *  - whatever happens to a string literal to make it a suitable argument to
 *      Printf.printf is deep type witchery. [[desire to know more intensifies]]
 *)

(* I am altering this program, pray I don't alter it further. *)

type stats = {
  mutable lines: int;
  mutable chars: int;
  mutable words: int;
  mutable sentences: int;
  histogram: int array;
}

type line_stats = {
  mutable words: int;
  mutable sentences: int;
}

let init_stats () = {
  lines = 0;
  chars = 0;
  words = 0;
  sentences = 0;
  histogram = Array.make 256 0;
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

(* I'm basically giving myself "credit for time served" for some of these
 *   exercises. after rewriting that shitty program I really don't feel like
 *   weighing in on whether our statistics are sensible (hint: they are not).
 *)

(* #1: two refs, x and y, initially x = 1 and y = 2, finally x = 2 and y =4,
 *   expression evaluates to 6: int
 * #2: two pointers to one thing vs. two pointers to two things
 * #3: (tail) recursion = iteration
 * #4: int array, bool array, int array array, int list array, int, ()
 *)

let sum_array = Array.fold_left (+) 0

let reverse_in_place arr =
  let last_ix = Array.length arr - 1 in
  for i = 0 to (Array.length arr / 2) - 1 do
    let tmp = arr.(last_ix - i) in
      arr.(last_ix - i) <- arr.(i);
      arr.(i) <- tmp
  done

let mult_table n =
  let table = Array.make_matrix n n 0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        table.(i).(j) <- (i + 1) * (j + 1)
      done
    done;
    table

(* these are kind of dumb *)

let to_lower = function
  | 'A'..'Z' as c -> char_of_int (int_of_char c lor 32)
  | c -> c

let to_upper = function
  | 'a'..'z' as c -> char_of_int (int_of_char c lxor 32)
  | c -> c
