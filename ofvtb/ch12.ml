(* solutions to exercises from Chapter 12, "In and Out", of
 *   "OCaml from the Very Beginning".
 *)

(* Notes for Slack:
 *   - so funny thing, my first attempt at `read_three` had no arguments, which
 *       of course meant that it tried to read three integers *when I
 *       defined it*! I'm so used to Haskell's monadic I/O that I forgot that
 *       in some languages things _happen_ when you talk about them.
 *       (of course, the rewrite from x to fun () -> x is a cheap way to get
 *       lazy evaluation, which is what we're really doing here, when you
 *       squint - we call the function on () to "force" the lazy value.)
 *   - the evaluation order of tuples is WEIRD in OCaml. it's right to left.
 *       (Ok, the manual says it's "unspecified", but in practice it's right
 *       to left). you notice if you start doing side effects inside
 *       tuple construction. apparently this (undocumented, except as "don't
 *       count on it") choice is for the performance of the bytecode
 *       interpreter.
 *       of course, all of this is downstream of the original sin of creating
 *         an impure functional language.
 *   - by the end of this I was just tired and pissed. tedious code that bears
 *       no resemblance to what you'd do in real software. uggggggggggggh.
 *)

let rec print_int_list xs =
  let rec go = function
    | [] -> ()
    | [x] -> print_int x
    | x::xs ->
        print_int x;
        print_string "; ";
        go xs
   in
     print_string "[";
     go xs;
     print_string "]"

(* I mean, lots of exceptions - I/O failure, bad parse...?
 * watchu want me to do about it?
 *)
let read_three () =
  try
    let x = read_int ()
    and y = read_int ()
    and z = read_int ()
     in Some (x, y, z)
  with _ -> None

(* my first attempt, because I thought it was cute. give this 1 2 3,
 *   get (3, 2, 1)! that is NOT the evaluation order I expected.
 *)
let read_three_hmm () =
  try Some (read_int (), read_int (), read_int ())
  with _ -> None

(* this is dumb, just read from a channel until EOF, nobody wants interactive
 *   input like this since 1971.
 * arguably the End_of_file -> [] should only apply if you end on a value,
 *   otherwise you're missing a key, but I'd write a proper parser at that
 *   point, using exceptions for this sucks.
 *)
let rec read_dict () =
  try
    match read_line () with
      | "done" -> []
      | k' -> match int_of_string_opt k' with
          | None -> print_string "that's not a number!\n"; read_dict ()
          | Some k -> let v = read_line ()
                       in (k, v)::read_dict ()
  with
    End_of_file -> []

(* UPDATE: this is nicer - using an "exception pattern" to avoid a try-with *)
let rec read_dict' () = match read_line () with
  | "done" -> []
  | k' -> match int_of_string_opt k' with
     | None -> print_string "that's not a number!\n"; read_dict ()
     | Some k -> begin
         match read_line () with
           | v -> (k, v)::read_dict ()
           | exception End_of_file -> raise (Failure "no value for key")
         end
  | exception End_of_file -> []

(* FUCK I miss the Haskell prelude [and range syntax] here
 * so fucking tedious
 *)
let rec from_to l u =
  if l > u
    then raise (Invalid_argument "lower bound greater than upper")
    else if l = u
      then [l]
      else l::from_to (l + 1) u

let up_to = from_to 1

let times_table n =
  let range = up_to n
  and show_mult i j = string_of_int (i * j)
   in let row i = String.concat "\t" ((List.map (show_mult i)) range)
       in String.concat "\n" (List.map row range)

let times_table_to_file path n =
  let f = open_out path
   in
     output_string f (times_table n);
     close_out f

let lines_in_file chan =
  let rec go n =
    try
      (* explicitly "throw away" the result to silence the warning here *)
      let _ = input_line chan in go (n + 1)
    with
      _ -> n
   in go 0

(* I ain't dealing with those cases. This is a dumb function. This is not the
 *   way you would do any of this. For several reasons.
 * the whole point of exceptions like this is that I don't have a smart thing to
 *   do here, so I'm just going to let them rip!
 * that said, we DO need to close file1 if an exception happens opening file2,
 *   etc.
 * I could roll that by hand, it'd suck, or I could use OCaml's equivalent to
 *   Haskell's `bracket` - kind of like a functional try-finally
 *)
let copy_file path1 path2 =
  let rec do_copy in_chan out_chan =
    try
      let l = input_line in_chan
       in
         output_string out_chan l;
         output_char out_chan '\n';
         do_copy in_chan out_chan
    with
      End_of_file -> ()
   in let src = open_in path1
       in let do_action () =
            let dst = open_out path2
             in Fun.protect (fun () -> do_copy src dst)
                  ~finally:(fun () -> close_out dst)
           in Fun.protect do_action ~finally:(fun () -> close_in src)
