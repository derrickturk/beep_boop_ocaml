(* IIRC that let () = ... is conventional for OCaml "main";
 * in any case it saves a ;;
 *)

let reversed_lines chan =
  let rec go acc =
    match input_line chan with
      | line -> go (line::acc)
      | exception End_of_file -> acc
  in go []

let rec write_lines chan = function
  | [] -> ()
  | line::rest ->
      output_string chan line;
      output_char chan '\n';
      write_lines chan rest

let reverse src dst = write_lines dst (reversed_lines src)

let reverse_file src dst =
  let in_chan = open_in src in
  let action () =
    let out_chan = open_out dst in
      Fun.protect (fun () -> reverse in_chan out_chan)
        ~finally:(fun () -> close_out_noerr out_chan)
  in Fun.protect action ~finally:(fun () -> close_in_noerr in_chan)

let () = try
  match Sys.argv with
    | [|_; src; dst|] -> reverse_file src dst
    | _ -> print_endline "Usage: reverse <src> <dst>";
with e ->
  print_string "Error: ";
  print_endline (Printexc.to_string e);
  exit 1
