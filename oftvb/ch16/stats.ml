(* TODO: use argv.(0) in usage msg, print to stderr *)
try
  match Sys.argv with
    | [|_; path|] -> Textstat.print_file_stats path
    | _ -> print_endline "Usage: stats <path>";
with e ->
  print_string "Error: ";
  print_endline (Printexc.to_string e);
  exit 1
