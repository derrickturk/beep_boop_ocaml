let ogrep str chan =
  let pat = Str.regexp_string str in
  let rec go () = match input_line chan with
    | line -> begin
        match Str.search_forward pat line 0 with
          | _ -> print_endline line; go ()
          | exception Not_found -> go ()
      end
    | exception End_of_file -> ()
  in go ()

let () = try
  match Sys.argv with
    | [|_; pat; file|] ->
        let chan = open_in file in
          Fun.protect (fun () -> ogrep pat chan)
            ~finally:(fun () -> close_in_noerr chan)
    | _ -> print_endline "Usage: ogrep <pat> <file>";
with e ->
  print_string "Error: ";
  print_endline (Printexc.to_string e);
  exit 1
