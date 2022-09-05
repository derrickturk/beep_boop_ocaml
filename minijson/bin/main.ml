open Minijson

let () =
  let doc = In_channel.(input_all stdin) in
  match Parser.json (String.trim doc) with
    | Some (obj, "") -> print_endline (Json.show obj)
    | Some (obj, rest) ->
        print_endline ("found : " ^ Json.show obj);
        print_endline ("with leftover: " ^ rest)
    | None -> print_endline "parse failed"
