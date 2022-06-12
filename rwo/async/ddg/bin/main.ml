open Core
open Async

let ddg_base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json"

let query_uri query = Uri.add_query_param ddg_base_uri ("q", [query])

let defn_from_json json =
  match Yojson.Safe.from_string json with
    | `Assoc kvs ->
        let find k = match List.Assoc.find kvs k ~equal:String.equal with
          | None | Some (`String "") -> None
          | Some s -> Some (Yojson.Safe.to_string s)
        in begin match find "Abstract" with
          | Some _ as x -> x
          | None -> find "Definition"
        end
    | _ -> None

let get_defn word =
  let%bind _, body = Cohttp_async.Client.get (query_uri word) in
  let%map body = Cohttp_async.Body.to_string body in
  word, defn_from_json body

let print_defn (word, defn) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.make (String.length word) '-')
    (match defn with
       | None -> "No definition found!"
       | Some d -> String.concat (Wrapper.wrap (Wrapper.make 70) d) ~sep:"\n")

let search_and_print words =
  let%map results = Deferred.all (List.map words ~f:get_defn) in
  List.iter results ~f:print_defn

let () =
  let cmd = Command.async
    ~summary: "Get definitions from DuckDuckGo"
    (let%map_open.Command words = anon (sequence ("word" %: string)) in
     fun () -> search_and_print words)
  in Command_unix.run cmd
