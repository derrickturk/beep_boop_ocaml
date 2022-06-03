open Base
open Stdio

module type Query_handler = sig
  type config

  val sexp_of_config: config -> Sexp.t
  val config_of_sexp: Sexp.t -> config

  val name: string

  type t

  val create: config -> t

  val eval: t -> Sexp.t -> Sexp.t Or_error.t
end

module Unique: Query_handler with type config = int = struct
  type config = int [@@deriving sexp]
  type t = { mutable next_id: int }

  let name = "unique"
  let create first_id = { next_id = first_id }

  let eval t sexp = match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as e -> e
    | Ok () ->
        let response = Ok (Int.sexp_of_t t.next_id) in
        t.next_id <- t.next_id + 1;
        response
end

module List_dir: Query_handler with type config = string = struct
  type config = string [@@deriving sexp]
  type t = { cwd: string }

  let name = "ls"
  let create cwd = { cwd }

  (* that's a yikes from me dawg *)
  let is_abs p = String.length p > 0 && Char.(=) p.[0] '/'

  let eval t sexp = match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as e -> e
    | Ok dir ->
        (* that's a yikes from me dawg *)
        let dir = if is_abs dir then dir else Core.Filename.concat t.cwd dir in
        try Ok (Array.sexp_of_t String.sexp_of_t (Caml.Sys.readdir dir))
        with _ -> Or_error.error_string "failed to read directory"
end

module type Some_query_handler = sig
  module Q: Query_handler
  val this: Q.t
end

(* wrap up a handler with its module (as an FCM) into a FCM as a
 *   sort of "existential type"
 * a is a phantom type parameter, more or less
 *)
let make_some_handler
    (type a) (module Q: Query_handler with type config = a) cfg =
  (module struct
    module Q = Q
    let this = Q.create cfg
  end: Some_query_handler)

let build_dispatch_table handlers =
  let table = Hashtbl.create (module String) in
  let see_handler ((module H: Some_query_handler) as h) =
    Hashtbl.set table ~key:H.Q.name ~data:h in
  List.iter handlers ~f:see_handler;
  table

let dispatch table = function
  | Sexp.List [Sexp.Atom name; query] ->
      begin match Hashtbl.find table name with
        | None -> Or_error.error "could not find matching handler"
            name String.sexp_of_t
        | Some (module H: Some_query_handler) ->
            H.Q.eval H.this query
      end
  | _ -> Or_error.error_string "malformed query"

let rec cli table =
  printf ">>> %!";
  let result = match In_channel.(input_line stdin) with
    | None -> `Stop
    | Some line ->
        match Or_error.try_with (fun () -> Core.Sexp.of_string line) with
          | Error e -> `Continue (Error.to_string_hum e)
          | Ok (Sexp.Atom "quit") -> `Stop
          | Ok q -> begin match dispatch table q with
              | Error e -> `Continue (Error.to_string_hum e)
              | Ok s -> `Continue (Sexp.to_string_hum s)
            end
  in
  match result with
    | `Stop -> ()
    | `Continue msg ->
        printf "%s\n%!" msg;
        cli table

let () = cli @@ build_dispatch_table
  [ make_some_handler (module Unique) 0
  ; make_some_handler (module List_dir) "."
  ]
