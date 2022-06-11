(* ocamlfind ocamlopt -linkpkg -package core,async,ppx_jane -thread -o echo echo.ml *)

open Core
open Async

let rec copy_blocks buf r w =
  match%bind Reader.read r buf with
    | `Eof -> return ()
    | `Ok bytes ->
        (* pushback: we don't loop until a write flushes *)
        Writer.write w (Bytes.to_string buf) ~len:bytes;
        let%bind () = Writer.flushed w in
        copy_blocks buf r w

let run () =
  let host_and_port = Tcp.Server.create
    (Tcp.Where_to_listen.of_port 8765)
    (fun _addr r w ->
      let buf = Bytes.create (16 * 1024) in
      copy_blocks buf r w)
    ~on_handler_error:`Raise
  in
  ignore host_and_port

let () =
  run ();
  never_returns (Scheduler.go ())
