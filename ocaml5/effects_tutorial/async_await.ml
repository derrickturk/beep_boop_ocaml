(* async_await.ml, from https://github.com/ocamllabs/ocaml-effects-tutorial,
 * ported to Ocaml 5.0 alpha
 *)

open Effect.Deep
open Printf

module type Scheduler = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct
  type 'a _promise =
    | Waiting of ('a, unit) continuation list
    | Done of 'a

  type 'a promise = 'a _promise ref

  type _ Effect.t +=
    | Async: (unit -> 'a) -> 'a promise Effect.t
    | Yield: unit Effect.t
    | Await: 'a promise -> 'a Effect.t

  let async f = perform (Async f)

  let yield () = perform Yield

  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match_with main ()
          { retc = begin fun v ->
              begin match !pr with
                | Done _ -> failwith "that shouldn't happen!"
                | Waiting l ->
                    pr := Done v;
                    List.iter (fun k -> enqueue (fun () -> continue k v)) l
              end;
              dequeue ()
            end
          ; exnc = raise
          ; effc = fun (type b) (eff: b Effect.t) -> match eff with
              | Async f -> Some (
                  fun (k: (b, _) continuation) ->
                    let p = ref (Waiting []) in
                    enqueue (fun () -> continue k p);
                    fork (ref (Waiting [])) f
                )
              | Yield -> Some (
                  fun (k: (b, _) continuation) ->
                    enqueue (continue k);
                    dequeue ()
                )
              | Await p -> Some (
                  fun (k: (b, _) continuation) ->
                    begin match !p with
                      | Done v -> continue k v
                      | Waiting l -> p := Waiting (k::l)
                    end;
                    dequeue ()
                )
              | _ -> None
          }
    in
    fork (ref (Waiting [])) main
end

open Scheduler

let main () =
  let task name () =
    Printf.printf "starting %s\n%!" name;
    let v = Random.int 100 in
    Printf.printf "yielding %s\n%!" name;
    yield ();
    Printf.printf "ending %s with %d\n%!" name v;
    v
  in
  let pa = async (task "a") in
  let pb = async (task "b") in
  let pc = async (fun () -> await pa + await pb) in
  Printf.printf "Sum is %d\n" (await pc);
  assert (await pa + await pb = await pc)

let _ = run main

