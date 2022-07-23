open Effect
open Effect.Deep

(* it's tempting to write:
type 'a future =
  | Done of 'a
  | Pending of ('a, unit) continuation list ref
 * or even to write that and then drop the Done case, but
 * we have to consider the possibility that the Async task finishes
 * before anybody Awaits it!
 *)

type 'a future_state =
  | Done of 'a
  | Pending of ('a, unit) continuation list

type 'a future = 'a future_state ref

let new_future () = ref (Pending [])

type _ Effect.t +=
  | Yield: unit Effect.t
  | Async: (unit -> 'a) -> 'a future Effect.t
  | Await: 'a future -> 'a Effect.t

let yield () = perform Yield
let async f = perform (Async f)
let await fut = perform (Await fut)

let rec run_async_on: 'a. (unit -> 'a)
                       -> (unit -> unit) Queue.t
                       -> 'a future
                       -> unit =
  fun f q fut ->
    let open Queue in
    let defer k = add k q in 
    let next () = try take q () with Empty -> () in
    match_with f ()
      { retc = begin
          fun v -> match !fut with
            | Done _ -> failwith "impossible"
            | Pending waiters ->
                List.iter (fun k -> defer (fun () -> continue k v)) waiters;
                fut := Done v;
                next ()
        end
      ; exnc = raise
      ; effc = fun (type a) (eff: a Effect.t) -> match eff with
          | Yield -> Some begin
              fun (k: (a, _) continuation) ->
                defer (continue k);
                next ()
            end
          | Async f -> Some begin
              fun (k: (a, _) continuation) ->
                let fut = new_future () in
                defer (fun () -> run_async_on f q fut);
                continue k fut
            end
          | Await fut -> Some begin
              fun (k: (a, _) continuation) ->
                match !fut with
                  | Done v -> continue k v
                  | Pending waiters ->
                      fut := Pending (k::waiters);
                      next ()
            end
          | _ -> None
      }

let run_async f =
  let q = Queue.create () in
  run_async_on f q (new_future ())

let () =
  run_async begin fun () ->
    print_endline "first this";
    let _ = async (fun () ->
      print_endline "followed by this";
      yield ();
      print_endline "followed by the other";
    ) in
    let slow45 = async (fun () ->
      yield ();
      45
    ) in
    let slow33 = async (fun () ->
      yield ();
      33
    ) in
    yield ();
    print_endline "then that";
    let _ = async (fun () -> print_endline "what?") in
    Printf.printf "sum is %d\n" (await slow45 + await slow33);
    yield ();
    Printf.printf "product is %d\n" (await slow45 * await slow33)
  end
