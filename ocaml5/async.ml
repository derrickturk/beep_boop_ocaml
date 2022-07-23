open Effect
open Effect.Deep

type _ Effect.t +=
  | Yield: unit Effect.t
  | Async: (unit -> unit) -> unit Effect.t

let yield () = perform Yield
let async f = perform (Async f)

let rec run_async_on f q =
  let open Queue in
  let defer k = add k q in 
  let next () = try take q () with Empty -> () in
  match_with f ()
    { retc = next
    ; exnc = raise
    ; effc = fun (type a) (eff: a Effect.t) -> match eff with
        | Yield -> Some begin
            fun (k: (a, _) continuation) ->
              defer (continue k);
              next ()
          end
        | Async f -> Some begin
            fun (k: (a, _) continuation) ->
              defer (fun () -> run_async_on f q);
              continue k ()
          end
        | _ -> None
    }

let run_async f =
  let q = Queue.create () in
  run_async_on f q

let () =
  run_async begin fun () ->
    print_endline "first this";
    async begin fun () ->
      print_endline "followed by this";
      yield ();
      print_endline "followed by the other";
    end;
    yield ();
    print_endline "then that"
  end
