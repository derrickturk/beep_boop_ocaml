open Effect
open Effect.Deep

type _ Effect.t +=
  | Yield: unit Effect.t

let yield () = perform Yield

let run_async f =
  let open Queue in
  let q = create () in
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
        | _ -> None
    }

let () =
  run_async begin fun () ->
    print_endline "first this";
    yield ();
    print_endline "then that"
  end
