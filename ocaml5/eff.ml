open Effect
open Effect.Deep

type _ Effect.t += Xchg: int -> int t

let comp1 () = perform (Xchg 0) + perform (Xchg 1)

let n =
  try_with comp1 ()
    { effc = fun (type a) (eff: a t) -> match eff with
        | Xchg n -> Some (fun (k: (a, _) continuation) -> continue k (n + 1))
        | _ -> None
    }

type 'a status =
  | Complete of 'a
  | Suspended of { msg: int; cont: (int, 'a status) continuation }

let step (f: unit -> 'a) (): 'a status =
  match_with f ()
    { retc = (fun v -> Complete v)
    ; exnc = raise
    ; effc = fun (type a) (eff: a t) -> match eff with
        | Xchg msg ->
            Some (fun (cont: (a, _) continuation) -> Suspended { msg; cont })
        | _ -> None
    }

let rec run_both a b =
  match a (), b () with
    | Complete va, Complete vb -> (va, vb)
    | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
        run_both (fun () -> continue k1 m2) (fun () -> continue k2 m1)
    | _ -> failwith "goofed"

let comp2 () = perform (Xchg 21) * perform (Xchg 21)

let hmm = run_both (step comp1) (step comp2)
