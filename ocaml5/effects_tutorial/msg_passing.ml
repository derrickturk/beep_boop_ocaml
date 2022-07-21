(* an interpretation of message passing, from
 * https://github.com/ocamllabs/ocaml-effects-tutorial,
 * ported to Ocaml 5.0 alpha
 *)

open Printf
open Effect
open Effect.Deep

type _ Effect.t += Xchg: int -> int t

type status =
  | Done
  | Susp of int * (int, status) continuation

let step f v () =
  match_with f v
    { retc = (fun _ -> Done)
    ; exnc = raise
    ; effc = fun (type a) (eff: a t) -> match eff with
        | Xchg v -> Some (
            fun (k: (a, _) continuation) -> Susp (v, k)
          )
        | _ -> None
    }

let rec run_both a b =
  match a (), b () with
    | Done, Done -> ()
    | Susp (v1, k1), Susp (v2, k2) ->
        run_both (fun () -> continue k1 v2) (fun () -> continue k2 v1)
    | _ -> failwith "non-synchronized Xchg"

let rec f name = function
  | 0 -> ()
  | n ->
      Printf.printf "%s: sending %d\n%!" name n;
      let v = perform (Xchg n) in
      Printf.printf "%s: received %d\n%!" name v;
      f name (n-1)

let _ = run_both (step (f "a") 3) (step (f "b") 3)
