(* state1.ml, from https://github.com/ocamllabs/ocaml-effects-tutorial,
 * ported to Ocaml 5.0 alpha
 *)

open Printf
open Effect
open Effect.Deep

module type STATE = sig
  type t
  val get : unit -> t
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t += Get: t Effect.t

  let get () = perform Get

  let run f ~init =
    let comp =
      match_with f ()
        { retc = (fun () -> fun _ -> ())
        ; exnc = raise
        ; effc = fun (type a) (eff: a Effect.t) -> match eff with
            | Get -> Some (
                fun (k: (a, _) continuation) -> fun (s: t) -> continue k s s
              )
            | _ -> None
        }
    in comp init
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%d\n" (IS.get ());
  printf "%s\n" (SS.get ());
  printf "%s\n" (SS.get ())

let _ = IS.run (fun () -> SS.run foo ~init:"forty two") ~init: 42
