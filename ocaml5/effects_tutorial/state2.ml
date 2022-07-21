(* state2.ml, from https://github.com/ocamllabs/ocaml-effects-tutorial,
 * ported to Ocaml 5.0 alpha
 *
 * I... actually kind of understand this.
 *)

open Printf
open Effect
open Effect.Deep

module type STATE = sig
  type t
  val put     : t -> unit
  val get     : unit -> t
  val history : unit -> t list
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t +=
    | Get: t Effect.t
    | Put: t -> unit Effect.t
    | History: t list Effect.t

  let get () = perform Get

  let put v = perform (Put v)

  let history () = perform History

  let run f ~init =
    let comp =
      match_with f ()
        { retc = (fun () -> fun _ -> ())
        ; exnc = raise
        ; effc = fun (type a) (eff: a Effect.t) -> match eff with
            | Get -> Some (
                fun (k: (a, _) continuation) ->
                  fun ((s, _) as ctx: t * t list) ->
                    continue k s ctx
              )
            | Put v -> Some (
                fun (k: (a, _) continuation) ->
                  fun ((s, ss): t * t list) ->
                    continue k () (v, v::ss)
              )
            | History -> Some (
                fun (k: (a, _) continuation) ->
                  fun ((_, ss) as ctx: t * t list) ->
                    continue k (List.rev ss) ctx
              )
            | _ -> None
        }
    in comp (init, [])

end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  assert (0 = IS.get ());
  IS.put 42;
  assert (42 = IS.get ());
  IS.put 21;
  assert (21 = IS.get ());
  SS.put "hello";
  assert ("hello" = SS.get ());
  SS.put "world";
  assert ("world" = SS.get ());
  assert ([42; 21] = IS.history ())

let _ = IS.run (fun () -> SS.run foo ~init:"") ~init:0
