(* exceptions.ml, from
 * https://github.com/ocamllabs/ocaml-effects-tutorial
 * ported to Ocaml 5.0 alpha
 *)

open Effect
open Effect.Deep

type _ Effect.t += Exception: exn -> 'a t

let raise (e : exn) : 'a = perform (Exception e)

let try_with (f : unit -> 'a) (h : exn -> 'a) : 'a =
  try_with f ()
    { effc = fun (type b) (eff: b t) -> match eff with
        | Exception e -> Some (fun (k: (b, _) continuation) -> h e)
        | _ -> None
    }

exception Invalid_argument

(** [sqrt f] returns the square root of [f].
    @raise Invalid_argument if f < 0. *)
let sqrt f =
  if f < 0.0 then raise Invalid_argument
  else sqrt f

let _ =
  try_with (fun () ->
    let r = sqrt 42.42 in
    Printf.printf "%f\n%!" r;
    let r = sqrt (-1.0) in
    Printf.printf "%f\n" r)
  (function
    | Invalid_argument -> Printf.printf "Invalid_argument to sqrt\n"
    | _ -> Printf.printf "something else\n")
