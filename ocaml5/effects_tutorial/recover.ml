(* input_line_eff.ml, from
 * https://github.com/ocamllabs/ocaml-effects-tutorial
 * ported to Ocaml 5.0 alpha
 *)

open Effect
open Effect.Deep

type _ Effect.t += Conversion_failure: string -> int t

let int_of_string l =
  match int_of_string_opt l with
    | Some l -> l
    | None -> perform (Conversion_failure l)

let rec sum_up acc =
  let l = input_line stdin in
  acc := !acc + int_of_string l;
  sum_up acc

let _ =
  let r = ref 0 in
  try
    try_with sum_up r
      { effc = fun (type a) (eff: a t) -> match eff with
          | Conversion_failure s -> Some (
              fun (k: (a, _) continuation) ->
                Printf.fprintf stderr "Failboat: %s\n" s;
                continue k 0
            )
          | _ -> None
      }
  with
    | End_of_file -> Printf.printf "Sum is %d\n" !r
