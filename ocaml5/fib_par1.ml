(* Exercise #1 from https://github.com/kayceesrk/ocaml5-tutorial *)

let n = try int_of_string Sys.argv.(1) with _ -> 40

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fib_par n =
  (* Only use parallelism when problem size is large enough *)
  if n > 20 then begin
    let d1 = Domain.spawn (fun () -> fib (n - 1)) in
    let d2 = Domain.spawn (fun () -> fib (n - 2)) in
    Domain.join d1 + Domain.join d2
  end else fib n

let main () =
  let r = fib_par n in
  Printf.printf "fib(%d) = %d\n%!" n r

let _ = main ()
