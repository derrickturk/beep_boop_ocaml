let xs = Fqueue_foldable.empty
  |> (fun q -> Fqueue_foldable.enqueue q 23)
  |> (fun q -> Fqueue_foldable.enqueue q 45)
  |> (fun q -> Fqueue_foldable.enqueue q 95)

let () = Fqueue_foldable.iter xs ~f:(fun x -> print_int x; print_newline ())
