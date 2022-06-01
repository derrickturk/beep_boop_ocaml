module type Fqueue_Ext = sig
  type 'a t
  include (module type of Fqueue) with type 'a t := 'a t
  include Foldable.FoldableExt with type 'a t := 'a t
end

module FqueueFoldable: Fqueue_Ext = struct
  include Fqueue
  include Foldable.MakeFoldable (Fqueue)
end

let xs = FqueueFoldable.empty
  |> (fun q -> FqueueFoldable.enqueue q 23)
  |> (fun q -> FqueueFoldable.enqueue q 45)
  |> (fun q -> FqueueFoldable.enqueue q 95)

let () = FqueueFoldable.iter xs ~f:(fun x -> print_int x; print_newline ())
