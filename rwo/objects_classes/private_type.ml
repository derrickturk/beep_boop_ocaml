open Base

module Stack = struct
  class ['a] stack init = object
    val mutable xs: 'a list = init

    method pop = match xs with
      | hd::tl ->
          xs <- tl;
          Some hd
      | _ -> None

    method push x =
      xs <- x::xs

    method iter f = List.iter xs ~f
  end

  type 'a t = 'a stack

  let make init = new stack init
end

(* try making this compile, while keeping `iter` in `Stack` but hiding it in
 *   this signature, without `private` - can't be done!
 *)
module HideIter: sig
  type 'a t = private < pop: 'a option; push: 'a -> unit; .. >
  val make: 'a list -> 'a t
end = Stack
