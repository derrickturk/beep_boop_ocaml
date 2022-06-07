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

(* this lets you e.g. inherit the class *)
module Exposed: sig
  class ['a] stack: 'a list -> object
    val mutable xs: 'a list
    method pop: 'a option
    method push: 'a -> unit
    (* "Just as with module types, you don’t have to give a type for
     *   everything; anything you omit will be hidden" this is a FUCKING LIE
     * try omitting iter here
     *)
    method iter: ('a -> unit) -> unit
  end

  type 'a t = 'a stack

  val make: 'a list -> 'a t
end = Stack
