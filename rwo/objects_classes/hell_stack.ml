(* I want my damn map method! *)

open Base

module rec R: sig
  class ['a] stack: 'a list -> object
    method pop: 'a option
    method push: 'a -> unit
    method iter: ('a -> unit) -> unit
    method fold: 'b. 'b -> ('b -> 'a -> 'b) -> 'b
    method map: 'b. ('a -> 'b) -> 'b R.stack_wrapper
  end

  type 'a stack_wrapper = { stack: 'a stack }
end = struct
  class ['a] stack init = object
    val mutable xs: 'a list = init

    method pop = match xs with
      | hd::tl ->
          xs <- tl;
          Some hd
      | _ -> None

    method push x =
      xs <- x::xs

    method iter f = List.iter ~f xs

    (* explicit "forall" in this type to introduce 'b *)
    method fold: 'b. 'b -> ('b -> 'a -> 'b) -> 'b =
      fun init f -> List.fold xs ~init ~f

    method map: 'b. ('a -> 'b) -> 'b R.stack_wrapper =
      fun f -> { R.stack = new R.stack (List.map xs ~f) }
  end

  type 'a stack_wrapper = { stack: 'a stack }
end
