(* I like Oleg's lazier cheat for this *)

open Base

type 'a stack_proxy = SP of 'a list

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

  method map: 'b. ('a -> 'b) -> 'b stack_proxy =
    fun f -> SP (List.map xs ~f)
end

let map (s: 'a stack) ~f = let SP xs = s#map f in new stack xs
