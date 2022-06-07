open Base

type 'a iterator = < get: 'a option; next: unit >

class ['a] list_iterator xs = object
  val mutable xs: 'a list = xs

  method get = List.hd xs

  method next = match xs with
    | [] -> ()
    | _::tl -> xs <- tl
end

class ['a] stack init = object
  val mutable xs: 'a list = init

  method pop = match xs with
    | hd::tl ->
        xs <- tl;
        Some hd
    | _ -> None

  method push x =
    xs <- x::xs

  method iterator = new list_iterator xs

  method iter f = List.iter ~f xs

  (* explicit "forall" in this type to introduce 'b *)
  method fold: 'b. 'b -> ('b -> 'a -> 'b) -> 'b =
    fun init f -> List.fold xs ~init ~f

  (* this CANNOT work - see https://alan.petitepomme.net/cwn/2015.10.27.html#5
   * it's really horrifying
  method map: 'b. ('a -> 'b) -> 'b stack =
    fun f -> new stack (List.map xs ~f)
   *)
end

class sstack init = object
  inherit [string] stack init
  method print = List.iter xs ~f:Stdio.print_endline
end

class dub_stack init = object
  inherit [int] stack init as super
  method push hd = super#push (hd * 2)
end
