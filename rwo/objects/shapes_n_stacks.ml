open Base

type 'a stack = < push: 'a -> unit; pop: 'a option >

let stack init = object
  val mutable xs = init
  method push x = xs <- x::xs
  method pop = match xs with
    | [] -> None
    | hd::tl ->
        xs <- tl;
        Some hd
end

let array_stack init = object
  val xs = Stack.of_list init
  method push x = Stack.push xs x
  method pop = Stack.pop xs
end

type shape = < area: float >
type square = < area: float; width: int >
type circle = < area: float; radius: int >

let square x = object
  method width = x
  method area = Float.of_int (x * x)
end

let circle r = object
  method radius = r
  method area = 3.14 *. (Float.of_int r) **. 2.
end

(* using explicit covariance annotations, we can preserve coercibility
 *   even for abstract types
 *)
module Abs_either: sig
  type (+'a, +'b) t
  val left: 'a -> ('a, 'b) t
  val right: 'b -> ('a, 'b) t
  val elim: ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
end = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
  let left x = Left x
  let right x = Right x
  let elim l r = function
    | Left x -> l x
    | Right x -> r x
end

let left_square_as_shape =
  (Abs_either.left (square 3) :> (shape, _) Abs_either.t)

let square_stack: square stack = stack [square 45; square 99]
let circle_stack: circle stack = array_stack [circle 34; circle 81]

let total_area shape_stacks =
  let stack_area acc stack =
    let rec loop acc =
      match stack#pop with
        | Some s -> loop (acc +. s#area)
        | None -> acc
    in loop acc
  in List.fold shape_stacks ~init:0.0 ~f:stack_area

(* won't work - contravariance on push requires shape :> square,
 *   which can't be
let _ = total_area [ (square_stack :> shape stack)
                   ; (circle_stack :> shape stack)
                   ]
 *)

type 'a readonly_stack = < pop: 'a option >

(* but this works fine *)
let a = total_area [ (square_stack :> shape readonly_stack)
                   ; (circle_stack :> shape readonly_stack)
                   ]
