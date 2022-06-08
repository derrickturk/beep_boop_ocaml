open Base

(* these only have binary equality against the identical type *)

class square w = object (self: 'self)
  method width = w
  method area = Float.of_int (self#width * self#width)
  method equals (other: 'self) = other#width = self#width
end

class circle r = object (self: 'self)
  method radius = r
  method area = 3.14 *. (Float.of_int self#radius) **. 2.
  method equals (other: 'self) = other#radius = self#radius
end

class dongy_circle r = object (self)
  inherit circle r
  method dong = self#radius + 17
end

(* polymorphic equality by carrying a "representation type" tag *)

(* extensible variants, one useful use, I guess *)
type shape_repr = ..
type shape = < area: float; equals: shape -> bool; repr: shape_repr >

type shape_repr += Square of int
class poly_square w = object (self)
  method width = w
  method area = Float.of_int (self#width * self#width)
  method equals (other: shape) = match other#repr with
    | Square w -> self#width = w
    | _ -> false
  method repr = Square (self#width)
end

type shape_repr += Circle of int
class poly_circle r = object (self)
  method radius = r
  method area = 3.14 *. (Float.of_int self#radius) **. 2.
  method equals (other: shape) = match other#repr with
    | Circle r -> r = self#radius
    | _ -> false
  method repr = Circle (self#radius)
end

let het_equals =
  (new poly_square 3 :> shape)#equals (new poly_circle 3 :> shape)
