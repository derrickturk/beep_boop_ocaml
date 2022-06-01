(* fragments of a hologram rose tree, or,
 * two interpretations of "can you write a function like map which works over
 * lists( of lists)* ?"
 *)

(* in this interpretation, the nesting may vary throughout the "tree" *)
module RoseTree = struct
  type 'a t = L of 'a | R of 'a t list
  let rec map f = function
    | L x -> f x
    | R ts -> R (List.map (map f) ts)
end

(* in this interpretation, we accept only consistent nesting: lists,
 *   lists of lists, and so on... *)
module NestyList = struct
  type z = |
  type 'n s = |

  type (_, 'a) t =
    | List : 'a list -> (z, 'a) t
    | Nest : ('n, 'a) t list -> ('n s, 'a) t

  let rec map: type n. ('a -> 'b) -> (n, 'a) t -> (n, 'b) t =
    fun f l -> match l with
      | List xs -> List (List.map f xs)
      | Nest xs -> Nest (List.map (map f) xs)
end
