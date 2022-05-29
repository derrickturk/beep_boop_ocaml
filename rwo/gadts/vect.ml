type z = |
type 'n s = |

type _ fin =
  | Z: 'n s fin
  | S: 'n fin -> 'n s fin

type (_, 'a) vect =
  | Nil: (z, 'a) vect
  | VCons: 'a * ('n, 'a) vect -> ('n s, 'a) vect

let rec get: type n. n fin -> (n, 'a) vect -> 'a = fun i v -> match i, v with
  | Z, VCons (x, _) -> x
  | S i, VCons (_, xs) -> get i xs
