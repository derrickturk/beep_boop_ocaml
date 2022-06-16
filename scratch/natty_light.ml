module type Natty_light_intf = sig
  type t

  val of_int: int -> t option
  val to_int: t -> int

  val take: t -> 'a list -> 'a list
end

module Natty_light: Natty_light_intf = struct
  type t = int

  let of_int n = if n < 0 then None else Some n
  let to_int n = n

  let rec take n xs = match n, xs with
    | 0, _ -> []
    | _, [] -> []
    | _, hd::tl -> hd::take (n - 1) tl
end

let good = match Natty_light.of_int 5 with
  | Some n -> Some (Natty_light.take n [1;2;3;4;5;6;7])
  | None -> None

let bad = match Natty_light.of_int (-5) with
  | Some n -> Some (Natty_light.take n [1;2;3;4;5;6;7])
  | None -> None
