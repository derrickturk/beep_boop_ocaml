type t =
  | Null
  | Bool of bool
  | String of string
  | List of t list
(* left as an exercise for the reader:
  | Number of float
  | Object of (string * t) list
 *)
[@@deriving show]
