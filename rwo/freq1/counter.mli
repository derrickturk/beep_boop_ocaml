type t

val empty: t

val see_line: t -> string -> t

val to_list: t -> (string * int) list

type median =
  | Median of string
  | Before_and_after of string * string

val median: t -> median option
