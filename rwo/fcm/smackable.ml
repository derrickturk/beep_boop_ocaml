module type Smackable = sig
  type t
  val smack: t -> t
end

let int_smacker = (module struct
  type t = int
  let smack x = x + 3
end: Smackable with type t = int)

let float_smacker = (module struct
  type t = float
  let smack x = x +. 7.2
end: Smackable with type t = float)

(* common use for locally abstract type *)
let smack_list (type a) (module S: Smackable with type t = a) (l: a list) =
  List.map S.smack l

(* a more interesting use for a locally abstract type *)
let make_smackable (type a) smack = (module struct
  type t = a
  let smack = smack
end: Smackable with type t = a)
