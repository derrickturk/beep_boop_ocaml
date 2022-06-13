(* these work because of the covariance annotation on Kessel.t *)
val assoc: Yojson.Basic.t Kessel.t
val bool: [> `Bool of bool] Kessel.t
val int: [> `Int of int] Kessel.t 
val list: Yojson.Basic.t Kessel.t
val null: [> `Null] Kessel.t
val string: [> `String of string] Kessel.t

val json: Yojson.Basic.t Kessel.t
