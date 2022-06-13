(* these work because of the covariance annotation on Kessel.t *)
val int: [> `Int of int] Kessel.t 
val bool: [> `Bool of bool] Kessel.t
val list: [> `List of Yojson.Basic.t list] Kessel.t
val null: [> `Null] Kessel.t
val string: [> `String of string] Kessel.t

val json: Yojson.Basic.t Kessel.t
