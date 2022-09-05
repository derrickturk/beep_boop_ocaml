type 'a parser = string -> ('a * string) option

val json: Json.t parser
val list: Json.t parser
val string: Json.t parser
val bool: Json.t parser
val null: Json.t parser
