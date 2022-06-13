type 'a t

type error = { expected: string; found: string }

val parse: 'a t -> string -> ('a * string, error) result
val parse_all: 'a t -> string -> ('a, error) result

val map: ('a -> 'b) -> 'a t -> 'b t
val return: 'a -> 'a t
val ap: ('a -> 'b) t -> 'a t -> 'b t
val prod: 'a t -> 'b t -> ('a * 'b) t
val bind: 'a t -> ('a -> 'b t) -> 'b t

val (let*): 'a t -> ('a -> 'b t) -> 'b t
val (let+): 'a t -> ('a -> 'b) -> 'b t
val (and+): 'a t -> 'b t -> ('a * 'b) t

val (<$>): ('a -> 'b) -> 'a t -> 'b t
val (<*>): ('a -> 'b) t -> 'a t -> 'b t
val (>>=): 'a t -> ('a -> 'b t) -> 'b t
val ( *> ): 'a t -> 'b t -> 'b t
val ( <* ): 'a t -> 'b t -> 'a t
val (>>|): 'a t -> ('a -> 'b) -> 'b t

val opt: 'a t -> 'a option t
val alt: 'a t -> 'a t -> 'a t
val empty: 'a t

val (<|>): 'a t -> 'a t -> 'a t

val l: string -> string t
val ws: unit t
val lexeme: 'a t -> 'a t

val chars: (char -> bool) -> string t
val chars1: (char -> bool) -> expected:string -> string t
val digits1: string t

val unsigned_int: int t
val signed_int: int t
