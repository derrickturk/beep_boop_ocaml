type 'a future

val yield: unit -> unit
val async: (unit -> 'a) -> 'a future
val await: 'a future -> 'a
val run_async_on: (unit -> 'a) -> (unit -> unit) Queue.t -> 'a future -> unit
val run_async: (unit -> 'a) -> unit
