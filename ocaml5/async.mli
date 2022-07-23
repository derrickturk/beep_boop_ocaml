val yield: unit -> unit
val async: (unit -> unit) -> unit
val run_async_on: (unit -> unit) -> (unit -> unit) Queue.t -> unit
val run_async: (unit -> unit) -> unit
