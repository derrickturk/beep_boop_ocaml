open Async
open Core

module type Delayer = sig
  type t
  val create: Time.Span.t -> t
  val schedule: t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer: Delayer = struct
  type t = { delay: Time.Span.t; jobs: (unit -> unit) Queue.t }

  let create delay = { delay; jobs = Queue.create () }

  let schedule { delay; jobs } f =
    let ivar = Ivar.create () in
    Queue.enqueue jobs (fun () -> upon (f ()) (Ivar.fill ivar));
    upon (after delay) (fun () ->
      let job = Queue.dequeue_exn jobs in
      job ()
    );
    Ivar.read ivar
end
