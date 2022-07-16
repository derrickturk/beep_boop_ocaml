(* Exercise #3 from https://github.com/kayceesrk/ocaml5-tutorial *)

let n = try int_of_string Sys.argv.(1) with _ -> 10

module Atomic_stack : sig
  type 'a t
  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop  : 'a t -> 'a
end = struct
  type 'a t = {
    mutable contents: 'a list;
    mutex : Mutex.t;
    condition : Condition.t
  }

  let make () = {
    contents = [];
    mutex = Mutex.create ();
    condition = Condition.create ()
  }

  let push r v =
    Mutex.lock r.mutex;
    r.contents <- v::r.contents;
    Condition.signal r.condition;
    Mutex.unlock r.mutex

  let pop r =
    let rec try_pop r =
      match r.contents with
        | hd::tl ->
            r.contents <- tl;
            hd
        | [] ->
            Condition.wait r.condition r.mutex;
            try_pop r
    in
    Mutex.lock r.mutex;
    let res = try_pop r in
    Mutex.unlock r.mutex;
    res
end

let s = Atomic_stack.make ()

let rec producer n =
  if n = 0 then ()
  else begin
    Atomic_stack.push s n;
    Format.printf "Produced %d\n%!" n;
    producer (n-1)
  end

let rec consumer n acc =
  if n = 0 then acc
  else begin
    let v = Atomic_stack.pop s in
    Format.printf "Consumed %d\n%!" v;
    consumer (n-1) (n + acc)
  end

let main () =
  let p = Domain.spawn (fun _ -> producer n) in
  let c = Domain.spawn (fun _ -> consumer n 0) in
  Domain.join p;
  assert (Domain.join c = n * (n+1) / 2)

let _ = main ()
