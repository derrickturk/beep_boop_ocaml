type 'a t

(** an empty queue *)
val empty: 'a t

(** push [x] onto the back of [q] *)
val enqueue: 'a t -> 'a -> 'a t

(** returns [None] if [q] is empty; otherwise, [Some (x, q')] where [x] is the
      the first element in [q] and [q'] is the remaining queue *)
val dequeue: 'a t -> ('a * 'a t) option

(** left fold over [q] with initial value [init] and combining function [f] *)
val fold: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
