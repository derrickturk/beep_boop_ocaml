type 'a element = { value: 'a
                  ; mutable prev: 'a element option
                  ; mutable next: 'a element option
                  }

type 'a t = { mutable head: 'a element option }

let create () = { head = None }

let is_empty { head } = match head with
  | None -> true
  | Some _ -> false

let first { head } = head
let next { next; _ } = next
let prev { prev; _ } = prev
let value { value; _ } = value

let iter { head } ~f =
  let rec go = function
    | None -> ()
    | Some { value; next; _ } -> f value; go next
  in go head

let find { head } ~f =
  let rec go = function
    | None -> None
    | Some { value; next; _ } as el -> if f value then el else go next
  in go head

let insert_first l value =
  let new_head = { value; prev = None; next = l.head } in
    begin match l.head with
      | None -> ()
      | Some old_head ->
          old_head.prev <- Some new_head
    end;
    l.head <- Some new_head;
    new_head

let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
    begin match elt.next with
      | Some old_next -> old_next.prev <- Some new_elt
      | None -> ()
    end;
    elt.next <- Some new_elt;
    new_elt

let remove l ({ prev; next; _ } as elt) =
  begin match prev with
    | Some p -> p.next <- next
    | None -> l.head <- next (* we must be the head *)
  end;
  begin match next with
    | Some n -> n.prev <- prev
    | None -> ()
  end;
  elt.prev <- None;
  elt.next <- None (* does this matter? *)
