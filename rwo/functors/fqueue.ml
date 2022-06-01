open Base

type 'a t = 'a list * 'a list

let empty = ([], [])

let enqueue (front, back) x = (front, x::back)

let dequeue (front, back) = match front with
  | hd::tl -> Some (hd, (tl, back))
  | [] -> match List.rev back with
      | [] -> None
      | hd::tl -> Some (hd, (tl, []))

let fold (front, back) ~init ~f =
  let fold_front = List.fold front ~init ~f in
  List.fold_right back ~init:fold_front ~f:(Fn.flip f)
