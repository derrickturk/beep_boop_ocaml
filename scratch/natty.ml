type nat =
  | Zero
  | Succ of nat

let rec take n xs = match n, xs with
  | Zero, _ -> []
  | _, [] -> []
  | Succ n, hd::tl -> hd::take n tl

let of_int n =
  let rec real_of_int = function
    | 0 -> Zero
    | n -> Succ (real_of_int (n - 1))
  in if n < 0 then None else Some (real_of_int n)

let rec to_int = function
  | Zero -> 0
  | Succ n -> 1 + to_int n
