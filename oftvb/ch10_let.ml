(* ch 10, done with custom monadic and applicative let bindings for 'a option,
 *   and with let-punning in local binds
 *)

type expr =
  | Lit of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

let (let*) o f = match o with
  | None -> None
  | Some x -> f x

let (let+) o f = match o with
  | None -> None
  | Some x -> Some (f x)

let (and+) o1 o2 = match o1, o2 with
  | Some x, Some y -> Some (x, y)
  | _ -> None

(* from before *)
let rec power x n =
  if n < 0
    then raise (Invalid_argument "negative power")
    else if n = 0
      then 1
      else x * power x (n - 1)

let rec eval = function
  | Lit n -> Some n
  | Add (x, y) -> let+ x' = eval x and+ y' = eval y in x' + y'
  | Sub (x, y) -> let+ x' = eval x and+ y' = eval y in x' - y'
  | Mul (x, y) -> let+ x' = eval x and+ y' = eval y in x' * y'
  | Div (x, y) -> let* y' = eval y in
      if y' = 0
        then None
        else let* x' = eval x in Some (x' / y')
  | Pow (x, y) -> let+ x' = eval x and+ y' = eval y in power x' y'
