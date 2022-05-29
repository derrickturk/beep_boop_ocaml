(* ok, this sucks, usage-wise, but it's a fun use of polymorphic variants *)
module Fantomas: sig
    type 'a t

    val int: int -> int t
    val bool: bool -> bool t
    val if_then_else: bool t -> 'a t -> 'a t -> 'a t
    val eq: 'a t -> 'a t -> bool t
    val plus: int t -> int t -> int t

    val eval_int: int t -> int
    val eval_bool: bool t -> bool
end = struct
    type expr =
      | Int of int
      | Bool of bool
      | IfThenElse of expr * expr * expr
      | Eq of expr * expr
      | Plus of expr * expr

    type 'a t = expr

    let int x = Int x
    let bool x = Bool x
    let if_then_else b i e = IfThenElse (b, i, e)
    let eq x y = Eq (x, y)
    let plus x y = Plus (x, y)

    exception Type_error

    let rec eval = function
      | Int x -> `Int x
      | Bool x -> `Bool x
      | IfThenElse (b, i, e) -> begin match eval b with
          | `Bool c -> if c then eval i else eval e
          | _ -> raise Type_error
        end
      | Eq (x, y) -> `Bool (eval x = eval y)
      | Plus (x, y) -> match eval x, eval y with
          | `Int x, `Int y -> `Int (x + y)
          | _ -> raise Type_error

    let eval_int e = match eval e with
      | `Int x -> x
      | _ -> raise Type_error

    let eval_bool e = match eval e with
      | `Bool x -> x
      | _ -> raise Type_error
end

module Gadt: sig
    type 'a t

    val int: int -> int t
    val bool: bool -> bool t
    val if_then_else: bool t -> 'a t -> 'a t -> 'a t
    val eq: 'a t -> 'a t -> bool t
    val plus: int t -> int t -> int t

    val eval: 'a t -> 'a
end = struct
    type _ expr =
      | Lit: 'a -> 'a expr
      | IfThenElse: bool expr * 'a expr * 'a expr -> 'a expr
      | Eq: 'a expr * 'a expr -> bool expr
      | Plus: int expr * int expr -> int expr

    type 'a t = 'a expr

    let int x = Lit x
    let bool x = Lit x
    let if_then_else b i e = IfThenElse (b, i, e)
    let eq x y = Eq (x, y)
    let plus x y = Plus (x, y)

    let rec eval: type a. a expr -> a = function
      | Lit x -> x
      | IfThenElse (b, i, e) -> if eval b then eval i else eval e
      | Eq (x, y) -> eval x = eval y
      | Plus (x, y) -> eval x + eval y
end
