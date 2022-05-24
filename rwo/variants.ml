open Base

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = { field: mail_field
                      ; contains: string
                      }

let test field contains = Base { field; contains }

let ex_expr = And [ Or [ test To "doligez"
                       ; test CC "doligez"
                       ]
                  ; test Subject "runtime"
                  ]

let rec eval base_eval =
  let eval' = eval base_eval in function
    | Base b -> base_eval b
    | Const b -> b
    | And es -> List.for_all es ~f:eval'
    | Or es -> List.exists es ~f:eval'
    | Not e -> not (eval' e)

let rec simplify =
  let is_const b = function
    | Const b' -> Bool.(b = b')
    | _ -> false
  in
  let simplify_and es =
    if List.exists es ~f:(is_const false)
      then Const false
      else
        match List.filter es ~f:(fun x -> not (is_const true x)) with
        | [] -> Const true
        | [x] -> x
        | es' -> And es'
  in
  let simplify_or es =
    if List.exists es ~f:(is_const true)
      then Const true
      else
        match List.filter es ~f:(fun x -> not (is_const false x)) with
        | [] -> Const false
        | [x] -> x
        | es' -> Or es'
  in
  let simplify_not = function
    | Const b -> Const (not b)
    | Not e -> e
    | Base _ | And _ | Or _ as e -> Not e
  in function
    | Base _ | Const _ as x -> x
    | And es -> simplify_and (List.map es ~f:simplify)
    | Or es -> simplify_or (List.map es ~f:simplify)
    | Not e -> simplify_not (simplify e)
