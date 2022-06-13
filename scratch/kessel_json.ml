open Kessel

let int = let+ n = signed_int in `Int n

let bool =
  let+ b = (l "true" *> return true) <|> (l "false" *> return false) in
  `Bool b

let null = l "null" *> return `Null

let string =
  let escaped =  (l "\\b" *> return "\b")
             <|> (l "\\f" *> return "\x0c")
             <|> (l "\\n" *> return "\n")
             <|> (l "\\r" *> return "\r")
             <|> (l "\\t" *> return "\t")
             <|> (l "\\\"" *> return "\"")
             <|> (l "\\\\" *> return "\\")
  in
  let ordinary = chars1 (fun c -> c <> '\\' && c <> '"')
    ~expected:"characters except '\\' or '\"'"
  in
  let* _ = l "\"" in
  let* contents = many (escaped <|> ordinary) in
  let* _ = l "\"" in
  return (`String (String.concat "" contents))

(* I can't get this to typecheck without an explicit `fix` - dunno why *)

let list' json =
  let* _ = l "[" in
  let* contents = sep_by ~sep:(lexeme (l ",")) (lexeme json) in
  let* _ = l "]" in
  return (`List contents)

let json' json = int <|> bool <|> null <|> string <|> list' json

let json =
  let rec fix f = f (fix f) in
  fix json'

let list = list' json
