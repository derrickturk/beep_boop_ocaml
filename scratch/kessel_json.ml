open Kessel

let int = let+ n = signed_int in `Int n

let bool =
  let+ b = (l "true" *> return true) <|> (l "false" *> return false) in
  `Bool b

let null = l "null" *> return `Null

let string_literal =
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
  return (String.concat "" contents)

let string =
  let+ s = string_literal in
  `String s

(* I can't get this to typecheck without an explicit `fix` - dunno why *)

let list' json =
  print_endline "list'";
  let* _ = lexeme (l "[") in
  let* contents = sep_by ~sep:(lexeme (l ",")) (lexeme json) in
  let* _ = l "]" in
  return (`List contents)

let assoc' json =
  print_endline "assoc'";
  let kv =
    let+ k = lexeme string_literal
    and+ _ = lexeme (l ":")
    and+ v = json
    in (k, v)
  in
  let* _ = lexeme (l "{") in
  let* contents = sep_by ~sep:(lexeme (l ",")) (lexeme kv) in
  let* _ = l "}" in
  return (`Assoc contents)

let json' json = int <|> bool <|> null <|> string <|> list' json <|> assoc' json

let json =
  (*
  let rec json_inner inp = parse (json' json_inner) inp in
  make json_inner
  *)
  let rec fix f = f (fix f) in
  fix json'

let list = list' json

let assoc = assoc' json
