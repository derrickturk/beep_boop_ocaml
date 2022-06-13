(* ocamlfind ocamlc -linkpkg -package yojson kessel.mli kessel.ml kessel_json.mli kessel_json.ml json_demo.ml -g -o json_demo *)

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

let (list, assoc, json) =
  let list' (_, _, j) =
    let* _ = lexeme (l "[") in
    let* contents = sep_by ~sep:(lexeme (l ",")) (lexeme j) in
    let* _ = l "]" in
    return (`List contents)
  in
  let assoc' (_, _, j) =
    let kv =
      let+ k = lexeme string_literal
      and+ _ = lexeme (l ":")
      and+ v = j
      in (k, v)
    in
    let* _ = lexeme (l "{") in
    let* contents = sep_by ~sep:(lexeme (l ",")) (lexeme kv) in
    let* _ = l "}" in
    return (`Assoc contents)
  in
  let json' (l, a, _) = int <|> bool <|> null <|> string <|> l <|> a
  in
  fix3 (fun ps -> (list' ps, assoc' ps, json' ps))
