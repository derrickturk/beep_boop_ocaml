type error = { expected: string; found: string }

type 'a t = string -> ('a * string, error) result

let parse p inp = p inp

let parse_all p inp = match p inp with
  | Ok (matched, "") -> Ok matched
  | Ok (_, rest) ->
      Error { expected = "<end of input>"; found = String.escaped rest }
  | Error _ as e -> e

let map f p = fun inp -> match p inp with
  | Ok (matched, rest) -> Ok (f matched, rest)
  | Error e -> Error e

let return x = fun inp -> Ok (x, inp)

let ap pf px = fun inp -> match pf inp with
  | Ok (f, rest) -> begin match px rest with
      | Ok (x, rest) -> Ok (f x, rest)
      | Error e -> Error e
    end
  | Error e -> Error e

let prod p1 p2 = fun inp -> match p1 inp with
  | Ok (m1, rest) -> begin match p2 rest with
      | Ok (m2, rest) -> Ok ((m1, m2), rest)
      | Error e -> Error e
    end
  | Error e -> Error e

let bind p f = fun inp -> match p inp with
  | Ok (matched, rest) -> (f matched) rest
  | Error _ as e -> e

let (let*) p f = bind p f
let (let+) p f = map f p
let (and+) p1 p2 = prod p1 p2

let (<$>) f p = map f p
let (<*>) pf px = ap pf px
let (>>=) p f = bind p f

let ( *> ) p1 p2 = let* _ = p1 in p2

let ( <* ) p1 p2 =
  let* x = p1 in
  let* _ = p2 in
  return x

let (>>|) p f = map f p

let opt p inp = match p inp with
  | Ok (x, rest) -> Ok (Some x, rest)
  | _ -> Ok (None, inp)

(* TODO: thread the expected text around... *)
let alt p1 p2 inp = match p1 inp with
  | Ok _ as o -> o
  | _ -> p2 inp

let empty inp = Error { expected = "<failure>"; found = inp }

let (<|>) p1 p2 = alt p1 p2

let l lit inp =
  let open String in
  let len = length lit in
  if starts_with ~prefix:lit inp
    then Ok (lit, sub inp len (length inp - len))
    else Error { expected = escaped lit; found = escaped inp }

let ws inp = Ok ((), String.trim inp)

let lexeme p = p <* ws

(* this is super super ugly *)
let chars p inp =
  let open String in
  let s = to_seq inp in
  let remainder = ref s in
  let rec matching seq () = match seq () with
    | Seq.Cons (hd, tl) when p hd -> Seq.Cons (hd, fun () -> matching tl ())
    | Seq.Cons (_, _) as c ->
        remainder := (fun () -> c);
        Seq.Nil
    | Seq.Nil -> 
        remainder := (fun () -> Nil);
        Seq.Nil
  in
  let matched = of_seq (matching s) in
  Ok (matched, of_seq !remainder)

let chars1 p ~expected inp = match chars p inp with
  | Ok ("", _) -> Error { expected; found = String.escaped inp }
  | res -> res

let digits1 =
  let isdigit = function
    | '0'..'9' -> true
    | _ -> false
  in chars1 isdigit ~expected:"digits 0-9"

let unsigned_int = int_of_string <$> digits1

let signed_int =
  let sign = opt ((l "+" *> return `Plus) <|> (l "-" *> return `Minus)) in
  let* sign in
  let* num = unsigned_int in
  match sign with
    | None | Some `Plus -> return num
    | Some `Minus -> return (-num)