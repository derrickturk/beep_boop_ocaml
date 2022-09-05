type 'a parser = string -> ('a * string) option

let rec alt ps input = match ps with
  | [] -> None
  | p :: tl -> match p input with
      | Some _ as good -> good
      | _ -> alt tl input

let (.?[]) s ix = try
  Some (s.[ix])
with _ -> None

let literal lit ret input =
  if String.starts_with input ~prefix:lit
    then
      let total_len = String.length input in
      let prefix_len = String.length lit in
      let rem = String.sub input prefix_len (total_len - prefix_len) in
      Some (ret, rem)
    else None

let lexeme p input =
  match p input with
    | Some (ret, rest) ->
        let is_space = function
          | ' ' | '\n' | '\t' -> true
          | _ -> false
        in
        let i = ref 0 in
        while Option.map is_space (rest.?[!i]) = Some true do
          i := !i + 1
        done;
        Some (ret, String.sub rest !i (String.length rest - !i))
    | None -> None

let sandwich p_begin p_end p input =
  match p_begin input with
    | Some (_, rest) -> begin
        match p rest with
          | Some (ret, rest') -> begin
              match p_end rest' with
                | Some (_, rest'') -> Some (ret, rest'')
                | _ -> None
            end
          | _ -> None
      end
    | _ -> None

type escmode =
  | NotEscaped
  | Escaped

let string input =
  let rec rest_of_string so_far esc ix =
    match esc with
      | NotEscaped -> begin
          match input.?[ix] with
            | Some '\\' -> rest_of_string so_far Escaped (ix + 1)
            | Some '"' ->
                let result = so_far
                  |> List.rev |> List.to_seq |> String.of_seq
                in
                let prefix_len = ix + 1 in
                let rest =
                  String.sub input prefix_len (String.length input - prefix_len)
                in
                Some (Json.String result, rest)
            | Some c -> rest_of_string (c :: so_far) NotEscaped (ix + 1)
            | None -> None
        end
      | Escaped ->
          match input.?[ix] with
            | Some 't' -> rest_of_string ('\t' :: so_far) NotEscaped (ix + 1)
            | Some 'n' -> rest_of_string ('\n' :: so_far) NotEscaped (ix + 1)
            | Some '"' -> rest_of_string ('"' :: so_far) NotEscaped (ix + 1)
            | Some '\\' -> rest_of_string ('\\' :: so_far) NotEscaped (ix + 1)
            | Some c -> rest_of_string (c :: so_far) NotEscaped (ix + 1)
            | None -> None
  in
  match input.?[0] with
    | Some '"' -> rest_of_string [] NotEscaped 1
    | _ -> None

let bool input =
  let true_p input = literal "true" (Json.Bool true) input in
  let false_p input = literal "false" (Json.Bool false) input in
  alt [true_p; false_p] input

let null input = literal "null" Json.Null input

let rec list input =
  let list_innards input =
    let rec go so_far inp =
      match (lexeme (literal "," ()) inp) with
        | Some ((), inp') -> begin match lexeme json inp' with
            | Some (obj, inp'') -> go (obj :: so_far) inp''
            | None -> None
          end
        | None -> Some (Json.List (List.rev so_far), inp)
    in
    match lexeme json input with
      | Some (obj, rest) -> go [obj] rest
      | None -> Some (Json.List [], input)
  in
  sandwich
    (lexeme (literal "[" ()))
    (literal "]" ())
    list_innards
    input
and json input = alt [null; bool; string; list] input
