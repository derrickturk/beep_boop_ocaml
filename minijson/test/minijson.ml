open Minijson

let () =
  assert (Parser.null "null, 1, 2, 3" = Some (Json.Null, ", 1, 2, 3"));
  assert (Parser.null "something else" = None);
  assert (Parser.bool "true, 1, 2, 3" = Some (Json.Bool true, ", 1, 2, 3"));
  assert (Parser.bool "false, 1, 2, 3" = Some (Json.Bool false, ", 1, 2, 3"));
  assert (Parser.bool "something else" = None);
  assert (Parser.string "\"an easy one\", 1, 2, 3" = Some (Json.String "an easy one", ", 1, 2, 3"));
  assert (Parser.string "\"oh god \\\"why\\thave you done this\\\"\", 1, 2, 3" = Some (Json.String "oh god \"why\thave you done this\"", ", 1, 2, 3"));
  assert (Parser.string "something else" = None);
  assert (Parser.list "[true, null, false, \"potato\"], yada"
    = Some (Json.List [Json.Bool true; Json.Null; Json.Bool false; Json.String "potato"], ", yada"));
  assert (Parser.list "[ true ], yada"
    = Some (Json.List [Json.Bool true], ", yada"));
  assert (Parser.list "[], yada"
    = Some (Json.List [], ", yada"));
  assert (Parser.list "[true, ], yada" = None);
  assert (Parser.list "something else" = None);
  assert (Parser.json "null, 1, 2, 3" = Some (Json.Null, ", 1, 2, 3"));
  assert (Parser.json "something else" = None);
  assert (Parser.json "true, 1, 2, 3" = Some (Json.Bool true, ", 1, 2, 3"));
  assert (Parser.json "false, 1, 2, 3" = Some (Json.Bool false, ", 1, 2, 3"));
  assert (Parser.json "something else" = None);
  assert (Parser.json "\"an easy one\", 1, 2, 3" = Some (Json.String "an easy one", ", 1, 2, 3"));
  assert (Parser.json "\"oh god \\\"why\\thave you done this\\\"\", 1, 2, 3" = Some (Json.String "oh god \"why\thave you done this\"", ", 1, 2, 3"));
  assert (Parser.json "something else" = None);
  assert (Parser.json "[true, null, false, \"potato\"], yada"
    = Some (Json.List [Json.Bool true; Json.Null; Json.Bool false; Json.String "potato"], ", yada"));
  assert (Parser.json "[ true ], yada"
    = Some (Json.List [Json.Bool true], ", yada"));
  assert (Parser.json "[], yada"
    = Some (Json.List [], ", yada"));
  assert (Parser.json "[true, ], yada" = None);
  assert (Parser.json "something else" = None);
  assert (Parser.list "[[[[null]]]]" = Some (Json.List [Json.List [Json.List [Json.List [Json.Null]]]], ""))
