type basic_color =
  [ `Black
  | `Blue
  | `Cyan
  | `Green
  | `Magenta
  | `Red
  | `White
  | `Yellow
  ]

type color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB of int * int * int
  ]

type extended_color =
  [ color
  | `RGBA of int * int * int * int
  ]

let basic_color_to_int = function
  | `Black -> 0
  | `Blue -> 1
  | `Cyan -> 2
  | `Green -> 3
  | `Magenta -> 4
  | `Red -> 5
  | `White -> 6
  | `Yellow -> 7

let color_to_int = function
  | `Basic (c, w) -> let base = match w with `Bold -> 8 | `Regular -> 0 in
      base + basic_color_to_int c
  | `RGB (r, g, b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i

let extended_color_to_int = function
  | `RGBA (r, g, b, a) -> 256 + a + b * 6 + g * 36 + r * 216
  | `Gray i -> 2000 + i
  | #color as c -> color_to_int c
