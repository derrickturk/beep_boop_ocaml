open Base

let max_widths hdr rows =
  let widths = List.map ~f:String.length in
  let hdr_widths = widths hdr in
  let rows_widths = List.map ~f:widths rows in
  let elt_max = List.map2_exn ~f:max in
  List.fold ~f:elt_max ~init:hdr_widths rows_widths

let render_hrule widths =
  let bars = List.map ~f:(fun w -> String.make w '-') widths in
  "|-" ^ String.concat ~sep:"-+-" bars ^ "-|"

let pad s length = s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let padded = List.map2_exn ~f:pad row widths in
  "| " ^ String.concat ~sep:" | " padded ^ " |"

let render_table hdr rows =
  let widths = max_widths hdr rows in
  let lines = render_row hdr widths
    :: render_hrule widths
    :: List.map ~f:(fun r -> render_row r widths) rows
  in String.concat ~sep:"\n" lines
