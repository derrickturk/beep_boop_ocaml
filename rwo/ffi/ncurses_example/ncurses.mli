type window

val initscr : unit -> window
val newwin : int -> int -> int -> int -> window
val endwin : unit -> unit
val refresh : unit -> unit
val wrefresh : window -> unit
val addstr : string -> unit
val mvwaddch : window -> int -> int -> char -> int
val mvwaddstr : window -> int -> int -> string -> unit
val box : window -> char -> char -> unit
val cbreak : unit -> int
