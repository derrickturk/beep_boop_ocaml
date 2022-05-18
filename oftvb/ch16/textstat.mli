type stats

val lines: stats -> int
val characters: stats -> int
val words: stats -> int
val sentences: stats -> int
val histogram: stats -> int array

val channel_stats: in_channel -> stats
val file_stats: string -> stats

val print_file_stats: string -> unit
