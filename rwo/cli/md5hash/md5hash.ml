open Core

let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

let command = Command.basic
  ~summary:"Compute  the MD5 hash of a file"
  ~readme:(fun () -> "LOL")
  Command.Param.(map (anon ("filename" %: string))
    ~f:(fun filename () -> do_hash filename))

let () = Command_unix.run ~version:"0.1" ~build_info:"LOL" command
