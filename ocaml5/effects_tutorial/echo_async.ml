(* echo_async.ml, from https://github.com/ocamllabs/ocaml-effects-tutorial,
 * ported to Ocaml 5.0 alpha
 * (this shit doesn't work in utop)
 *)

open Effect
open Effect.Deep
open Printf

module type Aio = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)

  val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
  val recv   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
  val send   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int

  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Echo = struct
  open Printexc
  open Printf

  module Make (Aio : sig
    val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
    val recv   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
    val send   : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int
    val fork   : (unit -> unit) -> unit
    val run    : (unit -> unit) -> unit
    val non_blocking_mode : bool
    (* Are the sockets non-blocking *)
  end) = struct
    let send sock str =
      let len = Bytes.length str in
      let total = ref 0 in
      (try
          while !total < len do
            let write_count = Aio.send sock str !total (len - !total) [] in
            total := write_count + !total
          done
        with _ -> ()
        );
      !total

    let recv sock maxlen =
      let str = Bytes.create maxlen in
      let recvlen =
        try Aio.recv sock str 0 maxlen []
        with _ -> 0
      in
      Bytes.sub str 0 recvlen

    let close sock =
      try Unix.shutdown sock Unix.SHUTDOWN_ALL
      with _ -> () ;
      Unix.close sock

    let string_of_sockaddr = function
      | Unix.ADDR_UNIX s -> s
      | Unix.ADDR_INET (inet,port) ->
          (Unix.string_of_inet_addr inet) ^ ":" ^ (string_of_int port)

    (* Repeat what the client says until the client goes away. *)
    let rec echo_server sock addr =
      try
        let data = recv sock 1024 in
        if Bytes.length data > 0 then
          (ignore (send sock (Bytes.cat (Bytes.of_string ("server says: ")) data));
          echo_server sock addr)
        else
          let cn = string_of_sockaddr addr in
          (printf "echo_server : client (%s) disconnected.\n%!" cn;
          close sock)
      with
      | _ -> close sock

    let server () =
      (* Server listens on localhost at 9301 *)
      let addr, port = Unix.inet_addr_loopback, 9301 in
      printf "Echo server listening on 127.0.0.1:%d\n%!" port;
      let saddr = Unix.ADDR_INET (addr, port) in
      let ssock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      (* SO_REUSEADDR so we can restart the server quickly. *)
      Unix.setsockopt ssock Unix.SO_REUSEADDR true;
      Unix.bind ssock saddr;
      Unix.listen ssock 20;
      (* Socket is non-blocking *)
      if Aio.non_blocking_mode then Unix.set_nonblock ssock;
      try
        (* Wait for clients, and fork off echo servers. *)
        while true do
          let client_sock, client_addr = Aio.accept ssock in
          let cn = string_of_sockaddr client_addr in
          printf "server : client (%s) connected.\n%!" cn;
          if Aio.non_blocking_mode then Unix.set_nonblock client_sock;
          Aio.fork (fun () -> echo_server client_sock client_addr)
        done
      with
      | e ->
          print_endline @@ Printexc.to_string e;
          close ssock

    let start () = Aio.run server
  end
end

module Aio : Aio = struct

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  type file_descr = Unix.file_descr
  type sockaddr = Unix.sockaddr
  type msg_flag = Unix.msg_flag

  type _ Effect.t +=
    | Async: (unit -> 'a) -> 'a promise Effect.t
    | Yield: unit Effect.t
    | Await: 'a promise -> 'a Effect.t
    | Accept: file_descr -> (file_descr * sockaddr) Effect.t
    | Recv: file_descr * bytes * int * int * msg_flag list -> int Effect.t
    | Send: file_descr * bytes * int * int * msg_flag list -> int Effect.t

  let async f = perform (Async f)

  let yield () = perform Yield

  let await p = perform (Await p)

  let accept fd = perform (Accept fd)

  let recv fd buf pos len mode = perform (Recv (fd, buf, pos, len, mode))

  let send fd bus pos len mode = perform (Send (fd, bus, pos, len, mode))

  (********************)

  let ready_to_read fd =
    match Unix.select [fd] [] [] 0. with
    | [], _, _ -> false
    | _ -> true

  let ready_to_write fd =
    match Unix.select [] [fd] [] 0. with
    | _, [], _ -> false
    | _ -> true

  let q = Queue.create ()
  let enqueue t = Queue.push t q

  type blocked = Blocked : 'a Effect.t * ('a, unit) continuation -> blocked

  (* tasks blocked on reads *)
  let br = Hashtbl.create 13
  (* tasks blocked on writes *)
  let bw = Hashtbl.create 13

  let rec schedule () =
    if not (Queue.is_empty q) then
      (* runnable tasks available *)
      Queue.pop q ()
    else if Hashtbl.length br = 0 && Hashtbl.length bw = 0 then
      (* no runnable tasks, and no blocked tasks => we're done. *)
      ()
    else begin (* no runnable tasks, but blocked tasks available *)
      let rd_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) br [] in
      let wr_fds = Hashtbl.fold (fun fd _ acc -> fd::acc) bw [] in
      let rdy_rd_fds, rdy_wr_fds, _ = Unix.select rd_fds wr_fds [] (-1.) in
      let rec resume ht = function
        | [] -> ()
        | x::xs ->
            begin match Hashtbl.find ht x with
            | Blocked (Recv (fd, buf, pos, len, mode), k) ->
                enqueue (fun () -> continue k (Unix.recv fd buf pos len mode))
            | Blocked (Accept fd, k) ->
                enqueue (fun () -> continue k (Unix.accept fd))
            | Blocked (Send (fd, buf, pos, len, mode), k) ->
                enqueue (fun () -> continue k (Unix.send fd buf pos len mode))
            | Blocked _ -> failwith "impossible"
            end;
            Hashtbl.remove ht x;
            resume ht xs
      in
      resume br rdy_rd_fds;
      resume br rdy_wr_fds;
      schedule ()
    end

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main -> match_with main ()
        { retc = begin fun v ->
             let l = match !pr with
               | Waiting l -> l
               | _ -> failwith "impossible"
             in
             List.iter (fun k -> enqueue (fun () -> continue k v)) l;
             pr := Done v;
             schedule ()
           end
        ; exnc = raise
        ; effc = fun (type b) (eff: b Effect.t) -> match eff with
            | Async f -> Some (fun (k: (b, _) continuation) ->
                let pr = ref (Waiting []) in
                enqueue (fun () -> continue k pr);
                fork pr f
              )
            | Yield -> Some (fun (k: (b, _) continuation) ->
                enqueue (continue k);
                schedule ()
              )
            | Await p -> Some (fun (k: (b, _) continuation) ->
                begin match !p with
                | Done v -> continue k v
                | Waiting l -> begin
                    p := Waiting (k::l);
                    schedule ()
                  end
                end
              )
            | Accept fd as e -> Some (fun (k: (b, _) continuation) ->
                if ready_to_read fd then
                  continue k (Unix.accept fd)
                else begin
                  Hashtbl.add br fd (Blocked (e, k));
                  schedule ()
                end
              )
            | Send (fd,buf,pos,len,mode) as e -> Some (fun (k: (b, _) continuation) ->
                if ready_to_write fd then
                  continue k (Unix.send fd buf pos len mode)
                else begin
                  Hashtbl.add bw fd (Blocked (e, k));
                  schedule ()
                end
              )
            | Recv (fd,buf,pos,len,mode) as e -> Some (fun (k: (b, _) continuation) ->
                if ready_to_read fd then
                  continue k (Unix.recv fd buf pos len mode)
                else begin
                  Hashtbl.add br fd (Blocked (e, k));
                  schedule ()
                end
              )
            | _ -> None
        }
    in
    fork (ref (Waiting [])) main
end

module M = Echo.Make(struct
  let accept = Aio.accept
  let recv = Aio.recv
  let send = Aio.send
  let fork f = ignore (Aio.async f)
  let run f = Aio.run f
  let non_blocking_mode = true
end)

let _ = M.start ()
