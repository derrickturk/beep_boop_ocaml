(* ocamlfind ocamlopt -thread -linkpkg qsort.ml -package core -package ctypes -package ctypes.foreign -o qsort *)

open Core
open Ctypes
open PosixTypes
open Foreign

let compare_t = ptr void @-> ptr void @-> returning int

let qsort = foreign "qsort"
  (ptr void @-> size_t @-> size_t @-> funptr compare_t @-> returning void)

let qsort' cmp arr =
  let open Unsigned.Size_t in
  let ty = CArray.element_type arr in
  let len = of_int (CArray.length arr) in
  let elsize = of_int (sizeof ty) in
  let start = to_voidp (CArray.start arr) in
  let compare l r = cmp !@(from_voidp ty l) !@(from_voidp ty r) in
  qsort start len elsize compare

let sort_stdin () =
  let arr =
    In_channel.input_line_exn In_channel.stdin
    |> String.split ~on:' '
    |> List.map ~f:int_of_string
    |> CArray.of_list int
  in
  qsort' Int.compare arr;
  CArray.to_list arr
    |> List.map ~f:Int.to_string |> String.concat ~sep:" " |> print_endline

let () =
  sort_stdin ()
