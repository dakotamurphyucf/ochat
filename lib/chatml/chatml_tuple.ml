open! Core

type 'a t =
  { first : 'a
  ; second : 'a
  ; rest : 'a list
  }
[@@deriving sexp, compare]

let create first second ~rest = { first; second; rest }

let of_list = function
  | first :: second :: rest -> Some { first; second; rest }
  | [] | [ _ ] -> None
;;

let to_list t = t.first :: t.second :: t.rest
let length t = 2 + List.length t.rest

let map t ~f =
  let first = f t.first in
  let second = f t.second in
  let rest = List.map t.rest ~f in
  { first; second; rest }
;;

let iter t ~f =
  f t.first;
  f t.second;
  List.iter t.rest ~f
;;

let fold t ~init ~f =
  let acc = f init t.first in
  let acc = f acc t.second in
  List.fold t.rest ~init:acc ~f
;;
