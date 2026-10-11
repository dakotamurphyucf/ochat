open! Core

type t =
  { value : kind
  ; span : Source.span
  }

and kind =
  | Name of string
  | Wildcard
  | Tuple of t Chatml_tuple.t
[@@deriving sexp]

let binders ?(checkpoint = fun () -> ()) t =
  let rec collect result t =
    checkpoint ();
    match t.value with
    | Name name -> (name, t.span) :: result
    | Wildcard -> result
    | Tuple children -> Chatml_tuple.fold children ~init:result ~f:collect
  in
  List.rev (collect [] t)
;;

let count ?(checkpoint = fun () -> ()) t =
  let rec collect result t =
    checkpoint ();
    match t.value with
    | Name _ -> result + 1
    | Wildcard -> result
    | Tuple children -> Chatml_tuple.fold children ~init:result ~f:collect
  in
  collect 0 t
;;
