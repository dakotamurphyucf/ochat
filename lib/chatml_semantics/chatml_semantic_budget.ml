open Core
module Limits = Chatml_semantic_limits
module Error = Chatml_semantic_error

type t =
  { limits : Limits.t
  ; mutable facts : int
  ; mutable edges : int
  ; mutable labels : int
  ; mutable work : int
  }

exception Rejected of Error.t

let reject error = raise (Rejected error)

let take_exn = function
  | Ok value -> value
  | Error error -> reject error
;;

let create limits = { limits; facts = 0; edges = 0; labels = 0; work = 0 }

let add current delta =
  if delta < 0 || current > Int.max_value - delta then reject Accounting_overflow;
  current + delta
;;

let admit t ~facts ~edges ~labels =
  let facts = add t.facts facts in
  let edges = add t.edges edges in
  let labels = add t.labels labels in
  if facts > Limits.max_published_facts t.limits then reject Publication_fact_limit;
  if edges > Limits.max_published_edges t.limits then reject Publication_edge_limit;
  if labels > Limits.max_published_label_bytes t.limits
  then reject Publication_label_byte_limit;
  t.facts <- facts;
  t.edges <- edges;
  t.labels <- labels
;;

let run t ~checkpoint ~f =
  Chatml.Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let checkpoint () =
      let work = add t.work 1 in
      if work > Limits.max_work_steps t.limits then reject Work_limit;
      t.work <- work;
      checkpoint ()
    in
    try Ok (f ~checkpoint) with
    | Rejected error -> Error error)
;;
