open! Core

type original =
  | Local_image of { part : int }
  | Local_document of
      { part : int
      ; media_type : string
      ; contents : string
      }

type t =
  { is_current : unit -> bool
  ; mutable parts_left : int
  ; mutable bytes_left : int
  ; mutable reversed : original list
  }

let create ~max_parts ~max_bytes ~is_current =
  { is_current; parts_left = max_parts; bytes_left = max_bytes; reversed = [] }
;;

let record t ~bytes original =
  if t.parts_left > 0 && bytes <= t.bytes_left && t.is_current ()
  then (
    t.parts_left <- t.parts_left - 1;
    t.bytes_left <- t.bytes_left - bytes;
    t.reversed <- original :: t.reversed)
;;

let record_image t ~part = record t ~bytes:0 (Local_image { part })

let record_document t ~part ~media_type ~contents =
  record t ~bytes:(String.length contents) (Local_document { part; media_type; contents })
;;

let take t =
  let reversed = t.reversed in
  t.reversed <- [];
  t.parts_left <- 0;
  if t.is_current () then List.rev reversed else []
;;
