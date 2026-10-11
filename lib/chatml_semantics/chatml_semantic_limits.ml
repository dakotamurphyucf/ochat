open Core
module Recorder = Chatml_typechecker.Semantic
module Frozen = Chatml_typechecker.Type_snapshot

type t =
  { max_input_bytes : int
  ; recording : Recorder.Limits.t
  ; type_graph : Frozen.Limits.t
  ; max_published_facts : int
  ; max_published_edges : int
  ; max_published_label_bytes : int
  ; max_work_steps : int
  }

let create
      ~max_input_bytes
      ~recording
      ~type_graph
      ~max_published_facts
      ~max_published_edges
      ~max_published_label_bytes
      ~max_work_steps
  =
  if
    max_input_bytes <= 0
    || max_published_facts <= 0
    || max_published_edges <= 0
    || max_published_label_bytes <= 0
    || max_work_steps <= 0
  then Or_error.error_string "Semantic analysis limits must be positive"
  else
    Ok
      { max_input_bytes
      ; recording
      ; type_graph
      ; max_published_facts
      ; max_published_edges
      ; max_published_label_bytes
      ; max_work_steps
      }
;;

let max_input_bytes t = t.max_input_bytes
let recording t = t.recording
let type_graph t = t.type_graph
let max_published_facts t = t.max_published_facts
let max_published_edges t = t.max_published_edges
let max_published_label_bytes t = t.max_published_label_bytes
let max_work_steps t = t.max_work_steps
