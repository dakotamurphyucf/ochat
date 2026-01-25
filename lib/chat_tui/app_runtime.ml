open Core
open Eio.Std

type op =
  | Streaming of
      { sw : Switch.t
      ; id : int
      }
  | Compacting of
      { sw : Switch.t
      ; id : int
      }
  | Starting_streaming of { id : int }
  | Starting_compaction of { id : int }

type submit_request =
  { text : string
  ; draft_mode : Model.draft_mode
  }

type queued_action =
  | Submit of submit_request
  | Compact

type t =
  { model : Model.t
  ; mutable op : op option
  ; pending : queued_action Queue.t
  ; quit_via_esc : bool ref
  ; mutable next_op_id : int
  ; mutable cancel_streaming_on_start : bool
  ; mutable cancel_compaction_on_start : bool
  }

let create ~model =
  { model
  ; op = None
  ; pending = Queue.create ()
  ; quit_via_esc = ref false
  ; next_op_id = 0
  ; cancel_streaming_on_start = false
  ; cancel_compaction_on_start = false
  }
;;

let alloc_op_id t =
  let id = t.next_op_id in
  t.next_op_id <- id + 1;
  id
;;
