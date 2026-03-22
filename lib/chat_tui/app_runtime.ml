open Core
open Eio.Std
module Manager = Chat_response.Moderator_manager
module Stream_moderator = Chat_response.In_memory_stream

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

type typeahead_op =
  | Typeahead of
      { sw : Switch.t
      ; id : int
      }
  | Starting_typeahead of { id : int }

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
  ; mutable typeahead_op : typeahead_op option
  ; moderator : Stream_moderator.moderator option
  ; mutable halted_reason : string option
  ; pending : queued_action Queue.t
  ; quit_via_esc : bool ref
  ; mutable next_op_id : int
  ; mutable cancel_streaming_on_start : bool
  ; mutable cancel_compaction_on_start : bool
  ; mutable cancel_typeahead_on_start : bool
  }

let visible_messages_of_history (t : t) (history : Openai.Responses.Item.t list)
  : Types.message list
  =
  match t.moderator with
  | None -> Conversation.of_history history
  | Some moderator ->
    Manager.effective_history moderator.manager history
    |> Result.ok_or_failwith
    |> Conversation.of_history
;;

let refresh_messages (t : t) : unit =
  Model.set_messages t.model (visible_messages_of_history t (Model.history_items t.model))
;;

let moderator_snapshot (t : t) : (Session.Moderator_snapshot.t option, string) result =
  match t.moderator with
  | None -> Ok None
  | Some moderator -> Result.map (Manager.snapshot moderator.manager) ~f:Option.some
;;

let create ?moderator ?halted_reason ~model () =
  { model
  ; op = None
  ; typeahead_op = None
  ; moderator
  ; halted_reason
  ; pending = Queue.create ()
  ; quit_via_esc = ref false
  ; next_op_id = 0
  ; cancel_streaming_on_start = false
  ; cancel_compaction_on_start = false
  ; cancel_typeahead_on_start = false
  }
;;

let alloc_op_id t =
  let id = t.next_op_id in
  t.next_op_id <- id + 1;
  id
;;
