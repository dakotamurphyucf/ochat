open! Core

(** Closed durable changes applied by the session actor and recovery. *)

type t =
  | Batch of t list
  | Created of Session_state.t
  | Lifecycle_changed of Session_state.Lifecycle.t
  | Workspace_changed of Workspace_instance.t
  | Canonical_entries_appended of Agent_protocol.History.entry list
  | Canonical_history_replaced of Agent_protocol.History.entry list
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Attachment_added of Agent_protocol.Session.Attachment.t
  | Attachment_removed of Agent_protocol.Id.Attachment.t
  | Permission_changed of Agent_protocol.Permission.t
  | Grant_changed of Agent_protocol.Grant.t
  | Job_changed of Agent_protocol.Job.t
  | Schedule_changed of Agent_protocol.Schedule.t
  | Invocation_changed of Agent_protocol.Invocation.t
  | Invocation_reconciled of Agent_protocol.Invocation.t
  (** Recovery-only terminalization/publication of an existing invocation,
        including older generations. Cannot admit, dispatch or create outcomes. *)
  | Subscription_changed of Agent_protocol.Subscription.t
  | Delivery_changed of Agent_protocol.Delivery.t
  | Delivery_committed of Agent_protocol.Delivery.t * Agent_protocol.History.entry
  | Moderator_changed of Jsonaf.t option
  | Shell_changed of Session.Shell_state.t
  | History_block_reserved of int64
  | Compaction_generation_changed of int
  | Compaction_archived of Session_state.Compaction_archive.t
  | Owner_lease_generation_changed of int64
  | Failure_changed of Agent_protocol.Error.t option
  | Halt_changed of string option
  | Reset_generation of int
[@@deriving sexp]

val apply : Session_state.t -> t -> (Session_state.t, Agent_protocol.Error.t) result
