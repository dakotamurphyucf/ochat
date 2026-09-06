(** Durable session events and recoverable operation-scoped live events. *)

module Durable : sig
  type kind =
    | Session_created
    | Session_state_changed
    | Session_updated
    | Attachment_owner_changed
    | History_message_deferred
    | History_appended
    | History_replaced
    | Moderator_overlay_changed
    | Moderator_notification
    | Permission_requested
    | Permission_resolved
    | Grant_created
    | Grant_revoked
    | Operation_started
    | Operation_completed
    | Operation_failed
    | Operation_cancelled
    | Operation_interrupted
    | Job_state_changed
    | Schedule_created
    | Schedule_state_changed
    | Schedule_cancelled
    | Prompt_upgraded
    | Workspace_state_changed
    | Session_error
  [@@deriving compare, equal, sexp]

  type visibility =
    | Full
    | Redacted
    | Hidden
  [@@deriving compare, equal, sexp]

  module Payload : sig
    type lifecycle_change =
      { desired_state : Session.desired_state
      ; observed_state : Session.observed_state
      }
    [@@deriving sexp]

    type prompt_upgrade =
      { prompt_id : Id.Prompt_definition.t
      ; previous_revision : Id.Prompt_revision.t
      ; current_revision : Id.Prompt_revision.t
      }
    [@@deriving sexp]

    type t =
      | Session_created of Session.t
      | Session_state_changed of lifecycle_change
      | Session_updated of Session.t
      | Attachment_owner_changed of Session.Attachment.t option
      | History_message_deferred of History.entry
      | History_appended of History.entry list
      | History_replaced of History.Window.t
      | Moderator_overlay_changed of Jsonaf.t
      | Moderator_notification of Jsonaf.t
      | Permission_requested of Permission.t
      | Permission_resolved of Permission.t
      | Grant_created of Grant.t
      | Grant_revoked of Grant.t
      | Operation_started of Operation.t
      | Operation_completed of Operation.t
      | Operation_failed of Operation.t
      | Operation_cancelled of Operation.t
      | Operation_interrupted of Operation.t
      | Job_state_changed of Job.t
      | Schedule_created of Schedule.t
      | Schedule_state_changed of Schedule.t
      | Schedule_cancelled of Schedule.t
      | Prompt_upgraded of prompt_upgrade
      | Workspace_state_changed of Workspace.t
      | Session_error of Error.t
    [@@deriving sexp]

    val kind : t -> kind
    val to_json : t -> Jsonaf.t
    val of_json : kind:kind -> Jsonaf.t -> (t, Error.t) result
  end

  type t =
    { session_id : Id.Session.t
    ; sequence : int64
    ; revision : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; visibility : visibility
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  (** [to_json t] encodes the parameters of a [session.event] notification. *)
  val to_json : t -> Jsonaf.t

  (** [with_replacement_snapshot event snapshot] adds complete replacement state
      to a [Session_updated] event and binds its session revision/sequence to the
      event. Other event kinds are unchanged. Filter the snapshot for the reader
      before publishing it. Older payloads omit this additive field. *)
  val with_replacement_snapshot : t -> Snapshot.t -> t

  (** [replacement_snapshot event] decodes optional replacement state and rejects
      mismatched session/revision/sequence anchors. *)
  val replacement_snapshot : t -> (Snapshot.t option, Error.t) result

  (** [of_json json] decodes a durable event projection. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [of_payload ... payload] constructs a full durable wire event with a
      kind derived from the closed payload variant. *)
  val of_payload
    :  session_id:Id.Session.t
    -> sequence:int64
    -> revision:int64
    -> timestamp:Timestamp.t
    -> Payload.t
    -> t

  (** [to_notification t] wraps the event in a JSON-RPC notification. *)
  val to_notification : t -> Envelope.t
end

module Recoverable : sig
  type kind =
    | Provider_stream
    | Sourced_stream
    | History_correlated_stream
    | Tool_started
    | Tool_progress
    | Tool_trace
    | Tool_finished
    | Agent_call_classified
    | Agent_call_progress
    | Activity
    | Compaction_progress
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; operation_id : Id.Operation.t
    ; operation_sequence : int64
    ; anchor_sequence : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  (** [to_json t] encodes the parameters of a [session.live_event] notification. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a recoverable live event. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_notification t] wraps the live event in a JSON-RPC notification. *)
  val to_notification : t -> Envelope.t
end
