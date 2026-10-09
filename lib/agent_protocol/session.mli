(** Session specifications, lifecycle projections, attachments, and core requests. *)

type execution_host =
  | Daemon
  | Embedded
[@@deriving compare, equal, sexp]

type stop_mode =
  | Graceful
  | Cancel
[@@deriving compare, equal, sexp]

type liveness =
  | Detached
  | Owner_bound of
      { disconnect_grace_ms : int
      ; stop_mode : stop_mode
      }
  | Process_bound
[@@deriving compare, equal, sexp]

type persistence =
  | Durable
  | Transient
[@@deriving compare, equal, sexp]

type desired_state =
  | Running
  | Stopped
[@@deriving compare, equal, sexp]

type observed_state =
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn of Id.Operation.t
  | Compacting of Id.Operation.t
  | Waiting_for_permission of Id.Permission.t
  | Stopping
  | Failed of Error.t
[@@deriving sexp]

type attachment_mode =
  | Owner_read_write
  | Read_write
  | Read_only
[@@deriving compare, equal, sexp]

(** Stable lifecycle codecs used by snapshots and durable event payloads. *)
val desired_state_to_string : desired_state -> string

val desired_state_of_json : Jsonaf.t -> (desired_state, Error.t) result
val observed_state_to_json : observed_state -> Jsonaf.t
val observed_state_of_json : Jsonaf.t -> (observed_state, Error.t) result

module Prompt_ref : sig
  type t =
    | Catalog of Id.Prompt_definition.t
    | Local_path of string
    | Generated of Id.Prompt_revision.t
    (** Pinned generated definition, identified in session summaries. This is not
        an execution grant; ordinary session.create cannot admit this reference.
        Generated creation requires the scoped delegation service. *)
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Workspace_request : sig
  type t =
    | Configured of Id.Workspace_definition.t
    | Current
    | Local_path of string
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Spec : sig
  type t =
    { execution_host : execution_host
    ; prompt : Prompt_ref.t
    ; workspace : Workspace_request.t
    ; liveness : liveness
    ; persistence : persistence
    ; permission_profile : string option
    ; start_immediately : bool
    ; display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving sexp]

  (** [create ...] validates host/liveness/persistence combinations and metadata. *)
  val create
    :  execution_host:execution_host
    -> prompt:Prompt_ref.t
    -> workspace:Workspace_request.t
    -> liveness:liveness
    -> persistence:persistence
    -> ?permission_profile:string
    -> start_immediately:bool
    -> ?display_name:string
    -> labels:(string * string) list
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t

  (** Structural decoding retains historical metadata strings; authored creation
      additionally uses [create], including at Create_request decoding. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Optional status-visible aggregate. This local sexp codec preserves presence;
    value decoding uses the same bounded validated safe summary JSON codec. It is
    not a generic deserializer for provider/private Presence payloads. *)
module Inference_summary : sig
  type t = Inference_query.Summary.t History_entry.Payload.Presence.t

  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
end

type t =
  { id : Id.Session.t
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; spec : Spec.t
  ; desired_state : desired_state
  ; observed_state : observed_state
  ; prompt_revision : Id.Prompt_revision.t option
  ; workspace_instance : Id.Workspace_instance.t option
  ; active_operation : Operation.t option
  ; revision : int64
  ; metadata_revision : int64 [@sexp.default 0L]
  ; organization : Session_organization.Values.t
        [@sexp.default Session_organization.Values.empty]
    (** Nonnegative organization counter. Streaming changes advance [revision]
        while leaving this counter intact. Legacy summaries default to zero. *)
  ; latest_event_sequence : int64
  ; inference_summary : Inference_summary.t
        [@sexp.default History_entry.Payload.Presence.Absent]
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Owner_lease : sig
  type t =
    { generation : int64
    ; expires_at : Timestamp.t
    ; disconnect_grace_until : Timestamp.t option
    ; principal_id : Id.Principal.t option [@sexp.option]
    ; reclaim_token_sha256 : string option [@sexp.option]
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attachment : sig
  type t =
    { id : Id.Attachment.t
    ; session_id : Id.Session.t
    ; mode : attachment_mode
    ; owner_lease : Owner_lease.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create_request : sig
  type t =
    { spec : Spec.t
    ; requested_mode : attachment_mode option
    ; subscribe : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module List_request : sig
  type t =
    { page : Page.Request.t
    ; desired_state : desired_state option
    ; prompt_id : Id.Prompt_definition.t option
    ; workspace_id : Id.Workspace_definition.t option
    ; owner_principal_id : Id.Principal.t option
    ; organization : Session_organization.Query.t
    ; labels : (string * string) list
    ; sort : Session_catalog_query.Sort.t
    ; archive : Session_catalog_query.Archive_filter.t
    ; creator_principal_id : Id.Principal.t option
    ; active_owner_principal_id : Id.Principal.t option
    }
  [@@deriving sexp]

  (** Validate native selectors and sort label keys without a JSON round trip. *)
  val normalize : t -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attach_request : sig
  type t =
    { session_id : Id.Session.t
    ; requested_mode : attachment_mode
    ; subscribe : bool
    ; after_sequence : int64 option
    ; reclaim_token : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Detach_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Renew_owner_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; lease_generation : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Start_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; queue_if_limited : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Stop_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; mode : stop_mode
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_operation_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; operation_id : Id.Operation.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Message_content : sig
  type kind =
    | Plain_text
    | Chatmd
  [@@deriving compare, equal, sexp]

  type t =
    { kind : kind
    ; text : string
    ; attachments : Blob.Input.t list
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Send_message_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; content : Message_content.t
    ; timing : Pending_input.Timing.t [@sexp.default Pending_input.Timing.Safe_boundary]
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Compact_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64 option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete_history_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; history_id : History.Id.t
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** An absent attachment requests retained access under current principal
    visibility and method scopes. A supplied attachment must remain current.
    Encoding omits absence; explicit JSON null is invalid. No execution admission. *)
module Export_request : sig
  type format =
    | Chatmd
    | Json
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t option
    ; format : format
    ; revision : int64 option
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Reset_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; keep_history : bool
    ; keep_tasks : bool
    ; keep_cache : bool
    ; keep_workspace : bool
    ; keep_grants : bool
    ; keep_labels : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Rebuild_request : sig
  type prompt_choice =
    | Pinned
    | Current_catalog
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; prompt_choice : prompt_choice
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Upgrade_prompt_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; target_revision : Id.Prompt_revision.t
    ; allow_migration : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete_request : sig
  type policy =
    | Archive
    | Remove
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; policy : policy
    ; confirmation : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
