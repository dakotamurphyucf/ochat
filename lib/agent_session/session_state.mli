open! Core

(** Complete immutable state owned by one session actor. *)

module Identity : sig
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; display_name : string option
    ; creating_principal : Agent_protocol.Id.Principal.t option
    ; created_at : Agent_protocol.Timestamp.t
    ; updated_at : Agent_protocol.Timestamp.t
    ; labels : (string * string) list
    ; generation : int
    }
  [@@deriving sexp]
end

module Spec : sig
  type t =
    { protocol : Agent_protocol.Session.Spec.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; prompt_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; workspace_instance : Workspace_instance.t
    ; permission_profile : string
    ; permission_profile_digest : string
    ; runtime_policy : string option
    ; quota_key : Quota_key.t option
    }
  [@@deriving sexp]
end

module Compaction_archive : sig
  type kind =
    | Compaction
    | Reset
    | Rebuild
    | Upgrade
  [@@deriving equal, sexp]

  type t =
    { operation_id : Agent_protocol.Id.Operation.t
    ; revision : int64
    ; sha256 : string
    ; kind : kind [@sexp.default Compaction]
    }
  [@@deriving sexp]
end

module Conversation : sig
  type t =
    { canonical_history : Agent_protocol.History.entry list
    ; deferred_user_entries : Agent_protocol.History.entry list
    ; initial_prompt_entry_count : int
    ; next_history_sequence : int64
    ; reserved_history_through : int64
    ; tasks : Jsonaf.t list
    ; kv_store : (string * string) list
    ; compaction_generation : int
    ; compaction_archives : Compaction_archive.t list [@sexp.list]
    }
  [@@deriving sexp]
end

module Lifecycle : sig
  type t =
    { desired : Agent_protocol.Session.desired_state
    ; observed : Agent_protocol.Session.observed_state
    }
  [@@deriving sexp]
end

module Counters : sig
  type t =
    { revision : int64
    ; event_sequence : int64
    ; transaction_sequence : int64
    ; owner_lease_generation : int64
    }
  [@@deriving sexp]
end

type t =
  { schema_version : int
  ; identity : Identity.t
  ; spec : Spec.t
  ; lifecycle : Lifecycle.t
  ; conversation : Conversation.t
  ; active_operation : Agent_protocol.Operation.t option
  ; permissions : Agent_protocol.Permission.t list
  ; grants : Agent_protocol.Grant.t list
  ; jobs : Agent_protocol.Job.t list
  ; schedules : Agent_protocol.Schedule.t list
  ; invocations : Agent_protocol.Invocation.t list [@sexp.list]
  ; attachments : Agent_protocol.Session.Attachment.t list
  ; moderator : Jsonaf.t option
  ; shell : Session.Shell_state.t
  ; halted : bool
  ; halt_reason : string option
  ; failure : Agent_protocol.Error.t option
  ; counters : Counters.t
  }
[@@deriving sexp]

val current_schema_version : int

(** Upgrade a supported legacy state before validation/replay. Old schema-2
    snapshots have no invocation records; unknown versions fail closed. *)
val upgrade_schema : t -> (t, Agent_protocol.Error.t) result

val create
  :  identity:Identity.t
  -> spec:Spec.t
  -> initial_history:Agent_protocol.History.entry list
  -> t

val validate : t -> (unit, Agent_protocol.Error.t) result
val summary : t -> Agent_protocol.Session.t
val snapshot : now:Agent_protocol.Timestamp.t -> t -> Agent_protocol.Snapshot.t
val history_window : Agent_protocol.History.entry list -> Agent_protocol.History.Window.t

(** Rendering-neutral committed moderator view. Contains only effective history
    and halt state, never interpreter state or queued internal events. *)
val moderator_projection : t -> Jsonaf.t
