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
    ; metadata_revision : int64 [@sexp.default 0L]
    ; organization : Agent_protocol.Session_organization.Values.t
          [@sexp.default Agent_protocol.Session_organization.Values.empty]
    }
  [@@deriving sexp]
end

module Spec : sig
  type t =
    { protocol : Agent_protocol.Session.Spec.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; prompt_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; configuration_revision : int64
    ; inference_target : (Inference.Selection.t[@sexp.opaque])
      (** Private frozen session selection, independent of children/model jobs.
          Historical absence converts to explicit Unresolved and grants no
          backend defaults or execution authority. *)
    ; delegation : Agent_store.Delegation_store.Reference.t option [@sexp.option]
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
    | Edit
    | Delete
  [@@deriving equal, sexp]

  (** Administrative reconciliation of an invocation in the checksummed archive.
      Original inputs and recorded outcomes remain in that archive, not copied
      into this index. An interruption reason records formerly unfinished work. *)
  type invocation_disposition =
    { invocation_id : Agent_protocol.Id.Invocation.t
    ; interruption_reason : string option [@sexp.option]
    ; output_entry_id : Agent_protocol.History.Id.t option [@sexp.option]
    ; publication_discarded : string option [@sexp.option]
    }
  [@@deriving sexp]

  type t =
    { operation_id : Agent_protocol.Id.Operation.t
    ; revision : int64
    ; sha256 : string
    ; kind : kind [@sexp.default Compaction]
    ; invocation_dispositions : invocation_disposition list [@sexp.list]
    }
  [@@deriving sexp]
end

module Conversation : sig
  type t =
    { canonical_history : Agent_protocol.History.entry list
    ; deferred_user_entries : Agent_protocol.History.entry list
    ; initial_prompt_entry_count : int
    ; next_history_sequence : int64
      (** Nonnegative allocator high-water mark, exclusive of every retained ID
          in this session's namespace, including moderator insertions/targets.
          Imported namespaces do not consume this allocator. Unused committed
          blocks and retired history may leave gaps below this watermark. *)
    ; reserved_history_through : int64
      (** Nonnegative exclusive committed reservation bound, at most
          [next_history_sequence]. Restoring a lower allocator watermark would
          recycle actual host occurrences. *)
    ; tasks : Jsonaf.t list
    ; kv_store : (string * string) list
    ; compaction_generation : int
    ; compaction_archives : Compaction_archive.t list [@sexp.list]
    ; authoring_reference_index : Jsonaf.t option [@sexp.option]
      (** Bounded, trusted receipt metadata retained through compaction. No topic
          prose or authority is stored here. Absent in legacy/ordinary sessions;
          cleared on generation replacement. Validated with the owning scope. *)
    ; authoring_publication : Chat_response.Authoring_publication.context option
          [@sexp.option]
      (** Schema20. Last trusted model-input authoring policy/context, retained
          for publication recovery and cleared on generation replacement. *)
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

(** Private activation gate. Pending records a selected configuration durably
    before initializer effects. It is never inferred from empty history. Recovery
    keeps Pending unloaded; only explicit activation may retry. Automatic jobs
    and runtime work require Ready. *)
module Runtime_initialization : sig
  type t =
    | Ready
    | Pending of { fresh_history : bool }
  [@@deriving equal, sexp]
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
  ; runtime_initialization : Runtime_initialization.t
  ; pending_initial_start : bool
    (** New generated creation's durable, unconsumed start intent. Older sessions
        never infer this from their original start_immediately configuration. *)
  ; stop_epoch : int64 (** Durable count of transitions from running to stopped intent. *)
  ; parent_stop_epoch : int64 option
    (** Parent stop counter already reconciled by this generated child. Legacy
        absence uses its private creation admission until first reconciliation. *)
  ; conversation : Conversation.t
  ; active_operation : Agent_protocol.Operation.t option
  ; automatic_turn_budget : Automatic_turn_budget.t option [@sexp.option]
    (** Qualified host scheduling accounting. Historical absence is preserved
        until the host explicitly enables it; runtime reload cannot reset it. *)
  ; permissions : Agent_protocol.Permission.t list
  ; grants : Agent_protocol.Grant.t list
  ; jobs : Agent_protocol.Job.t list
  ; inference_ledger : (Inference_ledger.t[@sexp.opaque])
  ; model_job_targets : Model_job_target.t list
    (** Exactly one private ID/generation binding per retained Model_call job.
        Source is captured at admission, root recipe target after prompt fetch;
        neither follows later parent selection changes. Unknown binding members
        remain owned by the state document carrier. *)
  ; schedules : Agent_protocol.Schedule.t list
  ; invocations : Agent_protocol.Invocation.t list [@sexp.list]
  ; managed_submissions : Managed_submission.t list [@sexp.list]
    (** Protected send identities and operation correlation. Historical terminal
        receipts survive generation replacement; they are not raw public state. *)
  ; managed_stops : Managed_stop.t list [@sexp.list]
    (** Immutable stop admission identities, retained across target restart/reset.
        A retried old key must not stop a subsequent runtime lifetime. *)
  ; moderator_executions : Agent_protocol.Moderator_execution.t list [@sexp.list]
  ; subscriptions : Agent_protocol.Subscription.t list [@sexp.list]
  ; deliveries : Agent_protocol.Delivery.t list [@sexp.list]
  ; ingress_registrations : External_ingress.t list [@sexp.list]
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

(** Upgrade supported legacy state before validation/replay. Schema 2 has no
    invocation records; schema 3 retains invocations but has no subscriptions
    or deliveries; schema 4 has no event receipts, and schema 5 has no event
    retirements; schema 6 has no event-owned invocations. Unknown versions and
    inconsistent legacy fields fail closed. *)
val upgrade_schema : t -> (t, Agent_protocol.Error.t) result

(** Fresh state with known-empty tracking coverage. The identity must have a
    valid session ID and nonnegative generation.
    @raise Failure if that native identity precondition is violated. Decoders
    admit untrusted identities through typed errors instead. *)
val create
  :  identity:Identity.t
  -> spec:Spec.t
  -> initial_history:Agent_protocol.History.entry list
  -> t

val validate : t -> (unit, Agent_protocol.Error.t) result

(** Planning-only validation before retiring the actual resource graph. The
    candidate retains the byte-exact previous ledger, admitted under its previous
    generation; all other native checks use the actual candidate identity. This
    grants no durable admission. The actor must overlay its CURRENT reconciled
    ledger and run ordinary [validate] before publishing a new generation. *)
val validate_administration_candidate
  :  t
  -> previous:t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Decode the bounded receipt index under this session/generation. Absence is an
    empty index; it does not scan archives or infer that topic prose is present. *)
val authoring_references
  :  t
  -> (Chat_response.Authoring_reference_index.t, Agent_protocol.Error.t) result

val summary : t -> Agent_protocol.Session.t
val snapshot : now:Agent_protocol.Timestamp.t -> t -> Agent_protocol.Snapshot.t
val history_window : Agent_protocol.History.entry list -> Agent_protocol.History.Window.t

(** Rendering-neutral committed moderator view. Contains only effective history
    and halt state, never interpreter state or queued internal events. *)
val moderator_projection : t -> Jsonaf.t

(** Payload-free extension summaries in stable identity order. *)
val extension_status : t -> Agent_protocol.Extension_status.t list
