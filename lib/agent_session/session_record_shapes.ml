open! Core
module X = Persistence_codec
module D = Document_schema

let v = D.Shape.value
let o = X.shape_exn
let f = X.fields_shape
let a = X.array_shape_exn
let n = D.Shape.nullable

let tag field cases =
  match D.Shape.tagged_object ~discriminator:field cases with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid tagged storage shape", (error : D.Error.t)]
;;

let cases field names = List.map names ~f:(fun name -> name, f [ field ])
let error = f [ "code"; "message"; "retryable"; "data" ]
let observer = f [ "script_id"; "source_sha256" ]
let part = f [ "index"; "item_sha256" ]

let authoring_topic =
  o [ "id", v; "document_sha256", v; "complete", v; "source", f [ "kind"; "identity" ] ]
;;

let authoring_guidance =
  o
    [ "version", v
    ; "context_identity", v
    ; "policy_fingerprint", v
    ; "payload_sha256", v
    ; "purpose", v
    ; "surface_id", v
    ; "topics", a ~identity_field:"id" authoring_topic
    ; ( "fragments"
      , a
          ~identity_field:"topic_id"
          (o [ "topic_id", v; "total_parts", v; "parts", a part ]) )
    ]
;;

let provenance =
  tag
    "type"
    (cases "type" [ "canonical"; "moderator_inserted" ]
     @ [ "runtime_authoring", o [ "type", v; "guidance", authoring_guidance ]
       ; "runtime_notification", f [ "type"; "delivery_id" ]
       ; "moderator_replaced", f [ "type"; "canonical_id" ]
       ])
;;

let history_entry =
  o
    [ "id", v
    ; "role", v
    ; "kind", v
    ; "payload", v
    ; "provenance", provenance
    ; "redacted", v
    ]
;;

let observed =
  tag
    "type"
    (cases
       "type"
       [ "stopped"; "queued_for_slot"; "starting"; "recovering"; "idle"; "stopping" ]
     @ [ "running_turn", f [ "type"; "operation_id" ]
       ; "compacting", f [ "type"; "operation_id" ]
       ; "waiting_for_permission", f [ "type"; "permission_id" ]
       ; "failed", o [ "type", v; "error", error ]
       ])
;;

let lifecycle = o [ "desired", v; "observed", observed ]

let prompt =
  tag
    "type"
    [ "catalog", f [ "type"; "prompt_id" ]
    ; "local_path", f [ "type"; "path" ]
    ; "generated", f [ "type"; "revision_id" ]
    ]
;;

let workspace_request =
  tag
    "type"
    [ "current", f [ "type" ]
    ; "configured", f [ "type"; "workspace_id" ]
    ; "local_path", f [ "type"; "path" ]
    ]
;;

let liveness =
  tag
    "type"
    [ "detached", f [ "type" ]
    ; "process_bound", f [ "type" ]
    ; "owner_bound", f [ "type"; "disconnect_grace_ms"; "stop_mode" ]
    ]
;;

let protocol_spec =
  o
    [ "execution_host", v
    ; "prompt", prompt
    ; "workspace", workspace_request
    ; "liveness", liveness
    ; "persistence", v
    ; "permission_profile", v
    ; "start_immediately", v
    ; "display_name", v
    ; "labels", v
    ]
;;

let operation =
  o
    [ "id", v
    ; "generation", v
    ; "kind", tag "type" [ "turn", f [ "type"; "reason" ]; "compaction", f [ "type" ] ]
    ; ( "state"
      , tag
          "type"
          (cases "type" [ "starting"; "running"; "cancelling"; "completed"; "cancelled" ]
           @ [ "failed", o [ "type", v; "error", error ]
             ; "interrupted", f [ "type"; "reason"; "retryable" ]
             ]) )
    ; "started_at", v
    ; "updated_at", v
    ]
;;

let permission =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "operation_id", v
    ; "invocation_id", v
    ; "call_id", v
    ; "tool_name", v
    ; "runtime_identity", v
    ; "invocation_display", v
    ; "rationale", v
    ; "effects", v
    ; "choices", v
    ; "created_at", v
    ; "expires_at", v
    ; "state", v
    ; "resolution", o [ "choice", v; "principal_id", v; "resolved_at", v; "reason", v ]
    ]
;;

let grant =
  f
    [ "id"
    ; "session_id"
    ; "principal_id"
    ; "tool_name"
    ; "identity_digest"
    ; "scope"
    ; "state"
    ; "created_at"
    ; "expires_at"
    ; "revoked_at"
    ; "revocation_reason"
    ]
;;

let work = tag "type" [ "job", f [ "type"; "id" ]; "subscription", f [ "type"; "id" ] ]

let outcome =
  tag
    "type"
    [ "complete", f [ "type"; "value" ]
    ; "pending", o [ "type", v; "work", work; "acknowledgement", v ]
    ; "fail", f [ "type"; "code"; "message"; "retryable"; "details" ]
    ; "cancelled", f [ "type"; "reason" ]
    ]
;;

let fingerprint = f [ "sha256"; "byte_length" ]

let routing =
  o
    [ "kind", v
    ; "original_name", v
    ; "original_payload", fingerprint
    ; "final_payload", fingerprint
    ; "canonical_payload", fingerprint
    ; "preparation", v
    ]
;;

let follow_up =
  tag
    "type"
    (List.map [ "pending"; "applied"; "compaction_accepted"; "discarded" ] ~f:(fun name ->
       ( name
       , f
           ([ "type"; "request_turn"; "request_compaction"; "end_session" ]
            @ if String.equal name "discarded" then [ "reason" ] else []) )))
;;

let observation =
  tag
    "status"
    (List.map [ "awaiting"; "observing"; "observed"; "failed" ] ~f:(fun name ->
       ( name
       , o
           ([ "script_id", v
            ; "source_sha256", v
            ; "status", v
            ; "follow_up", follow_up
            ; "compaction_operation_id", v
            ]
            @ if String.equal name "failed" then [ "reason", v ] else []) )))
;;

let invocation_context =
  f
    [ "id"
    ; "session_id"
    ; "generation"
    ; "origin"
    ; "tool_name"
    ; "implementation_revision"
    ; "capability_fingerprint"
    ; "input"
    ; "created_at"
    ; "provider_call_id"
    ; "call_entry_id"
    ; "parent_invocation"
    ; "parent_job"
    ; "deadline"
    ]
;;

let completion_contract =
  o
    [ "version", v
    ; "tool_name", v
    ; "tool_fingerprint", v
    ; "capability_pins", a ~identity_field:"name" (f [ "name"; "fingerprint" ])
    ; "completion_schema", v
    ; "max_output_bytes", v
    ; "max_output_depth", v
    ]
;;

let authoring_reference =
  o
    [ "version", v
    ; "query_identity", v
    ; "host_identity", v
    ; "capability_fingerprint", v
    ; "scope", v
    ; "surface_id", v
    ; "corpus_identity", v
    ; "response_sha256", v
    ; ( "topics"
      , (* The topic identity is nested in [topic.id], rather than a scalar
           element field. Positional ownership conservatively rejects edits
           when index-associated unknown data could move to another topic. *)
        a (o [ "topic", authoring_topic; "total_parts", v; "parts", a part ]) )
    ]
;;

let invocation =
  o
    [ "id", v
    ; "context", invocation_context
    ; ( "status"
      , tag
          "type"
          [ "admitted", f [ "type" ]
          ; "dispatching", f [ "type" ]
          ; "resolved", o [ "type", v; "outcome", outcome ]
          ; "published", o [ "type", v; "outcome", outcome ]
          ] )
    ; "output_entry_id", v
    ; "routing", n routing
    ; "publication_discarded", v
    ; "observation", n observation
    ; "parent_event", v
    ; "handler_intent", n (o [ "follow_up", follow_up; "compaction_operation_id", v ])
    ; "completion_contract", n completion_contract
    ; "authoring_reference", n authoring_reference
    ]
;;

let job =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "kind", v
    ; "payload", v
    ; ( "status"
      , tag
          "type"
          (cases "type" [ "queued"; "running"; "succeeded"; "cancelled" ]
           @ [ "waiting_permission", f [ "type"; "permission_id" ]
             ; "failed", o [ "type", v; "error", error ]
             ; "interrupted", f [ "type"; "reason" ]
             ; ( "waiting_completion"
               , o
                   [ "type", v
                   ; "schema_version", v
                   ; "invocation_id", v
                   ; "job_id", v
                   ; "work", work
                   ; "deadline", v
                   ; "completion_schema", v
                   ; "max_output_bytes", v
                   ; "max_output_depth", v
                   ] )
             ]) )
    ; ( "retry_policy"
      , tag
          "type"
          [ "never", f [ "type" ]
          ; "safe_retry", f [ "type"; "max_attempts"; "backoff_ms" ]
          ; "idempotent", f [ "type"; "key"; "max_attempts"; "backoff_ms" ]
          ] )
    ; "attempt", v
    ; "created_at", v
    ; "started_at", v
    ; "next_run_at", v
    ; "completed_at", v
    ; "result", v
    ; ( "delivery"
      , tag
          "type"
          [ "not_required", f [ "type" ]
          ; "pending", f [ "type" ]
          ; "delivered", f [ "type"; "delivered_at" ]
          ; "discarded", f [ "type"; "schema_version"; "discarded_at"; "reason" ]
          ] )
    ; ( "launch"
      , o
          [ "schema_version", v
          ; "owner_type", v
          ; "owner_id", v
          ; "nested_depth", v
          ; "moderator_source", observer
          ; "parent_job", f [ "id"; "attempt" ]
          ] )
    ; ( "progress"
      , o
          [ "version", v
          ; "sequence", v
          ; "channels", a ~identity_field:"channel" (f [ "channel"; "text"; "truncated" ])
          ] )
    ]
;;

let schedule_body =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "payload", v
    ; "created_at", v
    ; "next_due_at", v
    ; "misfire", v
    ; ( "status"
      , tag
          "type"
          (cases "type" [ "scheduled"; "delivering"; "delivered"; "cancelled" ]
           @ [ "failed", o [ "type", v; "error", error ] ]) )
    ; "delivery_count", v
    ; "last_delivery_at", v
    ]
;;

let schedule_ownership =
  o
    [ "schema_version", v
    ; "source", observer
    ; "creator_type", v
    ; "creator_id", v
    ; "subscription", f [ "id"; "epoch" ]
    ]
;;

let schedule =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "payload", v
    ; "created_at", v
    ; "next_due_at", v
    ; "misfire", v
    ; ( "status"
      , tag
          "type"
          (cases "type" [ "scheduled"; "delivering"; "delivered"; "cancelled" ]
           @ [ "failed", o [ "type", v; "error", error ] ]) )
    ; "delivery_count", v
    ; "last_delivery_at", v
    ; "ownership", n schedule_ownership
    ; "delivery_cancellation", v
    ]
;;

let decision =
  tag
    "kind"
    [ "approve", f [ "kind" ]
    ; "reject", f [ "kind"; "reason" ]
    ; "rewrite_args", f [ "kind"; "args" ]
    ; "redirect", f [ "kind"; "name"; "args" ]
    ]
;;

let moderator_execution =
  o
    [ "schema_version", v
    ; "id", v
    ; "session_id", v
    ; "generation", v
    ; "script_id", v
    ; "source_sha256", v
    ; "phase", v
    ; "event", v
    ; "checkpoint_sha256", v
    ; "created_at", v
    ; ( "status"
      , tag
          "kind"
          [ "running", f [ "kind" ]
          ; "completed", f [ "kind"; "checkpoint_sha256" ]
          ; "failed", o [ "kind", v; "error", outcome ]
          ; "interrupted", f [ "kind"; "reason" ]
          ] )
    ; "operation_id", v
    ; "job", f [ "job_id"; "attempt"; "deadline" ]
    ; "requests", f [ "request_turn"; "request_compaction"; "end_session" ]
    ; ( "delegation"
      , f
          [ "child_session_id"
          ; "child_generation"
          ; "child_invocation_id"
          ; "admission_sha256"
          ] )
    ; "decision", decision
    ; ( "intent"
      , tag
          "kind"
          [ "pending", f [ "kind" ]
          ; "applied", f [ "kind" ]
          ; "waiting_compaction", f [ "kind"; "operation_id" ]
          ; "discarded", f [ "kind"; "reason" ]
          ] )
    ; "compaction_operation_id", v
    ; "retirement", f [ "checkpoint_sha256"; "reason" ]
    ]
;;

let completion =
  tag
    "type"
    [ "succeeded", f [ "type"; "value" ]
    ; "failed", f [ "type"; "code"; "message"; "retryable"; "details" ]
    ; "cancelled", f [ "type"; "reason" ]
    ; "expired", f [ "type" ]
    ]
;;

let subscription =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "invocation_id", v
    ; "kind", v
    ; "created_at", v
    ; "deadline", v
    ; "wake", v
    ; "epoch", v
    ; "source", n observer
    ; "parent_job", n (f [ "job_id"; "attempt" ])
    ; "completion_schema", v
    ; "ingress_capability", v
    ; "timer_id", v
    ; "job_id", v
    ; "result", n completion
    ; "completed_at", v
    ]
;;

let delivery_status =
  tag
    "type"
    [ "pending", f [ "type" ]
    ; "committed", f [ "type"; "history_id"; "at" ]
    ; "failed", o [ "type", v; "error", outcome ]
    ]
;;

let wake_disposition =
  tag
    "type"
    [ "pending", f [ "type" ]
    ; "accepted", f [ "type"; "operation_id" ]
    ; "discarded", f [ "type"; "reason" ]
    ]
;;

let delivery_ownership = f [ "script_id"; "source_sha256"; "creator_type"; "creator_id" ]

let result_reference =
  o
    [ "type", v
    ; "version", v
    ; "session_id", v
    ; "job_id", v
    ; "generation", v
    ; "attempt", v
    ; "outcome", v
    ; "byte_length", v
    ; "sha256", v
    ; ( "artifact"
      , o
          [ "version", v
          ; "session_id", v
          ; "job_id", v
          ; "generation", v
          ; "attempt", v
          ; ( "blob"
            , f [ "id"; "kind"; "media_type"; "byte_length"; "digest"; "display_name" ] )
          ] )
    ]
;;

let projection =
  o
    [ "version", v
    ; "job_attempt", v
    ; "contract_sha256", v
    ; "result_sha256", v
    ; "rejected", v
    ; "result_reference", result_reference
    ]
;;

let delivery =
  o
    [ "id", v
    ; "session_id", v
    ; "generation", v
    ; "correlation", v
    ; "source", v
    ; "completion", completion
    ; "wake", v
    ; "created_at", v
    ; "attempt", v
    ; "status", delivery_status
    ; "invocation_id", v
    ; "work", n work
    ; "disclosure_pins", v
    ; "ownership", n delivery_ownership
    ; "wake_disposition", n wake_disposition
    ; "completion_projection", n projection
    ]
;;

let owner_lease =
  f
    [ "generation"
    ; "expires_at"
    ; "disconnect_grace_until"
    ; "principal_id"
    ; "reclaim_token_sha256"
    ]
;;

let attachment = o [ "id", v; "session_id", v; "mode", v; "owner_lease", n owner_lease ]

(* Presentation-shaped receipts are immutable. These shapes select the known
   projection for validation while the receipt retains the complete original JSON. *)
let history_window =
  o
    [ "entries", a ~identity_field:"id" history_entry
    ; "previous_cursor", v
    ; "next_cursor", v
    ; "reached_start", v
    ; "reached_end", v
    ; "structurally_complete", v
    ]
;;

let extension_status = f [ "version"; "kind"; "id"; "generation"; "state" ]

let inference_summary =
  let sum = tag "kind" [ "tokens", f [ "kind"; "tokens" ]; "overflow", f [ "kind" ] ] in
  let metric =
    o
      [ "actual", sum
      ; "actual_attempts", v
      ; "estimated", sum
      ; "estimated_attempts", v
      ; "mixed_estimators", v
      ; ( "unknown"
        , f
            [ "not_reported"
            ; "explicit_null"
            ; "interrupted"
            ; "not_submitted"
            ; "before_tracking"
            ] )
      ]
  in
  o
    [ "retained_attempts", v
    ; "turns", f [ "pending"; "completed"; "failed"; "cancelled"; "interrupted" ]
    ; ( "components"
      , o
          (List.map
             [ "input"
             ; "output"
             ; "reported_total"
             ; "cached_input"
             ; "cache_write_input"
             ; "reasoning_output"
             ]
             ~f:(fun name -> name, metric)) )
    ; ( "coverage"
      , f
          [ "before_tracking_unknown"
          ; "retired_attempts"
          ; "untracked_attempts"
          ; "retired_turns"
          ; "untracked_turns"
          ; "tracking_limit"
          ] )
    ; "accounting_revision", v
    ]
;;

let session_summary_fields =
  [ "id", v
  ; "creator", v
  ; "created_at", v
  ; "updated_at", v
  ; "generation", v
  ; "spec", protocol_spec
  ; "desired_state", v
  ; "observed_state", observed
  ; "prompt_revision", v
  ; "workspace_instance", v
  ; "active_operation", n operation
  ; "revision", v
  ; "metadata_revision", v
  ; "organization", Agent_store.Session_record_document.organization
  ; "inference_summary", n inference_summary
  ; "latest_event_sequence", v
  ]
;;

let session_summary = o session_summary_fields

let public_schedule =
  (* Public schedule receipts admit the current owned envelope and plain schedule.
     They are immutable, so fields of the other form remain in the original JSON. *)
  o
    [ "schema_version", v
    ; "schedule", schedule_body
    ; "ownership", schedule_ownership
    ; "delivery_cancellation", v
    ; "id", v
    ; "session_id", v
    ; "generation", v
    ; "payload", v
    ; "created_at", v
    ; "next_due_at", v
    ; "misfire", v
    ; ( "status"
      , tag
          "type"
          (cases "type" [ "scheduled"; "delivering"; "delivered"; "cancelled" ]
           @ [ "failed", o [ "type", v; "error", error ] ]) )
    ; "delivery_count", v
    ; "last_delivery_at", v
    ]
;;

let snapshot =
  o
    [ "session", session_summary
    ; "canonical_history", history_window
    ; "archived_revisions", v
    ; "effective_history", n history_window
    ; "deferred_entries", a ~identity_field:"id" history_entry
    ; "permissions", a ~identity_field:"id" permission
    ; "grants", a ~identity_field:"id" grant
    ; "jobs", a ~identity_field:"id" job
    ; "extension_status", a extension_status
    ; "schedules", a public_schedule
    ; "active_tool_calls", v
    ; "active_agent_calls", v
    ; "halted", v
    ; "halt_reason", v
    ; "failure", n error
    ; "revision", v
    ; "latest_event_sequence", v
    ]
;;

let event_session =
  o
    (session_summary_fields
     @ [ "extension_status", a extension_status; "replacement_snapshot", snapshot ])
;;

let workspace =
  o
    [ "id", v
    ; "name", v
    ; "kind", v
    ; "temporary_location", v
    ; "cleanup", v
    ; "access", v
    ; "conflict_domain", v
    ; "prompt_limits", a (f [ "prompt_id"; "max_root_agents"; "overflow" ])
    ; "availability", v
    ; "unavailable_reason", v
    ]
;;
