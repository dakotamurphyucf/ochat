open! Core
module P = Agent_protocol
module X = Persistence_codec
module J = P.Json_codec
module S = Session_state
module Shapes = Session_record_shapes
module D = Document_schema

let pairs_to_jsonaf pairs =
  `Array
    (List.map pairs ~f:(fun (name, value) ->
       `Object [ "name", `String name; "value", `String value ]))
;;

let pairs_of_jsonaf =
  X.list (fun json ->
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind name = X.required fields "name" J.string in
    let%map value = X.required fields "value" J.string in
    name, value)
;;

let pairs_shape =
  X.array_shape_exn
    ~allow_empty_identity:true
    ~identity_field:"name"
    (X.fields_shape [ "name"; "value" ])
;;

let delegation_of_jsonaf json =
  Agent_store.Delegation_store.reference_of_jsonaf json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request
      (Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error)))
;;

let owner_lease_to_jsonaf (t : P.Session.Owner_lease.t) =
  `Object
    [ "generation", X.int64_json t.generation
    ; "expires_at", P.Timestamp.to_json t.expires_at
    ; ( "disconnect_grace_until"
      , (X.option_json P.Timestamp.to_json) t.disconnect_grace_until )
    ; "principal_id", (X.option_json P.Id.Principal.to_json) t.principal_id
    ; "reclaim_token_sha256", (X.option_json X.text_json) t.reclaim_token_sha256
    ]
;;

let owner_lease_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind generation = X.required fields "generation" X.nonnegative_int64 in
  let%bind expires_at = X.required fields "expires_at" P.Timestamp.of_json in
  let%bind disconnect_grace_until =
    X.required fields "disconnect_grace_until" (X.nullable P.Timestamp.of_json)
  in
  let%bind principal_id =
    X.required fields "principal_id" (X.nullable P.Id.Principal.of_json)
  in
  let%bind reclaim_token_sha256 =
    X.required fields "reclaim_token_sha256" (X.nullable J.string)
  in
  let t : P.Session.Owner_lease.t =
    { generation; expires_at; disconnect_grace_until; principal_id; reclaim_token_sha256 }
  in
  Ok t
;;

let owner_lease_shape =
  X.shape_exn
    [ "generation", Document_schema.Shape.value
    ; "expires_at", Document_schema.Shape.value
    ; "disconnect_grace_until", X.nullable_shape Document_schema.Shape.value
    ; "principal_id", X.nullable_shape Document_schema.Shape.value
    ; "reclaim_token_sha256", X.nullable_shape Document_schema.Shape.value
    ]
;;

let attachment_to_jsonaf (t : P.Session.Attachment.t) =
  `Object
    [ "id", P.Id.Attachment.to_json t.id
    ; "session_id", P.Id.Session.to_json t.session_id
    ; ( "mode"
      , (fun mode ->
           `String
             (match mode with
              | P.Session.Owner_read_write -> "owner_read_write"
              | Read_write -> "read_write"
              | Read_only -> "read_only"))
          t.mode )
    ; "owner_lease", (X.option_json owner_lease_to_jsonaf) t.owner_lease
    ]
;;

let attachment_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "id" P.Id.Attachment.of_json in
  let%bind session_id = X.required fields "session_id" P.Id.Session.of_json in
  let%bind mode =
    X.required
      fields
      "mode"
      (J.enum
         ~name:"attachment mode"
         [ "owner_read_write", P.Session.Owner_read_write
         ; "read_write", Read_write
         ; "read_only", Read_only
         ])
  in
  let%bind owner_lease =
    X.required fields "owner_lease" (X.nullable owner_lease_of_jsonaf)
  in
  let t : P.Session.Attachment.t = { id; session_id; mode; owner_lease } in
  Ok t
;;

let attachment_shape =
  X.shape_exn
    [ "id", Document_schema.Shape.value
    ; "session_id", Document_schema.Shape.value
    ; "mode", Document_schema.Shape.value
    ; "owner_lease", X.nullable_shape owner_lease_shape
    ]
;;

let identity_to_jsonaf (t : S.Identity.t) =
  `Object
    [ "session_id", P.Id.Session.to_json t.session_id
    ; "display_name", (X.option_json X.text_json) t.display_name
    ; "creating_principal", (X.option_json P.Id.Principal.to_json) t.creating_principal
    ; "created_at", P.Timestamp.to_json t.created_at
    ; "updated_at", P.Timestamp.to_json t.updated_at
    ; "labels", pairs_to_jsonaf t.labels
    ; "generation", X.host_counter_to_json t.generation
    ; "metadata_revision", X.int64_json t.metadata_revision
    ; "organization", P.Session_organization.Values.to_json t.organization
    ]
;;

let identity_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind session_id = X.required fields "session_id" P.Id.Session.of_json in
  let%bind display_name = X.required fields "display_name" (X.nullable J.string) in
  let%bind creating_principal =
    X.required fields "creating_principal" (X.nullable P.Id.Principal.of_json)
  in
  let%bind created_at = X.required fields "created_at" P.Timestamp.of_json in
  let%bind updated_at = X.required fields "updated_at" P.Timestamp.of_json in
  let%bind labels = X.required fields "labels" pairs_of_jsonaf in
  let%bind generation = X.required fields "generation" X.host_counter_of_json in
  let%bind metadata_revision =
    X.required fields "metadata_revision" X.nonnegative_int64
  in
  let%bind organization =
    X.required fields "organization" P.Session_organization.Values.of_json
  in
  let t : S.Identity.t =
    { session_id
    ; display_name
    ; creating_principal
    ; created_at
    ; updated_at
    ; labels
    ; generation
    ; metadata_revision
    ; organization
    }
  in
  Ok t
;;

let identity_shape =
  X.shape_exn
    [ "session_id", Document_schema.Shape.value
    ; "display_name", X.nullable_shape Document_schema.Shape.value
    ; "creating_principal", X.nullable_shape Document_schema.Shape.value
    ; "created_at", Document_schema.Shape.value
    ; "updated_at", Document_schema.Shape.value
    ; "labels", pairs_shape
    ; "generation", Document_schema.Shape.value
    ; "metadata_revision", Document_schema.Shape.value
    ; "organization", Agent_store.Session_record_document.organization
    ]
;;

let spec_to_jsonaf (t : S.Spec.t) =
  `Object
    [ "protocol", P.Session.Spec.to_json t.protocol
    ; ( "prompt_definition_id"
      , (X.option_json P.Id.Prompt_definition.to_json) t.prompt_definition_id )
    ; "prompt_revision_id", P.Id.Prompt_revision.to_json t.prompt_revision_id
    ; "configuration_revision", X.int64_json t.configuration_revision
    ; "inference_target", Inference.Selection.to_json t.inference_target
    ; ( "delegation"
      , (X.option_json Agent_store.Delegation_store.reference_to_jsonaf) t.delegation )
    ; "workspace_instance", Workspace_instance.to_jsonaf t.workspace_instance
    ; "permission_profile", X.text_json t.permission_profile
    ; "permission_profile_digest", X.text_json t.permission_profile_digest
    ; "runtime_policy", (X.option_json X.text_json) t.runtime_policy
    ; "quota_key", (X.option_json Quota_key.to_jsonaf) t.quota_key
    ]
;;

let spec_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind protocol = X.required fields "protocol" P.Session.Spec.of_json in
  let%bind prompt_definition_id =
    X.required fields "prompt_definition_id" (X.nullable P.Id.Prompt_definition.of_json)
  in
  let%bind prompt_revision_id =
    X.required fields "prompt_revision_id" P.Id.Prompt_revision.of_json
  in
  let%bind configuration_revision =
    X.required fields "configuration_revision" X.nonnegative_int64
  in
  let%bind inference_target =
    X.required fields "inference_target" (fun json ->
      Inference.Selection.of_json json ~limits
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))))
  in
  let%bind delegation =
    X.required fields "delegation" (X.nullable delegation_of_jsonaf)
  in
  let%bind workspace_instance =
    X.required fields "workspace_instance" Workspace_instance.of_jsonaf
  in
  let%bind permission_profile = X.required fields "permission_profile" J.string in
  let%bind permission_profile_digest =
    X.required fields "permission_profile_digest" J.string
  in
  let%bind runtime_policy = X.required fields "runtime_policy" (X.nullable J.string) in
  let%bind quota_key = X.required fields "quota_key" (X.nullable Quota_key.of_jsonaf) in
  let t : S.Spec.t =
    { protocol
    ; prompt_definition_id
    ; prompt_revision_id
    ; configuration_revision
    ; inference_target
    ; delegation
    ; workspace_instance
    ; permission_profile
    ; permission_profile_digest
    ; runtime_policy
    ; quota_key
    }
  in
  Ok t
;;

let spec_shape =
  X.shape_exn
    [ "protocol", Shapes.protocol_spec
    ; "prompt_definition_id", X.nullable_shape Document_schema.Shape.value
    ; "prompt_revision_id", Document_schema.Shape.value
    ; "configuration_revision", Document_schema.Shape.value
    ; "inference_target", Document_schema.Shape.value
    ; "delegation", X.nullable_shape Agent_store.Delegation_store.reference_shape
    ; "workspace_instance", Workspace_instance.shape
    ; "permission_profile", Document_schema.Shape.value
    ; "permission_profile_digest", Document_schema.Shape.value
    ; "runtime_policy", X.nullable_shape Document_schema.Shape.value
    ; "quota_key", X.nullable_shape Quota_key.shape
    ]
;;

let archive_kind_to_jsonaf = function
  | S.Compaction_archive.Compaction -> `String "compaction"
  | Reset -> `String "reset"
  | Rebuild -> `String "rebuild"
  | Upgrade -> `String "upgrade"
  | Edit -> `String "edit"
  | Delete -> `String "delete"
  | Pending_input -> `String "pending_input"
;;

let archive_kind_of_jsonaf =
  J.enum
    ~name:"archive kind"
    [ "compaction", S.Compaction_archive.Compaction
    ; "reset", Reset
    ; "rebuild", Rebuild
    ; "upgrade", Upgrade
    ; "edit", Edit
    ; "delete", Delete
    ; "pending_input", Pending_input
    ]
;;

let disposition_to_jsonaf (t : S.Compaction_archive.invocation_disposition) =
  `Object
    [ "invocation_id", P.Id.Invocation.to_json t.invocation_id
    ; "interruption_reason", (X.option_json X.text_json) t.interruption_reason
    ; "output_entry_id", (X.option_json P.History.Id.to_json) t.output_entry_id
    ; "publication_discarded", (X.option_json X.text_json) t.publication_discarded
    ]
;;

let disposition_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind invocation_id = X.required fields "invocation_id" P.Id.Invocation.of_json in
  let%bind interruption_reason =
    X.required fields "interruption_reason" (X.nullable J.string)
  in
  let%bind output_entry_id =
    X.required fields "output_entry_id" (X.nullable P.History.Id.of_json)
  in
  let%bind publication_discarded =
    X.required fields "publication_discarded" (X.nullable J.string)
  in
  let t : S.Compaction_archive.invocation_disposition =
    { invocation_id; interruption_reason; output_entry_id; publication_discarded }
  in
  Ok t
;;

let disposition_shape =
  X.shape_exn
    [ "invocation_id", Document_schema.Shape.value
    ; "interruption_reason", X.nullable_shape Document_schema.Shape.value
    ; "output_entry_id", X.nullable_shape Document_schema.Shape.value
    ; "publication_discarded", X.nullable_shape Document_schema.Shape.value
    ]
;;

let archive_reference_to_jsonaf (t : S.Compaction_archive.t) =
  `Object
    [ "operation_id", P.Id.Operation.to_json t.operation_id
    ; "revision", X.int64_json t.revision
    ; "sha256", X.text_json t.sha256
    ; "kind", archive_kind_to_jsonaf t.kind
    ; ( "invocation_dispositions"
      , (X.list_json disposition_to_jsonaf) t.invocation_dispositions )
    ]
;;

let archive_reference_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind operation_id = X.required fields "operation_id" P.Id.Operation.of_json in
  let%bind revision = X.required fields "revision" X.nonnegative_int64 in
  let%bind sha256 = X.required fields "sha256" J.string in
  let%bind kind = X.required fields "kind" archive_kind_of_jsonaf in
  let%bind invocation_dispositions =
    X.required fields "invocation_dispositions" (X.list disposition_of_jsonaf)
  in
  let t : S.Compaction_archive.t =
    { operation_id; revision; sha256; kind; invocation_dispositions }
  in
  Ok t
;;

let archive_reference_shape =
  X.shape_exn
    [ "operation_id", Document_schema.Shape.value
    ; "revision", Document_schema.Shape.value
    ; "sha256", Document_schema.Shape.value
    ; "kind", Document_schema.Shape.value
    ; ( "invocation_dispositions"
      , X.array_shape_exn ~identity_field:"invocation_id" disposition_shape )
    ]
;;

let pending_document_result result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let conversation_to_jsonaf ~raw_pending ~limits (t : S.Conversation.t) =
  let open Result.Let_syntax in
  let%bind pending =
    List.map t.deferred_user_entries ~f:(fun document ->
      (if raw_pending
       then Pending_input_document.to_jsonaf document ~limits
       else Pending_input_document.known_jsonaf document ~limits)
      |> pending_document_result)
    |> Result.all
  in
  let%map dispositions =
    List.map t.pending_dispositions ~f:(fun document ->
      (if raw_pending
       then Pending_disposition_document.to_jsonaf document ~limits
       else Pending_disposition_document.known_jsonaf document ~limits)
      |> pending_document_result)
    |> Result.all
  in
  `Object
    [ "canonical_history", (X.list_json P.History.entry_to_json) t.canonical_history
    ; "deferred_user_entries", `Array pending
    ; "pending_revision", P.Pending_input.Revision.to_json t.pending_revision
    ; "pending_dispositions", `Array dispositions
    ; "initial_prompt_entry_count", X.integer_json t.initial_prompt_entry_count
    ; "next_history_sequence", X.int64_json t.next_history_sequence
    ; "reserved_history_through", X.int64_json t.reserved_history_through
    ; "tasks", (X.list_json Fn.id) t.tasks
    ; "kv_store", pairs_to_jsonaf t.kv_store
    ; "compaction_generation", X.integer_json t.compaction_generation
    ; ( "compaction_archives"
      , (X.list_json archive_reference_to_jsonaf) t.compaction_archives )
    ; "authoring_reference_index", (X.option_json Fn.id) t.authoring_reference_index
    ; ( "authoring_publication"
      , (X.option_json Chat_response.Authoring_publication.context_to_jsonaf)
          t.authoring_publication )
    ]
;;

let conversation_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind canonical_history =
    X.required fields "canonical_history" (X.list P.History.entry_of_json)
  in
  let%bind deferred_user_entries =
    X.required
      fields
      "deferred_user_entries"
      (X.list (fun json ->
         Pending_input_document.of_jsonaf json ~limits |> pending_document_result))
  in
  let%bind pending_revision =
    X.required fields "pending_revision" P.Pending_input.Revision.of_json
  in
  let%bind pending_dispositions =
    X.required
      fields
      "pending_dispositions"
      (X.list (fun json ->
         Pending_disposition_document.of_jsonaf json ~limits |> pending_document_result))
  in
  let%bind initial_prompt_entry_count =
    X.required fields "initial_prompt_entry_count" X.integer
  in
  let%bind next_history_sequence =
    X.required fields "next_history_sequence" X.nonnegative_int64
  in
  let%bind reserved_history_through =
    X.required fields "reserved_history_through" X.nonnegative_int64
  in
  let%bind tasks = X.required fields "tasks" (X.list X.raw) in
  let%bind kv_store = X.required fields "kv_store" pairs_of_jsonaf in
  let%bind compaction_generation = X.required fields "compaction_generation" X.integer in
  let%bind compaction_archives =
    X.required fields "compaction_archives" (X.list archive_reference_of_jsonaf)
  in
  let%bind authoring_reference_index =
    X.required fields "authoring_reference_index" (X.nullable X.raw)
  in
  let%bind authoring_publication =
    X.required
      fields
      "authoring_publication"
      (X.nullable Chat_response.Authoring_publication.context_of_jsonaf)
  in
  let t : S.Conversation.t =
    { canonical_history
    ; deferred_user_entries
    ; pending_revision
    ; pending_dispositions
    ; initial_prompt_entry_count
    ; next_history_sequence
    ; reserved_history_through
    ; tasks
    ; kv_store
    ; compaction_generation
    ; compaction_archives
    ; authoring_reference_index
    ; authoring_publication
    }
  in
  Ok t
;;

let conversation_shape =
  X.shape_exn
    [ "canonical_history", X.array_shape_exn ~identity_field:"id" Shapes.history_entry
    ; ( "deferred_user_entries"
      , X.array_shape_exn ~identity_field:"id" Pending_input_document.shape )
    ; "pending_revision", D.Shape.value
    ; ( "pending_dispositions"
      , X.array_shape_exn ~identity_field:"history_id" Pending_disposition_document.shape
      )
    ; "initial_prompt_entry_count", Document_schema.Shape.value
    ; "next_history_sequence", Document_schema.Shape.value
    ; "reserved_history_through", Document_schema.Shape.value
    ; "tasks", X.array_shape_exn Document_schema.Shape.value
    ; "kv_store", pairs_shape
    ; "compaction_generation", Document_schema.Shape.value
    ; ( "compaction_archives"
      , X.array_shape_exn ~identity_field:"operation_id" archive_reference_shape )
    ; "authoring_reference_index", X.nullable_shape Document_schema.Shape.value
    ; ( "authoring_publication"
      , X.nullable_shape Chat_response.Authoring_publication.context_shape )
    ]
;;

let lifecycle_to_jsonaf (t : S.Lifecycle.t) =
  `Object
    [ ( "desired"
      , (fun value -> `String (P.Session.desired_state_to_string value)) t.desired )
    ; "observed", P.Session.observed_state_to_json t.observed
    ]
;;

let lifecycle_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind desired = X.required fields "desired" P.Session.desired_state_of_json in
  let%bind observed = X.required fields "observed" P.Session.observed_state_of_json in
  let t : S.Lifecycle.t = { desired; observed } in
  Ok t
;;

let lifecycle_shape =
  X.shape_exn [ "desired", Document_schema.Shape.value; "observed", Shapes.observed ]
;;

let counters_to_jsonaf (t : S.Counters.t) =
  `Object
    [ "revision", X.int64_json t.revision
    ; "event_sequence", X.int64_json t.event_sequence
    ; "transaction_sequence", X.int64_json t.transaction_sequence
    ; "owner_lease_generation", X.int64_json t.owner_lease_generation
    ]
;;

let counters_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind revision = X.required fields "revision" X.nonnegative_int64 in
  let%bind event_sequence = X.required fields "event_sequence" X.nonnegative_int64 in
  let%bind transaction_sequence =
    X.required fields "transaction_sequence" X.nonnegative_int64
  in
  let%bind owner_lease_generation =
    X.required fields "owner_lease_generation" X.nonnegative_int64
  in
  let t : S.Counters.t =
    { revision; event_sequence; transaction_sequence; owner_lease_generation }
  in
  Ok t
;;

let counters_shape =
  X.shape_exn
    [ "revision", Document_schema.Shape.value
    ; "event_sequence", Document_schema.Shape.value
    ; "transaction_sequence", Document_schema.Shape.value
    ; "owner_lease_generation", Document_schema.Shape.value
    ]
;;

let initialization_to_jsonaf = function
  | S.Runtime_initialization.Ready -> `Object [ "state", `String "ready" ]
  | Pending { fresh_history } ->
    `Object [ "state", `String "pending"; "fresh_history", X.bool_json fresh_history ]
;;

let initialization_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  match%bind X.required fields "state" J.string with
  | "ready" -> Ok S.Runtime_initialization.Ready
  | "pending" ->
    let%map fresh_history = X.required fields "fresh_history" J.bool in
    S.Runtime_initialization.Pending { fresh_history }
  | _ -> Error (P.Error.invalid_request "unknown runtime initialization state")
;;

let initialization_shape =
  D.Shape.tagged_object
    ~discriminator:"state"
    [ "ready", X.shape_exn [ "state", D.Shape.value ]
    ; "pending", X.shape_exn [ "state", D.Shape.value; "fresh_history", D.Shape.value ]
    ]
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let ledger_error error =
  P.Error.invalid_request (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error))
;;

let ledger_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.inspect ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  Inference_ledger.of_document document ~limits:Inference_ledger.Limits.default
  |> Result.map_error ~f:ledger_error
;;

type run_state_field =
  | Missing
  | Nullable

let run_state_fields field value =
  match field, value with
  | Missing, None -> []
  | (Missing | Nullable), Some index -> [ "run_state", Run_state.to_jsonaf index ]
  | Nullable, None -> [ "run_state", `Null ]
;;

let state_to_jsonaf ?(raw_pending = false) (t : S.t) ~limits ~presence =
  let run_state_field =
    if Session_state_presence.run_state_is_absent presence then Missing else Nullable
  in
  let open Result.Let_syntax in
  let%bind conversation = conversation_to_jsonaf ~raw_pending ~limits t.conversation in
  let%map ledger =
    Inference_ledger.to_document t.inference_ledger |> Result.map_error ~f:ledger_error
  in
  `Object
    ([ "identity", identity_to_jsonaf t.identity
     ; "spec", spec_to_jsonaf t.spec
     ; "lifecycle", lifecycle_to_jsonaf t.lifecycle
     ; "runtime_initialization", initialization_to_jsonaf t.runtime_initialization
     ; "pending_initial_start", X.bool_json t.pending_initial_start
     ; "stop_epoch", X.int64_json t.stop_epoch
     ; "parent_stop_epoch", (X.option_json X.int64_json) t.parent_stop_epoch
     ; "conversation", conversation
     ; "active_operation", (X.option_json P.Operation.to_json) t.active_operation
     ; ( "automatic_turn_budget"
       , (X.option_json Automatic_turn_budget.to_jsonaf) t.automatic_turn_budget )
     ; "permissions", (X.list_json P.Permission.to_json) t.permissions
     ; "grants", (X.list_json P.Grant.to_json) t.grants
     ; "jobs", (X.list_json P.Job.to_json) t.jobs
     ; "inference_ledger", D.Document.json ledger
     ; "model_job_targets", X.list_json Model_job_target.to_json t.model_job_targets
     ; "schedules", (X.list_json P.Schedule.Storage.to_json) t.schedules
     ; "invocations", (X.list_json P.Invocation.Storage.to_json) t.invocations
     ; ( "managed_submissions"
       , (X.list_json Managed_submission.to_jsonaf) t.managed_submissions )
     ; "managed_stops", (X.list_json Managed_stop.to_jsonaf) t.managed_stops
     ]
     @ run_state_fields run_state_field t.run_state
     @ [ ( "moderator_executions"
         , (X.list_json P.Moderator_execution.to_json) t.moderator_executions )
       ; "subscriptions", (X.list_json P.Subscription.Storage.to_json) t.subscriptions
       ; ( "deliveries"
         , X.list_json
             (fun (delivery : P.Delivery.t) ->
                let binding_field =
                  if
                    Session_state_presence.delivery_binding_is_absent
                      presence
                      delivery.context.id
                  then P.Delivery.Storage.Missing
                  else P.Delivery.Storage.Nullable
                in
                P.Delivery.Storage.to_json_with_binding_field delivery ~binding_field)
             t.deliveries )
       ; ( "ingress_registrations"
         , (X.list_json External_ingress.to_jsonaf) t.ingress_registrations )
       ; "attachments", (X.list_json attachment_to_jsonaf) t.attachments
       ; "moderator", (X.option_json Fn.id) t.moderator
       ; "shell", Session.Shell_state.to_jsonaf t.shell
       ; "halted", X.bool_json t.halted
       ; "halt_reason", (X.option_json X.text_json) t.halt_reason
       ; "failure", (X.option_json P.Error.to_json) t.failure
       ; "counters", counters_to_jsonaf t.counters
       ])
;;

let equal_values previous ~limits next =
  let project state =
    state_to_jsonaf
      ~raw_pending:true
      state
      ~limits
      ~presence:Session_state_presence.authored
    |> X.document_result
  in
  let open Result.Let_syntax in
  let%bind previous = project previous in
  let%map next = project next in
  D.Json.equal previous next
;;

let state_fields_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind identity = X.required fields "identity" identity_of_jsonaf in
  let%bind spec = X.required fields "spec" (spec_of_jsonaf ~limits) in
  let%bind lifecycle = X.required fields "lifecycle" lifecycle_of_jsonaf in
  let%bind runtime_initialization =
    X.required fields "runtime_initialization" initialization_of_jsonaf
  in
  let%bind pending_initial_start = X.required fields "pending_initial_start" J.bool in
  let%bind stop_epoch = X.required fields "stop_epoch" X.nonnegative_int64 in
  let%bind parent_stop_epoch =
    X.required fields "parent_stop_epoch" (X.nullable X.nonnegative_int64)
  in
  let%bind conversation =
    X.required fields "conversation" (conversation_of_jsonaf ~limits)
  in
  let%bind active_operation =
    X.required fields "active_operation" (X.nullable P.Operation.of_json)
  in
  let%bind automatic_turn_budget =
    X.required fields "automatic_turn_budget" (X.nullable Automatic_turn_budget.of_jsonaf)
  in
  let%bind permissions = X.required fields "permissions" (X.list P.Permission.of_json) in
  let%bind grants = X.required fields "grants" (X.list P.Grant.of_json) in
  let%bind jobs = X.required fields "jobs" (X.list P.Job.of_json) in
  let%bind inference_ledger =
    X.required fields "inference_ledger" (ledger_of_jsonaf ~limits)
  in
  let%bind model_job_targets =
    X.required
      fields
      "model_job_targets"
      (X.list (fun json -> Model_job_target.of_json json ~limits))
  in
  let%bind schedules =
    X.required fields "schedules" (X.list P.Schedule.Storage.of_json)
  in
  let%bind invocations =
    X.required fields "invocations" (X.list P.Invocation.Storage.of_json)
  in
  let%bind managed_submissions =
    X.required fields "managed_submissions" (X.list Managed_submission.of_jsonaf)
  in
  let%bind managed_stops =
    X.required fields "managed_stops" (X.list Managed_stop.of_jsonaf)
  in
  let%bind run_state =
    J.optional_as fields "run_state" (X.nullable Run_state.of_jsonaf)
    |> Result.map ~f:Option.join
  in
  let%bind moderator_executions =
    X.required fields "moderator_executions" (X.list P.Moderator_execution.of_json)
  in
  let%bind subscriptions =
    X.required fields "subscriptions" (X.list P.Subscription.Storage.of_json)
  in
  let%bind deliveries =
    X.required fields "deliveries" (X.list P.Delivery.Storage.of_json)
  in
  let%bind ingress_registrations =
    X.required fields "ingress_registrations" (X.list External_ingress.of_jsonaf)
  in
  let%bind attachments = X.required fields "attachments" (X.list attachment_of_jsonaf) in
  let%bind moderator = X.required fields "moderator" (X.nullable X.moderator_of_jsonaf) in
  let%bind shell =
    X.required fields "shell" (fun json ->
      Session.Shell_state.of_jsonaf json |> Result.map_error ~f:P.Error.invalid_request)
  in
  let%bind halted = X.required fields "halted" J.bool in
  let%bind halt_reason = X.required fields "halt_reason" (X.nullable J.string) in
  let%bind failure = X.required fields "failure" (X.nullable P.Error.of_json) in
  let%bind counters = X.required fields "counters" counters_of_jsonaf in
  let t : S.t =
    { schema_version = S.current_schema_version
    ; identity
    ; spec
    ; lifecycle
    ; runtime_initialization
    ; pending_initial_start
    ; stop_epoch
    ; parent_stop_epoch
    ; conversation
    ; active_operation
    ; automatic_turn_budget
    ; permissions
    ; grants
    ; jobs
    ; inference_ledger
    ; model_job_targets
    ; schedules
    ; invocations
    ; managed_submissions
    ; managed_stops
    ; moderator_executions
    ; run_state
    ; subscriptions
    ; deliveries
    ; ingress_registrations
    ; attachments
    ; moderator
    ; shell
    ; halted
    ; halt_reason
    ; failure
    ; counters
    }
  in
  Ok t
;;

let state_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind state = state_fields_of_jsonaf ~limits json in
  let%map () = S.validate state in
  state
;;

let state_shape =
  X.shape_exn
    [ "identity", identity_shape
    ; "spec", spec_shape
    ; "lifecycle", lifecycle_shape
    ; "runtime_initialization", initialization_shape
    ; "pending_initial_start", Document_schema.Shape.value
    ; "stop_epoch", Document_schema.Shape.value
    ; "parent_stop_epoch", X.nullable_shape Document_schema.Shape.value
    ; "conversation", conversation_shape
    ; "active_operation", X.nullable_shape Shapes.operation
    ; "automatic_turn_budget", X.nullable_shape Automatic_turn_budget.shape
    ; "permissions", X.array_shape_exn ~identity_field:"id" Shapes.permission
    ; "grants", X.array_shape_exn ~identity_field:"id" Shapes.grant
    ; "jobs", X.array_shape_exn ~identity_field:"id" Shapes.job
    ; "inference_ledger", D.Shape.value
    ; ( "model_job_targets"
      , X.array_shape_exn ~identity_field:"job_id" Model_job_target.shape )
    ; "schedules", X.array_shape_exn ~identity_field:"id" Shapes.schedule
    ; "invocations", X.array_shape_exn ~identity_field:"id" Shapes.invocation
    ; ( "managed_submissions"
      , X.array_shape_exn ~identity_field:"history_id" Managed_submission.shape )
    ; "run_state", X.nullable_shape Run_state.shape
    ; "managed_stops", X.array_shape_exn ~identity_field:"id" Managed_stop.shape
    ; ( "moderator_executions"
      , X.array_shape_exn ~identity_field:"id" Shapes.moderator_execution )
    ; "subscriptions", X.array_shape_exn ~identity_field:"id" Shapes.subscription
    ; "deliveries", X.array_shape_exn ~identity_field:"id" Shapes.delivery
    ; ( "ingress_registrations"
      , X.array_shape_exn ~identity_field:"id" External_ingress.shape )
    ; "attachments", X.array_shape_exn ~identity_field:"id" attachment_shape
    ; "moderator", X.nullable_shape Moderator_checkpoint.shape
    ; "shell", Session.Shell_state.shape
    ; "halted", Document_schema.Shape.value
    ; "halt_reason", X.nullable_shape Document_schema.Shape.value
    ; "failure", X.nullable_shape Shapes.error
    ; "counters", counters_shape
    ]
;;

module Original = struct
  type t =
    { template : D.Document.t
    ; limits : D.Limits.t
    ; inference_ledger : Inference_ledger.t
    ; presence : Session_state_presence.t
    }
end

type t =
  { carrier : S.t D.Extension_carrier.t
  ; original : Original.t option
  }

let value t = D.Extension_carrier.value t.carrier

let with_value t value =
  { t with carrier = D.Extension_carrier.with_value t.carrier value }
;;

let authored value =
  { carrier = D.Extension_carrier.of_authored_value value; original = None }
;;

let validate_run_capacity (state : S.t) document =
  match state.run_state with
  | None -> Ok ()
  | Some index ->
    (match D.Json.field (D.Document.payload document) ~name:"run_state" with
     | Value raw ->
       let open Result.Let_syntax in
       let%bind additional_bytes =
         Run_job_capacity.bookkeeping_reserve index ~jobs:state.jobs |> X.document_result
       in
       Run_state.validate_encoded_capacity_with_reserve index ~additional_bytes raw
       |> X.document_result
     | Absent | Null ->
       Error
         (D.Error.Invalid_field
            { path = [ "run_state" ]; reason = "admitted run index is missing" }))
;;

let decoded carrier ~limits =
  let template =
    match D.Extension_carrier.template carrier with
    | Some template -> template
    | None -> raise_s [%sexp "decoded state document has no original template"]
  in
  let open Result.Let_syntax in
  let%bind state =
    Pending_carrier_admission.rehydrate
      (D.Extension_carrier.value carrier)
      ~document:template
      ~limits
  in
  let carrier = D.Extension_carrier.with_value carrier state in
  let%bind () = validate_run_capacity state template in
  let%map presence = Session_state_presence.of_document template ~limits in
  { carrier
  ; original =
      Some
        { template
        ; limits
        ; inference_ledger = (D.Extension_carrier.value carrier).S.inference_ledger
        ; presence
        }
  }
;;

let shape = state_shape
let unresolved_json = `Object [ "state", `String "unresolved" ]

let legacy_model_job_target json =
  let open Result.Let_syntax in
  match D.Json.field json ~name:"kind" with
  | Value (`String "model_call") ->
    let%bind id = Agent_store.Document_fields.required json "id" Result.return in
    let%map generation =
      Agent_store.Document_fields.required json "generation" (function
        | `Number value -> Ok (`String value)
        | _ ->
          Error
            (D.Error.Invalid_field
               { path = [ "generation" ]
               ; reason = "legacy job generation must be a number"
               }))
    in
    Some
      (`Object
          [ "job_id", id
          ; "generation", generation
          ; "source", unresolved_json
          ; "execution", unresolved_json
          ])
  | Absent | Null | Value _ -> Ok None
;;

let upgrade document ~limits =
  let open Result.Let_syntax in
  let%bind step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:1 ~f:(fun payload ->
      let%bind spec = Agent_store.Document_fields.required payload "spec" Result.return in
      let%bind spec =
        match spec with
        | `Object fields ->
          Ok
            (if List.Assoc.mem fields "inference_target" ~equal:String.equal
             then spec
             else `Object (fields @ [ "inference_target", unresolved_json ]))
        | _ ->
          Error
            (D.Error.Invalid_field
               { path = [ "spec" ]; reason = "state spec must be an object" })
      in
      let%bind bindings =
        match D.Json.field payload ~name:"model_job_targets" with
        | Null -> Ok `Null
        | Value bindings -> Ok bindings
        | Absent ->
          let%bind jobs =
            Agent_store.Document_fields.required
              payload
              "jobs"
              Agent_store.Document_fields.array
          in
          let%map bindings = List.map jobs ~f:legacy_model_job_target |> Result.all in
          `Array (List.filter_opt bindings)
      in
      match payload with
      | `Object fields ->
        let fields =
          List.map fields ~f:(fun (name, value) ->
            name, if String.equal name "spec" then spec else value)
        in
        let fields =
          if List.Assoc.mem fields "model_job_targets" ~equal:String.equal
          then fields
          else fields @ [ "model_job_targets", bindings ]
        in
        let fields =
          if List.Assoc.mem fields "runtime_initialization" ~equal:String.equal
          then fields
          else fields @ [ "runtime_initialization", `Object [ "state", `String "ready" ] ]
        in
        Ok (`Object fields)
      | _ ->
        Error
          (D.Error.Invalid_field { path = []; reason = "state payload must be an object" }))
  in
  let%bind ledger_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:2 ~f:(fun payload ->
      let%bind identity =
        Agent_store.Document_fields.required payload "identity" Result.return
      in
      let%bind identity_fields = X.object_ identity |> X.document_result in
      let%bind session_id =
        X.required identity_fields "session_id" P.Id.Session.of_json |> X.document_result
      in
      let%bind generation =
        X.required identity_fields "generation" X.host_counter_of_json
        |> X.document_result
      in
      let%bind ledger =
        match D.Json.field payload ~name:"inference_ledger" with
        | Absent ->
          let%bind ledger =
            Inference_ledger.create
              ~session_id
              ~generation
              ~before_tracking_unknown:true
              ~limits:Inference_ledger.Limits.default
            |> Result.map_error ~f:ledger_error
            |> X.document_result
          in
          Inference_ledger.to_document ledger
          |> Result.map_error ~f:ledger_error
          |> X.document_result
        | Null | Value _ ->
          let%bind raw =
            Agent_store.Document_fields.required payload "inference_ledger" Result.return
          in
          let%bind document = D.Document.inspect ~limits raw in
          let%bind ledger =
            Inference_ledger.of_document document ~limits:Inference_ledger.Limits.default
            |> Result.map_error ~f:ledger_error
            |> X.document_result
          in
          let%map () =
            Inference_ledger.validate
              ledger
              ~limits:Inference_ledger.Limits.default
              ~session_id
              ~generation
            |> Result.map_error ~f:ledger_error
            |> X.document_result
          in
          document
      in
      match payload with
      | `Object fields ->
        if List.Assoc.mem fields "inference_ledger" ~equal:String.equal
        then Ok payload
        else Ok (`Object (fields @ [ "inference_ledger", D.Document.json ledger ]))
      | _ -> Agent_store.Document_fields.invalid "payload" "must be an object")
  in
  let%bind metadata_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:3 ~f:(fun payload ->
      let%bind identity =
        Agent_store.Document_fields.required payload "identity" Result.return
      in
      let%bind display_name =
        Agent_store.Document_fields.required identity "display_name" Result.return
      in
      let%bind labels =
        Agent_store.Document_fields.required
          identity
          "labels"
          Agent_store.Document_fields.array
      in
      let%bind labels =
        List.map labels ~f:(fun pair ->
          let%bind name =
            Agent_store.Document_fields.required pair "name" Result.return
          in
          let%bind value =
            Agent_store.Document_fields.required pair "value" Result.return
          in
          match name with
          | `String name -> Ok (name, value)
          | _ -> Agent_store.Document_fields.invalid "labels.name" "must be a string")
        |> Result.all
      in
      let%bind spec = Agent_store.Document_fields.required payload "spec" Result.return in
      let%bind protocol =
        Agent_store.Document_fields.required spec "protocol" Result.return
      in
      match identity, spec, protocol, payload with
      | ( `Object identity_fields
        , `Object spec_fields
        , `Object protocol_fields
        , `Object fields ) ->
        let remove fields key =
          List.filter fields ~f:(fun (name, _) -> not (String.equal name key))
        in
        let replace fields key value = remove fields key @ [ key, value ] in
        let identity =
          if List.Assoc.mem identity_fields "metadata_revision" ~equal:String.equal
          then identity
          else `Object (identity_fields @ [ "metadata_revision", `String "0" ])
        in
        let%bind protocol_fields =
          match display_name with
          | `Null -> Ok (remove protocol_fields "display_name")
          | `String _ -> Ok (replace protocol_fields "display_name" display_name)
          | _ ->
            Agent_store.Document_fields.invalid "display_name" "must be a string or null"
        in
        let protocol = `Object (replace protocol_fields "labels" (`Object labels)) in
        Ok
          (`Object
              (replace
                 (replace fields "identity" identity)
                 "spec"
                 (`Object (replace spec_fields "protocol" protocol))))
      | _ ->
        Agent_store.Document_fields.invalid "identity" "metadata mirrors must be objects")
  in
  let%bind configuration_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:4 ~f:(fun payload ->
      let%bind spec = Agent_store.Document_fields.required payload "spec" Result.return in
      let%bind spec =
        match spec with
        | `Object fields ->
          Ok
            (if List.Assoc.mem fields "configuration_revision" ~equal:String.equal
             then spec
             else `Object (fields @ [ "configuration_revision", `String "0" ]))
        | _ -> Agent_store.Document_fields.invalid "spec" "must be an object"
      in
      match payload with
      | `Object fields ->
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 name, if String.equal name "spec" then spec else value)))
      | _ -> Agent_store.Document_fields.invalid "payload" "must be an object")
  in
  let%bind organization_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:5 ~f:(fun payload ->
      let%bind identity =
        D.Json.field payload ~name:"identity"
        |> function
        | D.Json.Value (`Object fields) -> Ok fields
        | D.Json.Absent | Null | Value _ ->
          Agent_store.Document_fields.invalid "identity" "must be an object"
      in
      let identity =
        if List.Assoc.mem identity "organization" ~equal:String.equal
        then `Object identity
        else
          `Object
            (identity
             @ [ ( "organization"
                 , P.Session_organization.Values.to_json
                     P.Session_organization.Values.empty )
               ])
      in
      match payload with
      | `Object fields ->
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 name, if String.equal name "identity" then identity else value)))
      | _ -> Agent_store.Document_fields.invalid "payload" "must be an object")
  in
  let%bind history_revision_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:6 ~f:(fun payload ->
      let module F = Agent_store.Document_fields in
      let%bind conversation = F.required payload "conversation" Result.return in
      let entries = X.initialize_history_revisions in
      let%bind canonical = F.required conversation "canonical_history" entries in
      let%bind deferred = F.required conversation "deferred_user_entries" entries in
      match conversation, payload with
      | `Object conversation_fields, `Object fields ->
        let conversation =
          `Object
            (List.map conversation_fields ~f:(fun (name, value) ->
               ( name
               , if String.equal name "canonical_history"
                 then canonical
                 else if String.equal name "deferred_user_entries"
                 then deferred
                 else value )))
        in
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 name, if String.equal name "conversation" then conversation else value)))
      | _ -> F.invalid "conversation" "must be an object")
  in
  let%bind run_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:7 ~f:Result.return
  in
  let%bind pending_input_step =
    D.Conversion.Step.of_function ~kind:"session.state" ~from_version:8 ~f:(fun payload ->
      let module F = Agent_store.Document_fields in
      let%bind identity = F.required payload "identity" Result.return in
      let%bind generation =
        F.required identity "generation" (fun json ->
          X.host_counter_of_json json |> X.document_result)
      in
      let generation = X.integer_json generation in
      let%bind conversation = F.required payload "conversation" Result.return in
      let%bind deferred = F.required conversation "deferred_user_entries" Result.return in
      let%bind deferred =
        match deferred with
        | `Array entries ->
          List.map entries ~f:(fun entry ->
            let%map id = F.required entry "id" Result.return in
            `Object
              [ "id", id
              ; "entry", entry
              ; "generation", generation
              ; "binding", `Object [ "kind", `String "safe_boundary" ]
              ; "owner", Pending_input_document.Owner.to_json Unknown
              ])
          |> Result.all
          |> Result.map ~f:(fun entries -> `Array entries)
        | _ -> F.invalid "deferred_user_entries" "must be an array"
      in
      match conversation, payload with
      | `Object conversation_fields, `Object fields ->
        if
          List.exists conversation_fields ~f:(fun (name, _) ->
            String.equal name "pending_revision"
            || String.equal name "pending_dispositions")
        then F.invalid "conversation" "pending semantic-name collision"
        else (
          let conversation =
            `Object
              (List.map conversation_fields ~f:(fun (name, value) ->
                 ( name
                 , if String.equal name "deferred_user_entries" then deferred else value ))
               @ [ ( "pending_revision"
                   , P.Pending_input.Revision.to_json P.Pending_input.Revision.zero )
                 ; "pending_dispositions", `Array []
                 ])
          in
          Ok
            (`Object
                (List.map fields ~f:(fun (name, value) ->
                   name, if String.equal name "conversation" then conversation else value))))
      | _ -> F.invalid "conversation" "must be an object")
  in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ "session.state", 9 ]
      ~max_steps:8
      ~max_operations:100_000
      ~steps:
        [ step
        ; ledger_step
        ; metadata_step
        ; configuration_step
        ; organization_step
        ; history_revision_step
        ; run_step
        ; pending_input_step
        ]
  in
  D.Conversion.upgrade conversion document
;;

let codec ~limits ~presence =
  match
    D.Domain_codec.create_validated
      ~limits
      ~kind:"session.state"
      ~version:9
      ~shape
      ~supported_semantics:[]
      ~validate:(fun state -> X.document_result (S.validate state))
      ~decode:(fun json -> X.document_result (state_of_jsonaf ~limits json))
      ~encode:(fun state -> X.document_result (state_to_jsonaf state ~limits ~presence))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid session state codec", (error : D.Error.t)]
;;

let codec_for (t : t) ~limits =
  let presence =
    match t.original with
    | None -> Session_state_presence.authored
    | Some original -> original.presence
  in
  codec ~limits ~presence
;;

let decode ~limits document =
  let%bind.Result document = upgrade document ~limits in
  D.Domain_codec.decode (codec ~limits ~presence:Session_state_presence.authored) document
  |> Result.bind ~f:(decoded ~limits)
;;

let validate_ledger_carrier (t : t) ~limits =
  let open Result.Let_syntax in
  match t.original with
  | None -> Ok ()
  | Some original ->
    let%bind () = D.Document.validate original.template ~limits in
    let%bind previous =
      if D.Limits.equal original.limits limits
      then Ok original.inference_ledger
      else (
        let%bind raw =
          Agent_store.Document_fields.required
            (D.Document.payload original.template)
            "inference_ledger"
            Result.return
        in
        ledger_of_jsonaf ~limits raw |> X.document_result)
    in
    Inference_ledger.validate_update previous ~incoming:(value t).inference_ledger
    |> Result.map_error ~f:ledger_error
    |> X.document_result
;;

let encode t ~limits =
  let open Result.Let_syntax in
  let%bind () = validate_ledger_carrier t ~limits in
  let%bind document =
    match t.original with
    | Some _ -> D.Domain_codec.encode (codec_for t ~limits) t.carrier
    | None ->
      let%bind.Result () = S.validate (value t) |> X.document_result in
      let%bind.Result payload =
        state_to_jsonaf
          ~raw_pending:true
          (value t)
          ~limits
          ~presence:Session_state_presence.authored
        |> X.document_result
      in
      let%bind.Result document =
        D.Document.create ~limits ~kind:"session.state" ~version:9 ~payload
      in
      let%bind.Result admitted = decode ~limits document in
      D.Domain_codec.encode (codec_for admitted ~limits) admitted.carrier
  in
  let%map () = validate_run_capacity (value t) document in
  document
;;

let adopt previous ~limits incoming =
  let open Result.Let_syntax in
  let%bind () = validate_ledger_carrier previous ~limits in
  let%bind () = validate_ledger_carrier incoming ~limits in
  let%bind () =
    Inference_ledger.validate_update
      (value previous).inference_ledger
      ~incoming:(value incoming).inference_ledger
    |> Result.map_error ~f:ledger_error
    |> X.document_result
  in
  D.Domain_codec.adopt
    (codec_for previous ~limits)
    ~previous:previous.carrier
    ~incoming:incoming.carrier
  |> Result.bind ~f:(decoded ~limits)
;;

let retire_canonical_entries t ~retained_ids ~initial_count ~limits =
  let open Result.Let_syntax in
  let invalid reason =
    D.Error.Invalid_field { path = [ "conversation"; "canonical_history" ]; reason }
  in
  let previous = value t in
  let canonical = previous.conversation.canonical_history in
  let ids = Hash_set.of_list (module P.History.Id) retained_ids in
  let retained =
    List.filter canonical ~f:(fun entry -> Hash_set.mem ids entry.P.History.id)
  in
  let%bind () =
    if
      List.equal
        P.History.Id.equal
        retained_ids
        (List.map retained ~f:(fun entry -> entry.P.History.id))
    then Ok ()
    else
      Error
        (invalid "retirement must retain an exact ordered canonical identity subsequence")
  in
  let%bind document = encode t ~limits in
  let payload = D.Document.payload document in
  let%bind conversation =
    Agent_store.Document_fields.required payload "conversation" Result.return
  in
  let%bind raw_history =
    Agent_store.Document_fields.required
      conversation
      "canonical_history"
      Agent_store.Document_fields.array
  in
  let replace json name replacement =
    match json with
    | `Object fields ->
      Ok
        (`Object
            (List.map fields ~f:(fun (key, old) ->
               key, if String.equal key name then replacement else old)))
    | _ -> Error (invalid "retirement container must be an object")
  in
  let%bind retained_raw =
    match
      List.map2 canonical raw_history ~f:(fun entry raw ->
        Option.some_if (Hash_set.mem ids entry.P.History.id) raw)
    with
    | Ok values -> Ok (List.filter_opt values)
    | Unequal_lengths -> Error (invalid "canonical history carrier length differs")
  in
  let%bind conversation =
    replace conversation "canonical_history" (`Array retained_raw)
  in
  let%bind conversation =
    replace conversation "initial_prompt_entry_count" (X.integer_json initial_count)
  in
  let%bind payload = replace payload "conversation" conversation in
  let%bind raw = replace (D.Document.json document) "payload" payload in
  let%bind document = D.Document.inspect ~limits raw in
  let%bind carrier =
    D.Domain_codec.decode
      (codec ~limits ~presence:Session_state_presence.authored)
      document
  in
  let%map state =
    Pending_carrier_admission.rehydrate
      (D.Extension_carrier.value carrier)
      ~document
      ~limits
  in
  { carrier = D.Extension_carrier.with_value carrier state; original = t.original }
;;

let retire_canonical_suffix t ~retained_ids ~limits =
  let previous = value t in
  let prefix =
    List.take previous.conversation.canonical_history (List.length retained_ids)
  in
  if
    List.is_empty retained_ids
    || not
         (List.equal
            P.History.Id.equal
            retained_ids
            (List.map prefix ~f:(fun entry -> entry.P.History.id)))
  then
    Error
      (D.Error.Invalid_field
         { path = [ "conversation"; "canonical_history" ]
         ; reason = "retirement must retain an exact nonempty canonical identity prefix"
         })
  else
    retire_canonical_entries
      t
      ~retained_ids
      ~initial_count:
        (Int.min
           previous.conversation.initial_prompt_entry_count
           (List.length retained_ids))
      ~limits
;;

let retire_canonical_deletion t ~deletion ~limits =
  let open Result.Let_syntax in
  let%bind () =
    History_deletion.validate_basis deletion (value t)
    |> Persistence_codec.document_result
  in
  retire_canonical_entries
    t
    ~retained_ids:
      (List.map (History_deletion.canonical_history deletion) ~f:(fun entry ->
         entry.P.History.id))
    ~initial_count:(History_deletion.initial_prompt_entry_count deletion)
    ~limits
;;

let transfer_pending_internal
      t
      ~plan
      ~archive
      ~expiry_archive
      ~limits
      ~encode_previous
      ~decode_incoming
  =
  let open Result.Let_syntax in
  let invalid reason =
    D.Error.Invalid_field { path = [ "conversation"; "deferred_user_entries" ]; reason }
  in
  let previous = value t in
  let%bind () = Pending_plan.validate_basis plan previous |> X.document_result in
  let%bind previous_document = encode_previous t ~limits in
  let%bind () =
    match Pending_plan.requires_archive plan, archive with
    | true, None -> Error (invalid "pending retirement requires exact previous archive")
    | false, None -> Ok ()
    | (true | false), Some reference ->
      let%bind captured =
        D.Document.create
          ~limits
          ~kind:"session.compaction_archive"
          ~version:1
          ~payload:(`Object [ "state", D.Document.json previous_document ])
      in
      let kind_allowed =
        match reference.Session_state.Compaction_archive.kind with
        | Pending_input | Reset | Rebuild | Upgrade -> true
        | Compaction | Edit | Delete -> false
      in
      if
        kind_allowed
        && Int64.equal reference.revision previous.counters.revision
        && List.is_empty reference.invocation_dispositions
        && String.equal
             reference.sha256
             (Agent_store.Document_record.digest (D.Document.to_string captured))
      then Ok ()
      else
        Error (invalid "pending archive does not bind exact previous admitted document")
  in
  let%bind () =
    match Pending_plan.expired_dispositions plan, expiry_archive with
    | [], None -> Ok ()
    | [], Some _ -> Error (invalid "unexpected pending expiry archive")
    | _ :: _, None -> Error (invalid "pending expiry requires explicit archived custody")
    | records, Some reference ->
      let%bind expected =
        Pending_archive.create
          previous
          ~operation_id:(Pending_archive.Reference.operation_id reference)
          ~pending_revision:(Pending_plan.revision plan)
          ~records
          ~limits
        |> X.document_result
      in
      if Pending_archive.Reference.equal reference (Pending_archive.reference expected)
      then Ok ()
      else Error (invalid "pending expiry archive differs from exact retired records")
  in
  let%bind next = Pending_plan.apply plan previous |> X.document_result in
  let%bind conversation =
    Agent_store.Document_fields.required
      (D.Document.payload previous_document)
      "conversation"
      Result.return
  in
  let%bind raw_canonical =
    Agent_store.Document_fields.required
      conversation
      "canonical_history"
      Agent_store.Document_fields.array
  in
  let count = List.length (Pending_plan.adopted_entries plan) in
  let adopted = List.take previous.conversation.deferred_user_entries count in
  let%bind () =
    if
      List.equal
        P.History.equal_entry
        (List.map adopted ~f:Pending_input_document.entry)
        (Pending_plan.adopted_entries plan)
    then Ok ()
    else Error (invalid "adoption must transfer an exact queue prefix")
  in
  let%bind raw_adopted =
    List.map adopted ~f:(fun document ->
      Pending_input_document.entry_jsonaf document ~limits)
    |> Result.all
  in
  let%bind raw_pending =
    List.map (Pending_plan.pending plan) ~f:(fun document ->
      Pending_input_document.to_jsonaf document ~limits)
    |> Result.all
  in
  let%bind raw_dispositions =
    List.map (Pending_plan.dispositions plan) ~f:(fun document ->
      Pending_disposition_document.to_jsonaf document ~limits)
    |> Result.all
  in
  let replace_fields json replacements =
    match json with
    | `Object fields ->
      Ok
        (`Object
            (List.map fields ~f:(fun (name, old) ->
               ( name
               , Option.value
                   (List.Assoc.find replacements name ~equal:String.equal)
                   ~default:old ))))
    | _ -> Error (invalid "pending transfer container must be an object")
  in
  let%bind conversation =
    replace_fields
      conversation
      [ "canonical_history", `Array (raw_canonical @ raw_adopted)
      ; "deferred_user_entries", `Array raw_pending
      ; "pending_revision", P.Pending_input.Revision.to_json (Pending_plan.revision plan)
      ; "pending_dispositions", `Array raw_dispositions
      ]
  in
  let%bind payload =
    replace_fields (D.Document.payload previous_document) [ "conversation", conversation ]
  in
  let%bind json =
    replace_fields (D.Document.json previous_document) [ "payload", payload ]
  in
  let%bind document = D.Document.inspect ~limits json in
  let%bind incoming = decode_incoming ~limits document in
  let incoming_value = value incoming in
  let%bind () =
    if
      List.equal
        P.History.equal_entry
        incoming_value.conversation.canonical_history
        next.conversation.canonical_history
      && List.equal
           Pending_input_document.equal
           incoming_value.conversation.deferred_user_entries
           next.conversation.deferred_user_entries
      && List.equal
           Pending_disposition_document.equal
           incoming_value.conversation.pending_dispositions
           next.conversation.pending_dispositions
    then Ok ()
    else Error (invalid "pending custody transfer differs from pure candidate")
  in
  Ok incoming
;;

let transfer_pending t ~plan ~archive ~expiry_archive ~limits =
  transfer_pending_internal
    t
    ~plan
    ~archive
    ~expiry_archive
    ~limits
    ~encode_previous:encode
    ~decode_incoming:decode
;;

module Admitted = struct
  type nonrec state_document = t

  type t =
    { state_document : state_document
    ; document : D.Document.t
    }

  let state_document t = t.state_document
  let document t = t.document
end

module Transaction = struct
  type document = t
  type t = { staged : document }

  let value t = value t.staged
  let with_value t state = { staged = with_value t.staged state }

  let prefix_codec ~limits ~presence =
    match
      D.Domain_codec.create
        ~limits
        ~kind:"session.state"
        ~version:9
        ~shape
        ~supported_semantics:[]
        ~decode:(fun json -> state_fields_of_jsonaf ~limits json |> X.document_result)
        ~encode:(fun state ->
          state_to_jsonaf state ~limits ~presence |> X.document_result)
    with
    | Ok codec -> codec
    | Error error ->
      raise_s [%sexp "invalid private transaction codec", (error : D.Error.t)]
  ;;

  let presence t =
    match t.original with
    | None -> Session_state_presence.authored
    | Some original -> original.presence
  ;;

  let encode_prefix t ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_ledger_carrier t ~limits in
    let%bind document =
      match t.original with
      | Some _ ->
        D.Domain_codec.encode (prefix_codec ~limits ~presence:(presence t)) t.carrier
      | None ->
        let%bind payload =
          state_to_jsonaf
            ~raw_pending:true
            (value { staged = t })
            ~limits
            ~presence:Session_state_presence.authored
          |> X.document_result
        in
        let%bind document =
          D.Document.create ~limits ~kind:"session.state" ~version:9 ~payload
        in
        let%bind carrier =
          D.Domain_codec.decode
            (prefix_codec ~limits ~presence:Session_state_presence.authored)
            document
        in
        D.Domain_codec.encode
          (prefix_codec ~limits ~presence:Session_state_presence.authored)
          carrier
    in
    let%map () = validate_run_capacity (value { staged = t }) document in
    document
  ;;

  let decode_prefix ~limits document =
    let open Result.Let_syntax in
    let%bind carrier =
      D.Domain_codec.decode
        (prefix_codec ~limits ~presence:Session_state_presence.authored)
        document
    in
    let%bind state =
      Pending_carrier_admission.rehydrate_transaction_prefix
        (D.Extension_carrier.value carrier)
        ~document
        ~limits
    in
    let%bind () = validate_run_capacity state document in
    let%map presence = Session_state_presence.of_document document ~limits in
    { carrier = D.Extension_carrier.with_value carrier state
    ; original =
        Some
          { template = document
          ; limits
          ; inference_ledger = state.S.inference_ledger
          ; presence
          }
    }
  ;;

  let begin_ document ~limits =
    let open Result.Let_syntax in
    let%bind () = S.validate (value { staged = document }) |> X.document_result in
    let%map _ = encode_prefix document ~limits in
    { staged = document }
  ;;

  let checkpoint t ~limits =
    let%map.Result _ = encode t.staged ~limits in
    t.staged
  ;;

  let transfer_pending t ~plan ~archive ~expiry_archive ~limits =
    let encode_previous =
      if Pending_plan.requires_archive plan || Option.is_some archive
      then encode
      else encode_prefix
    in
    let%map.Result staged =
      transfer_pending_internal
        t.staged
        ~plan
        ~archive
        ~expiry_archive
        ~limits
        ~encode_previous
        ~decode_incoming:decode_prefix
    in
    { staged }
  ;;

  let finish_encoded t ~(next : S.t) ~limits =
    let open Result.Let_syntax in
    let expected = value t in
    let normalized =
      { next with
        identity = { next.identity with updated_at = expected.identity.updated_at }
      ; counters =
          { next.counters with
            revision = expected.counters.revision
          ; event_sequence = expected.counters.event_sequence
          ; transaction_sequence = expected.counters.transaction_sequence
          }
      }
    in
    let%bind equal = equal_values expected ~limits normalized in
    let%bind () =
      if equal
      then Ok ()
      else
        Error
          (D.Error.Invalid_field
             { path = []
             ; reason = "candidate differs from ordered admitted durable mutation"
             })
    in
    let candidate = (with_value t next).staged in
    let%map document = encode candidate ~limits in
    { Admitted.state_document = candidate; document }
  ;;

  let finish t ~next ~limits =
    Result.map (finish_encoded t ~next ~limits) ~f:Admitted.state_document
  ;;
end
