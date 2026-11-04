open! Core
module P = Agent_protocol
module D = Document_schema
module R = Recurrence_child_reservation
module Store = Agent_store.Delegation_store
module Raw = Agent_store.Delegation_captured_origin

type t =
  { record : Store.record
  ; raw : Raw.t
  ; reservation : R.t
  ; definition : Prompt_definition.t
  }

let reservation t = t.reservation
let definition t = t.definition
let invalid message = Error (P.Error.invalid_request message)

let document_result result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let spec_digest spec =
  let open Result.Let_syntax in
  let json =
    Session_state_document.project_spec
      { spec with Session_state.Spec.delegation = None }
      ~workspace:Workspace_instance.to_jsonaf
  in
  let%bind () = D.Json.validate ~limits:Raw.limits json |> document_result in
  let%map canonical = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string canonical |> to_hex)
;;

let create definition ~reservation ~initial_spec =
  let open Result.Let_syntax in
  let%bind () = R.validate reservation ~definition in
  let%bind definition =
    Recurrence_target.fresh_definition_json (Recurrence_definition.target definition)
  in
  let%bind initial_spec_sha256 = spec_digest initial_spec in
  Raw.create ~reservation:(R.to_json reservation) ~definition ~initial_spec_sha256
  |> document_result
;;

let of_record (record : Store.record) =
  let open Result.Let_syntax in
  let%bind raw =
    match record.admission.captured_recurrence, record.admission.authored_tool with
    | Some raw, None -> Ok raw
    | None, (Some _ | None) | Some _, Some _ ->
      invalid "delegation does not have the captured recurrence source kind"
  in
  let%bind reservation = R.of_json (Raw.reservation raw) in
  let%bind definition = Prompt_definition.of_jsonaf (Raw.definition raw) in
  let%bind key = R.delegation_key reservation in
  if
    Store.Key.equal record.key key
    && String.equal record.request_sha256 (R.request_sha256 reservation)
    && P.Id.Session.equal
         record.admission.child_session_id
         (R.child_session_id reservation)
    && P.Id.Transaction.equal
         record.admission.transaction_id
         (R.transaction_id reservation)
    && P.Id.Prompt_revision.equal
         record.admission.revision_id
         (R.artifact_revision_id reservation)
    && String.equal record.admission.manifest_sha256 (R.manifest_sha256 reservation)
  then Ok { record; raw; reservation; definition }
  else invalid "captured origin differs from delegation admission"
;;

let validate_source t (state : Session_state.t) =
  let expected = Store.reference t.record in
  let source_matches =
    match state.spec.protocol.prompt with
    | Generated id -> P.Id.Prompt_revision.equal id (R.artifact_revision_id t.reservation)
    | Catalog _ | Local_path _ -> false
  in
  let parent_epoch_matches =
    match t.record.admission.parent_stop_epoch, state.parent_stop_epoch with
    | Some admitted, Some acknowledged -> Int64.(acknowledged >= admitted)
    | None, (None | Some _) | Some _, None -> false
  in
  let inference_matches =
    match
      ( t.record.admission.inference_target
      , Inference.Selection.view state.spec.inference_target )
    with
    | Some expected, Captured actual -> Inference.Request.Target.equal expected actual
    | None, (Captured _ | Unresolved) | Some _, Unresolved -> false
  in
  if
    source_matches
    && parent_epoch_matches
    && inference_matches
    && P.Id.Session.equal state.identity.session_id expected.child_session_id
    && P.Id.Prompt_revision.equal state.spec.prompt_revision_id expected.revision_id
    && Option.exists state.spec.delegation ~f:(Store.Reference.equal expected)
    && Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.equal
         state.spec.root_invocation
         t.definition.root_invocation
    && String.equal state.spec.permission_profile t.definition.permission_profile
    && Option.equal String.equal state.spec.runtime_policy t.definition.runtime_policy
  then Ok ()
  else invalid "captured child source differs from immutable origin"
;;

let validate_initial_state t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = validate_source t state in
  let%bind digest = spec_digest state.spec in
  (* This new source kind is created before any Run owner is installed. Empty
     retained lists cannot erase evidence in the index's other owned fields. *)
  let no_runs = Option.is_none state.run_state in
  let stopped_pending =
    match
      state.lifecycle.desired, state.lifecycle.observed, state.runtime_initialization
    with
    | Stopped, Stopped, Pending { fresh_history = true } -> true
    | (Stopped | Running), _, (Ready | Pending _) -> false
  in
  let summary = Inference_ledger.summary state.inference_ledger in
  let turns = P.Inference_query.Summary.turns summary in
  let coverage = P.Inference_query.Summary.coverage summary in
  let no_inference =
    Int64.equal (Inference_ledger.revision state.inference_ledger) 0L
    && Int64.equal (P.Inference_query.Summary.retained_attempts summary) 0L
    && List.for_all
         [ turns.pending
         ; turns.completed
         ; turns.failed
         ; turns.cancelled
         ; turns.interrupted
         ; coverage.retired_attempts
         ; coverage.untracked_attempts
         ; coverage.retired_turns
         ; coverage.untracked_turns
         ]
         ~f:(fun count -> Int64.equal count 0L)
    && (not coverage.before_tracking_unknown)
    &&
    match coverage.tracking_status with
    | Available -> true
    | Limited _ -> false
  in
  let conversation = state.conversation in
  let no_history =
    List.is_empty conversation.canonical_history
    && List.is_empty conversation.deferred_user_entries
    && List.is_empty conversation.pending_dispositions
    && P.Pending_input.Revision.equal
         conversation.pending_revision
         P.Pending_input.Revision.zero
    && Int.equal conversation.initial_prompt_entry_count 0
    && Int64.equal conversation.next_history_sequence 0L
    && Int64.equal conversation.reserved_history_through 0L
    && List.is_empty conversation.tasks
    && List.is_empty conversation.kv_store
    && Int.equal conversation.compaction_generation 0
    && List.is_empty conversation.compaction_archives
    && Option.is_none conversation.authoring_reference_index
    && Option.is_none conversation.authoring_publication
  in
  if
    String.equal digest (Raw.initial_spec_sha256 t.raw)
    && stopped_pending
    && (not state.pending_initial_start)
    && (not state.spec.protocol.start_immediately)
    && Option.is_none state.active_operation
    && Option.is_none state.failure
    && Option.is_none state.moderator
    && List.is_empty state.permissions
    && List.is_empty state.grants
    && List.is_empty state.jobs
    && List.is_empty state.model_job_targets
    && List.is_empty state.schedules
    && List.is_empty state.invocations
    && List.is_empty state.moderator_executions
    && no_runs
    && no_inference
    && no_history
    && Int.equal state.identity.generation 0
    && P.Timestamp.equal state.identity.created_at t.record.admission.created_at
    && Option.exists
         state.identity.creating_principal
         ~f:(P.Id.Principal.equal (R.principal_id t.reservation))
  then Ok ()
  else invalid "captured child is not the original never-initialized stopped state"
;;
