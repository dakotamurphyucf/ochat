open! Core

type reset_options =
  { keep_history : bool
  ; keep_tasks : bool
  ; keep_grants : bool
  ; keep_labels : bool
  ; workspace_instance : Workspace_instance.t option
  }

let conversation state options =
  let previous = state.Session_state.conversation in
  { previous with
    canonical_history =
      (if options.keep_history
       then previous.canonical_history
       else List.take previous.canonical_history previous.initial_prompt_entry_count)
  ; deferred_user_entries = []
  ; tasks = (if options.keep_tasks then previous.tasks else [])
  ; kv_store = (if options.keep_tasks then previous.kv_store else [])
  ; authoring_reference_index = None
  ; authoring_publication = None
  }
;;

let spec state options =
  Option.value_map
    options.workspace_instance
    ~default:state.Session_state.spec
    ~f:(fun workspace_instance ->
      let quota_key =
        Option.map state.spec.quota_key ~f:(fun quota_key ->
          { quota_key with
            Quota_key.conflict_domain = workspace_instance.conflict_domain
          })
      in
      { state.spec with workspace_instance; quota_key })
;;

let reset_state (state : Session_state.t) options =
  let labels = if options.keep_labels then state.identity.labels else [] in
  let metadata_revision =
    if List.equal [%equal: string * string] labels state.identity.labels
    then state.identity.metadata_revision
    else Int64.succ state.identity.metadata_revision
  in
  let spec = spec state options in
  { state with
    identity =
      { state.identity with
        generation = state.identity.generation + 1
      ; labels
      ; metadata_revision
      }
  ; spec = { spec with protocol = { spec.protocol with labels } }
  ; lifecycle = { desired = Stopped; observed = Stopped }
  ; pending_initial_start = false
  ; conversation = conversation state options
  ; active_operation = None
  ; permissions = []
  ; grants = (if options.keep_grants then state.grants else [])
  ; jobs = []
  ; model_job_targets = []
  ; schedules = []
  ; invocations = []
  ; managed_submissions = []
  ; subscriptions = []
  ; ingress_registrations = []
  ; deliveries = []
  ; moderator = None
  ; shell = (if options.keep_grants then state.shell else Session.Shell_state.empty)
  ; halted = false
  ; halt_reason = None
  ; failure = None
  }
;;

let plan_reset state options =
  let open Result.Let_syntax in
  let%bind () = Session_state.validate state in
  if Int.equal state.Session_state.identity.generation Int.max_value
  then
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:"session generation overflow"
         ~retryable:false
         ())
  else if
    (not options.keep_labels)
    && (not (List.is_empty state.identity.labels))
    && Int64.equal state.identity.metadata_revision Int64.max_value
  then Error (Agent_protocol.Error.invalid_request "metadata revision exhausted")
  else (
    let candidate = reset_state state options in
    let%map () =
      Session_state.validate_administration_candidate candidate ~previous:state
    in
    candidate)
;;

let admit_generation (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind inference_ledger =
    Inference_ledger.with_generation
      state.inference_ledger
      ~generation:state.identity.generation
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.create
        Invalid_state
        ~message:(Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error))
        ~retryable:false
        ())
  in
  let state = { state with inference_ledger } in
  let%map () = Session_state.validate state in
  state
;;

let reset state options = Result.bind (plan_reset state options) ~f:admit_generation

let upgrade (state : Session_state.t) revision =
  { state with
    Session_state.spec = { state.spec with prompt_revision_id = revision }
  ; moderator = None
  ; shell = Session.Shell_state.empty
  ; failure = None
  }
;;

let plan_rebuild state revision =
  let open Result.Let_syntax in
  let previous = state in
  let%bind state =
    plan_reset
      state
      { keep_history = false
      ; keep_tasks = true
      ; keep_grants = false
      ; keep_labels = true
      ; workspace_instance = None
      }
  in
  let state = upgrade state revision in
  let candidate =
    { state with
      conversation =
        { state.conversation with canonical_history = []; initial_prompt_entry_count = 0 }
    }
  in
  let%map () = Session_state.validate_administration_candidate candidate ~previous in
  candidate
;;

let rebuild state revision = Result.bind (plan_rebuild state revision) ~f:admit_generation

let archive ~archive_reference ~previous (candidate : Session_state.t) kind =
  let open Result.Let_syntax in
  let%bind candidate, invocation_dispositions =
    match kind with
    | Session_state.Compaction_archive.Compaction | Upgrade | Edit | Delete ->
      Ok (candidate, [])
    | Reset | Rebuild ->
      let first =
        Int64.max
          candidate.conversation.next_history_sequence
          candidate.conversation.reserved_history_through
      in
      if Int64.(first < 0L || first > of_int Int.max_value)
      then
        Error
          (Agent_protocol.Error.invalid_request
             "administrative history sequence is out of range")
      else (
        let source =
          { previous with Session_state.conversation = candidate.conversation }
        in
        let%bind plan =
          Invocation_recovery.plan
            ~state:source
            ~namespace:(Agent_protocol.Id.Session.to_string candidate.identity.session_id)
            ~first_sequence:(Int64.to_int_exn first)
            ~reason:"invocation interrupted by administrative history replacement"
        in
        let%map reconciled = Session_delta.apply source (Batch plan.deltas) in
        let dispositions =
          List.filter_map reconciled.invocations ~f:(fun current ->
            let original =
              List.find_exn previous.invocations ~f:(fun old ->
                Agent_protocol.Id.Invocation.compare old.context.id current.context.id = 0)
            in
            if
              Sexp.equal
                (Agent_protocol.Invocation.sexp_of_t original)
                (Agent_protocol.Invocation.sexp_of_t current)
            then None
            else
              Some
                Session_state.Compaction_archive.
                  { invocation_id = current.context.id
                  ; interruption_reason =
                      (match original.status, current.status with
                       | ( (Admitted | Dispatching)
                         , (Resolved (Cancelled reason) | Published (Cancelled reason)) )
                         -> Some reason
                       | _ -> None)
                  ; output_entry_id = current.output_entry_id
                  ; publication_discarded = current.publication_discarded
                  })
        in
        ( { candidate with
            conversation =
              { candidate.conversation with
                canonical_history = reconciled.conversation.canonical_history
              ; next_history_sequence = Int64.of_int plan.next_sequence
              ; reserved_history_through = Int64.of_int plan.next_sequence
              }
          }
        , dispositions ))
  in
  let%bind reference =
    archive_reference ~previous ~kind (Agent_protocol.Id.Operation.create ())
  in
  Ok
    { candidate with
      Session_state.conversation =
        { candidate.conversation with
          compaction_archives =
            { reference with invocation_dispositions }
            :: previous.conversation.compaction_archives
        }
    }
;;

let upgrade_payload previous state =
  if
    Agent_protocol.Id.Prompt_revision.compare
      previous.Session_state.spec.prompt_revision_id
      state.Session_state.spec.prompt_revision_id
    = 0
  then []
  else
    Option.to_list
      (Option.map state.spec.prompt_definition_id ~f:(fun prompt_id ->
         Agent_protocol.Event.Durable.Payload.Prompt_upgraded
           { prompt_id
           ; previous_revision = previous.spec.prompt_revision_id
           ; current_revision = state.spec.prompt_revision_id
           }))
;;

let payloads ~previous state =
  Agent_protocol.Event.Durable.Payload.
    [ Session_updated (Session_state.summary state)
    ; History_replaced
        (Session_state.history_window state.Session_state.conversation.canonical_history)
    ; Session_state_changed
        { desired_state = state.lifecycle.desired
        ; observed_state = state.lifecycle.observed
        }
    ]
  @ upgrade_payload previous state
;;
