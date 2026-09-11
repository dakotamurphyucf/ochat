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
  { state with
    Session_state.identity =
      { state.identity with
        generation = state.identity.generation + 1
      ; labels = (if options.keep_labels then state.identity.labels else [])
      }
  ; spec = spec state options
  ; lifecycle = { desired = Stopped; observed = Stopped }
  ; pending_initial_start = false
  ; conversation = conversation state options
  ; active_operation = None
  ; permissions = []
  ; grants = (if options.keep_grants then state.grants else [])
  ; jobs = []
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

let reset state options =
  if Int.equal state.Session_state.identity.generation Int.max_value
  then
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:"session generation overflow"
         ~retryable:false
         ())
  else Ok (reset_state state options)
;;

let upgrade (state : Session_state.t) revision =
  { state with
    Session_state.spec = { state.spec with prompt_revision_id = revision }
  ; moderator = None
  ; shell = Session.Shell_state.empty
  ; failure = None
  }
;;

let rebuild state revision =
  let open Result.Let_syntax in
  let%map state =
    reset
      state
      { keep_history = false
      ; keep_tasks = true
      ; keep_grants = false
      ; keep_labels = true
      ; workspace_instance = None
      }
  in
  let state = upgrade state revision in
  { state with
    conversation =
      { state.conversation with canonical_history = []; initial_prompt_entry_count = 0 }
  }
;;

let archive ~previous (candidate : Session_state.t) kind =
  let open Result.Let_syntax in
  let%bind candidate, invocation_dispositions =
    match kind with
    | Session_state.Compaction_archive.Compaction | Upgrade -> Ok (candidate, [])
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
  let reference =
    Compaction_archive.reference_for
      previous
      ~kind
      (Agent_protocol.Id.Operation.create ())
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
