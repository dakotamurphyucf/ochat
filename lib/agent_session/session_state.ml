open! Core

module Identity = struct
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
  [@@deriving sexp, fields ~iterators:map_poly]
end

module Spec = struct
  type 'workspace fields =
    { protocol : Agent_protocol.Session.Spec.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; root_invocation :
        Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
          [@sexp.default
            Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.Ui]
    ; prompt_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; configuration_revision : int64 [@sexp.default 0L]
    ; inference_target : (Inference.Selection.t[@sexp.opaque])
    ; delegation : Agent_store.Delegation_store.Reference.t option [@sexp.option]
    ; workspace_instance : 'workspace
    ; permission_profile : string
    ; permission_profile_digest : string
    ; runtime_policy : string option
    ; quota_key : Quota_key.t option
    }
  [@@deriving sexp]

  type t = Workspace_instance.t fields [@@deriving sexp]

  module Resource_identity = struct
    type spec = t

    type t =
      { spec : Sexp.t
      ; inference : Inference.Selection.t
      }

    let capture (spec : spec) =
      let protocol = { spec.protocol with display_name = None; labels = [] } in
      { spec = sexp_of_t { spec with protocol }; inference = spec.inference_target }
    ;;

    let equal t other =
      Sexp.equal t.spec other.spec
      && Inference.Selection.equal t.inference other.inference
    ;;
  end
end

module Compaction_archive = struct
  type kind =
    | Compaction
    | Reset
    | Rebuild
    | Upgrade
    | Edit
    | Delete
    | Pending_input
  [@@deriving equal, enumerate, sexp]

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

module Conversation = struct
  type t =
    { canonical_history : Agent_protocol.History.entry list
    ; deferred_user_entries : Pending_input_document.t list
    ; pending_revision : Agent_protocol.Pending_input.Revision.t
    ; pending_dispositions : Pending_disposition_document.t list
    ; initial_prompt_entry_count : int
    ; next_history_sequence : int64
    ; reserved_history_through : int64
    ; tasks : Jsonaf.t list
    ; kv_store : (string * string) list
    ; compaction_generation : int
    ; compaction_archives : Compaction_archive.t list [@sexp.list]
    ; authoring_reference_index : Jsonaf.t option [@sexp.option]
    ; authoring_publication : Chat_response.Authoring_publication.context option
          [@sexp.option]
    }
  [@@deriving sexp]
end

module Lifecycle = struct
  type t =
    { desired : Agent_protocol.Session.desired_state
    ; observed : Agent_protocol.Session.observed_state
    }
  [@@deriving sexp]
end

module Runtime_initialization = struct
  type t =
    | Ready
    | Pending of { fresh_history : bool }
  [@@deriving equal, sexp]
end

module Counters = struct
  type t =
    { revision : int64
    ; event_sequence : int64
    ; transaction_sequence : int64
    ; owner_lease_generation : int64
    }
  [@@deriving equal, sexp]
end

type 'spec fields =
  { schema_version : int
  ; identity : Identity.t
  ; spec : 'spec
  ; lifecycle : Lifecycle.t
  ; runtime_initialization : Runtime_initialization.t
  ; pending_initial_start : bool [@sexp.default false]
  ; stop_epoch : int64 [@sexp.default 0L]
  ; parent_stop_epoch : int64 option [@sexp.option]
  ; conversation : Conversation.t
  ; active_operation : Agent_protocol.Operation.t option
  ; automatic_turn_budget : Automatic_turn_budget.t option [@sexp.option]
  ; recovery_windows : (Recovery_window_ledger.t option[@sexp.opaque])
  ; recovery_effective : (Recovery_effective_ledger.t option[@sexp.opaque])
  ; permissions : Agent_protocol.Permission.t list
  ; grants : Agent_protocol.Grant.t list
  ; jobs : Agent_protocol.Job.t list
  ; inference_ledger : (Inference_ledger.t[@sexp.opaque])
  ; model_job_targets : Model_job_target.t list
  ; schedules : Agent_protocol.Schedule.t list
  ; invocations : Agent_protocol.Invocation.t list [@sexp.list]
  ; managed_submissions : Managed_submission.t list [@sexp.list]
  ; managed_stops : Managed_stop.t list [@sexp.list]
  ; moderator_executions : Agent_protocol.Moderator_execution.t list [@sexp.list]
  ; run_state : Run_state.t option [@sexp.option]
  ; goal_catalog : Goal_catalog.t option [@sexp.option]
  ; goal_accounts : (Goal_accounts.t option[@sexp.opaque])
  ; delegated_usage : (Delegated_usage.t option[@sexp.opaque])
  ; child_contributions : (Child_contributions.t option[@sexp.opaque])
  ; goal_descendants : (Goal_descendants.t option[@sexp.opaque])
  ; policy_canonical_usage : (Policy_canonical_usage.t option[@sexp.opaque])
  ; goal_policy_contributions : (Goal_policy_contributions.t option[@sexp.opaque])
  ; instruction_selection : Instruction_selection.t option [@sexp.option]
  ; recovery_notes : (Recovery_note_session.t[@sexp.opaque]) option [@sexp.option]
  ; recovery_note_shares : (Recovery_note_share.t[@sexp.opaque]) option [@sexp.option]
  ; recurrence_catalog : Recurrence_catalog.t option [@sexp.option]
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
[@@deriving sexp, fields ~iterators:map_poly]

type t = Spec.t fields [@@deriving sexp]

let same_ordinary_ready_reload_basis (t : t) ~(expected : t) =
  let identity = { t.identity with updated_at = expected.identity.updated_at } in
  let counters =
    { t.counters with
      revision = expected.counters.revision
    ; event_sequence = expected.counters.event_sequence
    ; transaction_sequence = expected.counters.transaction_sequence
    }
  in
  let current =
    { t with
      identity = expected.identity
    ; schedules = expected.schedules
    ; counters = expected.counters
    }
  in
  (* Preserve exact construction inputs, including any new fields. The scheduler
     owns its live table, and commits advance the stamps independently of it. *)
  List.for_all
    (Identity.Fields.map_poly
       { f =
           (fun field ->
             phys_equal (Field.get field identity) (Field.get field expected.identity))
       })
    ~f:Fn.id
  && Counters.equal counters expected.counters
  && List.for_all
       (Fields_of_fields.map_poly
          { f =
              (fun field ->
                phys_equal (Field.get field current) (Field.get field expected))
          })
       ~f:Fn.id
;;

let execution_migration_required (t : t) =
  match t.lifecycle.observed with
  | Failed failure -> Agent_protocol.Error.equal_code failure.code Migration_required
  | Stopped ->
    Option.exists t.failure ~f:(fun failure ->
      Agent_protocol.Error.equal_code failure.code Migration_required)
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping -> false
;;

let lifecycle_pointer (t : t) = Option.bind t.run_state ~f:Run_state.moderator_lifecycle

let validate_moderator_lifecycle (t : t) =
  let open Result.Let_syntax in
  match lifecycle_pointer t, t.run_state with
  | None, _ -> Ok ()
  | Some pointer, Some index ->
    let%bind source =
      Run_source_installation.captured
        (Run_state.installation index)
        ~generation:t.identity.generation
    in
    Moderator_lifecycle.validate
      pointer
      ~session_id:t.identity.session_id
      ~generation:t.identity.generation
      ~current_source:source
      ~executions:t.moderator_executions
  | Some _, None -> assert false
;;

let validate_moderator_lifecycle_index_update (previous : t) incoming =
  let before = lifecycle_pointer previous in
  let after = Run_state.moderator_lifecycle incoming in
  if Option.equal Moderator_lifecycle.equal before after
  then Ok ()
  else if
    Option.is_none after
    && Option.exists previous.run_state ~f:(fun index ->
      not
        (Run_source_installation.equal
           (Run_state.installation index)
           (Run_state.installation incoming)))
  then Ok ()
  else
    Error
      (Agent_protocol.Error.invalid_request
         "ordinary run index update cannot select or discard lifecycle evidence")
;;

let validate_moderator_lifecycle_update
      ~previous_runs
      ~previous_generation
      ~previous_executions
      (incoming : t)
  =
  let before = Option.bind previous_runs ~f:Run_state.moderator_lifecycle in
  let after = lifecycle_pointer incoming in
  match before, after with
  | None, None -> Ok ()
  | _ ->
    let open Result.Let_syntax in
    let%bind () = validate_moderator_lifecycle incoming in
    let newly_admitted =
      (* Receipts are immutable. Exact shared list identity proves that no ID was
         admitted; it grants no source or effect authority. *)
      if phys_equal previous_executions incoming.moderator_executions
      then []
      else (
        let previous_ids =
          Hash_set.of_list
            (module Agent_protocol.Id.Moderator_execution)
            (List.map previous_executions ~f:(fun previous ->
               previous.Agent_protocol.Moderator_execution.context.id))
        in
        List.filter incoming.moderator_executions ~f:(fun execution ->
          let c = execution.Agent_protocol.Moderator_execution.context in
          Int.equal c.generation incoming.identity.generation
          && Moderator_lifecycle.lifecycle_phase c.phase
          && not (Hash_set.mem previous_ids c.id)))
    in
    let invalid () =
      Error
        (Agent_protocol.Error.invalid_request
           "lifecycle pointer transition has no unique new claim or source invalidation")
    in
    (match before, after with
     | None, None -> assert false
     | Some before, Some after when Moderator_lifecycle.equal before after ->
       if List.is_empty newly_admitted then Ok () else invalid ()
     | _, Some after ->
       (match newly_admitted with
        | [ execution ]
          when Agent_protocol.Id.Moderator_execution.equal
                 execution.context.id
                 (Moderator_lifecycle.execution_id after)
               &&
               match execution.status with
               | Running -> true
               | Completed _ | Failed _ | Interrupted _ -> false -> Ok ()
        | _ -> invalid ())
     | Some before, None ->
       let source_changed =
         (not (Int.equal previous_generation incoming.identity.generation))
         ||
         match incoming.run_state with
         | None -> true
         | Some index ->
           (match
              Run_source_installation.captured
                (Run_state.installation index)
                ~generation:incoming.identity.generation
            with
            | Error _ -> true
            | Ok current ->
              not
                (Agent_protocol.Run_source.equal
                   current
                   (Moderator_lifecycle.source before)))
       in
       if source_changed || not (List.is_empty newly_admitted) then Ok () else invalid ())
;;

let policy_context (t : t) =
  let open Result.Let_syntax in
  let%bind current_source =
    match t.run_state with
    | None -> Ok None
    | Some index ->
      let installation = Run_state.installation index in
      (match installation.Run_source_installation.source with
       | None -> Ok None
       | Some _ ->
         Run_source_installation.captured installation ~generation:t.identity.generation
         |> Result.map ~f:Option.some)
  in
  Policy_cause_data.Context.create
    ~session_id:t.identity.session_id
    ~generation:t.identity.generation
    ~current_source
    ~events:t.moderator_executions
    ~invocations:t.invocations
    ~jobs:t.jobs
    ~runs:t.run_state
    ~accounts:t.goal_accounts
    ~catalog:t.goal_catalog
    ~descendants:t.goal_descendants
;;

let goal_policy_projection (t : t) =
  Goal_policy_ordered.Projection.create
    ~session_id:t.identity.session_id
    ~session_revision:t.counters.revision
    ~catalog:t.goal_catalog
    ~accounts:t.goal_accounts
    ~ledger:t.inference_ledger
    ~descendants:t.goal_descendants
    ~children:t.child_contributions
    ~causal:t.goal_policy_contributions
    ~canonical:t.policy_canonical_usage
;;

let goal_policy_context_at_revision (t : t) ~owner ~session_revision =
  let open Result.Let_syntax in
  let%bind projection = goal_policy_projection t in
  let%bind () =
    match Goal_policy_ordered.Projection.owner projection with
    | Some actual when Agent_protocol.Session_ref.equal actual owner -> Ok ()
    | None | Some _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "Goal view has no matching actual full owner")
  in
  Goal_policy_totals.Context.create
    ~owner
    ~catalog:t.goal_catalog
    ~accounts:t.goal_accounts
    ~ledger:t.inference_ledger
    ~descendants:t.goal_descendants
    ~children:t.child_contributions
    ~causal:t.goal_policy_contributions
    ~canonical:t.policy_canonical_usage
    ~session_revision
;;

let goal_policy_context (t : t) ~owner =
  goal_policy_context_at_revision t ~owner ~session_revision:t.counters.revision
;;

let validate_goal_policy (t : t) ~session_revision =
  if Option.is_none t.goal_policy_contributions && Option.is_none t.policy_canonical_usage
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind projection = goal_policy_projection t in
    match Goal_policy_ordered.Projection.owner projection with
    | None ->
      Error
        (Agent_protocol.Error.invalid_request "positive Goal policy has no full owner")
    | Some owner ->
      let%bind () =
        Option.value_map t.goal_policy_contributions ~default:(Ok ()) ~f:(fun book ->
          Goal_policy_contributions.validate_context
            book
            ~owner
            ~catalog:t.goal_catalog
            ~accounts:t.goal_accounts
            ~canonical:t.policy_canonical_usage
            ~ledger:t.inference_ledger
            ~session_revision)
      in
      let%bind inline =
        Goal_policy_contributions_carrier.inline
          ~owner
          ~canonical:t.policy_canonical_usage
          ~ledger:t.inference_ledger
          ~session_revision
      in
      let%bind () =
        if List.length inline <= Goal_policy_contributions.max_streams
        then Ok ()
        else
          Error
            (Agent_protocol.Error.invalid_request "inline Goal stream capacity exhausted")
      in
      let%bind context = goal_policy_context_at_revision t ~owner ~session_revision in
      Option.value_map t.goal_accounts ~default:(Ok ()) ~f:(fun accounts ->
        List.fold_result (Goal_accounts.documents accounts) ~init:() ~f:(fun () account ->
          Goal_policy_totals.capture
            context
            ~goal_id:
              (Goal_accounting.Stored.goal_id (Goal_accounting_document.stored account))
          |> Result.map ~f:ignore))
;;

let current_schema_version = 29

let upgrade_schema (t : t) =
  if t.schema_version < 29 && Option.is_some t.goal_policy_contributions
  then
    Error
      (Agent_protocol.Error.invalid_request "Goal causal books require session schema29")
  else if t.schema_version = 28
  then Ok { t with schema_version = current_schema_version }
  else if t.schema_version < 28 && Option.is_some t.policy_canonical_usage
  then
    Error
      (Agent_protocol.Error.invalid_request
         "policy canonical usage requires session schema 28")
  else if t.schema_version = current_schema_version
  then Ok t
  else if t.schema_version = 27
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version < 27
    && (Option.is_some t.goal_descendants
        || Option.exists t.child_contributions ~f:Child_contributions.has_goal_origins)
  then
    Error
      (Agent_protocol.Error.invalid_request
         "descendant Goal evidence requires session schema 27")
  else if t.schema_version = 26
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version < 26
    && (Option.is_some t.delegated_usage || Option.is_some t.child_contributions)
  then
    Error
      (Agent_protocol.Error.invalid_request "delegated usage requires session schema 26")
  else if t.schema_version = 25
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version < 25
    && Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.equal
         t.spec.root_invocation
         Headless
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"headless root invocation requires session schema 25"
         ~retryable:false
         ())
  else if t.schema_version = 24
  then Ok { t with schema_version = current_schema_version }
  else if t.schema_version = 23
  then Ok { t with schema_version = current_schema_version }
  else if t.schema_version = 22
  then Ok { t with schema_version = current_schema_version }
  else if t.schema_version = 21
  then Ok { t with schema_version = current_schema_version }
  else if t.schema_version = 20
  then Ok { t with schema_version = current_schema_version }
  else if Option.is_some t.conversation.authoring_publication
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"authoring publication context requires session schema 20"
         ~retryable:false
         ())
  else if t.schema_version = 19
  then Ok { t with schema_version = current_schema_version }
  else if
    List.exists t.invocations ~f:(fun invocation ->
      Option.is_some invocation.Agent_protocol.Invocation.authoring_reference)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"invocation authoring references require session schema 19"
         ~retryable:false
         ())
  else if t.schema_version = 18
  then Ok { t with schema_version = current_schema_version }
  else if Option.is_some t.conversation.authoring_reference_index
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"authoring reference index requires session schema 18"
         ~retryable:false
         ())
  else if t.schema_version = 17
  then Ok { t with schema_version = current_schema_version }
  else if not (List.is_empty t.managed_stops)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"managed stop receipts require session schema 17"
         ~retryable:false
         ())
  else if t.schema_version = 16
  then Ok { t with schema_version = current_schema_version }
  else if not (List.is_empty t.managed_submissions)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"managed submission receipts require session schema 16"
         ~retryable:false
         ())
  else if t.schema_version = 15
  then Ok { t with schema_version = current_schema_version }
  else if
    List.exists t.moderator_executions ~f:(fun event ->
      Option.is_some event.Agent_protocol.Moderator_execution.delegation
      || Option.is_some event.decision)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"delegated moderator receipts require session schema 15"
         ~retryable:false
         ())
  else if t.schema_version = 14
  then Ok { t with schema_version = current_schema_version }
  else if Option.is_some t.parent_stop_epoch
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"parent stop acknowledgement requires session schema 14"
         ~retryable:false
         ())
  else if t.schema_version = 13
  then Ok { t with schema_version = current_schema_version }
  else if not (Int64.equal t.stop_epoch 0L)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"durable stop epochs require session schema 13"
         ~retryable:false
         ())
  else if t.schema_version = 12
  then Ok { t with schema_version = current_schema_version }
  else if t.pending_initial_start
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"generated initial start intent requires session schema 12"
         ~retryable:false
         ())
  else if t.schema_version = 11
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version < current_schema_version
    && (Option.is_some t.spec.delegation
        ||
        match t.spec.protocol.prompt with
        | Generated _ -> true
        | _ -> false)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"generated delegation references require session schema 11"
         ~retryable:false
         ())
  else if t.schema_version = 10
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version < current_schema_version
    && List.exists
         (t.conversation.canonical_history
          @ List.map t.conversation.deferred_user_entries ~f:Pending_input_document.entry
         )
         ~f:(fun entry ->
           match entry.Agent_protocol.History.provenance with
           | Runtime_authoring _ -> true
           | _ -> false)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"authoring guidance provenance requires session schema 10"
         ~retryable:false
         ())
  else if t.schema_version = 9
  then Ok { t with schema_version = current_schema_version }
  else if
    List.exists t.jobs ~f:(fun job ->
      match job.Agent_protocol.Job.delivery with
      | Discarded _ -> true
      | _ -> false)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"discarded job deliveries require session schema 9"
         ~retryable:false
         ())
  else if t.schema_version = 8
  then Ok { t with schema_version = current_schema_version }
  else if not (List.is_empty t.ingress_registrations)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"external ingress registrations require session schema 8"
         ~retryable:false
         ())
  else if
    List.exists t.permissions ~f:(fun permission ->
      match permission.owner with
      | Invocation _ -> true
      | Operation _ -> false)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"invocation-owned permissions require session schema 8"
         ~retryable:false
         ())
  else if t.schema_version = 7
  then Ok { t with schema_version = current_schema_version }
  else if
    List.exists t.invocations ~f:(fun invocation ->
      Option.is_some invocation.parent_event)
  then
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"event-owned invocations require session schema 7"
         ~retryable:false
         ())
  else if t.schema_version = 6
  then Ok { t with schema_version = current_schema_version }
  else if
    t.schema_version = 5
    && List.for_all t.moderator_executions ~f:(fun event ->
      Option.is_none event.retirement)
  then Ok { t with schema_version = current_schema_version }
  else if
    List.is_empty t.moderator_executions
    && (t.schema_version = 4
        || ((t.schema_version = 3 || (t.schema_version = 2 && List.is_empty t.invocations))
            && List.is_empty t.subscriptions
            && List.is_empty t.deliveries))
  then Ok { t with schema_version = current_schema_version }
  else
    Error
      (Agent_protocol.Error.create
         Migration_required
         ~message:"unsupported session state schema or inconsistent legacy records"
         ~retryable:false
         ())
;;

let ledger_error error =
  Agent_protocol.Error.invalid_request
    (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error))
;;

let fresh_inference_ledger_exn (identity : Identity.t) =
  Inference_ledger.create
    ~session_id:identity.session_id
    ~generation:identity.generation
    ~before_tracking_unknown:false
    ~limits:Inference_ledger.Limits.default
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let create_fields ~(identity : Identity.t) ~spec ~desired ~initial_history =
  let inference_ledger = fresh_inference_ledger_exn identity in
  { schema_version = current_schema_version
  ; identity
  ; spec
  ; lifecycle = { desired; observed = Stopped }
  ; runtime_initialization = Ready
  ; pending_initial_start = false
  ; stop_epoch = 0L
  ; parent_stop_epoch = None
  ; conversation =
      { canonical_history = initial_history
      ; deferred_user_entries = []
      ; pending_revision = Agent_protocol.Pending_input.Revision.zero
      ; pending_dispositions = []
      ; initial_prompt_entry_count = List.length initial_history
      ; next_history_sequence = 0L
      ; reserved_history_through = 0L
      ; tasks = []
      ; kv_store = []
      ; compaction_generation = 0
      ; compaction_archives = []
      ; authoring_reference_index = None
      ; authoring_publication = None
      }
  ; active_operation = None
  ; automatic_turn_budget = None
  ; recovery_windows = None
  ; recovery_effective = None
  ; permissions = []
  ; grants = []
  ; jobs = []
  ; inference_ledger
  ; model_job_targets = []
  ; schedules = []
  ; invocations = []
  ; managed_submissions = []
  ; managed_stops = []
  ; moderator_executions = []
  ; run_state = None
  ; goal_catalog = None
  ; goal_accounts = None
  ; delegated_usage = None
  ; child_contributions = None
  ; goal_descendants = None
  ; policy_canonical_usage = None
  ; goal_policy_contributions = None
  ; instruction_selection = None
  ; recovery_notes = None
  ; recovery_note_shares = None
  ; recurrence_catalog = None
  ; subscriptions = []
  ; deliveries = []
  ; ingress_registrations = []
  ; attachments = []
  ; moderator = None
  ; shell = Session.Shell_state.empty
  ; halted = false
  ; halt_reason = None
  ; failure = None
  ; counters =
      { revision = 0L
      ; event_sequence = 0L
      ; transaction_sequence = 0L
      ; owner_lease_generation = 0L
      }
  }
;;

let create ~(identity : Identity.t) ~(spec : Spec.t) ~initial_history =
  let spec =
    { spec with
      Spec.protocol =
        { spec.protocol with
          display_name = identity.Identity.display_name
        ; labels = identity.labels
        }
    }
  in
  let desired =
    if spec.Spec.protocol.start_immediately
    then Agent_protocol.Session.Running
    else Stopped
  in
  create_fields ~identity ~spec ~desired ~initial_history
;;

let authoring_references t =
  let module R = Chat_response.Authoring_reference_index in
  let scope =
    Chat_response.Authoring_materialization.session_scope
      ~session_id:t.identity.session_id
      ~generation:t.identity.generation
  in
  match t.conversation.authoring_reference_index with
  | None -> R.empty ~scope ()
  | Some json -> R.of_json ~scope json
;;

let nonnegative name value =
  if Int64.(value >= 0L)
  then Ok ()
  else
    Error
      (Agent_protocol.Error.create
         Journal_corrupt
         ~message:(name ^ " is negative")
         ~retryable:false
         ())
;;

let validate_delegation (t : t) =
  let module D = Agent_store.Delegation_store in
  let invalid () =
    Error
      (Agent_protocol.Error.create
         Journal_corrupt
         ~message:"generated session identity or delegation reference is inconsistent"
         ~retryable:false
         ())
  in
  match t.spec.protocol.prompt, t.spec.delegation with
  | (Catalog _ | Local_path _), None -> Ok ()
  | Generated revision, Some reference ->
    let open Result.Let_syntax in
    let%bind () =
      D.validate_reference reference
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    (match
       Option.is_none t.spec.prompt_definition_id
       && Agent_protocol.Session.equal_persistence t.spec.protocol.persistence Durable
       && Agent_protocol.Id.Prompt_revision.equal revision t.spec.prompt_revision_id
       && Agent_protocol.Id.Prompt_revision.equal revision reference.revision_id
       && Agent_protocol.Id.Session.equal t.identity.session_id reference.child_session_id
     with
     | true -> Ok ()
     | false -> invalid ())
  | _ -> invalid ()
;;

let validate_model_job_targets (t : t) =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Agent_protocol.Id.Job) in
  let%bind () =
    List.fold_result t.model_job_targets ~init:() ~f:(fun () binding ->
      let id = Model_job_target.job_id binding in
      let%bind job =
        List.find t.jobs ~f:(fun job -> Agent_protocol.Id.Job.equal job.id id)
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.invalid_request
                  "inference target references an absent job")
      in
      match job.kind with
      | Model_call
        when Int.equal job.generation (Model_job_target.generation binding)
             && Agent_protocol.Id.Session.equal job.session_id t.identity.session_id
             && not (Hash_set.mem seen id) ->
        Hash_set.add seen id;
        Ok ()
      | Model_call
      | Nested_agent
      | Scheduled_event
      | Async_tool
      | Shell_process
      | Compaction ->
        Error
          (Agent_protocol.Error.invalid_request
             "model job target kind, owner, generation or uniqueness is invalid"))
  in
  List.fold_result t.jobs ~init:() ~f:(fun () job ->
    match job.kind with
    | Model_call when not (Hash_set.mem seen job.id) ->
      Error
        (Agent_protocol.Error.invalid_request
           "model job lacks an explicit inference selection")
    | Model_call
    | Nested_agent
    | Scheduled_event
    | Async_tool
    | Shell_process
    | Compaction -> Ok ())
;;

let validate_domain (t : t) ~recovery_revision =
  let open Result.Let_syntax in
  let%bind () =
    Option.value_map t.recovery_notes ~default:(Ok ()) ~f:(fun catalog ->
      Recovery_note_session.validate catalog ~session_id:t.identity.session_id)
  in
  let%bind () =
    match t.recovery_note_shares with
    | None -> Ok ()
    | Some shares ->
      let owner = Recovery_note_share.owner shares in
      if
        Agent_protocol.Id.Session.equal
          (Agent_protocol.Session_ref.session_id owner)
          t.identity.session_id
      then Recovery_note_session.check_owner t.recovery_notes ~owner
      else
        Error
          (Agent_protocol.Error.invalid_request
             "recovery note shares belong to another session")
  in
  let%bind () =
    if Int64.(t.spec.configuration_revision < 0L)
    then Error (Agent_protocol.Error.invalid_request "negative configuration revision")
    else Ok ()
  in
  let%bind () =
    List.fold_result t.conversation.deferred_user_entries ~init:() ~f:(fun () document ->
      if
        Int.equal
          (Agent_protocol.Pending_input.generation
             (Pending_input_document.value document))
          t.identity.generation
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "pending input generation differs from session"))
  in
  let disposition_ids = Hash_set.create (module Agent_protocol.History.Id) in
  let%bind () =
    List.fold_result t.conversation.pending_dispositions ~init:() ~f:(fun () document ->
      let value = Pending_disposition_document.value document in
      let id = Pending_disposition.history_id value in
      if
        Pending_disposition.generation value > t.identity.generation
        || Agent_protocol.Pending_input.Revision.compare
             (Pending_disposition.pending_revision value)
             t.conversation.pending_revision
           > 0
        || Hash_set.mem disposition_ids id
        || List.exists t.conversation.deferred_user_entries ~f:(fun document ->
          Agent_protocol.History.Id.equal id (Pending_input_document.entry document).id)
      then
        Error
          (Agent_protocol.Error.invalid_request
             "invalid pending disposition identity, generation or revision")
      else (
        Hash_set.add disposition_ids id;
        Ok ()))
  in
  let%bind () = validate_delegation t in
  let%bind () = validate_model_job_targets t in
  let%bind moderator = Moderator_checkpoint.decode t.moderator in
  let%bind () =
    match moderator with
    | None -> Ok ()
    | Some snapshot ->
      Session.Moderator_state.Identity_snapshot.validate_history_ids
        snapshot
        ~history_ids:
          (List.map
             (t.conversation.canonical_history
              @ List.map
                  t.conversation.deferred_user_entries
                  ~f:Pending_input_document.entry)
             ~f:(fun entry -> entry.Agent_protocol.History.id))
      |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let%bind () =
    match
      List.find_a_dup
        (t.conversation.canonical_history
         @ List.map t.conversation.deferred_user_entries ~f:Pending_input_document.entry)
        ~compare:(fun a b ->
          Agent_protocol.History.Id.compare a.Agent_protocol.History.id b.id)
    with
    | None -> Ok ()
    | Some _ -> Error (Agent_protocol.Error.invalid_request "duplicate history identity")
  in
  let%bind retained_history =
    Invocation_history.Validated_history.create t.conversation.canonical_history
  in
  let%bind () =
    let%bind deferred =
      History_codec.all_of_protocol
        (List.map t.conversation.deferred_user_entries ~f:Pending_input_document.entry)
    in
    let entries =
      Invocation_history.Validated_history.entries retained_history @ deferred
    in
    let%bind () =
      History_entry.validate_relations entries
      |> Result.map_error ~f:Agent_protocol.Error.invalid_request
    in
    let conversation = t.conversation in
    let%bind () =
      nonnegative "next history sequence" conversation.next_history_sequence
    in
    let%bind () =
      nonnegative "reserved history sequence" conversation.reserved_history_through
    in
    let%bind () =
      if
        Int64.(
          conversation.reserved_history_through <= conversation.next_history_sequence)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "history reservation exceeds the allocator high-water mark")
    in
    let namespace = Agent_protocol.Id.Session.to_string t.identity.session_id in
    let check_id id =
      if
        String.equal (History_entry.Id.namespace id) namespace
        && Int64.(
             of_int (History_entry.Id.sequence id) >= conversation.next_history_sequence)
      then
        Error
          (Agent_protocol.Error.invalid_request
             "retained history identity is not below the allocator high-water mark")
      else Ok ()
    in
    let%bind () =
      List.fold_result entries ~init:() ~f:(fun () entry ->
        check_id (History_entry.id entry))
    in
    match moderator with
    | None -> Ok ()
    | Some snapshot ->
      let module M = Session.Moderator_state.Identity_snapshot in
      let ids =
        List.map (snapshot.prepended_items @ snapshot.appended_items) ~f:(fun item ->
          item.M.Inserted.entry_id)
        @ List.map snapshot.replacements ~f:(fun item -> item.M.Replacement.target_id)
        @ List.map snapshot.tombstones ~f:(fun item -> item.M.Tombstone.target_id)
      in
      List.fold_result ids ~init:() ~f:(fun () id -> check_id id)
  in
  let%bind () =
    match t.automatic_turn_budget with
    | None -> Ok ()
    | Some budget -> Automatic_turn_budget.validate budget
  in
  let%bind () =
    match t.recovery_windows with
    | None -> Ok ()
    | Some ledger ->
      let summaries = Hashtbl.create (module Agent_protocol.Id.Operation) in
      List.iter t.conversation.compaction_archives ~f:(fun archive ->
        if Compaction_archive.equal_kind archive.kind Compaction
        then Hashtbl.add_multi summaries ~key:archive.operation_id ~data:archive);
      let summary_reference operation ~f =
        match Hashtbl.find summaries operation with
        | None -> false
        | Some references -> List.exists references ~f
      in
      let has_summary_archive (source : Recovery_window_ledger.Source.archive) =
        summary_reference source.operation ~f:(fun archive ->
          Int64.equal archive.revision source.basis.revision
          && String.equal archive.sha256 (Chatml_source.Digest.to_hex source.digest))
      in
      let has_summary_operation operation =
        summary_reference operation ~f:(fun _ -> true)
      in
      Recovery_window_ledger.validate_state
        ledger
        ~session_id:t.identity.session_id
        ~generation:t.identity.generation
        ~revision:recovery_revision
        ~history:t.conversation.canonical_history
        ~has_summary_archive
        ~has_summary_operation
  in
  let%bind () =
    Option.value_map t.recovery_effective ~default:(Ok ()) ~f:(fun ledger ->
      Recovery_effective_ledger.validate_state
        ledger
        ~windows:t.recovery_windows
        ~publication_revision:recovery_revision)
  in
  let seen_invocations = Hash_set.create (module Agent_protocol.Id.Invocation) in
  let%bind () =
    List.fold_result t.invocations ~init:() ~f:(fun () invocation ->
      let%bind () = Agent_protocol.Invocation.validate invocation in
      let%bind () =
        Extension_invariants.invocation_event_owner
          ~events:t.moderator_executions
          invocation
      in
      let%bind () =
        Invocation_history.Validated_history.validate_retained retained_history invocation
      in
      let context = invocation.context in
      if
        Agent_protocol.Id.Session.compare context.session_id t.identity.session_id <> 0
        || context.generation > t.identity.generation
        || Hash_set.mem seen_invocations context.id
      then
        Error
          (Agent_protocol.Error.create
             Journal_corrupt
             ~message:"invocation owner, generation or uniqueness is invalid"
             ~retryable:false
             ())
      else (
        Hash_set.add seen_invocations context.id;
        Ok ()))
  in
  let%bind () =
    let seen = Hash_set.create (module Agent_protocol.Id.Moderator_execution) in
    let running = ref false in
    List.fold_result t.moderator_executions ~init:() ~f:(fun () execution ->
      let module E = Agent_protocol.Moderator_execution in
      let%bind () = E.validate execution in
      let c = execution.E.context in
      let%bind () =
        match c.job with
        | None -> Ok ()
        | Some _ when c.generation < t.identity.generation -> Ok ()
        | Some reference ->
          (match
             List.find t.jobs ~f:(fun job ->
               Agent_protocol.Id.Job.equal job.id reference.job_id)
           with
           | Some job
             when job.generation = c.generation
                  && Agent_protocol.Id.Session.equal job.session_id c.session_id
                  && reference.attempt <= job.attempt -> Ok ()
           | _ ->
             Error
               (Agent_protocol.Error.create
                  Journal_corrupt
                  ~message:"moderator event references an unknown or newer job attempt"
                  ~retryable:false
                  ()))
      in
      if
        (not (Agent_protocol.Id.Session.equal c.session_id t.identity.session_id))
        || c.generation > t.identity.generation
        || Hash_set.mem seen c.id
      then
        Error
          (Agent_protocol.Error.create
             Journal_corrupt
             ~message:"invalid moderator execution ownership or duplicate identity"
             ~retryable:false
             ())
      else (
        Hash_set.add seen c.id;
        match execution.status with
        | Running when !running ->
          Error
            (Agent_protocol.Error.create
               Journal_corrupt
               ~message:"multiple running moderator event executions"
               ~retryable:false
               ())
        | Running ->
          running := true;
          Ok ()
        | Completed _ | Failed _ | Interrupted _ -> Ok ()))
  in
  let%bind () =
    Extension_invariants.validate
      ~session_id:t.identity.session_id
      ~generation:t.identity.generation
      ~invocations:t.invocations
      ~subscriptions:t.subscriptions
      ~deliveries:t.deliveries
      ~jobs:t.jobs
      ~schedules:t.schedules
      ~events:t.moderator_executions
  in
  let%bind () =
    List.fold_result t.subscriptions ~init:() ~f:(fun () subscription ->
      Job_launch.validate_subscription
        ~invocations:t.invocations
        ~events:t.moderator_executions
        ~jobs:t.jobs
        subscription)
  in
  let%bind () =
    let registrations = t.ingress_registrations in
    let duplicate =
      List.contains_dup registrations ~compare:(fun a b ->
        Agent_protocol.Id.Capability.compare a.External_ingress.context.id b.context.id)
      || List.contains_dup
           (List.concat_map registrations ~f:(fun value ->
              value.External_ingress.receipts))
           ~compare:(fun a b -> Agent_protocol.Id.Ingress_event.compare a.id b.id)
    in
    match duplicate with
    | true ->
      Error (Agent_protocol.Error.invalid_request "duplicate external ingress identity")
    | false ->
      List.fold_result registrations ~init:() ~f:(fun () value ->
        let%bind () = External_ingress.validate value in
        let%bind () =
          match
            Agent_protocol.Id.Session.equal value.context.session_id t.identity.session_id
            && value.context.generation <= t.identity.generation
          with
          | true -> Ok ()
          | false ->
            Error (Agent_protocol.Error.invalid_request "foreign external ingress owner")
        in
        let%bind subscription =
          List.find t.subscriptions ~f:(fun subscription ->
            Agent_protocol.Id.Subscription.equal
              subscription.context.id
              value.context.subscription_id)
          |> Result.of_option
               ~error:
                 (Agent_protocol.Error.invalid_request
                    "missing external ingress subscription")
        in
        External_ingress.validate_owner value subscription)
  in
  let%bind () =
    List.fold_result t.schedules ~init:() ~f:(fun () schedule ->
      Schedule_ownership.validate
        ~invocations:t.invocations
        ~events:t.moderator_executions
        ~subscriptions:t.subscriptions
        schedule)
  in
  let%bind () =
    List.fold_result t.jobs ~init:() ~f:(fun () job ->
      let%bind () = Agent_protocol.Job.validate_result job in
      let%bind () =
        match job.Agent_protocol.Job.progress with
        | None -> Ok ()
        | Some _ ->
          Error
            (Agent_protocol.Error.create
               Journal_corrupt
               ~message:"transient job progress cannot be durable state"
               ~retryable:false
               ())
      in
      Job_launch.validate
        ~invocations:t.invocations
        ~events:t.moderator_executions
        ~jobs:t.jobs
        job)
  in
  let%bind () =
    List.fold_result t.deliveries ~init:() ~f:(fun () delivery ->
      Delivery_ownership.validate
        ~invocations:t.invocations
        ~events:t.moderator_executions
        ~subscriptions:t.subscriptions
        delivery)
  in
  let%bind () =
    List.fold_result t.jobs ~init:() ~f:(fun () job ->
      Job_dependency.validate
        ~invocations:t.invocations
        ~events:t.moderator_executions
        ~jobs:t.jobs
        ~subscriptions:t.subscriptions
        job)
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.create
        Journal_corrupt
        ~message:error.message
        ~retryable:false
        ())
  in
  let%bind () =
    List.fold_result t.permissions ~init:() ~f:(fun () permission ->
      match permission.owner with
      | Operation _ -> Ok ()
      | Invocation id ->
        (match
           List.find t.invocations ~f:(fun invocation ->
             Agent_protocol.Id.Invocation.equal invocation.context.id id)
         with
         | Some invocation
           when Agent_protocol.Id.Session.equal
                  permission.session_id
                  t.identity.session_id
                && permission.generation = invocation.context.generation -> Ok ()
         | _ ->
           Error
             (Agent_protocol.Error.create
                Journal_corrupt
                ~message:"permission invocation owner is missing or inconsistent"
                ~retryable:false
                ())))
  in
  let%bind () = nonnegative "revision" t.counters.revision in
  let%bind () = nonnegative "stop epoch" t.stop_epoch in
  let%bind () =
    match t.parent_stop_epoch, t.spec.delegation with
    | None, _ -> Ok ()
    | Some epoch, Some _ -> nonnegative "parent stop epoch" epoch
    | Some _, None ->
      Error
        (Agent_protocol.Error.invalid_request
           "parent stop acknowledgement requires delegation")
  in
  let%bind () =
    match
      ( t.pending_initial_start
      , t.spec.delegation
      , t.spec.protocol.start_immediately
      , t.lifecycle.desired
      , t.active_operation )
    with
    | false, _, _, _, _ | true, Some _, true, Stopped, None -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request "invalid generated initial start intent")
  in
  let%bind () = nonnegative "event sequence" t.counters.event_sequence in
  let%bind _ =
    List.fold_result t.managed_submissions ~init:[] ~f:(fun seen receipt ->
      let%bind () = Managed_submission.validate receipt in
      let current =
        match receipt.status with
        | Terminal _ -> true
        | Deferred | Ready | Assigned _ ->
          Int.equal receipt.generation t.identity.generation
          && Option.exists
               t.spec.delegation
               ~f:(Agent_store.Delegation_store.Reference.equal receipt.reference)
      in
      let correlation =
        match receipt.status with
        | Assigned id ->
          Option.exists t.active_operation ~f:(fun operation ->
            Agent_protocol.Id.Operation.equal id operation.id
            && Int.equal receipt.generation operation.generation
            &&
            match operation.kind with
            | Turn _ -> true
            | Compaction -> false)
        | Deferred ->
          List.exists t.conversation.deferred_user_entries ~f:(fun document ->
            Agent_protocol.History.Id.equal
              (Pending_input_document.entry document).id
              receipt.history_id)
        | Ready | Terminal _ -> true
      in
      match
        current
        && correlation
        && receipt.generation <= t.identity.generation
        && Agent_protocol.Id.Session.equal
             receipt.reference.child_session_id
             t.identity.session_id
        && not
             (List.exists seen ~f:(fun previous ->
                Managed_submission.same_key previous receipt
                || Agent_protocol.History.Id.equal previous.history_id receipt.history_id))
      with
      | true -> Ok (receipt :: seen)
      | false ->
        Error
          (Agent_protocol.Error.invalid_request
             "managed submission identity/generation is inconsistent"))
  in
  let%bind () = nonnegative "transaction sequence" t.counters.transaction_sequence in
  let%bind _ =
    List.fold_result t.managed_stops ~init:[] ~f:(fun seen receipt ->
      let%bind () = Managed_stop.validate receipt in
      match
        receipt.generation <= t.identity.generation
        && Int64.(receipt.stop_epoch <= t.stop_epoch)
        && Agent_protocol.Id.Session.equal
             receipt.reference.child_session_id
             t.identity.session_id
        && not
             (List.exists seen ~f:(fun previous ->
                Managed_stop.same_key previous receipt
                || Agent_protocol.Id.Transaction.equal previous.id receipt.id))
      with
      | true -> Ok (receipt :: seen)
      | false ->
        Error
          (Agent_protocol.Error.invalid_request
             "managed stop identity/generation is inconsistent"))
  in
  let%bind references = authoring_references t in
  let%bind () =
    match t.conversation.authoring_publication with
    | None -> Ok ()
    | Some context ->
      Chat_response.Authoring_publication.validate_context
        context
        ~session_id:t.identity.session_id
        ~generation:t.identity.generation
  in
  let%bind _ =
    Chat_response.Authoring_presence.remember
      ~previous:(Chat_response.Authoring_reference_index.receipts references)
      ~history:t.conversation.canonical_history
  in
  if t.schema_version <> current_schema_version
  then
    Error
      (Agent_protocol.Error.create
         Journal_corrupt
         ~message:"unsupported session state schema"
         ~retryable:false
         ())
  else if t.identity.generation < 0
  then
    Error
      (Agent_protocol.Error.create
         Journal_corrupt
         ~message:"negative session generation"
         ~retryable:false
         ())
  else Ok ()
;;

let queue_provenance (t : t) : Queue_provenance_validation.t =
  { session_id = t.identity.session_id
  ; generation = t.identity.generation
  ; revision = t.counters.revision
  ; moderator = t.moderator
  ; runs = t.run_state
  ; jobs = t.jobs
  ; schedules = t.schedules
  ; ingress_registrations = t.ingress_registrations
  ; invocations = t.invocations
  ; events = t.moderator_executions
  }
;;

module Native_validation = struct
  type state = t
  type t = state

  let create (t : state) =
    let open Result.Let_syntax in
    let%bind () = Queue_provenance_validation.validate (queue_provenance t) in
    let%bind () = Job_inference_provenance.validate ~jobs:t.jobs ~runs:t.run_state in
    let%bind () =
      Invocation_inference_provenance.validate
        ~invocations:t.invocations
        ~jobs:t.jobs
        ~events:t.moderator_executions
        ~runs:t.run_state
    in
    let%bind () = Goal_run_evidence.validate ~catalog:t.goal_catalog ~runs:t.run_state in
    let%bind () =
      match t.goal_accounts with
      | None -> Ok ()
      | Some accounts ->
        Goal_accounts.validate
          accounts
          ~catalog:t.goal_catalog
          ~ledger:t.inference_ledger
          ~runs:t.run_state
    in
    let%bind () =
      match t.delegated_usage, t.spec.delegation with
      | None, (None | Some _) -> Ok ()
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request
             "own delegated usage requires its actual delegation reference")
      | Some own, Some reference ->
        if
          Agent_store.Delegation_store.Reference.equal
            reference
            (Delegated_usage.reference own)
        then Delegated_usage.validate own ~ledger:t.inference_ledger
        else
          Error
            (Agent_protocol.Error.invalid_request
               "delegated usage admission identity differs")
    in
    let%bind () =
      match t.child_contributions with
      | None -> Ok ()
      | Some children ->
        let%bind _ = Child_contributions.to_jsonaf children in
        List.fold_result
          (Child_contributions.references children)
          ~init:()
          ~f:(fun () reference ->
            if
              Agent_protocol.Id.Session.equal
                reference.Agent_store.Delegation_store.Reference.key.parent_session_id
                t.identity.session_id
              && not
                   (Agent_protocol.Id.Session.equal
                      reference.child_session_id
                      t.identity.session_id)
            then Ok ()
            else
              Error
                (Agent_protocol.Error.invalid_request
                   "child contribution belongs to another parent"))
    in
    let%bind () =
      match t.goal_descendants with
      | None ->
        if Option.exists t.child_contributions ~f:Child_contributions.has_goal_origins
        then
          Error
            (Agent_protocol.Error.invalid_request
               "scoped child contributions require their descendant accounting owner")
        else Ok ()
      | Some descendants ->
        Goal_descendants.validate_context
          descendants
          ~session_id:t.identity.session_id
          ~ledger:t.inference_ledger
          ~children:t.child_contributions
          ~catalog:t.goal_catalog
          ~accounts:t.goal_accounts
          ~delegation:t.spec.delegation
    in
    let%bind () =
      match t.policy_canonical_usage with
      | None -> Ok ()
      | Some accounting ->
        let%bind context = policy_context t in
        Policy_canonical_usage.validate accounting ~context ~ledger:t.inference_ledger
    in
    let%bind () =
      match t.instruction_selection with
      | None -> Ok ()
      | Some selection ->
        let summary = Instruction_selection.summary selection in
        if
          (summary.project_selected || summary.skills > 0)
          && Int64.(
               t.counters.revision >= max_value
               || t.counters.event_sequence >= max_value
               || t.counters.transaction_sequence >= max_value)
        then
          Error
            (Agent_protocol.Error.create
               Resource_limit
               ~message:"active instructions require Clear counter headroom"
               ~retryable:false
               ())
        else
          Instruction_selection.validate
            selection
            ~session_id:t.identity.session_id
            ~generation:t.identity.generation
    in
    let%bind () =
      match t.goal_catalog with
      | None -> Ok ()
      | Some catalog ->
        Goal_catalog.validate
          catalog
          ~session_id:t.identity.session_id
          ~generation:t.identity.generation
    in
    let%bind () =
      match t.recurrence_catalog with
      | None -> Ok ()
      | Some catalog ->
        Recurrence_catalog.validate
          catalog
          ~session_id:t.identity.session_id
          ~generation:t.identity.generation
    in
    let%bind () =
      match t.run_state with
      | None -> Ok ()
      | Some runs ->
        Run_state.validate
          runs
          ~session_id:t.identity.session_id
          ~generation:t.identity.generation
    in
    let%map () = validate_moderator_lifecycle t in
    t
  ;;

  let validate_at (t : t) ~policy_revision ~recovery_revision =
    let open Result.Let_syntax in
    let%bind () = validate_goal_policy t ~session_revision:policy_revision in
    let%bind () = validate_domain t ~recovery_revision in
    let instruction_clear =
      Option.value_map t.instruction_selection ~default:0 ~f:(fun selection ->
        let summary = Instruction_selection.summary selection in
        if summary.project_selected || summary.skills > 0 then 1 else 0)
    in
    let%bind recurrence_steps =
      match t.recurrence_catalog with
      | None -> Ok 0
      | Some catalog ->
        Result.map
          (Recurrence_catalog.reservation catalog)
          ~f:Recurrence_catalog.Reservation.required_transition_steps
    in
    let additional_steps = instruction_clear + recurrence_steps in
    let%bind () =
      Session_terminal_capacity.validate
        ~index:t.run_state
        ~jobs:t.jobs
        ~permissions:t.permissions
        ~schedules:t.schedules
        ~additional_steps
        ~additional_events:instruction_clear
        ~revision:t.counters.revision
        ~transaction_sequence:t.counters.transaction_sequence
        ~event_sequence:t.counters.event_sequence
    in
    let%bind identity_metadata =
      Agent_protocol.Session_metadata.Values.create
        ~display_name:t.identity.display_name
        ~labels:t.identity.labels
    in
    let%bind spec_metadata =
      Agent_protocol.Session_metadata.Values.create
        ~display_name:t.spec.protocol.display_name
        ~labels:t.spec.protocol.labels
    in
    let%bind () =
      if
        Int64.(t.identity.metadata_revision < zero)
        || not
             (Agent_protocol.Session_metadata.Values.equal
                identity_metadata
                spec_metadata)
      then
        Error
          (Agent_protocol.Error.invalid_request
             "session metadata mirrors or revision are inconsistent")
      else Ok ()
    in
    Inference_ledger.validate
      t.inference_ledger
      ~limits:Inference_ledger.Limits.default
      ~session_id:t.identity.session_id
      ~generation:t.identity.generation
    |> Result.map_error ~f:ledger_error
  ;;

  let validate_remaining t ~policy_revision =
    validate_at t ~policy_revision ~recovery_revision:t.counters.revision
  ;;

  let validate_ordered_prefix t ~policy_revision =
    let revision = t.counters.revision in
    if Option.is_none t.recovery_windows
    then validate_remaining t ~policy_revision
    else if Int64.equal revision Int64.max_value
    then
      Error
        (Agent_protocol.Error.invalid_request
           "recovery window publication revision exhausted")
    else validate_at t ~policy_revision ~recovery_revision:(Int64.succ revision)
  ;;
end

let validate (t : t) =
  let%bind.Result checkpoint = Native_validation.create t in
  Native_validation.validate_remaining checkpoint ~policy_revision:t.counters.revision
;;

let validate_administration_candidate t ~previous =
  let open Result.Let_syntax in
  let%bind () = validate previous in
  let%bind () = validate_domain t ~recovery_revision:t.counters.revision in
  let%bind before =
    Inference_ledger.to_document previous.inference_ledger
    |> Result.map_error ~f:ledger_error
  in
  let%bind after =
    Inference_ledger.to_document t.inference_ledger |> Result.map_error ~f:ledger_error
  in
  if
    Agent_protocol.Id.Session.equal t.identity.session_id previous.identity.session_id
    && String.equal
         (Jsonaf.to_string (Document_schema.Document.json before))
         (Jsonaf.to_string (Document_schema.Document.json after))
  then Ok ()
  else
    Error
      (Agent_protocol.Error.invalid_request
         "administration must retain the original inference ledger")
;;

let summary_fields (t : 'workspace Spec.fields fields) ~workspace =
  Agent_protocol.Session.
    { id = t.identity.session_id
    ; creator = t.identity.creating_principal
    ; created_at = t.identity.created_at
    ; updated_at = t.identity.updated_at
    ; generation = t.identity.generation
    ; spec = t.spec.protocol
    ; desired_state = t.lifecycle.desired
    ; observed_state = t.lifecycle.observed
    ; prompt_revision = Some t.spec.prompt_revision_id
    ; workspace_instance = Some (workspace t.spec.workspace_instance)
    ; active_operation = t.active_operation
    ; revision = t.counters.revision
    ; metadata_revision = t.identity.metadata_revision
    ; organization = t.identity.organization
    ; inference_summary = Value (Inference_ledger.summary t.inference_ledger)
    ; latest_event_sequence = t.counters.event_sequence
    }
;;

let summary t =
  summary_fields t ~workspace:(fun workspace -> workspace.Workspace_instance.id)
;;

let history_window entries =
  Agent_protocol.History.Window.
    { entries
    ; previous_cursor = None
    ; next_cursor = None
    ; reached_start = true
    ; reached_end = true
    ; structurally_complete = true
    }
;;

let effective_entry ~canonical (entry : Chat_response.Moderation.Effective_entry.t) =
  match entry.provenance with
  | Canonical -> canonical entry.entry
  | Moderator_inserted _ ->
    History_codec.to_protocol ~provenance:Moderator_inserted entry.entry
  | Moderator_replacement { target_id; _ } ->
    { (canonical entry.entry) with provenance = Moderator_replaced target_id }
;;

let effective_history t =
  match Moderator_checkpoint.decode t.moderator with
  | Error error -> failwith error.Agent_protocol.Error.message
  | Ok None -> None
  | Ok (Some snapshot) ->
    let history =
      History_codec.all_of_protocol t.conversation.canonical_history
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
      |> Result.ok_or_failwith
    in
    Chat_response.Moderator_manager.effective_entries_of_snapshot snapshot history
    |> Result.ok_or_failwith
    |> List.map
         ~f:
           (effective_entry
              ~canonical:
                (History_codec.canonical_encoder
                   ~previous:t.conversation.canonical_history))
    |> history_window
    |> Option.some
;;

let moderator_projection t =
  `Object
    ([ ("halted", if t.halted then `True else `False) ]
     @ Option.to_list
         (Option.map (effective_history t) ~f:(fun history ->
            "effective_history", Agent_protocol.History.Window.to_json history))
     @ Option.to_list
         (Option.map t.halt_reason ~f:(fun reason -> "halt_reason", `String reason)))
;;

let extension_status t =
  List.map t.invocations ~f:Agent_protocol.Extension_status.invocation
  @ List.map t.moderator_executions ~f:Agent_protocol.Extension_status.moderator_execution
  @ List.map t.subscriptions ~f:Agent_protocol.Extension_status.subscription
  @ List.map t.deliveries ~f:Agent_protocol.Extension_status.delivery
  |> List.sort ~compare:(fun a b ->
    String.compare a.Agent_protocol.Extension_status.id b.id)
;;

let snapshot ~now t =
  let grants =
    Security_grant.list
      ~now
      ~session_id:t.identity.session_id
      ~creating_principal:t.identity.creating_principal
      ~generic:t.grants
      ~shell:t.shell
  in
  Agent_protocol.Snapshot.
    { session = summary t
    ; lifecycle = None
    ; canonical_history = history_window t.conversation.canonical_history
    ; archived_revisions =
        List.map t.conversation.compaction_archives ~f:(fun archive -> archive.revision)
    ; effective_history = effective_history t
    ; deferred_entries =
        List.map t.conversation.deferred_user_entries ~f:Pending_input_document.entry
    ; permissions = t.permissions
    ; grants
    ; jobs = t.jobs
    ; instruction_status =
        Some
          (Instruction_selection.summary
             (Option.value t.instruction_selection ~default:Instruction_selection.empty))
    ; extension_status = extension_status t
    ; schedules = t.schedules
    ; goals = Option.value_map t.goal_catalog ~default:[] ~f:Goal_catalog.goals
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = t.halted
    ; halt_reason = t.halt_reason
    ; failure = t.failure
    ; revision = t.counters.revision
    ; latest_event_sequence = t.counters.event_sequence
    }
;;
