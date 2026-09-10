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
    }
  [@@deriving sexp]
end

module Spec = struct
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

module Compaction_archive = struct
  type kind =
    | Compaction
    | Reset
    | Rebuild
    | Upgrade
  [@@deriving equal, sexp]

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

module Lifecycle = struct
  type t =
    { desired : Agent_protocol.Session.desired_state
    ; observed : Agent_protocol.Session.observed_state
    }
  [@@deriving sexp]
end

module Counters = struct
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
  ; moderator_executions : Agent_protocol.Moderator_execution.t list [@sexp.list]
  ; subscriptions : Agent_protocol.Subscription.t list [@sexp.list]
  ; deliveries : Agent_protocol.Delivery.t list [@sexp.list]
  ; attachments : Agent_protocol.Session.Attachment.t list
  ; moderator : Jsonaf.t option
  ; shell : Session.Shell_state.t
  ; halted : bool
  ; halt_reason : string option
  ; failure : Agent_protocol.Error.t option
  ; counters : Counters.t
  }
[@@deriving sexp]

let current_schema_version = 8

let upgrade_schema t =
  if t.schema_version = current_schema_version
  then Ok t
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

let create ~identity ~spec ~initial_history =
  let desired =
    if spec.Spec.protocol.start_immediately
    then Agent_protocol.Session.Running
    else Stopped
  in
  { schema_version = current_schema_version
  ; identity
  ; spec
  ; lifecycle = { desired; observed = Stopped }
  ; conversation =
      { canonical_history = initial_history
      ; deferred_user_entries = []
      ; initial_prompt_entry_count = List.length initial_history
      ; next_history_sequence = 0L
      ; reserved_history_through = 0L
      ; tasks = []
      ; kv_store = []
      ; compaction_generation = 0
      ; compaction_archives = []
      }
  ; active_operation = None
  ; permissions = []
  ; grants = []
  ; jobs = []
  ; schedules = []
  ; invocations = []
  ; moderator_executions = []
  ; subscriptions = []
  ; deliveries = []
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

let validate t =
  let open Result.Let_syntax in
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
        Invocation_history.validate_retained
          ~history:t.conversation.canonical_history
          invocation
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
  let%bind () = nonnegative "event sequence" t.counters.event_sequence in
  let%bind () = nonnegative "transaction sequence" t.counters.transaction_sequence in
  let%bind () =
    nonnegative "next history sequence" t.conversation.next_history_sequence
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

let summary t =
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
    ; workspace_instance = Some t.spec.workspace_instance.id
    ; active_operation = t.active_operation
    ; revision = t.counters.revision
    ; latest_event_sequence = t.counters.event_sequence
    }
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
    History_codec.to_protocol ~provenance:(Moderator_replaced target_id) entry.entry
;;

let effective_history t =
  Option.bind t.moderator ~f:(fun json ->
    match Jsonaf.member "identity_snapshot_sexp" json with
    | Some (`String encoded) ->
      let snapshot =
        Session.Moderator_state.Identity_snapshot.t_of_sexp (Sexp.of_string encoded)
      in
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
    | _ -> None)
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
    ; canonical_history = history_window t.conversation.canonical_history
    ; archived_revisions =
        List.map t.conversation.compaction_archives ~f:(fun archive -> archive.revision)
    ; effective_history = effective_history t
    ; deferred_entries = t.conversation.deferred_user_entries
    ; permissions = t.permissions
    ; grants
    ; jobs = t.jobs
    ; extension_status = extension_status t
    ; schedules = t.schedules
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = t.halted
    ; halt_reason = t.halt_reason
    ; failure = t.failure
    ; revision = t.counters.revision
    ; latest_event_sequence = t.counters.event_sequence
    }
;;
