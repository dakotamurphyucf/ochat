open! Core

(** Bounded failure-only fixture progress. Observations retain compact metadata,
    never State/history/ledger/payloads. Reporting performs no actor or transport call. *)
module Failure_diagnostic : sig
  type t

  val create : now:(unit -> float) -> t
  val reset : t -> unit
  val mark : t -> string -> unit
  val observe : t -> Agent_session.Session_state.t -> unit
  val report : t -> context:Sexp.t -> unit
  val operation_status : Agent_protocol.Operation.state -> string
  val observed_status : Agent_protocol.Session.observed_state -> string
end = struct
  module P = Agent_protocol

  type t =
    { now : unit -> float
    ; mutable started_wall : float
    ; mutable started_cpu : float
    ; mutable phases : (string * float * float) list
    ; mutable states : (P.Id.Session.t * Sexp.t) list
    }

  let create ~now =
    { now
    ; started_wall = now ()
    ; started_cpu = Stdlib.Sys.time ()
    ; phases = []
    ; states = []
    }
  ;;

  let reset t =
    t.started_wall <- t.now ();
    t.started_cpu <- Stdlib.Sys.time ();
    t.phases <- [];
    t.states <- []
  ;;

  let elapsed t = t.now () -. t.started_wall, Stdlib.Sys.time () -. t.started_cpu

  let mark t phase =
    let wall, cpu = elapsed t in
    t.phases <- List.take ((String.prefix phase 256, wall, cpu) :: t.phases) 16
  ;;

  let outcome = function
    | P.Invocation.Complete _ -> "complete"
    | Pending _ -> "pending"
    | Fail error -> "fail:" ^ String.prefix error.code 128
    | Cancelled _ -> "cancelled"
  ;;

  let invocation_status = function
    | P.Invocation.Admitted -> "admitted"
    | Dispatching -> "dispatching"
    | Resolved result -> "resolved:" ^ outcome result
    | Published result -> "published:" ^ outcome result
  ;;

  let operation_status = function
    | P.Operation.Starting -> "starting"
    | Running -> "running"
    | Cancelling -> "cancelling"
    | Completed -> "completed"
    | Failed _ -> "failed"
    | Cancelled -> "cancelled"
    | Interrupted _ -> "interrupted"
  ;;

  let observed_status = function
    | P.Session.Stopped -> "stopped"
    | Queued_for_slot -> "queued_for_slot"
    | Starting -> "starting"
    | Recovering -> "recovering"
    | Idle -> "idle"
    | Running_turn _ -> "running_turn"
    | Compacting _ -> "compacting"
    | Waiting_for_permission _ -> "waiting_for_permission"
    | Stopping -> "stopping"
    | Failed error -> "failed:" ^ Sexp.to_string ([%sexp_of: P.Error.code] error.code)
  ;;

  let latest entries = List.drop entries (Int.max 0 (List.length entries - 12))

  let observe t (state : Agent_session.Session_state.t) =
    let observed_wall, observed_cpu = elapsed t in
    let active =
      Option.map state.active_operation ~f:(fun operation ->
        operation.P.Operation.id, operation_status operation.state)
    in
    let invocations =
      latest state.invocations
      |> List.map ~f:(fun invocation ->
        ( invocation.P.Invocation.context.id
        , String.prefix invocation.context.tool_name 128
        , invocation_status invocation.status ))
    in
    let receipts =
      latest state.managed_submissions
      |> List.map ~f:(fun submission ->
        submission.Agent_session.Managed_submission.history_id, submission.status)
    in
    let metadata =
      [%message
        "cached actor"
          ~session_id:(state.identity.session_id : P.Id.Session.t)
          ~generation:(state.identity.generation : int)
          (observed_wall : float)
          (observed_cpu : float)
          ~revision:(state.counters.revision : int64)
          ~event_sequence:(state.counters.event_sequence : int64)
          ~desired:(state.lifecycle.desired : P.Session.desired_state)
          ~observed:(observed_status state.lifecycle.observed : string)
          (active : (P.Id.Operation.t * string) option)
          ~deferred_count:(List.length state.conversation.deferred_user_entries : int)
          ~permission_count:(List.length state.permissions : int)
          ~invocation_count:(List.length state.invocations : int)
          (invocations : (P.Id.Invocation.t * string * string) list)
          ~receipt_count:(List.length state.managed_submissions : int)
          (receipts : (P.History.Id.t * Agent_session.Managed_submission.status) list)]
    in
    t.states
    <- List.take
         ((state.identity.session_id, metadata)
          :: List.filter t.states ~f:(fun (id, _) ->
            not (P.Id.Session.equal id state.identity.session_id)))
         16
  ;;

  let report t ~context =
    let wall, cpu = elapsed t in
    let phases = List.rev t.phases in
    let states = List.map t.states ~f:snd in
    Eio.traceln
      "authored timeout diagnostic %s"
      (Sexp.to_string_hum
         [%message
           "fixture timeout"
             (context : Sexp.t)
             (wall : float)
             (cpu : float)
             (phases : (string * float * float) list)
             (states : Sexp.t list)])
  ;;
end

(** Explicit synthetic selected backend for offline daemon fixtures. The mock
    owns response completion; observations are intentionally not a durable usage
    ledger in these tests. This fixture initializes its RNG before allocating a
    fresh host namespace, including when constructed before the Eio harness.
    Production compositions must supply actual tracking. *)
let inference_policy ~default_model ~post_stream =
  Mirage_crypto_rng_unix.use_default ();
  let namespace =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let fixture = Inference_fixture.create ~namespace ~default_model ~post_stream in
  Agent_server.Session_factory.
    { capture_inference_target =
        (fun ~prompt_revision_id:_ ~config ->
          Inference_fixture.capture_config fixture config)
    ; recapture_inference_target =
        (fun ~current ~prompt_revision_id:_ ~config ->
          Inference_fixture.recapture_config fixture ~current config)
    ; migrate_inference_target = None
    ; migrate_model_job_target = None
    ; approve_inference_target_change =
        (fun ~current ~proposed ->
          let open Result.Let_syntax in
          let%bind context = Inference_fixture.resolve fixture current in
          Inference_runtime.Context.derive context ~target:proposed
          |> Result.map ~f:ignore)
    ; resolve_inference_context = Inference_fixture.resolve fixture
    ; runtime_inference_ports =
        (fun _ ->
          Ok
            { new_preparation_id = (Inference_fixture.identity fixture).new_preparation_id
            ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
            ; on_attempt = ignore
            ; on_observation = ignore
            ; on_completion = ignore
            })
    }
;;

let delegation_stage payload =
  let document =
    Document_schema.Document.decode ~limits:Document_schema.Limits.default payload
    |> Result.map_error ~f:(fun _ -> "invalid fixture delegation document")
    |> Result.ok_or_failwith
  in
  assert (String.equal (Document_schema.Document.kind document) "delegation.intent");
  assert (Int.equal (Document_schema.Document.version document) 6);
  match
    Document_schema.Json.field (Document_schema.Document.payload document) ~name:"stage"
  with
  | Value (`String "reserved") -> Agent_store.Delegation_store.Reserved
  | Value (`String "artifact_installed") -> Artifact_installed
  | Value (`String "child_installed") -> Child_installed
  | Value (`String "linked") -> Linked
  | Absent | Null | Value _ -> failwith "invalid fixture delegation stage"
;;

(* Keep actual polling/I/O waits while controlling the time observed by durable
   deadline bookkeeping. Resuming excludes time spent paused; it never jumps
   past deadlines merely because fixture work was slow. *)
let controlled_monotonic_clock real_clock =
  let logical_now = ref (Eio.Time.Mono.now real_clock) in
  let last_real = ref !logical_now in
  let paused = ref false in
  (* The fixture owns clock transitions. Sleepers borrow the current promise;
     only pause/resume/advance rotate and complete it, waking every waiter to
     recheck logical time. A paused clock never polls the real clock in a loop. *)
  let changed = ref (Eio.Promise.create ()) in
  let notify_change () =
    let _, resolver = !changed in
    changed := Eio.Promise.create ();
    Eio.Promise.resolve resolver ()
  in
  let now () =
    let actual = Eio.Time.Mono.now real_clock in
    (match !paused with
     | true -> ()
     | false ->
       logical_now
       := Mtime.add_span !logical_now (Mtime.span !last_real actual) |> Option.value_exn);
    last_real := actual;
    !logical_now
  in
  let module Clock = struct
    type t = unit
    type time = Mtime.t

    let now = now

    let rec sleep_until () deadline =
      let current = now () in
      match Mtime.compare deadline current <= 0 with
      | true -> Eio.Fiber.yield ()
      | false ->
        (* Capture this generation before yielding, so a transition cannot be
           lost between observing the state and registering the wait. Real timer
           completion alone never proves a controlled deadline has elapsed. *)
        let change, _ = !changed in
        (match !paused with
         | true -> Eio.Promise.await change
         | false ->
           Eio.Fiber.first
             (fun () -> Eio.Time.Mono.sleep_span real_clock (Mtime.span current deadline))
             (fun () -> Eio.Promise.await change));
        sleep_until () deadline
    ;;
  end
  in
  let pause () =
    ignore (now ());
    paused := true;
    notify_change ()
  in
  let resume () =
    ignore (now ());
    paused := false;
    notify_change ()
  in
  let advance seconds =
    let span = Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) |> Option.value_exn in
    logical_now := Mtime.add_span (now ()) span |> Option.value_exn;
    notify_change ()
  in
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)), pause, resume, advance
;;

(* Logical wall time advances only after a completed wait. The fixture owns
   transitions; sleepers borrow a generation so pauses cancel stale real timers
   and valid completed real waits advance the earliest pending logical deadline. *)
let controlled_wall_clock real_clock ~initial =
  let logical_now = ref initial in
  let paused = ref false in
  let waiters = ref [] in
  let changed = ref (Eio.Promise.create ()) in
  let notify_change () =
    let _, resolver = !changed in
    changed := Eio.Promise.create ();
    Eio.Promise.resolve resolver ()
  in
  let advance_to deadline =
    logical_now := Float.max !logical_now deadline;
    notify_change ()
  in
  let module Clock = struct
    type t = unit
    type time = float

    let now () = !logical_now

    let rec wait deadline =
      match Float.(deadline <= !logical_now) with
      | true -> Eio.Fiber.yield ()
      | false ->
        let change, _ = !changed in
        (match !paused with
         | true -> Eio.Promise.await change
         | false ->
           let completed =
             Eio.Fiber.first
               (fun () ->
                  Eio.Time.sleep real_clock (deadline -. !logical_now);
                  true)
               (fun () ->
                  Eio.Promise.await change;
                  false)
           in
           (* Cancellation cleanup can yield: only this still-current,
              unpaused generation may complete the logical wait. *)
           if completed && (not !paused) && phys_equal change (fst !changed)
           then (
             (* A valid real wait advances shared virtual time to the earliest
                pending logical deadline. It need not be that waiter's own real
                timer: cooperative CPU work can make several timers runnable. *)
             let earliest =
               List.fold !waiters ~init:deadline ~f:(fun earliest (_, candidate) ->
                 if Float.(candidate > !logical_now)
                 then Float.min earliest candidate
                 else earliest)
             in
             advance_to earliest));
        wait deadline
    ;;

    let sleep_until () deadline =
      match Float.(deadline <= !logical_now) with
      | true -> Eio.Fiber.yield ()
      | false ->
        let token = ref () in
        waiters := (token, deadline) :: !waiters;
        Exn.protect
          ~finally:(fun () ->
            waiters
            := List.filter !waiters ~f:(fun (other, _) -> not (phys_equal token other)))
          ~f:(fun () -> wait deadline)
    ;;
  end
  in
  let pause () =
    paused := true;
    notify_change ()
  in
  let resume () =
    paused := false;
    notify_change ()
  in
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)), pause, resume, advance_to
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let observed_idle = function
  | Agent_protocol.Session.Idle -> true
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let temporary_root env =
  let name =
    Agent_protocol.Id.Transaction.create ()
    |> Agent_protocol.Id.Transaction.to_string
    |> fun value -> "ochat-restart-test-" ^ value
  in
  let path = Filename.concat "/tmp" name in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / path);
  path
;;

let permission_profile =
  Agent_server.Config.Permission_profile.
    { id = "restart.permission"
    ; tool_default = Allow
    ; approval_timeout_ms = None
    ; approval_fallback = Deny
    ; manifest_authorization = Assume_authorized
    }
;;

let config
      ?(profile = permission_profile)
      ?(manifest_grants = [])
      root
      workspace
      prompt_file
  =
  Agent_server.Config.
    { version = current_version
    ; source_file = Filename.concat root "server.sexp"
    ; server =
        { data_dir = Filename.concat root "data"
        ; session_helpers = []
        ; authoring_packages = []
        ; authoring_budget = None
        ; unix_socket = Filename.concat root "agent.sock"
        ; http =
            { enabled = false
            ; address = "127.0.0.1"
            ; port = 8787
            ; require_auth = true
            ; static_tokens_file = None
            ; oauth_validator = None
            ; reverse_proxy = None
            ; max_connections = 1_024
            ; idle_connection_timeout_ms = 300_000
            }
        ; shutdown_grace_ms = 5_000
        ; max_attachments_per_session = 1_024
        ; subscriber_queue_capacity = 512
        ; event_retention =
            { completed_stream_ms = 3_600_000
            ; response_artifact_ms = 3_600_000
            ; max_events_per_session = 100_000
            }
        ; durability =
            { journal_flush = Each
            ; journal_flush_ms = 1
            ; snapshot_every_events = 100
            ; snapshot_every_ms = 5_000
            }
        ; job_limits =
            { daemon_total = 16
            ; per_principal = 8
            ; per_prompt = 8
            ; per_workspace = 8
            ; per_session = 4
            ; per_kind = 16
            ; max_nested_depth = 8
            }
        ; unsafe_allow_unauthenticated_remote_http = false
        }
    ; workspaces =
        [ { id = "restart.workspace"
          ; source = Physical workspace
          ; access = Shared_write
          ; conflict_domain = None
          ; prompt_limits =
              [ { prompt = "restart.prompt"; max_root_agents = 1; overflow = Reject } ]
          }
        ]
    ; prompts =
        [ { id = "restart.prompt"
          ; path = prompt_file
          ; description = None
          ; allowed_workspaces = [ "restart.workspace" ]
          ; permission_profile = profile.id
          ; runtime_policy = None
          ; enabled = true
          }
        ]
    ; permission_profiles = [ profile ]
    ; manifest_grants
    }
;;

let scopes =
  Agent_protocol.Scope.Set.of_list
    [ List_prompts
    ; List_workspaces
    ; Create_sessions
    ; View_session_transcript
    ; Send_messages
    ; Own_sessions
    ; Answer_approvals
    ; View_security_state
    ; Manage_grants
    ; Read_audit
    ; Stop_sessions
    ; Delete_sessions
    ; Administer_configuration
    ; Diagnostics
    ; Submit_ingress
    ]
;;

let principal_with_scopes id scopes =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.of_string id |> protocol_ok)
    ~authentication_kind:"test"
    ~scopes
    ~attributes:[]
  |> protocol_ok
;;

let principal_with_id id = principal_with_scopes id scopes
let principal () = principal_with_id "pri_restart_test"

let connection daemon principal =
  let notifications = Eio.Stream.create 256 in
  let context =
    Agent_server.Connection_context.create
      ~connection_id:
        (Agent_protocol.Id.Attachment.create () |> Agent_protocol.Id.Attachment.to_string)
      ~principal
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  Agent_client.In_memory.create
    ~request:(fun command ->
      Agent_server.Dispatcher.dispatch_command
        (Agent_server.Daemon.dispatcher daemon)
        ~context
        command)
    ~notifications
    ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
;;

let initialize connection =
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"restart-test"
    ~implementation_version:"dev"
  |> protocol_ok
  |> ignore
;;

let session_spec
      ?(start_immediately = false)
      ?(liveness = Agent_protocol.Session.Detached)
      ()
  =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition "restart.prompt"))
    ~workspace:
      (Configured (Agent_server.Catalog_identity.workspace_definition "restart.workspace"))
    ~liveness
    ~persistence:Durable
    ~permission_profile:permission_profile.id
    ~start_immediately
    ~labels:[ "suite", "restart" ]
    ()
  |> protocol_ok
;;

let create_request ?(start_immediately = false) ?(key = "restart-create") () =
  let idempotency_key = Agent_protocol.Idempotency_key.of_string key |> protocol_ok in
  Agent_protocol.Session.Create_request.
    { spec = session_spec ~start_immediately ()
    ; requested_mode = Some Read_write
    ; subscribe = false
    ; idempotency_key
    }
;;

let create_session ?(start_immediately = false) ?(key = "restart-create") connection =
  Agent_client.Connection.request
    connection
    (Session_create (create_request ~start_immediately ~key ()))
  |> protocol_ok
  |> function
  | Agent_protocol.Public.Result.Session_create result ->
    result.session, (Option.value_exn result.attachment).attachment
  | _ -> failwith "unexpected create response"
;;

(* Current documents for restart fixtures. These use the real complete schema,
   never a historical runtime serialization wrapped in a JSON string. *)
let state_document state =
  Agent_session.Session_state_document.authored state
  |> Agent_session.Session_state_document.encode ~limits:Document_schema.Limits.default
  |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
;;

let roundtrip_state state =
  let open Result.Let_syntax in
  let%bind document = state_document state in
  let%map restored =
    Agent_session.Session_state_document.decode
      ~limits:Document_schema.Limits.default
      document
    |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
  in
  Agent_session.Session_state_document.value restored
;;

let authored_snapshot (state : Agent_session.Session_state.t) =
  let open Result.Let_syntax in
  let%bind payload = state_document state in
  Agent_store.Snapshot.create
    ~limits:Document_schema.Limits.default
    ~session_id:state.identity.session_id
    ~transaction_sequence:state.counters.transaction_sequence
    ~transaction_hash:None
    ~event_sequence:state.counters.event_sequence
    ~created_at:state.identity.updated_at
    ~prompt_artifact:
      (Agent_protocol.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
    ~workspace_identity:state.spec.workspace_instance.conflict_domain
    ~payload
;;
