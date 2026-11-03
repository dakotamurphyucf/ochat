open! Core
include Fixtures
module A = Agent_session
module P = Agent_protocol
module S = Agent_store.Session_store
module C = A.Recurrence_catalog
module E = A.Recurrence_entry
module H = A.Recurrence_frontier
module Command = A.Recurrence_command
module Host = Agent_server.Recurrence_automation_host
module Configuration = Agent_server.Recurrence_automation_configuration

let utc_bytes = [%blob "../recurrence_zone_data/fixtures/Etc_UTC.tzif"]
let server_id = P.Id.Server.of_string "srv_recurrence_runtime" |> protocol_ok
let session = P.Session_ref.create ~server_id ~session_id
let id name = P.Id.Recurrence.of_string ("rec_" ^ name) |> protocol_ok

let index_entry (state : A.Session_state.t) =
  assert (List.is_empty state.jobs && List.is_empty state.schedules);
  Agent_store.Session_index.Entry.
    { session = A.Session_state.summary state
    ; runnable_job_count = 0
    ; deliverable_job_count = 0
    ; earliest_schedule_due = None
    ; owner_grace_deadline = None
    ; pending_initial_start = state.pending_initial_start
    ; archived = false
    ; lifecycle_revision = P.Session_lifecycle.Revision.zero
    ; admission = Automatic
    }
;;

let metadata state =
  S.Metadata.
    { schema_version = S.current_metadata_schema_version
    ; session = A.Session_state.summary state
    ; prompt_artifact = P.Id.Prompt_revision.to_string state.spec.prompt_revision_id
    ; workspace_identity = state.spec.workspace_instance.conflict_domain
    ; data_schema_version = A.Session_state.current_schema_version
    }
;;

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; store : S.t
  ; handle : S.Handle.t
  ; initial : A.Session_state.t
  ; durable : A.Session_persistence.t
  ; writer : Agent_store.Commit_writer.t
  ; directory : string
  ; wall : P.Timestamp.t ref
  ; elapsed_ms : int ref
  ; reject : bool ref
  }

let with_store f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let manager, _, _ = handoff_definition ~declare_tool:false env in
      let snapshot =
        Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
      in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:true
      in
      let initial =
        { initial with
          identity =
            { initial.identity with
              (* Wall corrections stay within a real session lifetime. The
                 protocol correctly rejects updates predating creation. *)
              created_at =
                P.Timestamp.of_time_ns
                  (Time_ns.sub
                     (P.Timestamp.to_time_ns timestamp)
                     (Time_ns.Span.of_int_sec 86_400))
            }
        ; moderator = Some (A.Moderator_checkpoint.encode snapshot)
        }
      in
      let root =
        Filename.concat workspace_instance.canonical_root.native_path "runtime-store"
      in
      let store =
        S.create
          ~env
          ~sw
          ~root
          ~server_id
          ~process_start_identity:None
          ~lock_nonce:"runtime"
        |> store_ok
      in
      let handle =
        S.create_session_initialized
          store
          ~sw
          ~transaction_id:(P.Id.Transaction.create ())
          ~actor_lock_nonce:"runtime-actor"
          ~initialize:(fun ~staging_directory:_ ->
            S.Initial_projection.create
              ~metadata:(metadata initial)
              ~entry:(index_entry initial))
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () ->
          S.close_session store handle |> store_ok;
          S.close store |> store_ok)
        ~f:(fun () ->
          let directory = Filename.concat (S.Handle.directory handle) "runtime-journal" in
          let journal =
            Agent_store.Journal.create
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:64
            |> store_ok
          in
          let writer =
            Agent_store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:8
            |> store_ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_store.Commit_writer.close writer)
            ~f:(fun () ->
              let durable =
                A.Session_persistence.create
                  ~archive:(fun _ _ -> failwith "unexpected archive")
                  ~pending_archive:None
                  ~before_commit:
                    (Some
                       (fun state ->
                         S.prepare_canonical_projection
                           store
                           handle
                           ~metadata:(metadata state)
                           ~entry:(index_entry state)
                         |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
                  ~command_accepted:(fun _ _ -> ())
                  ~writer
                  ~durability:Flush
                  ~limits:document_limits
                  ~archive_limits:document_limits
                  ~retention_preflight:None
                  ~restored:(A.Session_persistence.Restored.authored initial)
                  ~previous_transaction_hash:None
              in
              f
                { env
                ; sw
                ; store
                ; handle
                ; initial
                ; durable
                ; writer
                ; directory
                ; wall = ref timestamp
                ; elapsed_ms = ref 0
                ; reject = ref false
                }))))
;;

let create_actor t ~sw ?(before_observe = ignore) ?(published = fun _ -> ()) () =
  let persistence : A.Session_actor.persistence =
    { archive_reference
    ; commit =
        (fun ~command_audit ~previous transition ->
          if !(t.reject)
          then
            Error
              (P.Error.create
                 Persistence_error
                 ~message:"definite no-write runtime fixture"
                 ~retryable:true
                 ())
          else A.Session_persistence.commit t.durable ~command_audit ~previous transition)
    }
  in
  let actor =
    A.Session_actor.create
      ~sw
      ~clock:(Eio.Stdenv.clock t.env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:t.initial
      ~persistence
      ~operation_worker:None
      ~services:
        { now = (fun () -> !(t.wall))
        ; monotonic_now =
            (fun () ->
              Mtime.add_span
                Mtime.min_stamp
                (Mtime.Span.of_uint64_ns Int64.(of_int !(t.elapsed_ms) * 1_000_000L))
              |> Option.value_exn)
        ; create_attachment_id = P.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "runtime-reclaim")
        ; state_committed =
            (fun state _ ->
              S.write_metadata
                ~entry:(index_entry state)
                t.store
                t.handle
                (metadata state)
              |> store_ok;
              published state)
        ; job_results = None
        ; subscription_limits = A.Staged_subscriptions.default_limits
        ; schedule_limits = A.Staged_schedules.default_limits
        ; notification_limits = A.Staged_notifications.default_limits
        ; ingress_limits = A.Staged_ingress.default_limits
        }
  in
  let controller =
    A.Recurrence_controller.create
      t.store
      ~handle:t.handle
      ~entry_projection:(fun state ->
        before_observe ();
        index_entry state)
    |> protocol_ok
  in
  A.Session_actor.set_recurrence_controller actor controller |> protocol_ok;
  actor
;;

let principal =
  Agent_server_test_support.principal_with_id (P.Id.Principal.to_string principal_id)
;;

let proof = Operator_authorization.trusted_local principal

let authorize state =
  if
    Agent_server.Authorization.session_visible_to
      principal
      (A.Session_state.summary state)
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"fixture controller hidden"
         ~retryable:false
         ())
;;

let state actor = A.Session_actor.state actor |> protocol_ok

let equal_state a b =
  Document_schema.Json.equal
    (Document_schema.Document.json (state_document a))
    (Document_schema.Document.json (state_document b))
;;

let catalog actor = Option.value (state actor).recurrence_catalog ~default:C.empty
let entry actor name = C.find (catalog actor) (id name) |> Option.value_exn
let frontier actor name = E.frontier (entry actor name) |> protocol_ok

let request actor key mutation =
  let state = state actor in
  let catalog = Option.value state.recurrence_catalog ~default:C.empty in
  let _, source = A.Goal_catalog_source.capture state |> protocol_ok in
  let needs_source = Command.Mutation.needs_source mutation in
  Command.create
    ~session_id:state.identity.session_id
    ~generation:source.generation
    ~source:(if needs_source then Some source else None)
    ~expected_lifecycle_revision:
      (if needs_source then Some P.Session_lifecycle.Revision.zero else None)
    ~expected_catalog_revision:(C.revision catalog)
    ~expected_control_revision:
      (Option.map (C.find catalog (Command.Mutation.id mutation)) ~f:E.control_revision)
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
;;

let call actor request =
  A.Session_actor.recurrence_command actor ~actor:proof ~authorize ~request |> protocol_ok
;;

let mutate actor key mutation = call actor (request actor key mutation)

let interval ?(mode = Recurrence_rules.Rule.Interval.Fixed_rate) ~first_due () =
  let period =
    Recurrence_rules.Rule.Period.create_ms 1000L
    |> Result.map_error ~f:(fun e ->
      Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  A.Recurrence_definition.Schedule.Interval
    { rule = Recurrence_rules.Rule.Interval.create ~mode ~period; first_due }
;;

let create actor ?(controller = session) name schedule =
  let state = state actor in
  let _, source = A.Goal_catalog_source.capture state |> protocol_ok in
  let target =
    A.Recurrence_target.create_existing
      ~session:controller
      ~source
      ~configuration_revision:state.spec.configuration_revision
      ~input:{ kind = Plain_text; text = "recurring input"; attachments = [] }
    |> protocol_ok
  in
  mutate
    actor
    ("create-" ^ name)
    (Create (id name, { schedule; target; policy = A.Recurrence_policy.default }))
  |> ignore;
  let enable = request actor ("enable-" ^ name) (Enable (id name)) in
  call actor enable |> ignore;
  enable
;;

let open_host t ?zone_directory ?(checkpoint = ignore) () =
  let configuration =
    Configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_runtime_planner" |> protocol_ok)
      ~controllers:[ session_id ]
      ~expires_at:None
      ~zone_directory
    |> protocol_ok
  in
  Host.open_owned
    configuration
    ~server_id
    ~sw:t.sw
    ~fs:(Eio.Stdenv.fs t.env)
    ~now:(fun () -> !(t.wall))
    ~checkpoint
  |> protocol_ok
;;

let install actor host =
  A.Session_actor.set_recurrence_planning_host actor (Host.planning_host host)
  |> protocol_ok
;;

let sweep actor = A.Session_actor.sweep_recurrences actor |> protocol_ok

let wall_at_ms value =
  P.Timestamp.of_time_ns
    (Time_ns.add (P.Timestamp.to_time_ns timestamp) (Time_ns.Span.of_int_ms value))
;;

let consumed actor name = (H.summary (frontier actor name)).consumed

let replayed t =
  let journal =
    Agent_store.Journal.open_existing
      ~env:t.env
      ~directory:t.directory
      ~max_payload_length:1048576
      ~max_segment_bytes:4194304L
      ~max_segment_frames:64
    |> store_ok
  in
  let scan = Agent_store.Journal.scan journal |> store_ok in
  List.fold
    scan.entries
    ~init:(A.Session_persistence.Restored.authored t.initial)
    ~f:(fun restored entry ->
      if Int.equal (Agent_store.Frame.flags entry.frame) 0
      then (
        let transaction =
          Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
          |> store_ok
        in
        A.Session_persistence.validate_transaction ~limits:document_limits transaction
        |> store_ok;
        A.Session_persistence.apply_transaction
          ~limits:document_limits
          restored
          transaction
        |> store_ok)
      else restored)
  |> A.Session_persistence.Restored.state
;;
