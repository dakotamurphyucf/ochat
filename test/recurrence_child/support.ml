open! Core
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module S = Agent_store.Session_store
module Ledger = Agent_store.Delegation_store
module Artifact = Agent_store.Prompt_artifact_store
module Daemon = Agent_server.Daemon
module Actor = A.Session_actor
module Registry = Agent_server.Session_registry
module Factory = Agent_server.Session_factory
module Target = A.Recurrence_target
module Entry = A.Recurrence_entry
module Catalog = A.Recurrence_catalog
module Frontier = A.Recurrence_frontier
module T = Agent_server_test_support

let protocol_ok = T.protocol_ok

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

(* Cleanup joins while the owning switch is live; a failed oracle retains its
   original exception even if joining the daemon also reports an error. *)
let protect_cleanup ~finally f =
  match f () with
  | value ->
    finally ();
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try finally () with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let state entry = Actor.state entry.Registry.actor |> protocol_ok
let loaded daemon id = Registry.find (Daemon.registry daemon) id |> Option.value_exn
let recurrence_id = P.Id.Recurrence.of_string "rec_captured_factory" |> protocol_ok
let principal = T.principal ()
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
         ~message:"fixture parent is hidden"
         ~retryable:false
         ())
;;

let entry parent =
  let current = state parent in
  Catalog.find (Option.value_exn current.recurrence_catalog) recurrence_id
  |> Option.value_exn
;;

let command_result parent key mutation =
  let current = state parent in
  let catalog = Option.value current.recurrence_catalog ~default:Catalog.empty in
  let _, source = A.Goal_catalog_source.capture current |> protocol_ok in
  let needs_source = A.Recurrence_command.Mutation.needs_source mutation in
  let request =
    A.Recurrence_command.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~source:(if needs_source then Some source else None)
      ~expected_lifecycle_revision:
        (if needs_source then Some P.Session_lifecycle.Revision.zero else None)
      ~expected_catalog_revision:(Catalog.revision catalog)
      ~expected_control_revision:
        (Option.map (Catalog.find catalog recurrence_id) ~f:Entry.control_revision)
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~mutation
    |> protocol_ok
  in
  Actor.recurrence_command parent.actor ~actor:proof ~authorize ~request
;;

let command parent key mutation = command_result parent key mutation |> protocol_ok

let replace json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "fixture expected object"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("fixture missing " ^ name)
;;

let target ?revision_id env daemon parent ~root_file =
  let current = state parent in
  let store = Daemon.store daemon in
  let artifacts =
    Artifact.open_existing
      ~env
      ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
    |> store_ok
  in
  let artifact = Artifact.load artifacts current.spec.prompt_revision_id |> store_ok in
  let artifact =
    match revision_id with
    | None -> artifact
    | Some revision_id ->
      Artifact.Artifact.with_identity
        artifact
        ~revision_id
        ~created_at:(P.Timestamp.now ())
      |> store_ok
  in
  let definition =
    A.Prompt_definition.create
      ~id:(Option.value_exn current.spec.prompt_definition_id)
      ~config_name:"captured-copy"
      ~root_file
      ~root_invocation:current.spec.root_invocation
      ~allowed_workspaces:(Option.to_list current.spec.workspace_instance.definition_id)
      ~permission_profile:current.spec.permission_profile
      ~runtime_policy:current.spec.runtime_policy
      ~enabled:true
      ~description:None
    |> store_ok
  in
  let captured_parent =
    Target.Parent.create
      ~workspace:current.spec.workspace_instance
      ~permission_profile:current.spec.permission_profile
      ~permission_profile_digest:current.spec.permission_profile_digest
      ~runtime_policy:current.spec.runtime_policy
      ~configuration_revision:current.spec.configuration_revision
      ~inference_selection:current.spec.inference_target
      ~capability_pins:[]
    |> protocol_ok
  in
  let target =
    Target.create_fresh
      ~mode:Single_turn
      ~artifact:(Agent_store.Captured_prompt_artifact.create artifact |> document_ok)
      ~definition
      ~parent:captured_parent
    |> protocol_ok
  in
  let document = Target.to_document target |> document_ok |> D.Document.json in
  let payload = field document "payload" in
  let definition =
    replace (field payload "definition") "future_definition" (`Number "1.00e+2")
  in
  replace document "payload" (replace payload "definition" definition)
  |> D.Document.inspect ~limits:Target.limits
  |> document_ok
  |> Target.of_document
  |> document_ok
;;

let authorize_creation current definition =
  let open Result.Let_syntax in
  let%bind () = authorize current in
  match Target.view (A.Recurrence_definition.target definition) with
  | Fresh_child { definition; artifact = _; parent = _; mode = _ }
    when definition.enabled && P.Principal.has_scope principal Create_sessions -> Ok ()
  | Fresh_child _ | Existing_session _ ->
    Error
      (P.Error.create
         Permission_denied
         ~message:"fixture host denies child creation"
         ~retryable:false
         ())
;;

let reserve_occurrence ?(actor = proof) parent occurrence =
  Actor.reserve_recurrence_child
    parent.Registry.actor
    ~actor
    ~authorize:authorize_creation
    ~occurrence
    ~expected_control_revision:(Entry.control_revision (entry parent))
  |> protocol_ok
;;

let ready_nominal parent target =
  let period =
    Recurrence_rules.Rule.Period.create_ms 600_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let schedule =
    A.Recurrence_definition.Schedule.Interval
      { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
      ; first_due = P.Timestamp.to_time_ns (P.Timestamp.now ())
      }
  in
  command
    parent
    "create-captured"
    (Create (recurrence_id, { schedule; target; policy = A.Recurrence_policy.default }))
  |> ignore;
  command parent "enable-captured" (Enable recurrence_id) |> ignore;
  Actor.sweep_recurrences parent.actor |> protocol_ok;
  let recurrence = entry parent in
  Frontier.ready (Entry.frontier recurrence |> protocol_ok) |> List.hd_exn
;;

let reserve parent target =
  let nominal = ready_nominal parent target in
  reserve_occurrence parent (A.Recurrence_nominal.key nominal)
;;

let with_planner env sw daemon parent f =
  let module Configuration = Agent_server.Recurrence_automation_configuration in
  let module Host = Agent_server.Recurrence_automation_host in
  let configuration =
    Configuration.create
      ~principal_id:(P.Id.Principal.of_string "pri_child_planner" |> protocol_ok)
      ~controllers:[ (state parent).identity.session_id ]
      ~expires_at:None
      ~zone_directory:None
    |> protocol_ok
  in
  let host =
    Host.open_owned
      configuration
      ~server_id:(S.server_id (Daemon.store daemon))
      ~sw
      ~fs:(Eio.Stdenv.fs env)
      ~now:P.Timestamp.now
      ~checkpoint:Eio.Fiber.check
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Host.close_and_wait host)
    (fun () ->
       Actor.set_recurrence_planning_host parent.actor (Host.planning_host host)
       |> protocol_ok;
       f ())
;;

let await_ready env parent =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
    let rec loop () =
      let current = state parent in
      if
        Option.is_some current.moderator
        && Option.is_none current.active_operation
        && T.observed_idle current.lifecycle.observed
      then current
      else (
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        loop ())
    in
    loop ())
;;

let start_parent_settled env sw connection parent =
  let changed = Eio.Condition.create () in
  let already_loaded = Agent_server.Runtime_owner.is_loaded parent.Registry.runtime in
  let previous =
    (state parent).moderator_executions
    |> List.map ~f:(fun receipt -> receipt.P.Moderator_execution.context.id)
  in
  let handle =
    Agent_client.Session_handle.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~session_id:(state parent).identity.session_id
      ~mode:Read_write
      ~on_update:(fun _ -> Eio.Condition.broadcast changed)
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Agent_client.Session_handle.close handle)
    (fun () ->
       Agent_client.Session_handle.start handle ~queue_if_limited:false
       |> protocol_ok
       |> ignore;
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Eio.Condition.loop_no_mutex changed (fun () ->
           let current = state parent in
           let started =
             List.exists current.moderator_executions ~f:(fun receipt ->
               (already_loaded
                || not
                     (List.mem
                        previous
                        receipt.context.id
                        ~equal:P.Id.Moderator_execution.equal))
               &&
               match receipt.context.phase, receipt.status with
               | (Session_start | Session_resume), Completed _ -> true
               | _ -> false)
           in
           let settled =
             List.for_all current.moderator_executions ~f:(fun receipt ->
               (match receipt.P.Moderator_execution.status with
                | Running -> false
                | Completed _ | Failed _ | Interrupted _ -> true)
               &&
               match receipt.intent with
               | None | Some Applied | Some (Discarded _) -> true
               | Some Pending | Some (Waiting_compaction _) -> false)
           in
           let queue_empty =
             A.Moderator_checkpoint.decode current.moderator
             |> protocol_ok
             |> Option.exists ~f:(fun snapshot ->
               List.is_empty snapshot.queued_internal_events && not snapshot.halted)
           in
           if
             started
             && settled
             && queue_empty
             && P.Session.equal_desired_state current.lifecycle.desired Running
             && T.observed_idle current.lifecycle.observed
             && Option.is_none current.active_operation
           then Some ()
           else None)))
;;

let assert_dormant daemon id =
  let child = loaded daemon id in
  let current = state child in
  assert (P.Session.equal_desired_state current.lifecycle.desired Stopped);
  (match current.lifecycle.observed with
   | Stopped -> ()
   | Queued_for_slot
   | Starting
   | Recovering
   | Idle
   | Running_turn _
   | Compacting _
   | Waiting_for_permission _
   | Stopping
   | Failed _ -> failwith "child is not stopped");
  (match current.runtime_initialization with
   | Pending { fresh_history = true } -> ()
   | _ -> assert false);
  assert (not current.pending_initial_start);
  assert (not current.spec.protocol.start_immediately);
  assert (List.is_empty current.conversation.canonical_history);
  [%test_eq: int64] 0L current.conversation.next_history_sequence;
  [%test_eq: int64] 0L current.conversation.reserved_history_through;
  assert (Option.is_none current.run_state);
  assert (Option.is_none current.moderator);
  assert (List.is_empty current.moderator_executions);
  assert (not (Agent_server.Runtime_owner.is_loaded child.runtime));
  let reference = Option.value_exn current.spec.delegation in
  let record =
    Ledger.resolve (S.delegations (Daemon.store daemon)) reference |> store_ok
  in
  let origin = A.Recurrence_child_origin.of_record record |> protocol_ok in
  A.Recurrence_child_origin.validate_initial_state origin current |> protocol_ok;
  child, record
;;

let assert_ordinary_execution_blocked env sw connection daemon child_id =
  let handle =
    Agent_client.Session_handle.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~session_id:child_id
      ~mode:Read_write
      ()
    |> protocol_ok
  in
  let rejected ~message ~retryable = function
    | Error (error : P.Error.t) ->
      [%test_eq: P.Error.code] Invalid_state error.code;
      [%test_eq: string] message error.message;
      [%test_eq: bool] retryable error.retryable
    | Ok _ -> failwith "ordinary execution activated a captured recurrence child"
  in
  protect_cleanup
    ~finally:(fun () -> Agent_client.Session_handle.close handle)
    (fun () ->
       Agent_client.Session_handle.start handle ~queue_if_limited:false
       |> rejected
            ~message:
              "captured recurrence child requires reciprocal occurrence Run admission"
            ~retryable:false;
       Agent_client.Session_handle.send_message
         handle
         { kind = Plain_text; text = "ordinary send must stay inert"; attachments = [] }
       |> rejected ~message:"session runtime is not already loaded" ~retryable:true);
  assert_dormant daemon child_id |> ignore
;;

let with_daemon
      ?policy
      ?(on_runtime_construction = fun () -> ())
      env
      root
      configuration
      ~provider_calls
      f
  =
  Eio.Switch.run (fun sw ->
    let policy =
      match policy with
      | Some policy -> policy
      | None ->
        T.inference_policy
          ~default_model:"fixture-model"
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            Int.incr provider_calls;
            failwith "captured child activated provider")
    in
    let policy =
      { policy with
        runtime_inference_ports =
          (fun actor ->
            on_runtime_construction ();
            policy.runtime_inference_ports actor)
      }
    in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config:configuration
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            qualify_chatml_extensions = true
          ; inference_policy = policy
          }
        ()
      |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
      (fun () ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () -> f sw daemon)))
;;
