open! Core
open Agent_server_test_support
open Retry_support
module Warm = Delegated_warm_resource_tests
module Polling = Ordinary_warm_resource_tests.Polling
module B = A.Runtime_builder
module E = P.Moderator_execution
module Queue = A.Moderator_queue
module Store = Agent_store.Session_store

(* This is intentionally not serialized in the checkpoint. Actual policy use
   observes the first module instance; a re-created module at explicit Resume
   fails rather than silently satisfying a checkpoint-only equality oracle. *)
let prompt =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>634 physically restored modern parent policy.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="cold-policy" language="chatml" kind="moderator" api="extensibility-v1">
let policy_seen = ref(0)
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  Task.bind(Schedule.after_ms(0, `String("634-authentic-queued-head")), fun ignored ->
    Task.pure(state + 1))
| `Pre_tool_call(call) ->
  if !policy_seen == 0 then (
    policy_seen := 1;
    Task.bind(Tool.call("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("policy.txt")}
    ])), fun result -> match result with
    | `Ok(value) -> Task.pure(state + 1)
    | `Error(code) -> Task.fail(code)))
  else Task.fail("634 repeated policy callback")
| `Session_resume ->
  if !policy_seen == 1 then Task.pure(state + 1000)
  else Task.fail("634 policy module was reconstructed before explicit Resume")
| `Internal_event(payload) ->
  (match payload with
   | `String("634-authentic-queued-head") -> Task.pure(state + 10)
   | _ -> Task.fail("634 unexpected captured queue payload"))
| _ -> Task.pure(state)
</script>|}
;;

type cold_case =
  | Running_unloaded
  | Stopped

type persisted =
  { session_id : P.Id.Session.t
  ; desired : P.Session.desired_state
  ; source : P.Run_source.t
  ; lifecycle : A.Moderator_lifecycle.t
  ; checkpoint : Jsonaf.t
  ; queue : Queue.t
  ; resume_count : int
  ; provider_count : int
  ; inference_port_count : int
  }

let observe_persisted (entry : Registry.entry) (counters : counters) =
  let current = Warm.settled entry in
  assert (Option.is_none current.active_operation);
  assert (
    List.for_all current.schedules ~f:(fun schedule ->
      Option.is_none schedule.P.Schedule.delivery_cancellation));
  let index = Option.value_exn current.run_state in
  let lifecycle = A.Run_state.moderator_lifecycle index |> Option.value_exn in
  let selected =
    match
      List.filter current.moderator_executions ~f:(fun receipt ->
        P.Id.Moderator_execution.equal
          receipt.E.context.id
          (A.Moderator_lifecycle.execution_id lifecycle))
    with
    | [ receipt ] -> receipt
    | _ -> failwith "634 saved lifecycle has no unique actual receipt"
  in
  (match selected.status with
   | Completed _ -> ()
   | Running | Failed _ | Interrupted _ ->
     raise_s [%sexp "634 saved lifecycle was not completed", (selected : E.t)]);
  let source = Warm.source current in
  assert (P.Run_source.equal source (A.Moderator_lifecycle.source lifecycle));
  let checkpoint = Option.value_exn current.moderator in
  let queue =
    A.Moderator_checkpoint.queue current.moderator |> protocol_ok |> Option.value_exn
  in
  assert (P.Run_source.equal source (Queue.source queue));
  { session_id = current.identity.session_id
  ; desired = current.lifecycle.desired
  ; source
  ; lifecycle
  ; checkpoint
  ; queue
  ; resume_count = List.length (Warm.completed current Session_resume)
  ; provider_count = !(counters.providers)
  ; inference_port_count = !(counters.inference_ports)
  }
;;

(* Pollers are parked while the real Factory driver consumes pending startup.
   That callback creates this genuine zero-delay timer. Claim and deliver it
   through the actual Actor/Owner protocol without a competing scheduler pass. *)
let enqueue_startup_timer (entry : Registry.entry) =
  let observed, due = Actor.due_schedules entry.actor |> protocol_ok in
  (match observed with
   | Idle -> ()
   | Stopped
   | Queued_for_slot
   | Starting
   | Recovering
   | Running_turn _
   | Compacting _
   | Waiting_for_permission _
   | Stopping
   | Failed _ -> failwith "634 startup timer did not follow actual idle startup");
  let timer =
    match due with
    | [ timer ] -> timer
    | _ -> failwith "634 startup did not create one actual due timer"
  in
  assert (Jsonaf.exactly_equal timer.payload (`String "634-authentic-queued-head"));
  let claimed =
    Actor.claim_schedule entry.actor ~schedule_id:timer.id ~generation:timer.generation
    |> protocol_ok
    |> Option.value_exn
  in
  Owner.deliver_schedule entry.runtime claimed |> protocol_ok;
  let current = Warm.settled entry in
  let queue =
    A.Moderator_checkpoint.queue current.moderator |> protocol_ok |> Option.value_exn
  in
  let head =
    match Queue.entries queue with
    | [ head ] -> head
    | _ -> failwith "634 startup did not retain one authentic queued head"
  in
  match Queue.Entry.producer head with
  | Some (A.Queue_producer.Schedule_delivery actual) ->
    assert (P.Id.Schedule.equal timer.id actual.id);
    [%test_eq: int] timer.generation actual.generation;
    [%test_eq: int] 1 actual.delivery_count
  | Some
      ( Event _
      | Invocation _
      | Invocation_at_source _
      | Job_completion _
      | Ingress_delivery _
      | Source_setup _ )
  | None -> failwith "634 startup queue did not retain its actual Schedule producer"
;;

type seeded_target =
  | Existing_target of Registry.entry
  | Indexed_target

let with_seed_boundary ?(wrap_env = Fn.id) ~phase cold f =
  let polling = Polling.create () in
  let io = Warm.Handoff_io.create () in
  let counters : counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      failwith "634 policy-only fixture unexpectedly entered a provider")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  phase := "create daemon fixture";
  Creation_tests.with_daemon
    ~prompt_text:prompt
    ~sources:[ "policy.txt", "634 actual native policy read" ]
    ~qualify_chatml_extensions:true
    ~wrap_env:(fun env -> wrap_env (Warm.Handoff_io.wrap io (Polling.wrap polling env)))
    ~policy
    (fun env sw root daemon ->
       (* Creation passes its original env to f; only daemon clocks are wrapped.
          Both deadline layers therefore remain real while pollers are parked. *)
       Eio.Time.with_timeout_exn env#clock 90. (fun () ->
         protect_cleanup ~finally:polling.resume (fun () ->
           phase := "park pollers before explicitly driving initial lifecycle";
           Eio.Time.with_timeout_exn env#clock 30. (fun () -> Polling.park polling daemon);
           phase := "create ordinary source and drive actual startup";
           Source.with_loaded_entry
             ~before_close:polling.resume
             ~drive_startup:true
             daemon
             (fun ~entry ~runtime ~revision:_ ->
                Ordinary_warm_resource_tests.assert_live_source runtime;
                io.cache
                <- Some
                     (Filename.concat
                        (Store.Handle.cache_directory
                           (Option.value_exn entry.store_handle))
                        "cache.bin");
                [%test_eq: int] 0 io.cache_renames;
                [%test_eq: int]
                  1
                  (List.length (Warm.completed (Warm.settled entry) Session_start));
                phase := "claim and queue actual startup timer";
                enqueue_startup_timer entry;
                (match cold with
                 | Running_unloaded ->
                   phase := "unload actual Running Product";
                   Owner.unload_and_wait entry.runtime |> protocol_ok;
                   assert (not (Owner.is_loaded entry.runtime));
                   (match A.Prompt_source_lease.availability runtime.B.source_lease with
                    | Closed -> ()
                    | Missing_receipt | Live | Unavailable _ | Revoked ->
                      failwith
                        "634 unloaded runtime retained its physical source lifetime");
                   [%test_eq: int] 1 io.cache_renames
                 | Stopped ->
                   phase := "stop actual parent";
                   let writer, _ =
                     Actor.attach entry.actor ~mode:Read_write ~subscribe:false
                     |> protocol_ok
                   in
                   protect_cleanup
                     ~finally:(fun () ->
                       Actor.detach entry.actor writer.id |> protocol_ok)
                     (fun () ->
                        (* Preserve this owned queued delivery. Cancel intentionally retires
                        it, so that mode cannot establish later execution of the head. *)
                        Actor.stop entry.actor ~attachment_id:writer.id ~mode:Graceful
                        |> protocol_ok
                        |> ignore));
                phase := "observe saved lifecycle and queue";
                let saved = observe_persisted entry counters in
                [%test_eq: int] 0 saved.provider_count;
                [%test_eq: int] 0 saved.resume_count;
                (match cold, saved.desired with
                 | Running_unloaded, Running | Stopped, Stopped -> ()
                 | Running_unloaded, Stopped | Stopped, Running ->
                   failwith "634 physical cut did not retain its actual desired state");
                match cold with
                | Running_unloaded ->
                  (* This is physical Product/native teardown in the SAME canonical
                  Actor/Store, not cold-daemon recovery or a registered child. *)
                  f env sw root daemon (Existing_target entry) saved counters
                | Stopped ->
                  let old_store = Daemon.store daemon in
                  phase := "shutdown first physical daemon";
                  polling.resume ();
                  Daemon.shutdown daemon |> protocol_ok;
                  assert (Store.is_closed old_store);
                  phase := "start new On_demand daemon";
                  let reopened = Dispatch_recovery_tests.start env sw root ~policy in
                  protect_cleanup
                    ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
                    (fun () ->
                       Dispatch_recovery_tests.assert_indexed_only
                         (Daemon.registry reopened)
                         saved.session_id;
                       [%test_eq: int] saved.provider_count !(counters.providers);
                       [%test_eq: int]
                         saved.inference_port_count
                         !(counters.inference_ports);
                       f env sw root reopened Indexed_target saved counters)))))
;;

let with_seed ?wrap_env ~phase cold f =
  with_seed_boundary
    ?wrap_env
    ~phase
    cold
    (fun env sw root daemon target saved counters ->
       let parent =
         match target with
         | Existing_target entry -> entry
         | Indexed_target ->
           phase := "select actual Stopped target";
           Recurrence_target_acquisition_tests.select_entry daemon saved.session_id
       in
       f env sw root daemon parent saved counters)
;;

module Caller = Delegated_policy_caller_support
module C = Chat_response.Tool_capability
module I = P.Invocation
module N = A.Native_tool_invocation

(* Existing trusted-host native protocol. It owns a real submitted operation,
   canonical function-call entry and dispatching Invocation before borrowing the
   Caller. It is not a private Factory child relation or Independent permission. *)
type native_request =
  { caller_actor : Actor.t
  ; request : A.Script_tool_calls.preparation
  ; governance : Actor.Causal_governance.t
  ; authorize : unit -> (unit, P.Error.t) result
  ; capture_authorization : unit -> (unit -> (unit, P.Error.t) result, P.Error.t) result
  ; delegation : E.delegation
  }

let with_native_request ~phase env daemon ~target_actor expected ~original ~f =
  phase := "create and start genuine standalone Caller";
  Caller.with_standalone_caller
    ~env
    ~daemon
    ~source_state:expected
    ~original
    (fun caller_actor writer ->
       let before = Actor.state caller_actor |> protocol_ok in
       let candidate = P.Id.Invocation.create () in
       let invoke () =
         let borrowed = N.borrow () |> protocol_ok in
         let lending = N.borrowed_invocation borrowed in
         assert (P.Id.Session.equal lending.context.session_id before.identity.session_id);
         let actual_original =
           N.borrowed_agent_actor borrowed |> protocol_ok |> Option.value_exn
         in
         assert (Operator_authorization.is_current actual_original);
         assert (
           P.Id.Principal.equal
             (Operator_authorization.principal actual_original).id
             (Operator_authorization.principal original).id);
         let selected = N.borrowed_capabilities borrowed |> protocol_ok in
         let reference =
           C.find selected ~name:"read_file"
           |> Result.map_error ~f:(fun error -> error.C.message)
           |> Result.ok_or_failwith
           |> C.reference
         in
         let caller = N.borrowed_policy_caller borrowed |> protocol_ok in
         Exn.protect
           ~finally:(fun () -> A.Delegated_policy_caller.close caller)
           ~f:(fun () ->
             let call : Chat_response.Moderation.Tool_call.t =
               { id = P.Id.Invocation.to_string candidate
               ; name = reference.name
               ; args = `Object []
               ; kind = Function
               ; payload_text = "{}"
               ; meta = `Null
               }
             in
             let request : A.Script_tool_calls.preparation =
               { invocation_id = candidate
               ; session_id = before.identity.session_id
               ; generation = before.identity.generation
               ; owner = Native_call lending.context.id
               ; caller = Some caller
               ; selected
               ; call
               }
             in
             let check_original () =
               if Operator_authorization.is_current original
               then Ok ()
               else
                 Error
                   (P.Error.create
                      Permission_denied
                      ~message:"634 original revoked"
                      ~retryable:false
                      ())
             in
             let authorize ~read_target () =
               let open Result.Let_syntax in
               let%bind () = check_original () in
               let%bind (current : A.Session_state.t) = read_target () in
               let%bind () = check_original () in
               let principal = Operator_authorization.principal original in
               let%bind () =
                 if
                   P.Id.Session.equal
                     current.identity.session_id
                     expected.A.Session_state.identity.session_id
                   && Int.equal current.identity.generation expected.identity.generation
                   && P.Principal.has_scope principal Own_sessions
                   && Agent_server.Authorization.session_creator_visible_to
                        principal
                        ~creator:current.identity.creating_principal
                 then Ok ()
                 else
                   Error
                     (P.Error.create
                        Permission_denied
                        ~message:
                          "634 actual policy target is no longer selectable by original"
                        ~retryable:false
                        ())
               in
               Actor.check_delegated_policy_caller caller_actor request
             in
             let capture_authorization () =
               let%map.Result observation = Actor.observe_resource_state target_actor in
               authorize ~read_target:(fun () ->
                 Eio.Fiber.yield ();
                 Actor.Resource_observation.check_current observation)
             in
             let authorize =
               authorize ~read_target:(fun () -> Actor.state target_actor)
             in
             authorize () |> protocol_ok;
             let governance =
               Actor.Causal_governance.capture caller_actor ~request
               |> protocol_ok
               |> Actor.Causal_governance.bind_standalone caller_actor
               |> protocol_ok
             in
             f
               { caller_actor
               ; request
               ; governance
               ; authorize
               ; capture_authorization
               ; delegation =
                   { child_session_id = request.session_id
                   ; child_generation = request.generation
                   ; child_invocation_id = candidate
                   ; admission_sha256 = C.fingerprint selected
                   }
               })
       in
       let module Definition = struct
         type input = string

         let name = "read_file"
         let description = Some "634 actual standalone native policy caller"
         let type_ = "function"
         let parameters = `Object [ "type", `String "object" ]
         let input_of_string text = text
       end
       in
       let implementation =
         Ochat_function.create_function
           (module Definition)
           (fun input ->
              assert (String.equal input "{}");
              invoke ();
              Openai.Responses.Tool_output.Output.Text
                "634 actual native policy completed")
       in
       let selected =
         C.create
           ~owner:"634-trusted-native-host"
           ~resource_fingerprint:
             (Chatmd_shell_spec.Source_ref.digest "634 actual standalone host")
           [ ( Chatmd_shell_spec.Source_ref.digest
                 "634 actual native policy implementation"
             , implementation )
           ]
         |> Result.map_error ~f:(fun error -> error.C.message)
         |> Result.ok_or_failwith
       in
       let reference = List.hd_exn (C.references selected) in
       let ready, publish = Eio.Promise.create () in
       let exited, exit = Eio.Promise.create () in
       let calls = ref 0 in
       let worker =
         A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
           Exn.protect
             ~finally:(fun () -> Eio.Promise.resolve exit ())
             ~f:(fun () ->
               Int.incr calls;
               let outcome =
                 try
                   let id = P.Id.Invocation.create () in
                   let call_id =
                     History_entry.Id_source.allocate capabilities.id_source
                     |> Result.ok_or_failwith
                   in
                   let call =
                     Openai.Responses_history.create_with_id_exn
                       ~id:call_id
                       (Openai.Responses.Item.Function_call
                          { name = reference.name
                          ; arguments = "{}"
                          ; call_id = P.Id.Invocation.to_string id
                          ; _type = "function_call"
                          ; id = None
                          ; status = None
                          })
                   in
                   capabilities.commit_entry call |> protocol_ok;
                   let invocation =
                     I.create
                       { id
                       ; session_id = input.session_id
                       ; generation = input.session_generation
                       ; origin = Model
                       ; provider_call_id = Some (P.Id.Invocation.to_string id)
                       ; call_entry_id = Some call_id
                       ; parent_invocation = None
                       ; parent_job = None
                       ; tool_name = reference.name
                       ; implementation_revision = reference.implementation_revision
                       ; capability_fingerprint = C.fingerprint selected
                       ; input = `Object []
                       ; created_at = P.Timestamp.now ()
                       ; deadline = None
                       }
                     |> protocol_ok
                   in
                   let resolved =
                     N.run
                       ~capabilities
                       ~registry:(fun () -> selected)
                       ~reference
                       ~invocation
                       ~is_halted:(fun () -> false)
                       ~authorize:(fun _ _ ->
                         capabilities.original_agent_actor ()
                         |> Result.map ~f:(fun actual ->
                           assert (
                             Option.exists actual ~f:Operator_authorization.is_current)))
                       ~prepare_output:(function
                         | Openai.Responses.Tool_output.Output.Text text ->
                           Ok (`String text)
                         | Content _ ->
                           Error
                             (P.Error.invalid_request
                                "634 native caller returned nontext"))
                     |> protocol_ok
                   in
                   (match resolved.status with
                    | Resolved (Complete (`String text)) ->
                      [%test_eq: string] "634 actual native policy completed" text
                    | status ->
                      raise_s
                        [%sexp
                          "634 actual native caller did not resolve", (status : I.status)]);
                   Ok resolved
                 with
                 | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
               in
               Eio.Promise.resolve publish outcome;
               Eio.Fiber.await_cancel ()))
       in
       Actor.set_operation_worker caller_actor (Some worker) |> protocol_ok;
       let ids =
         A.History_id_source.create
           ~namespace:(P.Id.Session.to_string before.identity.session_id)
           ~block_size:8
           ~reserve:(fun ~count -> Actor.reserve_history_block caller_actor ~count)
         |> protocol_ok
       in
       let message =
         A.History_codec.user_text
           ~id:(A.History_id_source.allocate ids |> protocol_ok)
           "634 genuine standalone original policy caller"
         |> A.History_codec.to_protocol
       in
       phase := "submit actual standalone native operation";
       let submitted =
         Actor.submit_message_authenticated
           caller_actor
           ~actor:original
           ~attachment_id:writer
           message
         |> protocol_ok
       in
       let operation = Option.value_exn submitted.operation_id in
       phase := "await actual native policy outcome";
       let resolved =
         protect_cleanup
           ~finally:(fun () ->
             phase := "cancel and join actual standalone Caller";
             Eio.Cancel.protect (fun () ->
               Actor.cancel_operation
                 caller_actor
                 ~attachment_id:writer
                 ~operation_id:operation
               |> protocol_ok
               |> ignore;
               Eio.Time.with_timeout_exn env#clock 30. (fun () ->
                 Eio.Promise.await exited)))
           (fun () ->
              Eio.Time.with_timeout_exn env#clock 30. (fun () ->
                match Eio.Promise.await ready with
                | Ok resolved -> resolved
                | Error (exn, backtrace) ->
                  Exn.raise_with_original_backtrace exn backtrace))
       in
       [%test_eq: int] 1 !calls;
       assert (Option.is_some resolved.context.call_entry_id);
       ());
  ()
;;

let run_standalone_policy ~phase env daemon (parent : Registry.entry) expected ~original =
  let policy_receipt = ref None in
  with_native_request
    ~phase
    env
    daemon
    ~target_actor:parent.actor
    expected
    ~original
    ~f:(fun native ->
      phase := "prepare actual delegated cold policy";
      let decision =
        Owner.prepare_delegated_tool
          parent.runtime
          ~request:native.request
          ~governance:native.governance
          ~delegation:native.delegation
          ~event:(Pre_tool_call native.request.call)
          ~authorize:native.authorize
        |> protocol_ok
      in
      (match decision with
       | Approve -> ()
       | Reject _ | Rewrite_args _ | Redirect _ ->
         failwith "634 actual cold policy denied its native caller");
      let current = Warm.settled parent in
      let receipt =
        match Caller.policy_receipts current with
        | [ receipt ] -> receipt
        | _ -> failwith "634 real cold policy did not record one receipt"
      in
      assert (Option.is_none !policy_receipt);
      policy_receipt := Some receipt;
      match receipt.status, receipt.delegation, receipt.decision with
      | Completed _, Some actual, Some Approve ->
        assert (P.Id.Session.equal actual.child_session_id native.request.session_id);
        assert (
          P.Id.Invocation.equal actual.child_invocation_id native.request.invocation_id)
      | _ -> raise_s [%sexp "634 cold policy receipt lost actual caller", (receipt : E.t)]);
  Option.value_exn !policy_receipt
;;

(* Full cold-daemon registered-child bootstrap remains OCH-593 integration;
   these two component cases retain their actual independent lifetime cuts. *)
let check_policy_restore_body ~phase cold =
  with_seed ~phase cold (fun env _sw _root daemon parent saved counters ->
    let before = Warm.settled parent in
    assert (P.Session.equal_desired_state before.lifecycle.desired saved.desired);
    assert (not (Owner.is_loaded parent.runtime));
    assert (not (Owner.has_activation_handoff parent.runtime));
    assert (Jsonaf.exactly_equal saved.checkpoint (Option.value_exn before.moderator));
    assert (
      A.Moderator_lifecycle.equal
        saved.lifecycle
        (A.Run_state.moderator_lifecycle (Option.value_exn before.run_state)
         |> Option.value_exn));
    assert (P.Run_source.equal saved.source (Warm.source before));
    [%test_eq: int]
      saved.resume_count
      (List.length (Warm.completed before Session_resume));
    [%test_eq: int] saved.provider_count !(counters.providers);
    [%test_eq: int] saved.inference_port_count !(counters.inference_ports);
    let receipt = run_standalone_policy ~phase env daemon parent before ~original in
    phase := "observe actual policy ACK and unchanged saved queue";
    let after = Warm.settled parent in
    assert (P.Session.equal_desired_state after.lifecycle.desired saved.desired);
    assert (not (Owner.is_loaded parent.runtime));
    assert (not (Owner.has_activation_handoff parent.runtime));
    assert (P.Run_source.equal saved.source (Warm.source after));
    assert (P.Invocation.equal_observer receipt.context.source saved.source.observer);
    [%test_eq: int] saved.resume_count (List.length (Warm.completed after Session_resume));
    [%test_eq: int] saved.provider_count !(counters.providers);
    let after_queue =
      A.Moderator_checkpoint.queue after.moderator |> protocol_ok |> Option.value_exn
    in
    assert (
      Jsonaf.exactly_equal (Queue.to_jsonaf saved.queue) (Queue.to_jsonaf after_queue));
    let after_snapshot =
      A.Moderator_checkpoint.decode after.moderator |> protocol_ok |> Option.value_exn
    in
    (match after_snapshot.current_state with
     | Int 2 -> ()
     | _ -> failwith "634 actual policy did not advance restored state exactly once");
    let ports = !(counters.inference_ports) in
    let writer, _ =
      Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach parent.actor writer.id |> protocol_ok)
      (fun () ->
         phase := "explicit ordinary ensure-loaded/Resume";
         Owner.ensure_loaded_authenticated parent.runtime ~actor:original |> protocol_ok;
         phase := "explicit authenticated Actor.start";
         Actor.start ~actor:original parent.actor ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         phase := "drive genuine pending ordinary Resume";
         Factory.drain_idle_moderator (Daemon.factory daemon) parent
         |> protocol_ok
         |> ignore;
         phase := "observe settled ordinary Resume";
         let resumed = Warm.settled parent in
         [%test_eq: int]
           (saved.resume_count + 1)
           (List.length (Warm.completed resumed Session_resume));
         [%test_eq: int] ports !(counters.inference_ports);
         [%test_eq: int] saved.provider_count !(counters.providers);
         assert (Owner.is_loaded parent.runtime);
         let runtime = Warm.loaded parent in
         let native = Option.value_exn runtime.B.native_runtime in
         assert (Lazy.is_val native.capabilities);
         let manager = Option.value_exn runtime.moderator_manager in
         let actual =
           Chat_response.Moderator_manager.identity_snapshot manager
           |> Result.ok_or_failwith
         in
         let durable =
           A.Moderator_checkpoint.decode resumed.moderator
           |> protocol_ok
           |> Option.value_exn
         in
         assert (
           Jsonaf.exactly_equal
             (A.Moderator_checkpoint.encode actual)
             (A.Moderator_checkpoint.encode durable));
         (* The successful actual Resume already proves policy_seen was one:
            the source's Resume branch otherwise fails. Drain the captured head
            through the real ordinary entry, rather than racing a valid idle
            drainer with an intermediate checkpoint expectation. *)
         phase := "drain actual captured queue head";
         Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore;
         phase := "observe exact drained checkpoint and occurrence";
         let drained = Warm.settled parent in
         let snapshot =
           A.Moderator_checkpoint.decode drained.moderator
           |> protocol_ok
           |> Option.value_exn
         in
         (match snapshot.current_state with
          | Int 1012 -> ()
          | _ ->
            failwith
              "634 Resume/actual queued callback did not retain one module lifetime");
         assert (List.is_empty snapshot.queued_internal_events);
         let queue =
           A.Moderator_checkpoint.queue drained.moderator
           |> protocol_ok
           |> Option.value_exn
         in
         assert (List.is_empty (Queue.entries queue));
         [%test_eq: int64] (Queue.next_ordinal saved.queue) (Queue.next_ordinal queue);
         let queued =
           match Warm.completed drained Internal_event with
           | [ receipt ] -> receipt
           | _ ->
             failwith
               "634 explicit ordinary admission did not consume exactly one actual head"
         in
         let occurrence = Option.value_exn queued.queue_occurrence in
         assert (P.Run_source.equal saved.source (E.Queue_occurrence.source occurrence));
         [%test_eq: int64]
           (Queue.head_ordinal saved.queue)
           (E.Queue_occurrence.ordinal occurrence);
         (match E.Queue_occurrence.producer occurrence with
          | Captured actual ->
            assert (
              Option.equal
                A.Queue_producer.equal
                (Queue.Entry.producer (List.hd_exn (Queue.entries saved.queue)))
                actual)
          | Unknown -> failwith "634 actual queue consumption lost authentic producer");
         let later = Warm.loaded parent in
         assert (Option.equal phys_equal runtime.moderator_manager later.moderator_manager);
         assert (Option.equal phys_equal runtime.native_runtime later.native_runtime)))
;;

let check_policy_restore cold =
  let phase = ref "enter physical case" in
  match check_policy_restore_body ~phase cold with
  | () -> ()
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.traceln "634 physical case failed at phase: %s" !phase with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let%expect_test "physical stopped modern policy restores before explicit ordinary Resume" =
  check_policy_restore Stopped;
  print_endline
    "new physical daemon restored gracefully stopped policy and saved queue; actual \
     Caller and Resume observed one module lifetime; no early ordinary/provider work";
  [%expect
    {| new physical daemon restored gracefully stopped policy and saved queue; actual Caller and Resume observed one module lifetime; no early ordinary/provider work |}]
;;

let%expect_test
    "actually unloaded Running modern policy reconstructs without installing its worker"
  =
  check_policy_restore Running_unloaded;
  print_endline
    "actual Running Product physically closed before policy reconstruction; actual \
     Caller and Resume observed one new module lifetime; no early ordinary/provider work";
  [%expect
    {| actual Running Product physically closed before policy reconstruction; actual Caller and Resume observed one new module lifetime; no early ordinary/provider work |}]
;;
