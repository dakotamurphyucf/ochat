open! Core
open Agent_server_test_support
open Retry_support
module Checkpoint = A.Moderator_checkpoint

type mode =
  | Resume_then_halt
  | Unavailable_tool
[@@deriving sexp]

let prompt mode =
  sprintf
    {|<developer>Actual installed legacy recovery lifecycle.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state + 1)
| `Session_resume -> %s
| _ -> Task.pure(state)
</script>|}
    (match mode with
     | Resume_then_halt ->
       {|if state == 1 then Task.pure(state + 1)
  else Task.bind(Runtime.end_session("installed resume stops before provider"), fun ignored -> Task.pure(state + 1))|}
     | Unavailable_tool ->
       {|Task.bind(Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("resume-only.txt")}
  ])), fun result -> match result with
  | `Error(message) -> Task.fail(message)
  | `Ok(value) -> Task.pure(state + 1))|})
;;

let receipts current phase =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Moderator_execution.equal_phase receipt.P.Moderator_execution.context.phase phase)
;;

let checkpoint current =
  Checkpoint.decode current.A.Session_state.moderator |> protocol_ok |> Option.value_exn
;;

let runtime entry =
  Owner.with_loaded_runtime entry.Registry.runtime (fun runtime -> Ok runtime)
  |> protocol_ok
;;

let activation entry =
  Option.value_exn (runtime entry).A.Runtime_builder.moderator_activation
;;

let assert_original before current =
  let original = List.hd_exn (receipts before Session_start) in
  assert (
    List.exists current.A.Session_state.moderator_executions ~f:(fun candidate ->
      P.Moderator_execution.equal original candidate));
  [%test_eq: int] 1 (List.length (receipts current Session_start));
  List.iter (receipts current Session_resume) ~f:(fun resume ->
    assert (P.Invocation.equal_observer original.context.source resume.context.source))
;;

let assert_same before after =
  assert (
    Jsonaf.exactly_equal
      (state_document before |> Existing.store_ok |> Document_schema.Document.json)
      (state_document after |> Existing.store_ok |> Document_schema.Document.json))
;;

(* Operator_only is used only as an existing trusted lower-host composition,
   permitting deterministic manual calls to the genuine Factory recovery and
   Owner lease APIs. The separate552 host-lifetime case starts actual Execute
   after physical shutdown; it remains the scheduler integration regression. *)
let with_recovered mode f =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "installed resume must stop or reject before any provider")
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~prompt_text:(prompt mode)
    ~sources:[ "resume-only.txt", "unavailable original must not read this" ]
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         let old_actor, before =
           Goal_job_runtime_tests.with_loaded_manual_entry daemon (fun entry _ ->
             let before = state entry in
             [%test_eq: int] 1 (List.length (receipts before Session_start));
             [%test_eq: int] 0 (List.length (receipts before Session_resume));
             entry.actor, before)
         in
         let old_store = Daemon.store daemon in
         Daemon.shutdown daemon |> protocol_ok;
         assert (Agent_store.Session_store.is_closed old_store);
         let reopened =
           Daemon.start
             ~sw
             ~env:(env :> Eio_unix.Stdenv.base)
             ~config:(config root root (Filename.concat root "parent.chatmd"))
             ~tool_dir:root
             ~home:root
             ~process_start_identity:None
             ~options:
               { Daemon.default_options with
                 inference_policy = policy
               ; startup_mode = Operator_only
               }
             ()
           |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
           (fun () ->
              let indexed =
                Agent_store.Session_store.list_sessions_checked (Daemon.store reopened)
                |> Existing.store_ok
                |> List.find_exn ~f:(fun indexed ->
                  P.Id.Session.equal
                    indexed.Agent_store.Session_index.Entry.session.id
                    before.identity.session_id)
              in
              let recovered =
                Factory.recover_session (Daemon.factory reopened) indexed |> protocol_ok
              in
              (match
                 Registry.add
                   (Daemon.registry reopened)
                   ~session_id:before.identity.session_id
                   recovered
               with
               | Ok () -> ()
               | Error error ->
                 Agent_server.Session_registry.close_entry recovered;
                 raise_s [%sexp (error : P.Error.t)]);
              protect_cleanup
                ~finally:(fun () -> Agent_server.Session_registry.close_entry recovered)
                (fun () ->
                   assert (not (phys_equal old_actor recovered.actor));
                   assert (Owner.is_loaded recovered.runtime);
                   let pending = activation recovered in
                   assert (pending.pending ());
                   assert (not (pending.startup_pending ()));
                   let current = state recovered in
                   assert_original before current;
                   [%test_eq: int] 0 (List.length (receipts current Session_resume));
                   [%test_eq: int] 0 !providers;
                   f env recovered before providers))))
;;

let%expect_test
    "real legacy recovery resumes once; unavailable original stays unavailable"
  =
  with_recovered Resume_then_halt (fun _env recovered before providers ->
    (* This actual legacy Installed_resume is still pending after the Factory
       source stage has cleared. Its callback claims the ordinary installed
       owner, rather than retaining a constructor or joining its own stage. *)
    assert (Owner.source_lane_idle recovered.runtime);
    assert (Owner.drain_idle_moderator recovered.runtime |> protocol_ok);
    assert (Owner.source_lane_idle recovered.runtime);
    let completed = state recovered in
    assert_original before completed;
    (match receipts completed Session_resume with
     | [ { status = Completed _; _ } ] -> ()
     | actual -> raise_s [%sexp (actual : P.Moderator_execution.t list)]);
    assert (not ((activation recovered).pending ()));
    assert (not (Owner.drain_idle_moderator recovered.runtime |> protocol_ok));
    assert_same completed (state recovered);
    [%test_eq: int] 0 !providers;
    (* A second physical runtime from the SAME live Owner is initialized Ready.
       Do not drain its pending resume: the actual foreground handler must join
       it under this new Operation before reaching any provider. *)
    Owner.unload_and_wait recovered.runtime |> protocol_ok;
    Owner.ensure_loaded recovered.runtime |> protocol_ok;
    assert (Owner.source_lane_idle recovered.runtime);
    let resumed = activation recovered in
    assert (resumed.pending ());
    let writer, _ =
      Actor.attach recovered.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach recovered.actor writer.id |> protocol_ok)
      (fun () ->
         let id = A.History_id_source.allocate recovered.history_ids |> protocol_ok in
         let message =
           Owner.parse_user_content
             recovered.runtime
             ~id
             { kind = Plain_text
             ; text = "actual foreground after Ready reload"
             ; attachments = []
             }
           |> protocol_ok
           |> A.History_codec.to_protocol
         in
         let submission =
           Actor.submit_message_authenticated
             recovered.actor
             ~actor:original
             ~attachment_id:writer.id
             message
           |> protocol_ok
         in
         assert (Option.is_some submission.operation_id);
         let rec await () =
           let current =
             Actor.with_moderator_checkpoint recovered.actor (fun () ->
               Actor.state recovered.actor)
             |> protocol_ok
           in
           List.iter (receipts current Session_resume) ~f:(fun receipt ->
             match receipt.status with
             | Failed _ | Interrupted _ ->
               raise_s [%sexp (receipt : P.Moderator_execution.t)]
             | Running | Completed _ -> ());
           if Option.is_none current.active_operation && (checkpoint current).halted
           then current
           else (
             Eio.Fiber.yield ();
             await ())
         in
         let final = await () in
         assert_original before final;
         [%test_eq: int] 2 (List.length (receipts final Session_resume));
         (* The stopped runtime is retiring; inspect this same activation's
            factual completion instead of acquiring a new execution lease. *)
         assert (not (resumed.pending ()));
         [%test_eq: int] 0 !providers));
  with_recovered Unavailable_tool (fun _env recovered before providers ->
    let prior = state recovered in
    let error =
      match Owner.drain_idle_moderator recovered.runtime with
      | Ok _ -> failwith "recovery fabricated original Tool authority"
      | Error error -> error
    in
    (* Tool.call deliberately sanitizes the native admission refusal; the
       failed Event and absence of an Invocation prove no fresh tool owner. *)
    [%test_eq: P.Error.code] Invalid_state error.code;
    [%test_eq: string] "invocation.admission_failed" error.message;
    assert (not error.retryable);
    let failed = state recovered in
    assert_original before failed;
    (match receipts failed Session_resume with
     | [ { status = Failed _; _ } ] -> ()
     | actual -> raise_s [%sexp (actual : P.Moderator_execution.t list)]);
    assert (List.equal P.Invocation.equal prior.invocations failed.invocations);
    assert (Option.equal Jsonaf.exactly_equal prior.moderator failed.moderator);
    [%test_eq: int] 0 !providers;
    let repeated =
      Owner.with_loaded_runtime recovered.runtime (fun runtime ->
        (Option.value_exn runtime.A.Runtime_builder.moderator_activation).run ())
    in
    (match repeated with
     | Error repeated ->
       assert (Jsonaf.exactly_equal (P.Error.to_json error) (P.Error.to_json repeated))
     | Ok _ -> failwith "failed installed resume was replayed");
    assert_same failed (state recovered));
  print_endline
    "post-install resume ACK is once per runtime; foreground halt precedes provider; no \
     restored original";
  [%expect
    {| post-install resume ACK is once per runtime; foreground halt precedes provider; no restored original |}]
;;

type cold_start_race =
  | Timer_claim
  | Checkpoint_change

let cold_start_with_ordinary_timer ~connect race =
  let prompt_text =
    {|<developer>Schedule one ordinary legacy wake.</developer>
<script id="owner" language="chatml" kind="moderator">
type event = [ `Session_start | `Session_resume | `Wake ]
let initial_state = 0
let on_event : context -> int -> event -> int task = fun ctx state event ->
  match event with
  | `Session_start ->
    Task.bind(Schedule.after_ms(0, `Wake), fun ignored -> Task.pure(state + 1))
  | `Session_resume -> Task.pure(state)
  | `Wake -> Task.pure(state + 10)
</script>|}
  in
  let module Polling = Ordinary_warm_resource_tests.Polling in
  let polling = Polling.create () in
  let before_ports = ref ignore in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      failwith "legacy timer entered a provider")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          !before_ports actor;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~wrap_env:(Polling.wrap polling)
    ~policy
    (fun env _sw _root daemon ->
       protect_cleanup
         ~finally:(fun () -> polling.resume ())
         (fun () ->
            Eio.Time.with_timeout_exn env#clock 30. (fun () ->
              Polling.park polling daemon;
              let client = connect daemon in
              protect_cleanup
                ~finally:(fun () -> Agent_client.Connection.close client)
                (fun () ->
                   initialize client;
                   let session, writer = create_session client in
                   let entry =
                     Registry.find (Daemon.registry daemon) session.id |> Option.value_exn
                   in
                   let ready = state entry in
                   assert (not (Owner.is_loaded entry.runtime));
                   (match ready.runtime_initialization, ready.lifecycle.observed with
                    | Ready, Stopped -> ()
                    | _ -> failwith "public Create did not retain a cold Ready root");
                   let scheduled =
                     match Actor.due_schedules entry.actor |> protocol_ok with
                     | Stopped, [ scheduled ] -> scheduled
                     | _ -> failwith "creation did not persist one due timer"
                   in
                   assert (Option.is_none scheduled.ownership);
                   let claim () =
                     Actor.claim_schedule
                       entry.actor
                       ~schedule_id:scheduled.id
                       ~generation:scheduled.generation
                     |> protocol_ok
                   in
                   let claimed = ref None in
                   let replacement_checkpoint = ref None in
                   (* The normal inference-port supplier runs after selecting the cold
                      State and opening any constructor scope, before source install. *)
                   (before_ports
                    := fun actor ->
                         assert (phys_equal actor entry.actor);
                         before_ports := ignore;
                         let expected = state entry in
                         claimed := Some (Option.value_exn (claim ()));
                         let current = state entry in
                         let same_basis current =
                           A.Session_state.same_ordinary_ready_reload_basis
                             current
                             ~expected
                         in
                         assert (same_basis current);
                         assert (
                           not
                             (same_basis
                                { current with
                                  stop_epoch = Int64.succ current.stop_epoch
                                }));
                         let counters =
                           { current.counters with
                             owner_lease_generation =
                               Int64.succ current.counters.owner_lease_generation
                           }
                         in
                         assert (not (same_basis { current with counters }));
                         match race with
                         | Timer_claim -> ()
                         | Checkpoint_change ->
                           let previous = Option.value_exn current.moderator in
                           let checkpoint =
                             Checkpoint.of_jsonaf previous |> protocol_ok
                           in
                           let snapshot = Checkpoint.snapshot checkpoint in
                           let next =
                             Checkpoint.update
                               checkpoint
                               ~snapshot:{ snapshot with current_state = Int 42 }
                               ~queue:Keep
                             |> protocol_ok
                             |> Checkpoint.to_jsonaf
                           in
                           Actor.change_moderator actor (Some next)
                           |> protocol_ok
                           |> ignore;
                           replacement_checkpoint := Some next);
                   let result =
                     Agent_client.Connection.request_without_history
                       client
                       (Session_start
                          { session_id = session.id
                          ; attachment_id = writer.id
                          ; queue_if_limited = false
                          ; idempotency_key =
                              P.Idempotency_key.of_string "cold-start-with-ordinary-timer"
                              |> protocol_ok
                          })
                   in
                   let claimed = Option.value_exn !claimed in
                   match race with
                   | Checkpoint_change ->
                     let failure =
                       match result with
                       | Error failure -> failure
                       | Ok _ ->
                         failwith "cold Start installed a stale moderator checkpoint"
                     in
                     assert (P.Error.equal_code failure.code Conflict);
                     assert (not (Owner.is_loaded entry.runtime));
                     let after = state entry in
                     assert (Option.is_some !replacement_checkpoint);
                     assert (
                       Option.equal
                         Jsonaf.exactly_equal
                         after.moderator
                         !replacement_checkpoint);
                     print_s
                       [%sexp
                         (failure.code : P.Error.code)
                       , (after.lifecycle.desired : P.Session.desired_state)
                       , ((checkpoint after).current_state : Session.Snapshot.t)]
                   | Timer_claim ->
                     let started =
                       match result |> protocol_ok with
                       | Session_start result -> result.session
                       | _ -> failwith "public Session_start returned a different result"
                     in
                     let drain () =
                       Factory.drain_idle_moderator (Daemon.factory daemon) entry
                       |> protocol_ok
                     in
                     (* The scoped constructor may already have settled resume. *)
                     ignore (drain () : bool);
                     (match (checkpoint (state entry)).current_state with
                      | Int 1 -> ()
                      | _ -> failwith "unexpected checkpoint before timer wake");
                     Owner.deliver_schedule entry.runtime claimed |> protocol_ok;
                     ignore (drain () : bool);
                     assert (Option.is_none (claim ()));
                     assert (not (drain ()));
                     let delivered = state entry in
                     assert_original ready delivered;
                     (match receipts delivered Session_resume with
                      | [ { status = Completed _; _ } ] -> ()
                      | _ -> failwith "cold Start did not retain exactly one resume ACK");
                     let timer =
                       match delivered.schedules with
                       | [ ({ id; status = Delivered; _ } as timer) ] ->
                         assert (P.Id.Schedule.equal id scheduled.id);
                         timer
                       | schedules -> raise_s [%sexp (schedules : P.Schedule.t list)]
                     in
                     let snapshot = checkpoint delivered in
                     assert (List.is_empty snapshot.queued_internal_events);
                     print_s
                       [%sexp
                         (started.desired_state : P.Session.desired_state)
                       , (timer.delivery_count : int)
                       , (snapshot.current_state : Session.Snapshot.t)]))))
;;

let connect_authenticated daemon =
  Goal_recurrence_host_support.operator_connection daemon principal
;;

let%expect_test
    "cold public Start preserves an ordinary timer claimed during construction"
  =
  cold_start_with_ordinary_timer
    ~connect:(fun daemon -> connection daemon principal)
    Timer_claim;
  [%expect {| (Running 1 (Int 11)) |}];
  cold_start_with_ordinary_timer ~connect:connect_authenticated Timer_claim;
  [%expect {| (Running 1 (Int 11)) |}]
;;

let%expect_test "authenticated cold Start rejects a changed moderator checkpoint" =
  cold_start_with_ordinary_timer ~connect:connect_authenticated Checkpoint_change;
  [%expect {| (Conflict Stopped (Int 42)) |}]
;;
