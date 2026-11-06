open! Core
open Support
module Cold = Activation_tests
module Host = Agent_server.Recurrence_execution_host
module Configuration = Agent_server.Recurrence_execution_configuration
module Owner = Agent_server.Runtime_owner
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt

let denied = function
  | Error (error : P.Error.t) ->
    [%test_eq: P.Error.code] Permission_denied error.code;
    assert (not error.retryable)
  | Ok _ -> failwith "existing-target grant authorized a captured child"
;;

let frames env parent =
  let handle = Option.value_exn parent.Registry.store_handle in
  let journal =
    Agent_store.Journal.open_existing
      ~env
      ~directory:(S.Handle.journal_directory handle)
      ~max_payload_length:16777216
      ~max_segment_bytes:67108864L
      ~max_segment_frames:4096
    |> store_ok
  in
  (Agent_store.Journal.scan journal |> store_ok).entries
  |> List.map ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
;;

let parent_prompt =
  {|<developer>Actual configured captured-launch controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let child_prompt =
  {|<developer>Read the actual captured child's allowed local resource once.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="child" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("541-native.txt")}
  ])) in
  (match result with
  | `Ok(value) -> Task.pure(state + 1)
  | `Error(message) -> Task.fail(message))
| _ -> Task.pure(state)
</script>|}
;;

let await_native_startup child (admission : P.Run_receipt.t) =
  let selected =
    match (state child).invocations with
    | [ invocation ] -> invocation
    | invocations -> raise_s [%sexp (invocations : P.Invocation.t list)]
  in
  let expected_observation = Option.value_exn selected.observation in
  let rec await () =
    (* Native observation completion can publish no public event. Join its
       actual gate, release it before yielding for a not-yet-claimed observer. *)
    let current =
      Actor.with_moderator_checkpoint child.Registry.actor (fun () ->
        Actor.state child.actor)
      |> protocol_ok
    in
    let invocation =
      List.find_exn current.invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.context.id selected.context.id)
    in
    assert (P.Invocation.equal_context selected.context invocation.context);
    assert (P.Invocation.equal_status selected.status invocation.status);
    assert (
      Option.equal
        P.Inference_origin.equal
        selected.inference_origin
        invocation.inference_origin);
    let observation = Option.value_exn invocation.observation in
    assert (P.Invocation.equal_observer expected_observation.observer observation.observer);
    let observed =
      match observation.status with
      | Awaiting -> false
      | Observing | Observation_failed _ ->
        raise_s [%sexp "native observer did not complete", (invocation : P.Invocation.t)]
      | Observed ->
        (match observation.follow_up with
         | None | Some (Applied_follow_up _) -> true
         | Some
             ( Pending_follow_up _
             | Compaction_accepted_follow_up _
             | Discarded_follow_up _ ) ->
           raise_s [%sexp "unexpected native follow-up", (invocation : P.Invocation.t)])
    in
    let startup_completed =
      match current.moderator_executions with
      | [ receipt ] ->
        (match receipt.P.Moderator_execution.status with
         | Running -> false
         | Completed _ -> true
         | Failed _ | Interrupted _ ->
           raise_s [%sexp "captured startup failed", (receipt : P.Moderator_execution.t)])
      | receipts -> raise_s [%sexp (receipts : P.Moderator_execution.t list)]
    in
    let run =
      A.Run_state.find (Option.value_exn current.run_state) admission.run_id
      |> Option.value_exn
    in
    let completed =
      match run.lifecycle with
      | Terminal (Completed _) -> true
      | Admitted | Active | Waiting _ -> false
      | Terminal _ -> raise_s [%sexp "captured Run failed", (run : P.Run.t)]
    in
    if observed && startup_completed && completed
    then current
    else (
      Eio.Fiber.yield ();
      await ())
  in
  await ()
;;

let%expect_test
    "configured captured authority reaches the actual reservation, creation and startup"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let parent_file = Filename.concat root "parent.chatmd" in
         let child_file = Filename.concat root "child.chatmd" in
         List.iter
           [ parent_file, parent_prompt
           ; child_file, child_prompt
           ; Filename.concat root "541-native.txt", "541 actual captured native result"
           ]
           ~f:(fun (file, text) ->
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               Eio.Path.(Eio.Stdenv.fs env / file)
               text);
         let configuration = T.config root root parent_file in
         let prompt = List.hd_exn configuration.prompts in
         let configuration =
           { configuration with
             prompts =
               configuration.prompts
               @ [ { prompt with id = "cold-activation-target"; path = child_file } ]
           }
         in
         let counters : Cold.counters = { constructions = ref 0; providers = ref 0 } in
         with_daemon
           ~on_runtime_construction:(fun () -> Int.incr counters.constructions)
           env
           root
           configuration
           ~provider_calls:counters.providers
           (fun sw daemon ->
              let connection = T.connection daemon principal in
              protect_cleanup
                ~finally:(fun () -> Agent_client.Connection.close connection)
                (fun () ->
                   T.initialize connection;
                   let created, _ = T.create_session ~start_immediately:true connection in
                   let parent = loaded daemon created.id in
                   start_parent_settled env sw connection parent;
                   with_planner env sw daemon parent (fun () ->
                     let make captured_children =
                       let grant =
                         Configuration.Controller.create
                           ~session_id:created.id
                           ~existing_targets:
                             (if captured_children then [] else [ created.id ])
                           ~captured_children
                         |> protocol_ok
                       in
                       let policy =
                         Configuration.create
                           ~principal_id:
                             (P.Id.Principal.of_string "pri_541_captured" |> protocol_ok)
                           ~controllers:[ grant ]
                           ~expires_at:None
                         |> protocol_ok
                       in
                       Host.create
                         policy
                         ~factory:(Daemon.factory daemon)
                         ~server_id:(S.server_id (Daemon.store daemon))
                         ~sw
                         ~now:P.Timestamp.now
                       |> protocol_ok
                     in
                     let existing_only = make false in
                     let captured = make true in
                     protect_cleanup
                       ~finally:(fun () ->
                         Host.close existing_only;
                         Host.close captured)
                       (fun () ->
                          let denied_controller =
                            Host.controller existing_only parent |> protocol_ok
                          in
                          let controller =
                            Host.controller captured parent |> protocol_ok
                          in
                          let original = Host.Controller.actor controller in
                          assert (
                            P.Scope.Set.equal
                              (Operator_authorization.principal original).scopes
                              (P.Scope.Set.of_list
                                 [ Send_messages
                                 ; View_session_transcript
                                 ; Create_sessions
                                 ]));
                          Host.existing_target controller ~target:parent |> denied;
                          let linked = ref None in
                          let create_child parent target =
                            let nominal =
                              ready_nominal parent ~clock:(Eio.Stdenv.clock env) target
                            in
                            let before = state parent in
                            let before_frames = frames env parent in
                            let loaded =
                              (Registry.stats (Daemon.registry daemon)).loaded
                            in
                            let counts =
                              !(counters.constructions), !(counters.providers)
                            in
                            let definition = Entry.current (entry parent) in
                            Host.Controller.authorize_creation
                              denied_controller
                              before
                              definition
                            |> denied;
                            Actor.reserve_recurrence_child
                              parent.actor
                              ~actor:(Host.Controller.actor denied_controller)
                              ~authorize:
                                (Host.Controller.authorize_creation denied_controller)
                              ~occurrence:(A.Recurrence_nominal.key nominal)
                              ~expected_control_revision:
                                (Entry.control_revision (entry parent))
                            |> denied;
                            assert (
                              Jsonaf.exactly_equal
                                (T.state_document before |> store_ok |> D.Document.json)
                                (T.state_document (state parent)
                                 |> store_ok
                                 |> D.Document.json));
                            [%test_eq: string list] before_frames (frames env parent);
                            [%test_eq: int]
                              loaded
                              (Registry.stats (Daemon.registry daemon)).loaded;
                            [%test_eq: int * int]
                              counts
                              (!(counters.constructions), !(counters.providers));
                            let allowed =
                              Host.Controller.automatic_policy
                                controller
                                before
                                definition
                              |> protocol_ok
                            in
                            assert allowed;
                            let permit =
                              Actor.reserve_recurrence_child
                                parent.actor
                                ~actor:original
                                ~authorize:(Host.Controller.authorize_creation controller)
                                ~occurrence:(A.Recurrence_nominal.key nominal)
                                ~expected_control_revision:
                                  (Entry.control_revision (entry parent))
                              |> protocol_ok
                            in
                            let child =
                              Factory.create_recurrence_child
                                (Daemon.factory daemon)
                                ~parent
                                ~permit
                              |> protocol_ok
                            in
                            linked := Some child;
                            permit, child
                          in
                          Cold.with_cold_occurrence
                            ~catalog_target:true
                            ~create_child
                            env
                            daemon
                            parent
                            ~root_file:child_file
                            ~counters
                            ~original
                            (fun ~child ~intent ~attempt ~assert_dormant_effects ->
                               assert_dormant_effects ();
                               let selected =
                                 Host.captured_target
                                   controller
                                   ~child:(Option.value_exn !linked)
                                   ~target:child
                                 |> protocol_ok
                               in
                               assert (not (Owner.is_loaded child.runtime));
                               let target_actor = Host.Target.actor selected in
                               assert (
                                 P.Scope.Set.equal
                                   (Operator_authorization.principal target_actor).scopes
                                   (P.Scope.Set.of_list
                                      [ Send_messages; View_session_transcript ]));
                               let attempt =
                                 Attempt.create
                                   ~intent
                                   ~number:0
                                   ~principal_id:
                                     (Operator_authorization.principal target_actor).id
                                   ~expected_revision:(Attempt.expected_revision attempt)
                                   ~key:(Attempt.key attempt)
                                 |> protocol_ok
                               in
                               let execution =
                                 Host.Target.execution selected ~intent |> protocol_ok
                               in
                               let admission =
                                 Host.Execution.activate execution ~attempt
                                 |> Run_service_tests.activated child
                               in
                               let current = await_native_startup child admission in
                               let index = Option.value_exn current.run_state in
                               let run =
                                 A.Run_state.find index admission.run_id
                                 |> Option.value_exn
                               in
                               assert (
                                 P.Id.Principal.equal
                                   run.principal_id
                                   (Operator_authorization.principal original).id);
                               let origin =
                                 A.Run_state.recurrence_origin index run.id
                                 |> Option.value_exn
                               in
                               A.Recurrence_run_origin.validate origin ~intent ~attempt
                               |> protocol_ok;
                               A.Recurrence_run_origin.validate_run origin ~run ~admission
                               |> protocol_ok;
                               (match run.lifecycle with
                                | Terminal (Completed _) -> ()
                                | Admitted | Active | Waiting _ | Terminal _ ->
                                  raise_s [%sexp (run : P.Run.t)]);
                               let startup =
                                 match current.moderator_executions with
                                 | [ receipt ] -> receipt
                                 | receipts ->
                                   raise_s
                                     [%sexp (receipts : P.Moderator_execution.t list)]
                               in
                               assert (
                                 P.Moderator_execution.equal_phase
                                   Session_start
                                   startup.context.phase);
                               (match startup.status with
                                | Completed _ -> ()
                                | _ -> raise_s [%sexp (startup : P.Moderator_execution.t)]);
                               let invocation =
                                 match current.invocations with
                                 | [ invocation ] -> invocation
                                 | invocations ->
                                   raise_s [%sexp (invocations : P.Invocation.t list)]
                               in
                               [%test_eq: string] "read_file" invocation.context.tool_name;
                               assert (
                                 Option.equal
                                   P.Id.Moderator_execution.equal
                                   invocation.parent_event
                                   (Some startup.context.id));
                               assert (
                                 Option.equal
                                   P.Inference_origin.equal
                                   invocation.inference_origin
                                   (Some (Run run.id)));
                               (match invocation.status with
                                | Resolved (Complete value) | Published (Complete value)
                                  ->
                                  assert (
                                    String.is_substring
                                      (Jsonaf.to_string value)
                                      ~substring:"541 actual captured native result")
                                | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
                               assert (
                                 P.Invocation.equal_observer
                                   startup.context.source
                                   run.source.observer);
                               [%test_eq: int]
                                 run.source.generation
                                 startup.context.generation;
                               [%test_eq: int] 0 !(counters.providers);
                               assert (List.is_empty current.jobs))))))));
  print_endline
    "existing-target authority cannot reserve a captured child; complete confirmed state \
     and journal remain unchanged";
  print_endline
    "the configured creation original produces one real child and same-Run native \
     startup through the public service";
  [%expect
    {|
    existing-target authority cannot reserve a captured child; complete confirmed state and journal remain unchanged
    the configured creation original produces one real child and same-Run native startup through the public service
    |}]
;;
