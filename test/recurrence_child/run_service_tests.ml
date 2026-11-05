open! Core
open Support
module Cold = Activation_tests
module Service = Agent_server.Recurrence_run_service
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Res = Openai.Responses

let activated child = function
  | Error error -> Cold.authored_start_result child ~case:V1_children (Error error)
  | Ok (Service.Outcome.Attempted attempt) ->
    assert (Option.is_none (Service.Attempt.no_effects attempt));
    assert (Option.is_none (Service.Attempt.retry attempt));
    (match Service.Attempt.result attempt with
     | Ok (Activated admission) -> admission
     | Error error | Ok (Activation_failed { error; _ }) ->
       Cold.authored_start_result child ~case:V1_children (Error error)
     | Ok (Retained _ | Admission_uncertain _) ->
       failwith "fresh captured public startup did not acknowledge its handoff")
  | Ok (Retained _) -> failwith "fresh captured public startup was already retained"
;;

let row parent intent =
  Entry.frontier (entry parent)
  |> protocol_ok
  |> fun frontier ->
  Frontier.find_execution frontier (Intent.occurrence intent) |> Option.value_exn
;;

let selected_run child intent attempt =
  let current = state child in
  let index = Option.value_exn current.run_state in
  let admission =
    A.Run_state.receipt
      index
      ~principal_id:(Attempt.principal_id attempt)
      ~key:(Attempt.key attempt)
      ~request_sha256:(Attempt.request_sha256 attempt)
    |> protocol_ok
    |> Option.value_exn
  in
  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
  let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
  A.Recurrence_run_origin.validate origin ~intent ~attempt |> protocol_ok;
  A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
  assert (P.Run.Mode.equal run.mode Workflow);
  (match run.lifecycle with
   | Admitted | Active -> ()
   | Waiting _ | Terminal _ ->
     raise_s [%sexp "public Workflow ended without Finish", (run : P.Run.t)]);
  assert (
    not
      (List.exists (A.Run_state.receipts index) ~f:(fun receipt ->
         P.Id.Run.equal run.id receipt.run_id
         && P.Run_receipt.Kind.equal receipt.kind Terminal)));
  current, admission, run
;;

let%expect_test "public service activates a distinct captured rich authored Workflow" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Mirage_crypto_rng_unix.use_default ();
      let root = T.temporary_root env in
      protect_cleanup
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           let parent_file = Filename.concat root "parent.chatmd" in
           let target_file = Filename.concat root "public-authored.chatmd" in
           List.iter
             [ ( parent_file
               , {|<developer>Actual public recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
               )
             ; target_file, Cold.Authored_case.prompt V1_children
             ; ( Filename.concat root "activation-recipe.chatmd"
               , "<developer>Return the offline fixture result.</developer>" )
             ; ( Filename.concat root "activation-native.txt"
               , "520 actual allowed native result" )
             ]
             ~f:(fun (path, text) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / path)
                 text);
           let counters : Cold.counters = { constructions = ref 0; providers = ref 0 } in
           let entered, entered_u = Eio.Promise.create () in
           let release, release_u = Eio.Promise.create () in
           let release_provider () =
             if Option.is_none (Eio.Promise.peek release)
             then Eio.Promise.resolve release_u ()
           in
           let nested = ref 0 in
           let policy =
             T.inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs ->
                 Int.incr counters.providers;
                 let transcript =
                   `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
                 in
                 if
                   String.is_substring transcript ~substring:"520_START_FIRST"
                   && Option.is_none (Eio.Promise.peek entered)
                 then (
                   Eio.Promise.resolve entered_u ();
                   Eio.Promise.await release);
                 Cold.completed_response
                   (if String.is_substring transcript ~substring:"536_NATIVE_CHILD"
                    then (
                      Int.incr nested;
                      "536-native-child-result")
                    else "520-constructor-model-result"))
           in
           let configuration = T.config root root parent_file in
           let configured = List.hd_exn configuration.prompts in
           let configuration =
             { configuration with
               prompts =
                 configuration.prompts
                 @ [ { configured with id = "cold-activation-target"; path = target_file }
                   ]
             }
           in
           with_daemon
             ~policy
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
                     let created, _ =
                       T.create_session ~start_immediately:true connection
                     in
                     let parent = loaded daemon created.id in
                     start_parent_settled env sw connection parent;
                     with_planner env sw daemon parent (fun () ->
                       Cold.with_cold_occurrence
                         ~mode:Workflow
                         ~catalog_target:true
                         env
                         daemon
                         parent
                         ~root_file:target_file
                         ~counters
                         ~original:proof
                         (fun ~child ~intent ~attempt ~assert_dormant_effects ->
                            assert_dormant_effects ();
                            assert (not (phys_equal child.actor parent.actor));
                            let baseline =
                              !(counters.constructions), !(counters.providers)
                            in
                            let service =
                              Service.create
                                ~factory:(Daemon.factory daemon)
                                ~authorize:(fun _ current -> authorize current)
                                ~automatic_policy:(fun _ _ -> Ok true)
                            in
                            Eio.Switch.run (fun run_sw ->
                              protect_cleanup ~finally:release_provider (fun () ->
                                let starting =
                                  Eio.Fiber.fork_promise ~sw:run_sw (fun () ->
                                    Service.activate
                                      service
                                      ~controller:parent
                                      ~controller_actor:proof
                                      ~target:child
                                      ~target_actor:proof
                                      ~intent
                                      ~attempt)
                                in
                                let rec await_provider () =
                                  match Eio.Promise.peek entered with
                                  | Some () -> ()
                                  | None ->
                                    (match Eio.Promise.peek starting with
                                     | Some _ ->
                                       ignore
                                         (activated child (Eio.Promise.await_exn starting)
                                          : P.Run_receipt.t);
                                       failwith
                                         "public authored startup returned before its \
                                          first provider"
                                     | None ->
                                       Eio.Fiber.yield ();
                                       await_provider ())
                                in
                                await_provider ();
                                (* The provider has no assertions: inspect actual persisted
                         reciprocal evidence outside its exception converter. *)
                                let current, admission, run =
                                  selected_run child intent attempt
                                in
                                let associated =
                                  Execution.current_attempt (row parent intent)
                                in
                                assert (
                                  Attempt.equal
                                    attempt
                                    (Execution.Attempt.request associated));
                                let origin =
                                  Option.value_exn (Execution.Attempt.origin associated)
                                in
                                A.Recurrence_run_origin.validate_run
                                  origin
                                  ~run
                                  ~admission
                                |> protocol_ok;
                                [%test_eq: int] (snd baseline + 1) !(counters.providers);
                                assert (Option.is_none current.active_operation);
                                assert (List.is_empty current.invocations);
                                release_provider ();
                                let returned =
                                  Eio.Promise.await_exn starting |> activated child
                                in
                                assert (P.Run_receipt.equal admission returned);
                                Cold.await_authored_observations
                                  child
                                  ~client_error:(ref None);
                                let current, actual, run =
                                  selected_run child intent attempt
                                in
                                assert (P.Run_receipt.equal returned actual);
                                assert (Option.is_none current.active_operation);
                                [%test_eq: int] 2 (List.length current.jobs);
                                List.iter current.jobs ~f:(fun job ->
                                  assert (P.Job.equal_kind job.kind Model_call);
                                  [%test_eq: int] 1 job.attempt;
                                  assert (
                                    Option.equal
                                      P.Inference_origin.equal
                                      job.inference_origin
                                      (Some (Run run.id)));
                                  (match job.status with
                                   | Succeeded -> ()
                                   | _ -> raise_s [%sexp (job : P.Job.t)]);
                                  let work =
                                    A.Run_job_occurrence.work job |> protocol_ok
                                  in
                                  assert (
                                    List.exists run.terminal_work ~f:(fun proof ->
                                      P.Run_work.equal proof.work work
                                      && P.Run_work.Terminal.equal_outcome
                                           proof.outcome
                                           Succeeded)));
                                [%test_eq: int] 2 (List.length current.invocations);
                                let startup =
                                  match current.moderator_executions with
                                  | [ startup ] -> startup
                                  | receipts ->
                                    raise_s
                                      [%sexp (receipts : P.Moderator_execution.t list)]
                                in
                                assert (
                                  P.Moderator_execution.equal_phase
                                    startup.context.phase
                                    Session_start);
                                (match startup.status with
                                 | Completed _ -> ()
                                 | _ ->
                                   raise_s [%sexp (startup : P.Moderator_execution.t)]);
                                List.iter current.invocations ~f:(fun invocation ->
                                  assert (
                                    Option.equal
                                      P.Inference_origin.equal
                                      invocation.inference_origin
                                      (Some (Run run.id)));
                                  assert (
                                    Option.equal
                                      P.Id.Moderator_execution.equal
                                      invocation.parent_event
                                      (Some startup.context.id));
                                  let marker =
                                    match invocation.context.tool_name with
                                    | "read_file" -> "520 actual allowed native result"
                                    | "recipe_source" -> "536-native-child-result"
                                    | name ->
                                      raise_s
                                        [%sexp
                                          "unexpected public authored native"
                                        , (name : string)]
                                  in
                                  match invocation.status with
                                  | Resolved (Complete result)
                                  | Published (Complete result) ->
                                    assert (
                                      String.is_substring
                                        (Jsonaf.to_string result)
                                        ~substring:marker)
                                  | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
                                [%test_eq: int] 1 !nested;
                                [%test_eq: int] (snd baseline + 3) !(counters.providers);
                                let counts =
                                  !(counters.constructions), !(counters.providers)
                                in
                                (match
                                   Service.activate
                                     service
                                     ~controller:parent
                                     ~controller_actor:proof
                                     ~target:child
                                     ~target_actor:proof
                                     ~intent
                                     ~attempt
                                   |> protocol_ok
                                 with
                                 | Retained result ->
                                   assert (P.Run_receipt.equal returned result.admission);
                                   let imported = result.reconciliation |> protocol_ok in
                                   (match imported.timing with
                                    | No_successor -> ()
                                    | Live_phase | Recovery_adoption ->
                                      failwith
                                        "nonterminal Workflow adopted a completion phase"
                                    | Held error -> raise_s [%sexp (error : P.Error.t)]);
                                   (match imported.settlement with
                                    | Retained_exact execution ->
                                      assert (
                                        Execution.equal execution (row parent intent))
                                    | Imported _ | Detail_not_retained _ ->
                                      failwith
                                        "public Workflow replay changed retained evidence")
                                 | Attempted _ ->
                                   failwith "public authored replay acquired a new owner");
                                assert (
                                  Jsonaf.exactly_equal
                                    (T.state_document current
                                     |> store_ok
                                     |> D.Document.json)
                                    (T.state_document (state child)
                                     |> store_ok
                                     |> D.Document.json));
                                [%test_eq: int * int]
                                  counts
                                  (!(counters.constructions), !(counters.providers));
                                print_endline
                                  "distinct captured Workflow uses the real public \
                                   authored consumer";
                                print_endline
                                  "reciprocal ACK precedes Model/native/nested effects; \
                                   retained replay starts nothing")))))))));
  [%expect
    {|
    distinct captured Workflow uses the real public authored consumer
    reciprocal ACK precedes Model/native/nested effects; retained replay starts nothing
    |}]
;;
