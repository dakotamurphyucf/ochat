(* The scoped runtime consumer deliberately interrupts on return. This test
   qualifies real constructor effects and cleanup, without claiming a final
   successful Run handoff. *)
open! Core
open Support
module Owner = Agent_server.Runtime_owner
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Held = Actor.Held_run
module Activation = Actor.Recurrence_activation
module Res = Openai.Responses

type counters =
  { constructions : int ref
  ; providers : int ref
  }

let completed_response text =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = "520-constructor-result"
    ; status = "completed"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_text_delta
      { item_id = message.id
      ; output_index = 0
      ; content_index = 0
      ; delta = text
      ; type_ = "response.output_text.delta"
      }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let cold_prompt =
  {|<developer>Actual recurrence constructor Model fixture.</developer>
<tool name="recipe_source" agent="activation-recipe.chatmd" local/>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="controller" language="chatml" kind="moderator">
let initial_state = 0
let payload input = `Object([
  {key = "prompt"; value = `String("activation-recipe.chatmd")},
  {key = "input"; value = `String(input)},
  {key = "is_local"; value = `Bool(true)}
])
let on_event ctx state event = match event with
| `Session_start ->
  let* first = Model.call("agent_prompt_v1", payload("520_START_FIRST")) in
  (match first with
  | `Ok(value) ->
    let* native = Tool.call("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("activation-native.txt")}
    ])) in
    (match native with
    | `Ok(value) ->
      let* second = Model.call("agent_prompt_v1", payload("520_START_SECOND")) in
      (match second with
      | `Ok(value) -> Task.pure(state + 1)
      | `Refused(message) -> Task.fail(message)
      | `Error(message) -> Task.fail(message))
    | `Error(message) -> Task.fail(message))
  | `Refused(message) -> Task.fail(message)
  | `Error(message) -> Task.fail(message))
| _ -> Task.pure(state)
</script>|}
;;

(* before_response is unarmed during real parent startup. Once the genuine cold
   child has Held+reciprocal ACK+Checked, arm it. At the second actual transport
   entry resolve a Promise and wait on a release Promise. Outer test inspects
   actual child Job/Run/Event there, then releases. This is the honest fallback
   if the source's cold constructor remains Stopped and Ask is correctly denied. *)
let with_second_response_barrier ~armed ~entered ~release transcript =
  if !armed && String.is_substring transcript ~substring:"520_START_SECOND"
  then (
    Eio.Promise.resolve entered ();
    Eio.Promise.await release)
;;

(* Called OUTSIDE provider/Manager converters after the actual second-provider
   Promise. Runtime source uses the real default Allow/read-only resource; no
   permission is altered just to create the observation point. *)
let assert_constructor_prefix child held =
  let current = state child in
  let admission = Held.admission held in
  let index = Option.value_exn current.run_state in
  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
  assert (P.Run_source.equal run.source admission.source);
  (match run.lifecycle with
   | Admitted | Active | Waiting _ -> ()
   | Terminal _ -> failwith "first constructor child prematurely settled held Run");
  assert (
    not
      (List.exists (A.Run_state.receipts index) ~f:(fun receipt ->
         P.Id.Run.equal receipt.run_id run.id
         && P.Run_receipt.Kind.equal receipt.kind Terminal)));
  let model input =
    match
      List.filter current.jobs ~f:(fun job ->
        P.Job.equal_kind job.kind Model_call
        && Option.equal
             Jsonaf.exactly_equal
             (Jsonaf.member "payload" job.payload
              |> Option.bind ~f:(Jsonaf.member "input"))
             (Some (`String input)))
    with
    | [ job ] -> job
    | jobs ->
      raise_s [%sexp "missing exact actual constructor Job", (jobs : P.Job.t list)]
  in
  let first = model "520_START_FIRST" in
  let second = model "520_START_SECOND" in
  [%test_eq: int] 1 first.attempt;
  (match first.status, second.status with
   | Succeeded, Running -> ()
   | _ -> raise_s [%sexp (first : P.Job.t), (second : P.Job.t)]);
  List.iter [ first; second ] ~f:(fun job ->
    assert (Option.equal P.Inference_origin.equal job.inference_origin (Some (Run run.id))));
  assert (
    Option.equal
      Jsonaf.exactly_equal
      (Option.bind first.result ~f:(Jsonaf.member "final_text"))
      (Some (`String "520-constructor-model-result")));
  let first_work = A.Run_job_occurrence.work first |> protocol_ok in
  (match
     List.filter run.terminal_work ~f:(fun proof ->
       P.Run_work.equal proof.work first_work)
   with
   | [ proof ] -> assert (P.Run_work.Terminal.equal_outcome proof.outcome Succeeded)
   | proofs ->
     raise_s
       [%sexp
         "missing acknowledged first Model proof", (proofs : P.Run_work.Terminal.t list)]);
  let native =
    match
      List.filter current.invocations ~f:(fun invocation ->
        String.equal invocation.context.tool_name "read_file")
    with
    | [ invocation ] -> invocation
    | invocations ->
      raise_s
        [%sexp "missing actual constructor native", (invocations : P.Invocation.t list)]
  in
  assert (
    Option.equal P.Inference_origin.equal native.inference_origin (Some (Run run.id)));
  (match native.status with
   | Resolved (Complete result) | Published (Complete result) ->
     assert (
       String.is_substring
         (Jsonaf.to_string result)
         ~substring:"520 actual allowed native result")
   | _ -> raise_s [%sexp (native : P.Invocation.t)]);
  let parent = Option.value_exn native.parent_event in
  let startup =
    List.find_exn current.moderator_executions ~f:(fun receipt ->
      P.Id.Moderator_execution.equal receipt.context.id parent)
  in
  assert (P.Moderator_execution.equal_phase startup.context.phase Session_start);
  assert (P.Invocation.equal_observer startup.context.source run.source.observer);
  [%test_eq: int] startup.context.generation run.source.generation;
  match startup.status with
  | Running -> ()
  | _ -> raise_s [%sexp (startup : P.Moderator_execution.t)]
;;

let unchanged_effects counters ~baseline (before : A.Session_state.t) child =
  let after = state child in
  [%test_eq: int * int] baseline (!(counters.constructions), !(counters.providers));
  assert (not (Owner.is_loaded child.runtime));
  assert (Option.is_none after.active_operation);
  assert (List.is_empty after.jobs);
  assert (List.is_empty after.invocations);
  assert (List.is_empty after.moderator_executions);
  assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
  assert (
    List.equal
      P.History.equal_entry
      before.conversation.canonical_history
      after.conversation.canonical_history);
  [%test_eq: int64]
    before.conversation.next_history_sequence
    after.conversation.next_history_sequence;
  [%test_eq: int64]
    before.conversation.reserved_history_through
    after.conversation.reserved_history_through
;;

(* The real controller is v1, as required by the child Factory's parent
   moderation gate. The target is a separately captured legacy authored closure,
   not a changed controller Manager or a copied observer. *)
let cold_target env daemon parent ~root_file =
  let current = state parent in
  let definition =
    A.Prompt_definition.create
      ~id:(Option.value_exn current.spec.prompt_definition_id)
      ~config_name:"cold-activation-target"
      ~root_file
      ~root_invocation:current.spec.root_invocation
      ~allowed_workspaces:(Option.to_list current.spec.workspace_instance.definition_id)
      ~permission_profile:current.spec.permission_profile
      ~runtime_policy:current.spec.runtime_policy
      ~enabled:true
      ~description:None
    |> store_ok
  in
  let artifacts =
    Artifact.open_existing
      ~env
      ~root:
        (Agent_store.Data_root.prompt_artifacts_path (S.data_root (Daemon.store daemon)))
    |> store_ok
  in
  let revision =
    match
      A.Prompt_revision_builder.build
        ~env
        ~artifact_store:artifacts
        ~transaction_id:(P.Id.Transaction.create ())
        ~created_at:(P.Timestamp.now ())
        definition
    with
    | Ok revision -> revision
    | Error diagnostics ->
      raise_s [%sexp (diagnostics : A.Prompt_revision_builder.Diagnostic.t list)]
  in
  let source =
    A.Runtime_builder.Source.prepare
      revision
      ~script_preparation:Daemon.default_options.script_preparation
      ~invocation:Ui
      ~env
      ~checkpoint:Eio.Fiber.check
    |> protocol_ok
  in
  let observer =
    match A.Runtime_builder.Source.moderator source with
    | Legacy observer -> observer
    | Extension _ | No_moderator -> failwith "expected the actual legacy target source"
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
      ~artifact:
        (A.Prompt_revision.artifact revision
         |> Agent_store.Captured_prompt_artifact.create
         |> document_ok)
      ~definition
      ~parent:captured_parent
    |> protocol_ok
  in
  target, observer
;;

(* Parent is real Running/Ready/Idle after authenticated client create/start,
   with its actual planner installed via Support.with_planner. Source files and
   recipe file were written before Daemon.start. f stays within all live leases. *)
let with_cold_held env daemon parent ~root_file ~counters ~original f =
  let captured, target_observer = cold_target env daemon parent ~root_file in
  let permit = reserve parent captured in
  let reservation = Actor.Recurrence_creation.reservation permit in
  let linked =
    Factory.create_recurrence_child (Daemon.factory daemon) ~parent ~permit |> protocol_ok
  in
  let target_session = Factory.Recurrence_child.session linked in
  let child_id = P.Session_ref.session_id target_session in
  let child, _record = assert_dormant daemon child_id in
  let initial = state child in
  let baseline = !(counters.constructions), !(counters.providers) in
  let no_effects () = unchanged_effects counters ~baseline initial child in
  no_effects ();
  let recurrence = entry parent in
  let association =
    Entry.frontier recurrence
    |> protocol_ok
    |> Frontier.child_associations
    |> List.find_exn ~f:(fun association ->
      P.Id.Session.equal
        (A.Recurrence_child_association.reference association).child_session_id
        child_id)
  in
  assert (
    Ledger.Reference.equal
      (A.Recurrence_child_association.reference association)
      (Factory.Recurrence_child.reference linked));
  let child_data = Intent.Child.create ~reservation ~association |> protocol_ok in
  (* A proposed source is DATA. Actual verified Factory selection/binding below
     must match it, and admit_held checks the actual first source installation.
     The child is pristine (assert_dormant), so this is first epoch1, as in the
     existing Held_tests.original_request fixture; no source allocator is added. *)
  let proposed_source =
    P.Run_source.create
      ~observer:target_observer
      ~generation:initial.identity.generation
      ~installation_epoch:1L
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition:(Entry.current recurrence)
      ~control_revision:(Entry.control_revision recurrence)
      ~nominal_json:(A.Recurrence_child_reservation.nominal_json reservation)
      ~target:target_session
      ~source:proposed_source
      ~child:(Some child_data)
    |> protocol_ok
  in
  (match Intent.input intent with
   | Authored_start -> ()
   | User_submission _ -> assert false);
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:principal.id
      ~expected_revision:initial.counters.revision
      ~key:(P.Idempotency_key.of_string "520-cold-activation" |> protocol_ok)
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation
        child.actor
        ~actor:original
        ~authorize
        ~request:(Attempt.request attempt ~intent |> protocol_ok)
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "cold fixture reused an old Run"
  in
  protect_cleanup
    ~finally:(fun () -> Actor.end_run_preparation child.actor preparation |> protocol_ok)
    (fun () ->
       Factory.with_prepared_run_source
         (Daemon.factory daemon)
         child
         ~preparation
         (fun prepared ->
            no_effects ();
            let binding = Owner.Prepared.binding prepared in
            let selected = Actor.Source_binding.source binding in
            (match A.Runtime_builder.Source.selection selected with
             | Captured _ -> ()
             | Authored _ | Authored_child _ | Generated _ ->
               failwith "cold fixture lost actual captured-child source custody");
            (match A.Runtime_builder.Source.moderator selected with
             | Legacy observer | Extension observer ->
               assert (P.Invocation.equal_observer observer proposed_source.observer)
             | No_moderator -> failwith "cold fixture has no actual moderator");
            Actor.record_recurrence_execution
              parent.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~intent
              ~attempt
            |> protocol_ok
            |> ignore;
            no_effects ();
            let held =
              match
                Actor.admit_held_recurrence_run
                  child.actor
                  ~binding
                  ~intent
                  ~attempt
                  ~session:target_session
                |> protocol_ok
              with
              | Held held -> held
              | Retained _ -> failwith "cold fixture lacks live held owner"
              | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
            in
            no_effects ();
            let observe () =
              Actor.observe_recurrence_run
                child.actor
                ~actor:proof
                ~authorize
                ~intent
                ~attempt
              |> protocol_ok
              |> Option.value_exn
            in
            Actor.import_recurrence_execution
              parent.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~automatic_policy:(fun _ _ -> Ok true)
              ~observation:(observe ())
            |> protocol_ok
            |> ignore;
            no_effects ();
            let observation = observe () in
            let permit =
              Actor.acquire_recurrence_activation
                parent.actor
                ~actor:proof
                ~authorize:(fun current _ -> authorize current)
                ~observation
              |> protocol_ok
            in
            protect_cleanup
              ~finally:(fun () ->
                Actor.release_recurrence_activation parent.actor permit |> protocol_ok)
              (fun () ->
                 let checked =
                   Actor.check_recurrence_activation parent.actor permit ~observation
                   |> protocol_ok
                 in
                 no_effects ();
                 Held.check_basis held |> protocol_ok;
                 Owner.Prepared.check_basis prepared |> protocol_ok;
                 f child prepared held checked intent attempt))
       |> protocol_ok)
;;

let%test_unit
    "cold captured recurrence scopes actual constructor work without premature terminal"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         let child_prompt = Filename.concat root "cold-child.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / prompt)
           {|<developer>Actual recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / child_prompt)
           cold_prompt;
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / root / "activation-recipe.chatmd")
           "<developer>520_CONSTRUCTOR_RECIPE. Return the offline fixture \
            response.</developer>";
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / root / "activation-native.txt")
           "520 actual allowed native result";
         let counters = { constructions = ref 0; providers = ref 0 } in
         let armed = ref false in
         let entered, signal_entered = Eio.Promise.create () in
         let release, signal_release = Eio.Promise.create () in
         let released = ref false in
         let release_provider () =
           if not !released
           then (
             released := true;
             Eio.Promise.resolve signal_release ())
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               Int.incr counters.providers;
               let transcript =
                 `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
               in
               with_second_response_barrier
                 ~armed
                 ~entered:signal_entered
                 ~release
                 transcript;
               completed_response "520-constructor-model-result")
         in
         with_daemon
           ~policy
           ~on_runtime_construction:(fun () -> Int.incr counters.constructions)
           env
           root
           (T.config root root prompt)
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
                   let original_live = ref true in
                   let original =
                     Operator_authorization.guarded ~principal ~is_current:(fun () ->
                       !original_live)
                   in
                   let child, after =
                     with_planner env sw daemon parent (fun () ->
                       with_cold_held
                         env
                         daemon
                         parent
                         ~root_file:child_prompt
                         ~counters
                         ~original
                         (fun child prepared held checked intent attempt ->
                            let admission = Held.admission held in
                            let before_counts =
                              !(counters.constructions), !(counters.providers)
                            in
                            let callback_completed = ref false in
                            let escaped = ref None in
                            armed := true;
                            Eio.Switch.run (fun run_sw ->
                              let constructing =
                                Eio.Fiber.fork_promise ~sw:run_sw (fun () ->
                                  Owner.with_held_recurrence_runtime
                                    child.runtime
                                    ~prepared
                                    ~held
                                    ~checked
                                    (fun runtime continuation ->
                                       assert (
                                         Option.is_some
                                           runtime.A.Runtime_builder.moderator_manager);
                                       assert (Operator_authorization.is_current original);
                                       Activation.Continuation.check_basis continuation
                                       |> protocol_ok;
                                       let current = state child in
                                       let index = Option.value_exn current.run_state in
                                       let run =
                                         A.Run_state.find index admission.run_id
                                         |> Option.value_exn
                                       in
                                       (match run.lifecycle with
                                        | Admitted | Active | Waiting _ -> ()
                                        | Terminal _ ->
                                          failwith
                                            "startup callback completion prematurely \
                                             settled scoped Run");
                                       let startup =
                                         List.find_exn
                                           current.moderator_executions
                                           ~f:(fun event ->
                                             P.Moderator_execution.equal_phase
                                               event.context.phase
                                               Session_start)
                                       in
                                       (match startup.status with
                                        | Completed _ -> ()
                                        | _ ->
                                          raise_s
                                            [%sexp (startup : P.Moderator_execution.t)]);
                                       escaped := Some continuation;
                                       callback_completed := true;
                                       Ok ()))
                              in
                              protect_cleanup ~finally:release_provider (fun () ->
                                let rec await_second () =
                                  match Eio.Promise.peek entered with
                                  | Some () -> ()
                                  | None ->
                                    (match Eio.Promise.peek constructing with
                                     | Some _ ->
                                       Eio.Promise.await_exn constructing |> protocol_ok;
                                       failwith
                                         "constructor ended without real second Model \
                                          admission"
                                     | None ->
                                       Eio.Fiber.yield ();
                                       await_second ())
                                in
                                await_second ();
                                (* These are outside both native/provider exception
                                   converters and before the second completion. *)
                                assert_constructor_prefix child held;
                                assert (not !callback_completed);
                                assert (Operator_authorization.is_current original);
                                [%test_eq: int]
                                  (fst before_counts + 1)
                                  !(counters.constructions);
                                [%test_eq: int]
                                  (snd before_counts + 2)
                                  !(counters.providers);
                                release_provider ();
                                Eio.Promise.await_exn constructing |> protocol_ok;
                                assert !callback_completed));
                            armed := false;
                            let after = state child in
                            let index = Option.value_exn after.run_state in
                            let run =
                              A.Run_state.find index admission.run_id |> Option.value_exn
                            in
                            (* Honest CURRENT CPS contract: no handoff was issued.
                               Scoped cleanup interrupts, even though children
                               completed truthfully. It does not replay/start. *)
                            (match run.lifecycle with
                             | Terminal Interrupted -> ()
                             | _ -> raise_s [%sexp (run : P.Run.t)]);
                            assert (P.Run_source.equal run.source admission.source);
                            let origin =
                              A.Run_state.recurrence_origin index run.id
                              |> Option.value_exn
                            in
                            A.Recurrence_run_origin.validate origin ~intent ~attempt
                            |> protocol_ok;
                            A.Recurrence_run_origin.validate_run origin ~run ~admission
                            |> protocol_ok;
                            let receipts =
                              A.Run_state.receipts index
                              |> List.filter ~f:(fun receipt ->
                                P.Id.Run.equal receipt.run_id run.id)
                            in
                            [%test_eq: int]
                              1
                              (List.count receipts ~f:(fun receipt ->
                                 P.Run_receipt.Kind.equal receipt.kind Admission));
                            let terminal =
                              match
                                List.filter receipts ~f:(fun receipt ->
                                  P.Run_receipt.Kind.equal receipt.kind Terminal)
                              with
                              | [ receipt ] -> receipt
                              | receipts ->
                                raise_s [%sexp (receipts : P.Run_receipt.t list)]
                            in
                            [%test_eq: int64] run.revision terminal.run_revision;
                            assert (P.Run_source.equal terminal.source run.source);
                            List.iter after.jobs ~f:(fun job ->
                              assert (P.Job.equal_kind job.kind Model_call);
                              assert (
                                Option.equal
                                  P.Inference_origin.equal
                                  job.inference_origin
                                  (Some (Run run.id)));
                              (match job.status with
                               | Succeeded -> ()
                               | _ -> raise_s [%sexp (job : P.Job.t)]);
                              let work = A.Run_job_occurrence.work job |> protocol_ok in
                              match
                                List.filter run.terminal_work ~f:(fun proof ->
                                  P.Run_work.equal proof.work work)
                              with
                              | [ proof ] ->
                                assert (
                                  P.Run_work.Terminal.equal_outcome
                                    proof.outcome
                                    Succeeded)
                              | proofs ->
                                raise_s [%sexp (proofs : P.Run_work.Terminal.t list)]);
                            [%test_eq: int] 2 (List.length after.jobs);
                            assert (not (Owner.is_loaded child.runtime));
                            assert (Result.is_error (Held.check_basis held));
                            assert (
                              Result.is_error
                                (Activation.Continuation.check_basis
                                   (Option.value_exn !escaped)));
                            let stable_counts =
                              !(counters.constructions), !(counters.providers)
                            in
                            (match
                               Owner.with_held_recurrence_runtime
                                 child.runtime
                                 ~prepared
                                 ~held
                                 ~checked
                                 (fun _ _ -> failwith "consumed startup replayed")
                             with
                             | Error error ->
                               assert (P.Error.equal_code error.code Conflict)
                             | Ok _ -> failwith "consumed startup admitted again");
                            [%test_eq: int * int]
                              stable_counts
                              (!(counters.constructions), !(counters.providers));
                            assert (
                              Jsonaf.exactly_equal
                                (T.state_document after |> store_ok |> D.Document.json)
                                (T.state_document (state child)
                                 |> store_ok
                                 |> D.Document.json));
                            assert (Operator_authorization.is_current original);
                            Ok (child, after)))
                   in
                   (* All staged/Init lexical cleanup has completed while the
                      actual Actor still lives. Close the actual child entry
                      before loading CURRENT, as existing Factory tests do. *)
                   let settled = state child in
                   assert (
                     Jsonaf.exactly_equal
                       (T.state_document after |> store_ok |> D.Document.json)
                       (T.state_document settled |> store_ok |> D.Document.json));
                   let directory =
                     Option.value_exn child.store_handle |> S.Handle.snapshot_directory
                   in
                   child.close ();
                   let installed =
                     Agent_store.Snapshot.load_current
                       ~env
                       ~directory
                       ~max_payload_length:(64 * 1024 * 1024)
                     |> store_ok
                     |> Option.value_exn
                   in
                   let restored =
                     A.Session_state_document.decode
                       ~limits:D.Limits.default
                       installed.snapshot.payload
                     |> document_ok
                     |> A.Session_state_document.value
                   in
                   assert (
                     Jsonaf.exactly_equal
                       (T.state_document settled |> store_ok |> D.Document.json)
                       (T.state_document restored |> store_ok |> D.Document.json))))))
;;
