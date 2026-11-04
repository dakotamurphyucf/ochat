open! Core
open Support
module Owner = Agent_server.Runtime_owner
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Input = A.Runtime_input_preparation
module Accounting = A.Inference_ledger
module Observation = Inference.Observation
module Res = Openai.Responses

module Cut = struct
  type t =
    | Complete
    | Revoke_between_agents
    | Stop_after_first
  [@@deriving sexp_of]
end

let prompt =
  {|<developer>Actual loaded recurrence input owner.</developer>
<tool name="input_a" agent="input-a.chatmd" local/>
<tool name="input_b" agent="input-b.chatmd" local/>
<script id="controller" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let content : P.Session.Message_content.t =
  { kind = Chatmd
  ; text =
      {|<user>before <agent src="input-a.chatmd" local><user>533_INPUT_A</user></agent> between <agent src="input-b.chatmd" local><user>533_INPUT_B</user></agent> after</user>|}
  ; attachments = []
  }
;;

let assert_no_root (state : A.Session_state.t) =
  assert (Option.is_none state.active_operation);
  assert (List.is_empty state.jobs)
;;

let require_completed row =
  let handle = Accounting.Row.handle row in
  let record = Accounting.Row.record row in
  assert (Option.is_none (Accounting.Handle.operation_id handle));
  assert (Option.is_none (Accounting.Handle.invocation_id handle));
  assert (
    Transcript.Scope.equal
      (Accounting.Handle.scope handle)
      (Observation.Attempt_record.scope record));
  assert (
    Observation.Observation_id.equal
      (Accounting.Handle.accounting_id handle)
      (Observation.Attempt_record.accounting_id record));
  match Observation.Attempt_record.state record with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | (Refused | Incomplete _ | Failed _) as outcome ->
       raise_s [%sexp (outcome : Inference.Event.Terminal.outcome)])
  | Prepared | Running | Interrupted _ -> failwith "actual input attempt did not complete"
;;

(* The controller and target are separate actual registered Factory sessions.
   Only the target's continuously owned preparation supplies its original. *)
let with_loaded_held daemon controller target ~original f =
  let before = state target in
  let source = A.Recurrence_source.installed before |> protocol_ok in
  let session =
    P.Session_ref.create
      ~server_id:(S.server_id (Daemon.store daemon))
      ~session_id:before.identity.session_id
  in
  let selected =
    Target.create_existing
      ~session
      ~source
      ~configuration_revision:before.spec.configuration_revision
      ~input:content
    |> protocol_ok
  in
  assert (P.Run.Mode.equal (Target.run_mode selected |> protocol_ok) Single_turn);
  let nominal = ready_nominal controller selected in
  let recurrence = entry controller in
  (* The Ready row includes its retained disposition/carrier. Projecting and
     re-encoding only the nominal would erase those factual fields. *)
  let nominal_json =
    Entry.frontier recurrence
    |> protocol_ok
    |> Frontier.to_document
    |> document_ok
    |> D.Document.payload
    |> fun payload ->
    match field payload "ready" with
    | `Array rows ->
      List.find_exn rows ~f:(fun row ->
        let retained = A.Recurrence_nominal.of_json row |> protocol_ok in
        A.Recurrence_identity.Occurrence.equal
          (A.Recurrence_nominal.key nominal)
          (A.Recurrence_nominal.key retained))
    | _ -> failwith "actual controller has no complete Ready row"
  in
  let intent =
    Intent.create
      ~definition:(Entry.current recurrence)
      ~control_revision:(Entry.control_revision recurrence)
      ~nominal_json
      ~target:session
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id:principal.id
      ~expected_revision:(state target).counters.revision
      ~key:(P.Idempotency_key.of_string "533-loaded-input" |> protocol_ok)
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation
        target.actor
        ~actor:original
        ~authorize
        ~request:(Attempt.request attempt ~intent |> protocol_ok)
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "input fixture reused a retained Run"
  in
  protect_cleanup
    ~finally:(fun () -> Actor.end_run_preparation target.actor preparation |> protocol_ok)
    (fun () ->
       Factory.with_prepared_run_source
         (Daemon.factory daemon)
         target
         ~preparation
         (fun prepared ->
            Actor.record_recurrence_execution
              controller.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~intent
              ~attempt
            |> protocol_ok
            |> ignore;
            let held =
              match
                Actor.admit_held_recurrence_run
                  target.actor
                  ~binding:(Owner.Prepared.binding prepared)
                  ~intent
                  ~attempt
                  ~session
                |> protocol_ok
              with
              | Held held -> held
              | Retained _ -> failwith "input fixture has no live held owner"
              | Rejected error | Uncertain error -> raise_s [%sexp (error : P.Error.t)]
            in
            let observe () =
              Actor.observe_recurrence_run
                target.actor
                ~actor:proof
                ~authorize
                ~intent
                ~attempt
              |> protocol_ok
              |> Option.value_exn
            in
            Actor.import_recurrence_execution
              controller.actor
              ~actor:proof
              ~authorize:(fun current _ -> authorize current)
              ~automatic_policy:(fun _ _ -> Ok true)
              ~observation:(observe ())
            |> protocol_ok
            |> ignore;
            let observation = observe () in
            let permit =
              Actor.acquire_recurrence_activation
                controller.actor
                ~actor:proof
                ~authorize:(fun current _ -> authorize current)
                ~observation
              |> protocol_ok
            in
            protect_cleanup
              ~finally:(fun () ->
                Actor.release_recurrence_activation controller.actor permit |> protocol_ok)
              (fun () ->
                 let checked =
                   Actor.check_recurrence_activation controller.actor permit ~observation
                   |> protocol_ok
                 in
                 f prepared held checked intent attempt))
       |> protocol_ok)
;;

let run (cut : Cut.t) =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = T.temporary_root env in
    protect_cleanup
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let prompt_file = Filename.concat root "input-owner.chatmd" in
         List.iter
           [ prompt_file, prompt
           ; Filename.concat root "input-a.chatmd", "<developer>533_AGENT_A</developer>"
           ; Filename.concat root "input-b.chatmd", "<developer>533_AGENT_B</developer>"
           ]
           ~f:(fun (path, text) ->
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               Eio.Path.(Eio.Stdenv.fs env / path)
               text);
         let calls = ref [] in
         let original_live = ref true in
         let first, first_u = Eio.Promise.create () in
         let second, second_u = Eio.Promise.create () in
         let release_second, release_second_u = Eio.Promise.create () in
         let completed, completed_u = Eio.Promise.create () in
         let release_completion, release_completion_u = Eio.Promise.create () in
         let stop_armed = ref false in
         let release, release_u = Eio.Promise.create () in
         let release_first () =
           if Option.is_none (Eio.Promise.peek release)
           then Eio.Promise.resolve release_u ()
         in
         let release_all () =
           release_first ();
           if Option.is_none (Eio.Promise.peek release_completion)
           then Eio.Promise.resolve release_completion_u ();
           if Option.is_none (Eio.Promise.peek release_second)
           then Eio.Promise.resolve release_second_u ()
         in
         let policy =
           T.inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               let transcript =
                 `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
               in
               let marker =
                 if String.is_substring transcript ~substring:"533_AGENT_A"
                 then "A"
                 else if String.is_substring transcript ~substring:"533_AGENT_B"
                 then "B"
                 else failwith "unexpected provider outside actual input Agents"
               in
               calls := !calls @ [ marker ];
               if String.equal marker "A"
               then (
                 Eio.Promise.resolve first_u ();
                 Eio.Promise.await release)
               else (
                 Eio.Promise.resolve second_u ();
                 Eio.Promise.await release_second);
               Activation_tests.completed_response ("533-input-" ^ marker ^ "-result"))
         in
         let policy =
           { policy with
             runtime_inference_ports =
               (fun actor ->
                 Result.map (policy.runtime_inference_ports actor) ~f:(fun upstream ->
                   { upstream with
                     on_completion =
                       (fun completion ->
                         upstream.on_completion completion;
                         if !stop_armed
                         then (
                           stop_armed := false;
                           Eio.Promise.resolve completed_u ();
                           Eio.Promise.await release_completion))
                   }))
           }
         in
         let configuration = T.config root root prompt_file in
         let configuration =
           { configuration with
             workspaces =
               List.map
                 configuration.workspaces
                 ~f:(fun (workspace : Agent_server.Config.Workspace.t) ->
                   { workspace with
                     prompt_limits =
                       List.map
                         workspace.prompt_limits
                         ~f:(fun (limit : Agent_server.Config.Workspace.prompt_limit) ->
                           { limit with max_root_agents = 2 })
                   })
           }
         in
         let provider_calls = ref 0 in
         with_daemon ~policy env root configuration ~provider_calls (fun sw daemon ->
           let connection = T.connection daemon principal in
           protect_cleanup
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                T.initialize connection;
                let controller, _ =
                  T.create_session
                    ~start_immediately:true
                    ~key:"533-controller"
                    connection
                in
                let target_session, target_attachment =
                  T.create_session ~start_immediately:true ~key:"533-target" connection
                in
                let controller = loaded daemon controller.id in
                let target = loaded daemon target_session.id in
                start_parent_settled env sw connection controller;
                start_parent_settled env sw connection target;
                let before = state target in
                assert_no_root before;
                assert (List.is_empty (Accounting.rows before.inference_ledger));
                assert (List.is_empty !calls);
                let original =
                  Operator_authorization.guarded ~principal ~is_current:(fun () ->
                    !original_live)
                in
                let parsed = ref None in
                let parsed_effect_check = ref None in
                let escaped_execution = ref None in
                let admission = ref None in
                let input_result = ref None in
                let held_facts = ref None in
                (* The entire source/held scope is supervised. Stop cancels that
                   actual lease; the observer must remain outside its context. *)
                let worker =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    match
                      with_planner env sw daemon controller (fun () ->
                        with_loaded_held
                          daemon
                          controller
                          target
                          ~original
                          (fun prepared held checked intent attempt ->
                             let receipt = Actor.Held_run.admission held in
                             admission := Some receipt;
                             held_facts := Some (receipt, intent, attempt);
                             (stop_armed
                              := match cut with
                                 | Stop_after_first -> true
                                 | Complete | Revoke_between_agents -> false);
                             let result =
                               Owner.with_held_recurrence_runtime
                                 target.runtime
                                 ~prepared
                                 ~held
                                 ~checked
                                 (fun runtime continuation ->
                                    Actor.with_held_input
                                      target.actor
                                      ~continuation
                                      ~prepare:(fun ~scope execution ->
                                        escaped_execution := Some execution;
                                        Factory.prepare_held_input
                                          (Daemon.factory daemon)
                                          ~entry:target
                                          ~runtime
                                          ~scope)
                                      (fun ~scope:_ ~input ->
                                         let execution =
                                           Option.value_exn !escaped_execution
                                         in
                                         parsed_effect_check
                                         := Some (execution.Input.Execution.checkpoint ());
                                         parsed := Some input;
                                         Ok ()))
                             in
                             input_result := Some result;
                             Ok ()))
                    with
                    | () -> `Joined
                    | exception (Eio.Cancel.Cancelled _ as exn) ->
                      (match cut with
                       | Stop_after_first -> `Cancelled
                       | Complete | Revoke_between_agents ->
                         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                         Exn.raise_with_original_backtrace exn backtrace))
                in
                let await_boundary entered description =
                  let rec await () =
                    match Eio.Promise.peek entered with
                    | Some () -> ()
                    | None ->
                      (match Eio.Promise.peek worker with
                       | Some _ ->
                         ignore (Eio.Promise.await_exn worker);
                         (match !input_result with
                          | Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                          | Some (Ok ()) | None -> failwith description)
                       | None ->
                         Eio.Fiber.yield ();
                         await ())
                  in
                  await ()
                in
                let outcome =
                  protect_cleanup ~finally:release_all (fun () ->
                    await_boundary first "input ended before actual first provider";
                    assert_no_root (state target);
                    [%test_eq: string list] [ "A" ] !calls;
                    assert (Option.is_none !parsed);
                    (match cut with
                     | Complete | Stop_after_first -> ()
                     | Revoke_between_agents -> original_live := false);
                    release_first ();
                    (match cut with
                     | Revoke_between_agents -> ()
                     | Stop_after_first ->
                       await_boundary completed "input ended before completed first Agent";
                       let rows = Accounting.rows (state target).inference_ledger in
                       [%test_eq: int] 1 (List.length rows);
                       List.iter rows ~f:require_completed;
                       Actor.stop
                         target.actor
                         ~attachment_id:target_attachment.id
                         ~mode:Cancel
                       |> protocol_ok
                       |> ignore;
                       Eio.Promise.resolve release_completion_u ()
                     | Complete ->
                       await_boundary second "input ended before actual second provider";
                       let during = state target in
                       assert_no_root during;
                       let receipt = Option.value_exn !admission in
                       let run =
                         A.Run_state.find
                           (Option.value_exn during.run_state)
                           receipt.run_id
                         |> Option.value_exn
                       in
                       (match run.lifecycle with
                        | Admitted | Active | Waiting _ -> ()
                        | Terminal _ ->
                          failwith "first input Agent prematurely settled held Run");
                       let rows = Accounting.rows during.inference_ledger in
                       [%test_eq: int] 2 (List.length rows);
                       require_completed (List.hd_exn rows);
                       [%test_eq: string list] [ "A"; "B" ] !calls;
                       assert (Option.is_none !parsed);
                       Eio.Promise.resolve release_second_u ());
                    Eio.Promise.await_exn worker)
                in
                (match cut, outcome, !input_result with
                 | Complete, `Joined, Some (Ok ()) -> ()
                 | Revoke_between_agents, `Joined, Some (Error error) ->
                   if not (P.Error.equal_code error.code Permission_denied)
                   then raise_s [%sexp (error : P.Error.t)]
                 | Stop_after_first, `Joined, Some (Error error) ->
                   (match error.code with
                    | Conflict | Permission_denied -> ()
                    | _ -> raise_s [%sexp (error : P.Error.t)])
                 | Stop_after_first, `Cancelled, _ -> ()
                 | _, _, Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                 | _ -> failwith "input scope outcome disagrees with the actual cut");
                (* Joining the whole worker includes source lease cancellation
                   and all held/input/graph cleanup, before any final oracle. *)
                let after = state target in
                let receipt, intent, attempt = Option.value_exn !held_facts in
                let index = Option.value_exn after.run_state in
                let run = A.Run_state.find index receipt.run_id |> Option.value_exn in
                (match run.lifecycle with
                 | Terminal Interrupted -> ()
                 | _ -> raise_s [%sexp (run : P.Run.t)]);
                assert (P.Run_source.equal run.source receipt.source);
                let origin =
                  A.Run_state.recurrence_origin index run.id |> Option.value_exn
                in
                A.Recurrence_run_origin.validate origin ~intent ~attempt |> protocol_ok;
                assert_no_root after;
                assert (
                  List.equal
                    P.History.equal_entry
                    before.conversation.canonical_history
                    after.conversation.canonical_history);
                let after = state target in
                let rows = Accounting.rows after.inference_ledger in
                let expected =
                  match cut with
                  | Complete -> [ "A"; "B" ]
                  | Revoke_between_agents | Stop_after_first -> [ "A" ]
                in
                [%test_eq: string list] expected !calls;
                [%test_eq: int] (List.length expected) (List.length rows);
                List.iter rows ~f:require_completed;
                List.iter rows ~f:(fun row ->
                  assert (
                    P.Id.Session.equal
                      target_session.id
                      (Accounting.Handle.session_id (Accounting.Row.handle row))));
                (match cut, !parsed with
                 | Complete, Some input ->
                   (match !parsed_effect_check with
                    | Some (Error error) ->
                      [%test_eq: P.Error.code] Conflict error.code;
                      [%test_eq: string] "held input excludes new effects" error.message
                    | Some (Ok ()) | None ->
                      failwith "Parsed_ready retained an effect port");
                   let entry = Input.Prepared_input.entry input in
                   assert (P.History.equal_role entry.role User);
                   assert (P.History.equal_kind entry.kind Message);
                   assert (
                     Jsonaf.exactly_equal
                       (P.Session.Message_content.to_json
                          (Input.Prepared_input.content input))
                       (P.Session.Message_content.to_json content));
                   let json = Jsonaf.to_string entry.payload in
                   assert (String.is_substring json ~substring:"533-input-A-result");
                   assert (String.is_substring json ~substring:"533-input-B-result")
                 | (Revoke_between_agents | Stop_after_first), None ->
                   assert (Option.is_none !parsed_effect_check)
                 | _ -> failwith "parsed result disagrees with actual input cut");
                let execution = Option.value_exn !escaped_execution in
                assert (Result.is_error (execution.Input.Execution.checkpoint ()));
                assert (Option.is_some !admission);
                assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
                assert (
                  List.equal
                    P.Moderator_execution.equal
                    before.moderator_executions
                    after.moderator_executions);
                (match cut with
                 | Complete | Revoke_between_agents ->
                   assert (Owner.is_loaded target.runtime)
                 | Stop_after_first ->
                   (match after.lifecycle.desired with
                    | Stopped -> ()
                    | Running -> failwith "actual Stop did not persist");
                   (* Factory may already be retiring this exact runtime after
                      the committed Stop. Join that actual retirement instead
                      of racing its resource close with non-waiting unload. *)
                   Owner.unload_and_wait target.runtime |> protocol_ok;
                   assert (not (Owner.is_loaded target.runtime)));
                print_s
                  [%sexp
                    (cut : Cut.t)
                  , "actual input facts retained; no Operation or initializer replay"]))))
;;

let%expect_test "loaded held input joins actual Agents without granting a root Operation" =
  List.iter [ Cut.Complete; Revoke_between_agents; Stop_after_first ] ~f:run;
  [%expect
    {|
    (Complete "actual input facts retained; no Operation or initializer replay")
    (Revoke_between_agents
     "actual input facts retained; no Operation or initializer replay")
    (Stop_after_first
     "actual input facts retained; no Operation or initializer replay")
    |}]
;;
