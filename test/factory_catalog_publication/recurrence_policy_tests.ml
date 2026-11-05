open! Core
open Agent_server_test_support
open Retry_support
module Tools = A.Script_tool_calls
module W = A.Operation_worker
module Dispatch = Chat_response.In_memory_stream.Tool_dispatch
module C = Chat_response.Tool_capability

(* Lower host composition over an actual Factory runtime and service. The sibling
   composition workflow qualifies normal model Tool.call; this case isolates the
   SAME-service final guard after a real indexed target read has completed. *)
let invoke actor (caps : W.Capabilities.t) dispatch ~raw ~id ~name json =
  let allocate () =
    History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
  in
  let payload = Jsonaf.to_string json in
  let call =
    Openai.Responses_history.create_with_id_exn
      ~id:(allocate ())
      (Function_call
         { name
         ; arguments = payload
         ; call_id = id
         ; _type = "function_call"
         ; id = None
         ; status = None
         })
  in
  let request : Dispatch.request =
    { kind = Function
    ; original_name = name
    ; original_payload = payload
    ; name
    ; payload
    ; rejection = None
    ; call
    ; history =
        A.History_codec.all_of_protocol (state actor).conversation.canonical_history
        |> protocol_ok
    ; source = None
    ; parent_call_id = None
    }
  in
  assert (dispatch.Dispatch.commit_call request);
  let run_native implementation ~payload =
    let output = implementation.Ochat_function.run payload in
    (match output with
     | Openai.Responses.Tool_output.Output.Text text ->
       raw := Some (P.Invocation.outcome_of_json (Jsonaf.of_string text) |> protocol_ok)
     | _ -> failwith "native recurrence returned nontext");
    output
  in
  let result = dispatch.run ~run_native request ~authorize:ignore |> Option.value_exn in
  let output =
    Openai.Responses_history.create_with_id_exn
      ~id:(allocate ())
      (Function_call_output
         { call_id = id
         ; output = result.output
         ; id = None
         ; _type = "function_call_output"
         ; status = None
         })
  in
  (Option.value_exn result.commit_output) output
;;

let%expect_test "native target disclosure repeats actual host policy after indexed read" =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "lower native policy fixture must not invoke a provider")
  in
  let prompt = Source.prompt_text ^ "\n<tool name=\"recurrence_history\"/>" in
  Creation_tests.with_daemon
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~max_root_agents:2
    ~policy
    (fun env sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Source.with_loaded_entry daemon (fun ~entry:controller ~runtime ~revision:_ ->
           Source.with_loaded_entry daemon (fun ~entry:target ~runtime:_ ~revision:_ ->
             with_planner env sw daemon controller (fun () ->
               let occurrence = create_occurrence daemon controller target in
               ignore
                 (initial_absence ~controller env daemon target occurrence
                  : Actor.No_effects_observation.t);
               (* Close the actual target, then index its actual durable projection.
                 No replacement owner, source or decoded authority is invented. *)
               let target_id = (state target).identity.session_id in
               target.close ();
               let removed =
                 Registry.remove (Daemon.registry daemon) target_id |> Option.value_exn
               in
               assert (phys_equal removed.actor target.actor);
               let indexed =
                 Agent_store.Session_index.find
                   (Agent_store.Session_store.session_index (Daemon.store daemon))
                   target_id
                 |> Option.value_exn
               in
               Registry.index (Daemon.registry daemon) indexed;
               assert (Option.is_none (Registry.find (Daemon.registry daemon) target_id));
               let host_current = ref true in
               let reads = ref 0 in
               let failure =
                 P.Error.create
                   Permission_denied
                   ~message:"native read host policy revoked"
                   ~retryable:false
                   ~data:(`Object [ "phase", `String "after-real-target-read" ])
                   ()
               in
               let factory = Daemon.factory daemon in
               Registry.install_reader (Daemon.registry daemon) (fun indexed ->
                 let result = Factory.read_session factory indexed in
                 (match result with
                  | Ok observed
                    when P.Id.Session.equal observed.identity.session_id target_id ->
                    Int.incr reads;
                    host_current := false
                  | Ok _ | Error _ -> ());
                 result);
               let tools =
                 Option.value_exn runtime.A.Runtime_builder.moderator_script_tools
               in
               assert (Option.is_some (Tools.recurrence_service tools));
               let guarded =
                 Tools.with_authorization_guard tools ~check:(fun () ->
                   if !host_current then Ok () else Error failure)
               in
               let selected = Tools.current_capabilities tools in
               ignore
                 (C.find selected ~name:"recurrence_history"
                  |> Result.map_error ~f:(fun error -> error.C.message)
                  |> Result.ok_or_failwith
                  : C.binding);
               let finished, finish = Eio.Promise.create () in
               let worker_error = ref None in
               let raw = ref None in
               let worker =
                 W.create ~run:(fun ~sw:_ ~input caps ->
                   let outcome =
                     try
                       let dispatch =
                         Tools.native_dispatch
                           guarded
                           ~declared:selected
                           ~input
                           ~capabilities:caps
                       in
                       invoke
                         controller
                         caps
                         dispatch
                         ~raw
                         ~id:"final-policy-history"
                         ~name:"recurrence_history"
                         (`Object [ "id", P.Id.Recurrence.to_json recurrence_id ]);
                       let final_history =
                         A.History_codec.all_of_protocol
                           (state controller).conversation.canonical_history
                         |> protocol_ok
                       in
                       W.Completed
                         { final_history
                         ; runtime_requests = []
                         ; moderator_snapshot = runtime.moderator_snapshot ()
                         }
                     with
                     | exn ->
                       worker_error := Some (exn, Stdlib.Printexc.get_raw_backtrace ());
                       W.Failed
                         (P.Error.create
                            Internal_error
                            ~message:"native policy fixture worker failed"
                            ~retryable:false
                            ())
                   in
                   Eio.Promise.resolve finish ();
                   outcome)
               in
               let restore () =
                 Registry.install_reader
                   (Daemon.registry daemon)
                   (Factory.read_session factory);
                 Actor.set_runtime_worker
                   ~actor:original
                   ?moderator_manager:runtime.moderator_manager
                   controller.actor
                   ~worker:(Some runtime.worker)
                   ~inference:(Some runtime.inference_execution)
                 |> protocol_ok
               in
               protect_cleanup ~finally:restore (fun () ->
                 Actor.set_runtime_worker
                   ~actor:original
                   ?moderator_manager:runtime.moderator_manager
                   controller.actor
                   ~worker:(Some worker)
                   ~inference:(Some runtime.inference_execution)
                 |> protocol_ok;
                 let attachment, _ =
                   Actor.attach controller.actor ~mode:Read_write ~subscribe:false
                   |> protocol_ok
                 in
                 let history_id =
                   A.History_id_source.allocate controller.history_ids |> protocol_ok
                 in
                 let history =
                   A.History_codec.user_text
                     ~id:history_id
                     "Read actual retained recurrence history"
                   |> A.History_codec.to_protocol
                 in
                 let before = state controller in
                 let target_frames = Existing.frames env target in
                 let submitted =
                   Actor.submit_message_authenticated
                     controller.actor
                     ~actor:original
                     ~attachment_id:attachment.id
                     history
                   |> protocol_ok
                 in
                 Eio.Promise.await finished;
                 Option.iter !worker_error ~f:(fun (exn, backtrace) ->
                   Exn.raise_with_original_backtrace exn backtrace);
                 let rec await_terminal () =
                   let current = state controller in
                   if Option.is_some current.active_operation
                   then (
                     Eio.Fiber.yield ();
                     await_terminal ())
                   else current
                 in
                 let after = await_terminal () in
                 let operation = Option.value_exn submitted.operation_id in
                 let completed =
                   match
                     A.Durable_event_log.replay
                       controller.durable_events
                       ~after_sequence:0L
                       ~through_sequence:Int64.max_value
                   with
                   | Available events ->
                     List.exists events ~f:(fun event ->
                       match
                         P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                         |> protocol_ok
                       with
                       | Operation_completed candidate ->
                         P.Id.Operation.equal candidate.id operation
                       | _ -> false)
                   | Snapshot_required ->
                     failwith "lost actual native operation completion"
                 in
                 assert completed;
                 [%test_eq: int] 1 !reads;
                 assert (Operator_authorization.is_current original);
                 (match Option.value_exn !raw with
                  | Fail error ->
                    [%test_eq: string] (P.Error.code_to_string failure.code) error.code;
                    [%test_eq: string] failure.message error.message;
                    [%test_eq: bool] false error.retryable;
                    assert (Document_schema.Json.equal failure.data error.details)
                  | outcome ->
                    raise_s
                      [%sexp
                        "revoked host disclosed native target payload"
                      , (outcome : P.Invocation.outcome)]);
                 let catalog_json =
                   Option.map ~f:(fun catalog -> Catalog.to_jsonaf catalog |> protocol_ok)
                 in
                 assert (
                   Option.equal
                     Document_schema.Json.equal
                     (catalog_json before.recurrence_catalog)
                     (catalog_json after.recurrence_catalog));
                 [%test_eq: string list] target_frames (Existing.frames env target);
                 assert (Option.is_none (Registry.find (Daemon.registry daemon) target_id));
                 [%test_eq: int] 0 !providers;
                 print_endline
                   "actual target read completes once; original remains live; final \
                    same-service host denial discloses no payload"))))));
  [%expect
    {| actual target read completes once; original remains live; final same-service host denial discloses no payload |}]
;;
