open! Core
module P = Agent_protocol
module D = Agent_server.Daemon
module A = Agent_session.Session_actor
module F = Support.Background_fixture
module C = Support.Config_fixture
module T = Support.Temporary_environment
module L = Support.Load_fixture
module Report = Support.Load_report
module Res = Openai.Responses

let watcher =
  [%blob "../../chatml_extensibility_fixtures/x06-response-watcher/watcher.chatml"]
;;

let probe =
  [%blob "../../chatml_extensibility_fixtures/x06-response-watcher/probe.chatml"]
;;

let native_request =
  [%blob "../../chatml_extensibility_fixtures/x06-response-watcher/native-request.chatml"]
;;

let input_schema =
  [%blob "../../chatml_extensibility_fixtures/x06-response-watcher/input.json"]
;;

let any_schema = [%blob "../../chatml_extensibility_fixtures/x07-helper-session/any.json"]

let state daemon session =
  Agent_server.Session_registry.find (D.registry daemon) session
  |> Option.value_exn
  |> fun (entry : Agent_server.Session_registry.entry) ->
  A.state entry.actor |> F.protocol_ok
;;

let healthy_parent daemon session =
  let current = state daemon session in
  (match current.lifecycle.observed with
   | P.Session.Failed error ->
     raise_s [%sexp "watch parent lifecycle failed", (error.code : P.Error.code)]
   | _ -> ());
  (match current.active_operation with
   | Some { state = P.Operation.Failed error; _ } ->
     raise_s [%sexp "watch parent operation failed", (error.code : P.Error.code)]
   | _ -> ());
  current
;;

let elapsed clock started =
  Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now clock)) /. 1e9
;;

let answer ~id text =
  let item : Res.Response_stream.Item.t =
    Output_message
      { role = Assistant
      ; id
      ; status = "completed"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      ; phase = None
      ; _type = "message"
      }
  in
  [ Res.Response_stream.Output_item_added
      { item; output_index = 0; type_ = "response.output_item.added" }
  ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let function_call serial name arguments =
  let id = sprintf "real-watch-call-%d" serial in
  let item arguments status : Res.Response_stream.Item.t =
    Function_call
      { name; arguments; call_id = id; id = Some id; status; _type = "function_call" }
  in
  let arguments = Jsonaf.to_string arguments in
  [ Res.Response_stream.Output_item_added
      { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_done
      { arguments
      ; item_id = id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item arguments (Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let completion state name previous =
  List.find_map state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    if
      String.equal invocation.context.tool_name name
      && not
           (List.exists previous ~f:(fun old ->
              P.Id.Invocation.equal old.P.Invocation.context.id invocation.context.id))
    then (
      match invocation.status with
      | Published result -> Some result
      | _ -> None)
    else None)
;;

let complete = function
  | P.Invocation.Complete value -> value
  | outcome ->
    raise_s [%sexp "real-clock watch tool failed", (outcome : P.Invocation.outcome)]
;;

let run_variant env report ~helper =
  T.with_
    ~scenario:(if helper then "helper-watch" else "native-watch")
    ~env
    (fun temporary ->
       let helper_path =
         if helper
         then Some (Sys.getenv "OCHAT_E2E_HELPER_EXE" |> Option.value_exn)
         else None
       in
       let fixture =
         C.create temporary ~name:"native-watch" ~http_port:(F.reserve_port env)
       in
       let configuration =
         C.configuration fixture ()
         |> String.substr_replace_all
              ~pattern:"    (snapshot_every_events 10)\n"
              ~with_:""
         |> String.substr_replace_all ~pattern:"    (snapshot_every_ms 1000)" ~with_:""
         |> String.substr_replace_all
              ~pattern:"(idle_connection_timeout_ms 5000)"
              ~with_:"(idle_connection_timeout_ms 120000)"
         |> String.substr_replace_all
              ~pattern:"(tool_default deny)"
              ~with_:"(tool_default allow)"
       in
       let configuration =
         if helper
         then
           String.substr_replace_all
             configuration
             ~pattern:"(manifest_authorization deny)"
             ~with_:"(manifest_authorization assume_authorized)"
         else configuration
       in
       F.save fixture (C.config_path fixture) configuration;
       List.iter
         [ ( "watcher.chatml"
           , watcher
             ^ "\n\
                let initial_state = watch_initial_state\n\
                let on_event ctx state event = watch_on_event(ctx, state, event)\n" )
         ; "probe.chatml", probe
         ; "request.chatml", native_request
         ; "input.json", input_schema
         ; "any.json", any_schema
         ]
         ~f:(fun (name, text) ->
           F.save fixture (Filename.concat (C.physical_workspace fixture) name) text);
       (* Script sources belong beside the root prompt, not its workspace. *)
       List.iter
         [ "watcher.chatml"; "probe.chatml"; "request.chatml"; "input.json"; "any.json" ]
         ~f:(fun name ->
           let text =
             Eio.Path.load
               Eio.Path.(Eio.Stdenv.fs env / C.physical_workspace fixture / name)
           in
           F.save
             fixture
             (Filename.concat (Filename.dirname (C.prompt_path fixture)) name)
             text);
       let prompt =
         {|<authoring_context policy="manual"/>
<developer>Real-clock parent watcher workload.</developer>
<tool name="agent_create"/><tool name="agent_send"/><tool name="agent_wait"/><tool name="agent_read"/><tool name="agent_status"/>
<script id="request" language="chatml" kind="tool" src="request.chatml"/>
<tool name="watch_session_request" type="chatml" script="request" entrypoint="run" input_schema="any.json" output_schema="any.json"><uses tool="agent_wait"/><uses tool="agent_read"/><uses tool="agent_status"/></tool>
<script id="probe" language="chatml" kind="tool" src="probe.chatml"/>
<tool name="watch_probe" type="chatml" script="probe" entrypoint="run" input_schema="input.json" output_schema="any.json"><uses tool="watch_session_request"/></tool>
<script id="session_manager" language="chatml" kind="moderator" api="extensibility-v1" src="watcher.chatml"/>
<tool name="notify_when_agent_responds" type="moderator" moderator="session_manager" input_schema="input.json" output_schema="any.json" completion_schema="any.json"/>|}
       in
       let helper_request =
         {|let field json name = match Json.get_field(json, name) with | `Some(value) -> value | `None -> `Null
let run ctx input =
  let* response = Tool.call("session_bridge", `Object([{ key = "stdin"; value = `String(Json.stringify(input)) }])) in
  match response with
  | `Ok(`String(text)) -> (match Json.parse_opt(text) with
      | `Some(result) -> (match field(result, "type") with
          | `String("complete") -> Task.pure(`Complete(field(result, "value")))
          | _ -> Task.pure(`Fail({ code = "agent.helper.transport"; message = "Helper rejected watch request."; retryable = true; details = `Null })))
      | _ -> Task.pure(`Fail({ code = "agent.helper.transport"; message = "Invalid helper response."; retryable = true; details = `Null })))
  | _ -> Task.pure(`Fail({ code = "agent.helper.transport"; message = "Helper transport failed."; retryable = true; details = `Null }))|}
       in
       let prompt =
         match helper_path with
         | None -> prompt
         | Some executable ->
           F.save
             fixture
             (Filename.concat (Filename.dirname (C.prompt_path fixture)) "request.chatml")
             helper_request;
           String.substr_replace_all
             prompt
             ~pattern:
               "<uses tool=\"agent_wait\"/><uses tool=\"agent_read\"/><uses \
                tool=\"agent_status\"/>"
             ~with_:"<uses tool=\"session_bridge\"/>"
           ^ sprintf
               {|
<shell_access id="helper" cwd="${workspace}">
<capabilities sandbox="required" network="false" child_processes="true" arbitrary_code="true" privilege_change="false"><read path="${workspace}"/></capabilities>
<environment inherit="selected"><set name="PATH" value="/usr/bin:/bin"/></environment>
<policy default="allow"/><limits wall_time="30s" idle_time="none"/><audit format="none"/>
</shell_access><tool name="session_bridge" type="shell" mode="fixed" runtime="helper" command="%s" stdin="required" result="stdout"/>|}
               executable
       in
       F.save fixture (C.prompt_path fixture) prompt;
       let queued = ref None in
       let serial = ref 0 in
       let calls = ref 0 in
       let child_calls = ref 0 in
       let gated_calls = ref 0 in
       let entered, enter = Eio.Promise.create () in
       let release, resume = Eio.Promise.create () in
       let child_pause = ref (Some release) in
       let release_child = ref resume in
       let phase label =
         Report.record
           report
           env
           ("watch-phase-" ^ label)
           [ ("helper", if helper then `True else `False) ]
       in
       let restarted = ref None in
       let marker = "real-clock-child-result" in
       let large = String.concat (List.init 2500 ~f:(fun _ -> "📚\"\\\n")) in
       let provider ~sw:_ ~inputs =
         incr calls;
         let child =
           List.exists inputs ~f:(function
             | Res.Item.Input_message { role = Developer; content; _ } ->
               List.exists content ~f:(function
                 | Res.Input_message.Text text ->
                   String.equal
                     (String.strip text.text)
                     "Real-clock child watcher target."
                 | Image _ -> false)
             | _ -> false)
         in
         if child
         then (
           incr child_calls;
           let sent_message =
             List.exists inputs ~f:(function
               | Res.Item.Input_message { role = User; content; _ } ->
                 List.exists content ~f:(function
                   | Res.Input_message.Text text ->
                     List.mem
                       [ "Produce retained output."
                       ; "Interrupt this response by restart."
                       ]
                       (String.strip text.text)
                       ~equal:String.equal
                   | Image _ -> false)
               | _ -> false)
           in
           if not sent_message
           then Stdlib.Seq.empty
           else (
             incr gated_calls;
             phase "child-provider-entered";
             ignore (Eio.Promise.try_resolve enter ());
             Option.iter !child_pause ~f:(fun gate -> Eio.Promise.await gate);
             answer ~id:(sprintf "real-watch-answer-%d" !calls) (marker ^ large)))
         else (
           match !queued with
           | None -> Stdlib.Seq.empty
           | Some (name, arguments) ->
             queued := None;
             incr serial;
             function_call !serial name arguments)
       in
       let session_helpers =
         match helper_path with
         | None -> []
         | Some executable ->
           let limits = Shell_access.Request_channel.default_limits in
           let declaration : Agent_server.Session_helper_policy.t =
             { tool_name = "session_bridge"
             ; executable
             ; executable_sha256 =
                 Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / executable)
                 |> Chatmd_shell_spec.Source_ref.digest
             ; arguments = []
             ; operations = [ "read"; "status"; "wait" ]
             ; read_roots = [ C.physical_workspace fixture ]
             ; environment =
                 [ "PATH=/bin:/usr/bin"
                 ; "PAGER=cat"
                 ; "GIT_PAGER=cat"
                 ; "TERM=dumb"
                 ; "NO_COLOR=1"
                 ]
             ; private_paths = []
             ; max_request_bytes = limits.max_request_bytes
             ; max_response_bytes = limits.max_response_bytes
             ; max_requests = limits.max_requests
             }
           in
           Agent_server.Session_helper_policy.grants
             ~env
             ~protected_paths:[ C.config_path fixture; C.data_dir fixture ]
             [ declaration ]
           |> Result.ok_or_failwith
       in
       let options =
         { D.default_options with
           qualify_chatml_extensions = true
         ; session_helpers
         ; inference_policy =
             Agent_server_test_support.inference_policy
               ~default_model:"real-watch-fixture"
               ~post_stream:provider
         }
       in
       phase "fixture-ready";
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
         Support.Daemon_host.with_ env fixture ~options (fun _sw daemon ->
           try
             phase "daemon-ready";
             let durability =
               match
                 Agent_server.Config_parser.load ~env ~path:(C.config_path fixture)
               with
               | Error _ -> failwith "real watch configuration parse failed"
               | Ok raw ->
                 (match Agent_server.Config_validator.validate ~env raw with
                  | Error _ -> failwith "real watch configuration validation failed"
                  | Ok config -> config.server.durability)
             in
             F.require
               (durability.snapshot_every_events = 100
                && durability.snapshot_every_ms = 5000)
               "real watch must retain production checkpoint cadence";
             F.with_client ~sw:_sw env fixture (fun client ->
               let parent = F.create client "real-watch-parent" in
               phase "parent-created";
               let send index text =
                 ignore
                   (F.request
                      client
                      (Session_send_message
                         { session_id = parent.summary.id
                         ; attachment_id = parent.attachment_id
                         ; content = { kind = Plain_text; text; attachments = [] }
                         ; idempotency_key = F.key (sprintf "real-watch-send-%d" index)
                         })
                    : P.Method_result.t)
               in
               L.wait env "real watch parent initial idle" (fun () ->
                 Option.is_none (healthy_parent daemon parent.summary.id).active_operation);
               List.iter (List.range 0 8) ~f:(fun index ->
                 let initial = healthy_parent daemon parent.summary.id in
                 let previous = List.length initial.conversation.canonical_history in
                 let previous_attempts =
                   Agent_session.Inference_ledger.rows initial.inference_ledger
                   |> List.map ~f:(fun row ->
                     Agent_session.Inference_ledger.Row.handle row
                     |> Agent_session.Inference_ledger.Handle.ordinal)
                   |> Int64.Set.of_list
                 in
                 send index (String.make 1024 's');
                 L.wait env "real watch retained seed" (fun () ->
                   let current = healthy_parent daemon parent.summary.id in
                   let completed, pending =
                     Agent_session.Inference_ledger.rows current.inference_ledger
                     |> List.fold ~init:(false, false) ~f:(fun (completed, pending) row ->
                       let ordinal =
                         Agent_session.Inference_ledger.Row.handle row
                         |> Agent_session.Inference_ledger.Handle.ordinal
                       in
                       if Set.mem previous_attempts ordinal
                       then completed, pending
                       else (
                         match
                           Agent_session.Inference_ledger.Row.record row
                           |> Inference.Observation.Attempt_record.state
                         with
                         | Terminal terminal ->
                           (match Inference.Event.Terminal.outcome terminal with
                            | Completed -> true, pending
                            | Refused | Incomplete _ | Failed _ ->
                              raise_s
                                [%sexp
                                  "watch seed inference failed"
                                , (terminal : Inference.Event.Terminal.t)])
                         | Interrupted _ -> failwith "watch seed inference interrupted"
                         | Prepared | Running -> completed, true))
                   in
                   completed
                   && (not pending)
                   && Option.is_none current.active_operation
                   && List.is_empty current.conversation.deferred_user_entries
                   && List.length current.conversation.canonical_history > previous));
               phase "seeds-completed";
               let invoke name arguments =
                 L.wait env "real watch parent ready for invocation" (fun () ->
                   Option.is_none
                     (healthy_parent daemon parent.summary.id).active_operation);
                 phase ("invoke-" ^ name);
                 let before = (state daemon parent.summary.id).invocations in
                 queued := Some (name, arguments);
                 send (!serial + 100) name;
                 let outcome = ref None in
                 L.wait env "real watch invocation publication" (fun () ->
                   outcome
                   := completion (healthy_parent daemon parent.summary.id) name before;
                   Option.is_some !outcome);
                 Option.value_exn !outcome
               in
               let created =
                 invoke
                   "agent_create"
                   (`Object
                       [ "version", `Number "1"
                       ; "root_file", `String "child.chatmd"
                       ; ( "sources"
                         , `Array
                             [ `Object
                                 [ "path", `String "child.chatmd"
                                 ; ( "text"
                                   , `String
                                       "<developer>Real-clock child watcher \
                                        target.</developer>" )
                                 ]
                             ] )
                       ; "tools", `Array []
                       ; "start_immediately", `True
                       ; "idempotency_key", `String "real-watch-child"
                       ])
                 |> complete
               in
               let child = Jsonaf.member_exn "session_id" created in
               let child_session = P.Id.Session.of_json child |> F.protocol_ok in
               L.wait env "real watch child runnable" (fun () ->
                 let current = healthy_parent daemon child_session in
                 match current.lifecycle.observed with
                 | Idle -> Option.is_none current.active_operation
                 | _ -> false);
               phase "child-runnable";
               let sent =
                 invoke
                   "agent_send"
                   (`Object
                       [ "session_id", child
                       ; "message", `String "Produce retained output."
                       ; "idempotency_key", `String "real-watch-child-send"
                       ])
                 |> complete
               in
               phase "child-send-published";
               Eio.Promise.await entered;
               let started = Eio.Time.Mono.now (Eio.Stdenv.mono_clock env) in
               let subscription =
                 match
                   invoke
                     "notify_when_agent_responds"
                     (`Object
                         [ "session_id", child
                         ; "receipt_id", Jsonaf.member_exn "receipt_id" sent
                         ])
                 with
                 | Pending (Subscription id, _) -> id
                 | outcome ->
                   raise_s
                     [%sexp "real watch did not arm", (outcome : P.Invocation.outcome)]
               in
               let find () =
                 List.find_exn
                   (state daemon parent.summary.id).subscriptions
                   ~f:(fun item -> P.Id.Subscription.equal item.context.id subscription)
               in
               L.wait env "real watch timer armed" (fun () ->
                 Option.is_some (find ()).timer_id);
               phase "watch-timer-armed";
               Eio.Promise.resolve resume ();
               L.wait env "real watch committed notification" (fun () ->
                 List.exists
                   (state daemon parent.summary.id).deliveries
                   ~f:(fun delivery ->
                     match delivery.context.work, delivery.status with
                     | Some (Subscription id), Committed _ ->
                       P.Id.Subscription.equal id subscription
                     | _ -> false));
               let child_id = P.Id.Session.of_json child |> F.protocol_ok in
               let rec exact_text = function
                 | `String text -> String.equal text (marker ^ large)
                 | `Array items -> List.exists items ~f:exact_text
                 | `Object fields ->
                   List.exists fields ~f:(fun (_, value) -> exact_text value)
                 | `Null | `True | `False | `Number _ -> false
               in
               F.require
                 (List.exists
                    (state daemon child_id).conversation.canonical_history
                    ~f:(fun entry -> exact_text (P.History.entry_to_json entry)))
                 "real watch child canonical history lost exact escaped payload";
               let result = (find ()).result |> Option.value_exn in
               (match result with
                | Succeeded page ->
                  F.require
                    (String.is_substring (Jsonaf.to_string page) ~substring:marker)
                    "real watch lost target output"
                | result ->
                  raise_s [%sexp "real-clock watch failed", (result : P.Completion.t)]);
               L.wait env "real watch delivery drain" (fun () ->
                 let current = state daemon parent.summary.id in
                 Option.is_none current.active_operation
                 && List.for_all current.jobs ~f:(fun job ->
                   match job.status, job.delivery with
                   | ( (Succeeded | Failed _ | Cancelled | Interrupted _)
                     , (Delivered _ | Discarded _ | Not_required) ) -> true
                   | _ -> false));
               F.require
                 (List.count
                    (state daemon parent.summary.id).deliveries
                    ~f:(fun delivery ->
                      match delivery.context.work with
                      | Some (Subscription id) -> P.Id.Subscription.equal id subscription
                      | _ -> false)
                  = 1)
                 "real watch duplicated notification";
               Report.record
                 report
                 env
                 (if helper then "helper-watch-real-clock" else "native-watch-real-clock")
                 [ ( "duration_seconds"
                   , Jsonaf.Export.jsonaf_of_float
                       (elapsed (Eio.Stdenv.mono_clock env) started) )
                 ; "watch_timeout_ms", `Number "30000"
                 ; "checkpoint_events", `Number "100"
                 ; "checkpoint_ms", `Number "5000"
                 ; "retained_seed_turns", `Number "8"
                 ; "fragment_repeats", `Number "2500"
                 ; "provider_calls", `Number (Int.to_string !calls)
                 ; "gated_child_calls", `Number (Int.to_string !gated_calls)
                 ];
               let before_child_calls = !child_calls in
               let blocked, unblock = Eio.Promise.create () in
               release_child := unblock;
               child_pause := Some blocked;
               let sent =
                 invoke
                   "agent_send"
                   (`Object
                       [ "session_id", child
                       ; "message", `String "Interrupt this response by restart."
                       ; "idempotency_key", `String "real-watch-restart-send"
                       ])
                 |> complete
               in
               L.wait env "restart child provider entered" (fun () ->
                 !child_calls > before_child_calls);
               let restart_started = Eio.Time.Mono.now (Eio.Stdenv.mono_clock env) in
               let original =
                 match
                   invoke
                     "notify_when_agent_responds"
                     (`Object
                         [ "session_id", child
                         ; "receipt_id", Jsonaf.member_exn "receipt_id" sent
                         ])
                 with
                 | Pending (Subscription id, _) -> id
                 | outcome ->
                   raise_s
                     [%sexp "restart watch did not arm", (outcome : P.Invocation.outcome)]
               in
               L.wait env "restart watch timer armed" (fun () ->
                 List.exists
                   (state daemon parent.summary.id).subscriptions
                   ~f:(fun item ->
                     P.Id.Subscription.equal item.context.id original
                     && Option.is_some item.timer_id));
               phase "restart-watch-armed";
               restarted
               := Some (parent.summary.id, original, restart_started, !child_calls));
             phase "purposeful-daemon-shutdown"
           with
           | exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             (* Only failed fixture teardown releases its synthetic blocked port.
                Successful restart deliberately leaves it blocked so real daemon
                shutdown persists Interrupted rather than completing the child. *)
             Eio.Cancel.protect (fun () ->
               ignore (Eio.Promise.try_resolve !release_child ());
               child_pause := None;
               phase "failure-release-synthetic-gate");
             Stdlib.Printexc.raise_with_backtrace exn backtrace);
         phase "first-daemon-closed";
         child_pause := None;
         let parent_id, original, restart_started, expected_child_calls =
           Option.value_exn !restarted
         in
         Support.Daemon_host.with_ env fixture ~options (fun _sw daemon ->
           phase "restart-daemon-ready";
           L.wait env "real clock restart watch committed" (fun () ->
             List.exists (state daemon parent_id).deliveries ~f:(fun delivery ->
               match delivery.context.work, delivery.status with
               | Some (Subscription id), Committed _ ->
                 P.Id.Subscription.equal id original
               | _ -> false));
           let current = state daemon parent_id in
           let watched =
             List.find_exn current.subscriptions ~f:(fun item ->
               P.Id.Subscription.equal item.context.id original)
           in
           (match watched.result with
            | Some (Failed error) ->
              F.require
                (String.equal error.code "watcher.target_failed")
                "restart watcher lost interrupted receipt classification"
            | result ->
              raise_s
                [%sexp
                  "restart watch unexpected completion", (result : P.Completion.t option)]);
           let deliveries =
             List.count current.deliveries ~f:(fun delivery ->
               match delivery.context.work with
               | Some (Subscription id) -> P.Id.Subscription.equal id original
               | _ -> false)
           in
           F.require (deliveries = 1) "restart duplicated watch notification";
           F.require
             (!child_calls = expected_child_calls)
             "watch restart resubmitted child inference";
           Report.record
             report
             env
             (if helper then "helper-watch-real-restart" else "native-watch-real-restart")
             [ ( "duration_seconds"
               , Jsonaf.Export.jsonaf_of_float
                   (elapsed (Eio.Stdenv.mono_clock env) restart_started) )
             ; "watch_timeout_ms", `Number "30000"
             ; ( "provider_replays"
               , `Number (Int.to_string (!child_calls - expected_child_calls)) )
             ; "original_subscription_preserved", `True
             ; "deliveries", `Number (Int.to_string deliveries)
             ])))
;;

let run env report =
  run_variant env report ~helper:false;
  run_variant env report ~helper:true
;;
