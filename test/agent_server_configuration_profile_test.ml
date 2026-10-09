open! Core
open Agent_server_test_support
module P = Agent_protocol
module A = Agent_session.Session_actor
module B = Inference_host.Credential_bridge
module S = Inference_host.Provider_configuration
module D = Openai.Responses_driver
module R = Inference.Request
module RT = Inference_runtime
module Bridge_fixture = Credential_bridge_tests.Bridge_tests
module Provider_fixture = Inference_openai_tests.Adapter_tests
module Worker_fixture = Agent_session_test.Authoring_input_tests
module State_fixture = Agent_session_test.Inference_target_tests

let ok result =
  Result.map_error result ~f:(fun _ -> "configuration profile fixture")
  |> Result.ok_or_failwith
;;

let%expect_test "configuration RPC selects a declared same-owner profile at next root" =
  Bridge_fixture.with_fixture (fun env sw anchor ->
    Mirage_crypto_rng_unix.use_default ();
    let requests = ref [] in
    let response = ref 0 in
    let completions = ref [] in
    Provider_fixture.with_server
      env
      (fun body ->
         requests := !requests @ [ body ];
         Int.incr response;
         match !response with
         | 1 ->
           let call =
             Jsonaf.of_string
               {|{"type":"function_call","id":"configuration-item","call_id":"configuration-call","name":"block","arguments":"{}","status":"completed"}|}
           in
           Provider_fixture.frame
             (`Object
                 [ "type", `String "response.output_item.done"
                 ; "sequence_number", `Number "1"
                 ; "output_index", `Number "0"
                 ; "item", call
                 ])
           ^ Provider_fixture.terminal [ call ]
         | 2 | 3 -> Provider_fixture.terminal []
         | _ -> failwith "unexpected extra paid-request fixture dispatch")
      (fun _ endpoint ->
         let original, identity = Bridge_fixture.mapping "one" in
         let profile =
           D.Profile.create
             ~id:"one"
             ~account:None
             ~endpoint
             ~capabilities:
               (D.Capability.create
                  ~baseline:
                    [ Text_input, Supported
                    ; Function_tools, Supported
                    ; Opaque_replay, Supported
                    ; Setting "temperature", Supported
                    ]
                  ~models:[]
                |> ok)
             ~defaults:[]
           |> ok
         in
         let canonical =
           B.Mapping.create
             profile
             ~revision:"canonical-v1"
             ~binding:(B.Mapping.binding original)
             ~identity
           |> ok
         in
         let environment =
           B.Environment.Entry.create
             ~binding:(B.Mapping.binding canonical)
             ~identity
             ~name:"ONE"
             ~configuration_revision:None
             ~resolve:(fun ~sw:_ ->
               Ok
                 (Credential_registry.Environment.resolved
                    ~access:
                      (Provider_secret_store.Secret.of_bytes
                         (Bytes.of_string "synthetic-private-fixture-key")
                       |> ok)
                    ~configuration_revision:None
                    ~check_current:(fun () -> Ok ())))
             ~status:(fun () -> Available)
           |> ok
           |> List.return
           |> B.Environment.create
           |> ok
         in
         let opened =
           Bridge_fixture.open_host
             env
             sw
             anchor
             (Initialize (Bridge_fixture.id "configuration-profile-host"))
             ~mappings:[ canonical ]
             ~environment:(Some environment)
             ~authorize:Bridge_fixture.authorize
           |> ok
         in
         let bridge =
           B.create
             ~compatible_profiles:
               (B.Compatible_profile.of_string
                  {|[{"id":"alternative","credential_owner":"one","revision":"choice-v1","defaults":{}}]|}
                |> ok)
             ~approved_profiles:[ "one" ]
             (D.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) () |> ok)
             ~registry:(S.Opened.registry opened)
             ~mappings:[ canonical ]
             ~authorize:Bridge_fixture.authorize
             ~clock:(Eio.Stdenv.mono_clock env)
             ~maximum_wait:(Time_ns.Span.of_sec 0.05)
             ~transport_policy:Http_sse
             ~limits:RT.Limits.default
           |> ok
         in
         Bridge_fixture.configure bridge "one" "configure-profile-owner";
         let rec backend bridge =
           Inference_host.Backend.create
             ~capture_profile:(fun ~current ~profile ->
               B.capture
                 bridge
                 ~principal:"operator"
                 ~default_profile:profile
                 ~current:None
                 ~model:(R.Target.model current)
                 ~settings:[]
               |> Result.map_error ~f:B.preparation_error)
             ~capture:(fun ~current ~model ~settings ->
               B.capture
                 bridge
                 ~principal:"operator"
                 ~default_profile:"one"
                 ~current
                 ~model
                 ~settings
               |> Result.map_error ~f:B.preparation_error)
             ~resolve:(B.resolver bridge ~principal:"operator")
             ~with_response_limit:(fun ~max_body_bytes ->
               B.with_response_limit bridge ~max_body_bytes
               |> Result.map ~f:backend
               |> Result.map_error ~f:B.preparation_error)
         in
         let host =
           Inference_host.create_with_backend
             (backend bridge)
             ~default_model:"fixture-model"
             ~namespace:"configuration-profile-rpc"
           |> ok
         in
         let policy =
           Agent_server.Session_factory.
             { capture_inference_target =
                 (fun ~prompt_revision_id:_ ~config ->
                   Inference_host.capture_config host config)
             ; select_inference_profile = Inference_host.capture_profile host
             ; recapture_inference_target =
                 (fun ~current ~prompt_revision_id:_ ~config ->
                   Inference_host.recapture_config host ~current config)
             ; migrate_inference_target = None
             ; migrate_model_job_target = None
             ; approve_inference_target_change =
                 (fun ~current:_ ~proposed ->
                   Inference_host.resolve host proposed |> Result.map ~f:ignore)
             ; resolve_inference_context = Inference_host.resolve host
             ; runtime_inference_ports =
                 (fun _ ->
                   Ok
                     { new_preparation_id =
                         (Inference_host.identity host).new_preparation_id
                     ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
                     ; on_attempt = ignore
                     ; on_observation = ignore
                     ; on_completion = ignore
                     })
             }
         in
         let root = temporary_root env in
         Exn.protect
           ~finally:(fun () ->
             Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
           ~f:(fun () ->
             let workspace = Filename.concat root "workspace" in
             let prompt_file = Filename.concat root "agent.chatmd" in
             Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               Eio.Path.(Eio.Stdenv.fs env / prompt_file)
               "<developer>Offline compatible profile fixture.</developer>";
             let daemon =
               Agent_server.Daemon.start
                 ~sw
                 ~env
                 ~config:(config root workspace prompt_file)
                 ~tool_dir:root
                 ~home:root
                 ~process_start_identity:None
                 ~options:
                   { Agent_server.Daemon.default_options with inference_policy = policy }
                 ()
               |> protocol_ok
             in
             Exn.protect
               ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
               ~f:(fun () ->
                 let connection = connection daemon (principal ()) in
                 initialize connection;
                 let session, attachment = create_session connection in
                 let entry =
                   Agent_server.Session_registry.find
                     (Agent_server.Daemon.registry daemon)
                     session.id
                   |> Option.value_exn
                 in
                 let state () = A.state entry.actor |> protocol_ok in
                 let selected =
                   match Inference.Selection.view (state ()).spec.inference_target with
                   | Captured target -> target
                   | Unresolved -> failwith "missing canonical target"
                 in
                 let initial = Inference_host.resolve host selected |> ok in
                 let began, began_r = Eio.Promise.create () in
                 let release, release_r = Eio.Promise.create () in
                 let root_scope = ref None in
                 let profiles = ref [] in
                 let tool_tbl = String.Table.create () in
                 Hashtbl.set tool_tbl ~key:"block" ~data:(fun ~invocation:_ _ ->
                   Eio.Promise.resolve began_r ();
                   Eio.Promise.await release;
                   let id =
                     History_entry.Id.create ~namespace:"profile-child" ~sequence:0 |> ok
                   in
                   let request =
                     R.create
                       ~target:selected
                       ~history:
                         [ Agent_session.History_codec.user_text ~id "captured child" ]
                       ~tools:[]
                       ~assets:[]
                       ~limits:Transcript.Admission.default
                     |> ok
                   in
                   let child =
                     Inference_client.Execution.create
                       ~context:initial
                       ~identity:(Inference_host.identity host)
                       ~relation:
                         (Nested
                            { scope = Transcript.Scope.key (Option.value_exn !root_scope)
                            ; call_entry_id = None
                            ; call_alias = Some "configuration-call"
                            })
                       ~before_dispatch:ignore
                       ~on_attempt:(fun attempt ->
                         profiles
                         := !profiles
                            @ [ Inference.Observation.Configuration.profile
                                  (RT.Attempt.configuration attempt)
                              ])
                       ~on_completion:ignore
                       ~on_observation:ignore
                   in
                   Eio.Switch.run (fun sw ->
                     Inference_client.Execution.run child ~sw ~request ~on_event:ignore
                     |> ignore);
                   Openai.Responses.Tool_output.Output.Text "released");
                 let worker_config =
                   Worker_fixture.worker_config env (fun ~sw:_ ~inputs:_ ->
                     failwith "fake adapter must not dispatch")
                 in
                 let worker =
                   Agent_session.Turn_worker.create
                     ~root_binding:
                       (Chat_response.Root_binding.create (RT.Session.create ~sw))
                     { worker_config with
                       inference_context = initial
                     ; inference_identity = Inference_host.identity host
                     ; tool_tbl
                     ; tools =
                         [ Openai.Responses.Request.Tool.Function
                             { name = "block"
                             ; description = Some "Offline test barrier"
                             ; parameters =
                                 `Object
                                   [ "type", `String "object"
                                   ; "properties", `Object []
                                   ; "required", `Array []
                                   ; "additionalProperties", `False
                                   ]
                             ; strict = true
                             ; type_ = "function"
                             }
                         ]
                     ; on_inference_completion =
                         (fun completion ->
                           let evidence =
                             match Inference_client.Completion.outcome completion with
                             | Returned terminal ->
                               Inference.Event.Terminal.sexp_of_t terminal
                             | Interrupted { reason; delivery } ->
                               [%sexp
                                 (reason
                                  : Inference.Observation.Attempt_record.interruption)
                               , (delivery : Inference.Event.Terminal.delivery)]
                           in
                           completions := !completions @ [ evidence ])
                     ; permission_profile =
                         Agent_session_test.Fixtures.permission_policy
                           ~tool_default:Allow
                           ~fallback:Fallback_deny
                           ~evaluator:None
                           ~reviewer:None
                     ; on_inference_attempt =
                         (fun attempt ->
                           root_scope := Some (RT.Attempt.scope attempt);
                           profiles
                           := !profiles
                              @ [ Inference.Observation.Configuration.profile
                                    (RT.Attempt.configuration attempt)
                                ])
                     }
                 in
                 (* Starting through the host loads its runtime/owner before the
                    fixture installs its actual turn/tool worker. Actor.start alone
                    does not acquire the factory's runtime capability. *)
                 Agent_client.Connection.request_without_history
                   connection
                   (P.Command.Session_start
                      { session_id = session.id
                      ; attachment_id = attachment.id
                      ; queue_if_limited = false
                      ; idempotency_key =
                          P.Idempotency_key.of_string "profile-start" |> protocol_ok
                      })
                 |> protocol_ok
                 |> ignore;
                 assert (Agent_server.Runtime_owner.is_loaded entry.runtime);
                 A.set_operation_worker entry.actor (Some worker) |> protocol_ok;
                 A.add_job entry.actor (State_fixture.job (state ()))
                 |> protocol_ok
                 |> ignore;
                 Agent_client.Connection.request_without_history
                   connection
                   (P.Command.Session_send_message
                      { session_id = session.id
                      ; attachment_id = attachment.id
                      ; content = { kind = Plain_text; text = "hello"; attachments = [] }
                      ; idempotency_key =
                          P.Idempotency_key.of_string "profile-message" |> protocol_ok
                      ; timing = Agent_protocol.Pending_input.Timing.Safe_boundary
                      })
                 |> protocol_ok
                 |> ignore;
                 let fail_before_tool reason =
                   let current = state () in
                   raise_s
                     [%sexp
                       "configuration RPC did not reach registered tool"
                     , (reason : string)
                     , (current.lifecycle.observed : P.Session.observed_state)
                     , (current.failure : P.Error.t option)
                     , { http_requests = (List.length !requests : int)
                       ; prepared_profiles = (!profiles : string list)
                       ; completions = (!completions : Sexp.t list)
                       }]
                 in
                 (try
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                      Eio.Fiber.first
                        (fun () -> Eio.Promise.await began)
                        (fun () ->
                           let rec await_terminal () =
                             if Option.is_none (state ()).active_operation
                             then fail_before_tool "operation ended before tool dispatch"
                             else (
                               Eio.Fiber.yield ();
                               await_terminal ())
                           in
                           await_terminal ()))
                  with
                  | Eio.Time.Timeout -> fail_before_tool "tool boundary timed out");
                 let before = state () in
                 let patch =
                   P.Session_configuration.Patch.create
                     ~profile:"alternative"
                     ~settings:
                       [ R.Setting.create
                           ~name:"temperature"
                           ~value:(Value (`Number "0.5"))
                           ~provenance:Execution_override
                           ~limits:Document_schema.Limits.default
                         |> ok
                       ]
                     ()
                   |> protocol_ok
                 in
                 let result =
                   Agent_client.Connection.request_without_history
                     connection
                     (P.Command.Session_configuration_update
                        { session_id = session.id
                        ; attachment_id = attachment.id
                        ; expected_generation = before.identity.generation
                        ; expected_revision = before.spec.configuration_revision
                        ; patch
                        ; idempotency_key =
                            P.Idempotency_key.of_string "select-compatible-profile"
                            |> protocol_ok
                        })
                   |> function
                   | Ok result -> result
                   | Error rpc_error ->
                     (* Diagnose the exact retained history against the proposed
                        real host Context, without stripping or rewriting it. *)
                     let profile_target =
                       Inference_host.capture_profile
                         host
                         ~current:selected
                         ~profile:"alternative"
                       |> ok
                     in
                     let proposed =
                       Agent_session.Configuration_transition.apply
                         selected
                         ~patch
                         ~profile_target:(Some profile_target)
                       |> protocol_ok
                     in
                     let context = Inference_host.resolve host proposed |> ok in
                     let history =
                       Agent_session.History_codec.all_of_protocol
                         (before.conversation.canonical_history
                          @ List.map
                              before.conversation.deferred_user_entries
                              ~f:Agent_session.Pending_input_document.entry)
                       |> protocol_ok
                     in
                     let preflight = RT.Context.preflight_history context history in
                     raise_s
                       [%sexp
                         "configuration RPC rejected actual retained history"
                       , (rpc_error : P.Error.t)
                       , (preflight : (unit, RT.Preparation_error.t) Result.t)]
                 in
                 (match result with
                  | P.Method_result.Session_configuration_update _ -> ()
                  | _ -> failwith "wrong RPC result");
                 let next =
                   match Inference.Selection.view (state ()).spec.inference_target with
                   | Captured target -> target
                   | Unresolved -> assert false
                 in
                 [%test_eq: string] "alternative" (R.Target.profile next);
                 (match R.Target.auth_binding selected, R.Target.auth_binding next with
                  | Value one, Value two -> assert (R.Auth_binding.equal one two)
                  | _ -> assert false);
                 let job_target =
                   List.hd_exn (state ()).model_job_targets
                   |> Agent_session.Model_job_target.source
                   |> Inference.Selection.view
                 in
                 (match job_target with
                  | Captured target -> [%test_eq: string] "one" (R.Target.profile target)
                  | Unresolved -> assert false);
                 Eio.Promise.resolve release_r ();
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                   let rec await () =
                     if
                       Option.is_none (state ()).active_operation
                       && observed_idle (state ()).lifecycle.observed
                     then ()
                     else (
                       Eio.Fiber.yield ();
                       await ())
                   in
                   await ());
                 [%test_eq: string list] [ "one"; "one"; "alternative" ] !profiles;
                 [%test_eq: int] 3 (List.length !requests);
                 let body = List.last_exn !requests in
                 let input =
                   match Document_schema.Json.field body ~name:"input" with
                   | Value (`Array input) -> input
                   | _ -> failwith "next root did not dispatch retained native input"
                 in
                 let paired kind =
                   List.filter input ~f:(fun item ->
                     match
                       ( Document_schema.Json.field item ~name:"type"
                       , Document_schema.Json.field item ~name:"call_id" )
                     with
                     | Value (`String actual), Value (`String call_id) ->
                       String.equal actual kind
                       && String.equal call_id "configuration-call"
                     | _ -> false)
                 in
                 [%test_eq: int] 1 (List.length (paired "function_call"));
                 [%test_eq: int] 1 (List.length (paired "function_call_output"));
                 (match
                    Document_schema.Json.field
                      (List.hd_exn (paired "function_call_output"))
                      ~name:"output"
                  with
                  | Value (`String output) -> [%test_eq: string] "released" output
                  | _ -> failwith "next root lost the authored native tool result");
                 let canonical =
                   Agent_session.History_codec.all_of_protocol
                     (state ()).conversation.canonical_history
                   |> protocol_ok
                 in
                 let retained_call =
                   List.find_exn canonical ~f:(fun entry ->
                     match
                       History_entry.Payload.Semantic.view
                         (History_entry.Payload.semantic (History_entry.payload entry))
                     with
                     | Call { name; _ } -> String.equal name "block"
                     | Message _ | Result _ | Reasoning _ | Unknown _ -> false)
                 in
                 (match
                    History_entry.Payload.representation
                      (History_entry.payload retained_call)
                  with
                  | Captured { origin; raw } ->
                    [%test_eq: string option]
                      (Some "one")
                      (History_entry.Payload.Origin.profile origin);
                    assert (
                      Document_schema.Json.equal
                        raw
                        (List.hd_exn (paired "function_call")))
                  | Authored | Reconstructed _ ->
                    failwith "original native capture evidence lost");
                 (match Document_schema.Json.field body ~name:"temperature" with
                  | Value (`Number value) -> [%test_eq: float] 0.5 (Float.of_string value)
                  | _ -> failwith "real next root HTTP body omitted B settings");
                 print_endline
                   "actual RPC: root A, captured child A, next root B HTTP body; one \
                    binding and job A"))));
  [%expect
    {|actual RPC: root A, captured child A, next root B HTTP body; one binding and job A|}]
;;
