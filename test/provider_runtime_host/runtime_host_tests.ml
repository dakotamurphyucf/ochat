open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module C = Credential_registry
module S = Private_storage
module Secret = Provider_secret_store
module B = Inference_host.Credential_bridge
module A = Provider_operator.Profile_admin
module Actor = Operator_authorization
module Runtime = Provider_runtime
module Port = Agent_server.Provider_operator_port
module Backend = Inference_host.Backend
module RT = Inference_runtime
module Root = Agent_server.Developer_root_owner
module Run_store = Agent_store.Developer_run_store
module Session_store = Agent_store.Session_store
module Root_fixture = Developer_root_owner_test_suite.Root_owner_tests
module Store_fixture = Developer_run_store_test_suite.Developer_run_store_tests
module Native_fixture = Developer_root_owner_test_suite.Native_executor_tests
module Jobs = Agent_server.Developer_script_job_service
module Disclosure = Agent_server.Developer_result_disclosure
module Native = Agent_server.Developer_native_executor
module Capabilities = Chat_response.Tool_capability

exception Model_ack_failed

let ok r =
  Result.map_error r ~f:(fun _ -> "synthetic runtime-host fixture failed")
  |> Result.ok_or_failwith
;;

let inference_usage ~scope ~accounting_id =
  let module O = Inference.Observation in
  let actual value = O.Count.create (Actual value) |> ok in
  let unknown = O.Count.create (Unknown Not_reported) |> ok in
  let counts =
    O.Usage.
      { input = actual 1L
      ; output = actual 0L
      ; reported_total = unknown
      ; cached_input = unknown
      ; cache_write_input = unknown
      ; reasoning_output = unknown
      }
  in
  O.create
    ~scope
    ~id:accounting_id
    ~revision:1L
    ~payload:(Usage (O.Usage.create ~counts ~inclusions:[] |> ok))
    ~limits:O.Admission.observation
  |> ok
;;

let synthetic_call_context target =
  let module E = Inference.Event in
  let module H = History_entry.Payload in
  let rounds = ref 0 in
  let call =
    H.Semantic.create
      (Call
         { kind = Function
         ; name = "inspect"
         ; namespace = Absent
         ; input_bytes = "{}"
         ; async = Absent
         })
      ~metadata:{ H.Metadata.empty with call_id = Value "nested-call" }
    |> ok
    |> H.authored
  in
  let answer =
    H.Semantic.create
      (Message
         { form = Output
         ; role = Assistant
         ; content = [ Text { text = "done"; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:H.Metadata.empty
    |> ok
    |> H.authored
  in
  let candidate scope id payload local_execution =
    let semantic = H.semantic payload in
    let call_name =
      match H.Semantic.view semantic with
      | Call { name; _ } -> Some name
      | Message _ | Result _ | Reasoning _ | Unknown _ -> None
    in
    let item =
      Transcript.Item.create
        ~scope
        ~id:(Transcript.Item_id.of_string id |> ok)
        ~entry_id:None
        ~header:(Some (Transcript.Header.of_semantic semantic))
        ~call_name
      |> ok
    in
    E.create
      (Candidate_ready { item; payload; local_execution })
      ~limits:Transcript.Admission.default
    |> ok
  in
  let run
        _request
        ~sw:_
        ~scope
        ~accounting_id
        ~note_delivery:_
        ~on_event
        ~on_observation:_
        ~submission_guard:_
    =
    incr rounds;
    let output =
      if !rounds = 1
      then [ candidate scope "nested-eligible" call Tool_candidate ]
      else [ candidate scope "nested-answer" answer Not_eligible ]
    in
    List.iter output ~f:on_event;
    RT.Receipt.create
      ~terminal:
        (E.Terminal.create ~scope ~delivery:Response_started ~outcome:Completed |> ok)
      ~usage:(inference_usage ~scope ~accounting_id)
      ~output
      ~output_coverage:Response_output
      ~limits:RT.Limits.default
    |> ok
  in
  let prepare ~preparation_id request =
    let configuration =
      Inference.Observation.Configuration.of_target
        (Inference.Request.target request)
        ~preparation_id
        ~transport:In_process
        ~capabilities:[]
        ~limits:Inference.Observation.Admission.observation
      |> ok
    in
    RT.Plan.create
      ~request
      ~configuration
      ~fingerprint:"synthetic-call"
      ~run:(run request)
  in
  let adapter =
    RT.Adapter.create
      ~preflight_history:(fun ~target:_ _ -> Ok ())
      ~id:(Inference.Request.Target.adapter target)
      ~limits:RT.Limits.default
      ~bind:(fun _ -> Ok ())
      ~prepare
      ()
    |> ok
  in
  RT.Context.create adapter ~target |> ok, rounds
;;

let id s = M.Id.create s |> ok
let profile s = DTO.Profile_id.of_string s |> ok
let key s = P.Idempotency_key.of_string s |> ok

let principal =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_operator" |> ok)
    ~authentication_kind:"synthetic"
    ~scopes:(P.Scope.Set.of_list [ Provider_view; Provider_manage; Provider_select ])
    ~attributes:[]
  |> ok
;;

let denied =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_denied" |> ok)
    ~authentication_kind:"synthetic"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> ok
;;

let actor = Actor.trusted_local principal
let denied_actor = Actor.trusted_local denied

let authorized actor =
  Actor.is_current actor && P.Id.Principal.equal (Actor.principal actor).id principal.id
;;

let components = [ S.Name.create "authority" |> ok ]
let namespace = Secret.Namespace.create "runtime-host-test" |> ok
let server_id = P.Id.Server.of_string "srv_runtime_host" |> ok

let local_model_endpoint ?response_body env sw requests on_submit on_body =
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:4
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "model fixture did not bind TCP"
  in
  let completed =
    "data: \
     {\"type\":\"response.completed\",\"sequence_number\":1,\"response\":{\"object\":\"response\",\"id\":\"model-response\",\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"id\":\"model-message\",\"role\":\"assistant\",\"status\":\"completed\",\"content\":[{\"type\":\"output_text\",\"text\":\"model \
     answer\",\"annotations\":[]}] }]}}\n\n"
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    while true do
      let flow, _ = Eio.Net.accept ~sw socket in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        Exn.protect
          ~finally:(fun () -> Eio.Flow.close flow)
          ~f:(fun () ->
            let reader = Eio.Buf_read.of_flow flow ~max_size:2_000_000 in
            ignore (Eio.Buf_read.line reader : string);
            let rec headers length =
              let line = Eio.Buf_read.line reader in
              if String.is_empty line
              then length
              else (
                let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
                headers
                  (if String.Caseless.equal key "content-length"
                   then Int.of_string (String.strip value)
                   else length))
            in
            let body_length = headers 0 in
            let body = Eio.Buf_read.take body_length reader in
            incr requests;
            on_body body;
            on_submit ();
            let response =
              Option.bind response_body ~f:(fun respond -> respond !requests body)
              |> Option.value ~default:completed
            in
            Eio.Flow.copy_string
              (sprintf
                 "HTTP/1.1 200 OK\r\n\
                  Content-Type: text/event-stream\r\n\
                  Content-Length: %d\r\n\
                  Connection: close\r\n\
                  \r\n\
                  %s"
                 (String.length response)
                 response)
              flow);
        `Stop_daemon)
    done);
  sprintf "http://127.0.0.1:%d/v1/responses" port
;;

let with_fixture
      ?model_requests
      ?response_body
      ?(model_function_tools = false)
      ?(model_image_input = false)
      ?(model_reasoning = false)
      ?(on_model_submit = ignore)
      ?(on_model_body = ignore)
      f
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let suffix = P.Id.Transaction.create () |> P.Id.Transaction.to_string in
    let anchor =
      Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-runtime-host-" ^ suffix))
    in
    Eio.Path.mkdir ~perm:0o700 anchor;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let model_endpoint =
            match model_requests with
            | None -> "http://127.0.0.1:9/v1/responses"
            | Some requests ->
              local_model_endpoint
                ?response_body
                env
                sw
                requests
                on_model_submit
                on_model_body
          in
          let driver =
            Openai.Responses_driver.create
              ~net:(Eio.Stdenv.net env)
              ~clock:(Eio.Stdenv.clock env)
              ()
            |> ok
          in
          let transport =
            Provider_oauth.Transport.create
              ~net:(Eio.Stdenv.net env)
              ~clock:(Eio.Stdenv.mono_clock env)
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Provider_oauth.Transport.close transport)
            ~f:(fun () ->
              let oauth =
                Provider_oauth_registry.create
                  ~transport
                  ~policy:
                    (Provider_oauth.Policy.direct_codex
                       ~expected_account:None
                       ~callback_port:1455
                       ()
                     |> ok)
                  ~wall_clock:(Eio.Stdenv.clock env)
              in
              let make name =
                let identity =
                  M.Identity.api_key
                    ~host:(id "host")
                    ~provider:"openai"
                    ~billing:"api"
                    ~account:None
                    ~key_reference:(id name)
                  |> ok
                in
                let route =
                  Openai.Responses_driver.Profile.create
                    ~id:name
                    ~account:None
                    ~endpoint:model_endpoint
                    ~capabilities:
                      (Openai.Responses_driver.Capability.create
                         ~baseline:
                           ([ ( Openai.Responses_driver.Capability.Text_input
                              , Openai.Responses_driver.Capability.Supported )
                            ]
                            @ (if model_image_input
                               then
                                 [ ( Openai.Responses_driver.Capability.Image_input
                                   , Openai.Responses_driver.Capability.Supported )
                                 ]
                               else [])
                            @ (if model_reasoning
                               then
                                 [ ( Openai.Responses_driver.Capability.Setting "reasoning"
                                   , Openai.Responses_driver.Capability.Supported )
                                 ]
                               else [])
                            @
                            if model_function_tools
                            then
                              [ ( Openai.Responses_driver.Capability.Function_tools
                                , Openai.Responses_driver.Capability.Supported )
                              ; ( Openai.Responses_driver.Capability.Opaque_replay
                                , Openai.Responses_driver.Capability.Supported )
                              ]
                            else [])
                         ~models:[]
                       |> ok)
                    ~defaults:[]
                  |> ok
                in
                let mapping =
                  B.Mapping.create
                    route
                    ~revision:"configured"
                    ~binding:(id name)
                    ~identity
                  |> ok
                in
                let template =
                  A.Template.create
                    ~profile:(profile name)
                    ~binding:(id name)
                    ~revision:(DTO.Revision.of_string "configured" |> ok)
                    ~authentication:Api_key
                    ~expectation:(M.Expectation.exact identity)
                    ~expected_account:None
                    ~mapping:(fun actual ->
                      if M.Identity.equal identity actual
                      then Ok mapping
                      else Error B.Error.Invalid_mapping)
                  |> ok
                in
                template, mapping
              in
              let declarations = List.map [ "one"; "two" ] ~f:make in
              let serial = ref 0
              and login_calls = ref 0 in
              let new_operation () =
                incr serial;
                id (sprintf "operation-%d" !serial)
              in
              let create ?runtime_sw ?runtime_server_id ?runtime_anchor () =
                let sw = Option.value runtime_sw ~default:sw in
                let server_id = Option.value runtime_server_id ~default:server_id in
                let anchor = Option.value runtime_anchor ~default:anchor in
                Provider_runtime_host.create
                  ~compatible_profiles:[]
                  ~sw
                  ~env
                  ~server_id
                  ~anchor
                  ~components
                  ~host:(id "host")
                  ~secret_namespace:namespace
                  ~driver
                  ~templates:(List.map declarations ~f:fst)
                  ~mappings:(List.map declarations ~f:snd)
                  ~default_profile:(profile "one")
                  ~environment:None
                  ~environment_sources:[]
                  ~oauth
                  ~oauth_lease:None
                  ~start_login:(fun ~sw:_ ~template:_ ~mode:_ ->
                    incr login_calls;
                    failwith "unexpected OAuth login")
                  ~inference_principal:"operator"
                  ~authorize_bridge:(fun ~principal ~profile:_ ~operation:_ ->
                    String.equal principal "operator"
                    || String.equal principal "pri_operator")
                  ~authorize:(fun p ~operation:_ ~profile:_ -> authorized p)
                  ~authorize_setup:authorized
                  ~authorize_status:authorized
                  ~new_operation
                  ~new_revision:(fun () ->
                    incr serial;
                    DTO.Revision.of_string (sprintf "revision-%d" !serial) |> ok)
                  ~maximum_wait:(Time_ns.Span.of_sec 0.05)
                  ~limits:DTO.Limits.default
                  ~inference_limits:RT.Limits.default
                  ~transport_policy:Http_sse
                |> ok
              in
              f env sw anchor create login_calls))))
;;

let setup_request = { DTO.Setup_request.idempotency_key = key "original-setup" }

let setup runtime =
  Runtime.dispatch runtime ~actor (P.Command.Provider_setup setup_request) |> ok
;;

let status runtime =
  match
    Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None }) |> ok
  with
  | P.Method_result.Provider_status result -> result
  | _ -> failwith "wrong status result"
;;

let enroll runtime ~sw name key_name reads =
  Runtime.enroll_private_key
    runtime
    ~actor
    ~profile:(profile name)
    ~key:(key key_name)
    ~source_reference:("synthetic:" ^ key_name)
    ~sw
    ~read:(fun ~sw:_ ->
      incr reads;
      Secret.Secret.of_bytes (Bytes.of_string "synthetic-protected-key")
      |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential))
  |> ok
;;

let capture runtime =
  Backend.capture (Runtime.backend runtime) ~current:None ~model:"synthetic" ~settings:[]
  |> ok
;;

let%expect_test
    "developer model selection uses the actor principal and live opened bridge"
  =
  with_fixture (fun _ sw _ create _ ->
    let runtime = create () in
    let port = Runtime.operator_port runtime in
    (match Runtime.current_model_bridge runtime with
     | Error Store_unavailable -> ()
     | Ok _ | Error _ -> failwith "pre-setup bridge was exposed");
    ignore (setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (enroll runtime ~sw "one" "model-key" reads : DTO.Configuration_result.t);
    let target = capture runtime in
    (match Port.select_model port ~actor:denied_actor ~target with
     | Error error when P.Error.equal_code error.code Permission_denied -> ()
     | Ok _ | Error _ -> failwith "denied developer borrowed conversation authority");
    let selected = Port.select_model port ~actor ~target |> ok in
    assert (phys_equal (Port.Model_selection.actor selected) actor);
    assert (Inference.Request.Target.equal (Port.Model_selection.target selected) target);
    Port.Model_selection.check_current selected |> ok;
    ignore (enroll runtime ~sw "two" "other-model-key" reads : DTO.Configuration_result.t);
    let current_selection = (status runtime).selection |> Option.value_exn in
    ignore
      (Runtime.dispatch
         runtime
         ~actor
         (P.Command.Provider_select
            { profile = profile "two"
            ; expected_revision = current_selection.revision
            ; idempotency_key = key "other-default"
            })
       |> ok
       : P.Method_result.t);
    assert (String.equal (Inference.Request.Target.profile (capture runtime)) "two");
    assert (Inference.Request.Target.equal (Port.Model_selection.target selected) target);
    Port.Model_selection.check_current selected |> ok;
    Runtime.close runtime;
    assert (Result.is_error (Port.Model_selection.check_current selected));
    let reopened = create () in
    let reopened_port = Runtime.operator_port reopened in
    let reopened_target = capture reopened in
    let live = ref true in
    let expiring = Actor.guarded ~principal ~is_current:(fun () -> !live) in
    let expiring_selection =
      Port.select_model reopened_port ~actor:expiring ~target:reopened_target |> ok
    in
    live := false;
    assert (Result.is_error (Port.Model_selection.check_current expiring_selection));
    let retained = Port.select_model reopened_port ~actor ~target:reopened_target |> ok in
    Port.close reopened_port;
    assert (Result.is_error (Port.Model_selection.check_current retained));
    Port.close port);
  print_endline
    "pre-created port selected the real actor bridge; denied and closed issuers stayed \
     denied";
  [%expect
    {| pre-created port selected the real actor bridge; denied and closed issuers stayed denied |}]
;;

let%test_unit "saved Agent keeps baseline and derived child provider targets separate" =
  let model_requests = ref 0 in
  with_fixture ~model_requests ~model_reasoning:true (fun env sw anchor create _ ->
    let runtime = create () in
    ignore (setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (enroll runtime ~sw "one" "saved-agent-key" reads : DTO.Configuration_result.t);
    let target = capture runtime in
    let selected_model =
      Port.select_model (Runtime.operator_port runtime) ~actor ~target |> ok
    in
    let session =
      Session_store.create
        ~env
        ~sw
        ~root:(Eio.Path.native_exn Eio.Path.(anchor / "saved-agent-runs"))
        ~server_id
        ~process_start_identity:None
        ~lock_nonce:"saved-agent-model-test"
      |> ok
    in
    let store =
      Run_store.open_or_create
        session
        ~sw
        ~limits:
          (Root_fixture.store_limits
             ~max_job_payload_bytes:4096
             ~max_jobs_per_run:5
             ~max_document_bytes:131072
             ~max_inference_bytes_per_run:65536
             ~max_work_steps:4096
             ~max_document_fields:8192
             ~max_document_nodes:16384
             ())
        ~checkpoint:ignore
      |> ok
    in
    let owner =
      match
        P.Execution_owner.developer
          ~host_id:server_id
          ~run_id:(P.Id.Developer_run.of_string "dvr_saved_agent_model" |> ok)
          ~epoch:0
          ~principal_id:principal.id
        |> ok
      with
      | Developer owner -> owner
      | Conversation _ -> failwith "expected developer owner"
    in
    let captured = Root_fixture.capture () in
    let handle =
      Run_store.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
      |> fst
    in
    let root_ref = ref None in
    let scope_ref = ref None in
    let derived_prepared = ref None in
    let expired_frame_rejected = ref None in
    let saved_attempts = ref 0 in
    let observed = ref [] in
    let run_provider scope context sw =
      let request =
        Inference.Request.create
          ~target:(Inference_runtime.Context.target context)
          ~history:[]
          ~tools:[]
          ~assets:[]
          ~limits:Document_schema.Limits.default
        |> ok
      in
      ignore
        (Inference_client.run
           context
           ~sw
           ~identity:(Root.Model_inference.identity scope)
           ~relation:Transcript.Scope.Root
           ~request
           ~before_dispatch:(Root.Model_inference.before_dispatch scope)
           ~on_attempt:(Root.Model_inference.on_attempt scope)
           ~on_completion:(Root.Model_inference.on_completion scope)
           ~on_event:ignore
           ~on_observation:(Root.Model_inference.on_observation scope)
           ~submission_guard:(Root.Model_inference.submission_guard scope)
         |> ok
         : Inference_runtime.Receipt.t);
      request
    in
    let installed =
      Native_fixture.installed_agent_fixture_with_contents
        ~deferred:true
        ~child_contents:
          {|<config model="synthetic" reasoning_effort="high"/>
<developer>Original child instructions</developer>|}
        ~env
        ~sw
        ~data_root:(Agent_store.Data_root.path (Session_store.data_root session))
        ~observed
        ~on_run:(fun () -> incr saved_attempts)
        ~with_deferred_context:(fun selected continue ->
          let root = Option.value_exn !root_ref in
          Root.Model_inference.with_claimed_agent root ~selected ~f:(fun scope ~sw ->
            scope_ref := Some (scope, sw);
            let original = Root.Model_inference.context scope in
            assert (phys_equal original (Port.Model_selection.context selected_model |> ok));
            ignore (run_provider scope original sw : Inference.Request.t);
            let result = continue () in
            assert (phys_equal (Root.Model_inference.context scope) original);
            Option.iter !derived_prepared ~f:(fun prepared ->
              assert (
                Exn.does_raise (fun () ->
                  Root.Model_inference.before_dispatch scope prepared)));
            result)
          |> ok)
        ~run_selected_target:
          (fun
            ~target
            ~startup:_
            ~compilation_limits:_
            ~programs:_
            ~ctx:_
            ~input:_
            ~observer:_
            ~source:_ ->
          let module Saved =
            Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target
          in
          let scope, sw = Option.value_exn !scope_ref in
          let original = Root.Model_inference.context scope in
          let config =
            Chat_response.Config.of_elements
              (Agent_session.Prompt_revision.elements (Saved.revision target))
          in
          let selected_target =
            Chat_response.Inference_config.apply_overrides
              (Inference_runtime.Context.target original)
              config
              ~limits:Transcript.Admission.default
            |> ok
          in
          assert (
            not
              (Inference.Request.Target.equal
                 selected_target
                 (Inference_runtime.Context.target original)));
          let selected_context =
            Inference_runtime.Context.derive_in_session original ~target:selected_target
            |> ok
          in
          let source_target =
            Agent_session.Captured_agent_runner.Selected_content_target.Saved target
          in
          assert (
            Result.is_error
              (Root.Model_inference.with_selected_child_target
                 (Option.value_exn !root_ref)
                 ~target:source_target
                 ~selected_context:original
                 ~f:(fun _ -> failwith "mismatched child Context was admitted")));
          let first_entered, first_entered_u = Eio.Promise.create () in
          let second_entered, second_entered_u = Eio.Promise.create () in
          let release_first, release_first_u = Eio.Promise.create () in
          let release_second, release_second_u = Eio.Promise.create () in
          let first_done, first_done_u = Eio.Promise.create () in
          let second_done, second_done_u = Eio.Promise.create () in
          Eio.Fiber.fork ~sw (fun () ->
            Root.Model_inference.with_selected_child_target
              (Option.value_exn !root_ref)
              ~target:source_target
              ~selected_context
              ~f:(fun first_context ->
                assert (phys_equal (Root.Model_inference.context scope) first_context);
                Eio.Promise.resolve first_entered_u first_context;
                Eio.Promise.await second_entered;
                assert (phys_equal (Root.Model_inference.context scope) first_context);
                Eio.Promise.await release_first;
                Ok ())
            |> ok;
            Eio.Promise.resolve first_done_u ());
          Eio.Fiber.fork ~sw (fun () ->
            let first_context = Eio.Promise.await first_entered in
            Root.Model_inference.with_selected_child_target
              (Option.value_exn !root_ref)
              ~target:source_target
              ~selected_context
              ~f:(fun second_context ->
                assert (not (phys_equal first_context second_context));
                assert (phys_equal (Root.Model_inference.context scope) second_context);
                Eio.Promise.resolve second_entered_u ();
                Eio.Promise.await first_done;
                assert (phys_equal (Root.Model_inference.context scope) second_context);
                Eio.Promise.await release_second;
                Ok ())
            |> ok;
            Eio.Promise.resolve second_done_u ());
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            Eio.Promise.await second_entered);
          assert (phys_equal (Root.Model_inference.context scope) original);
          Eio.Promise.resolve release_first_u ();
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            Eio.Promise.await first_done);
          assert (phys_equal (Root.Model_inference.context scope) original);
          Eio.Promise.resolve release_second_u ();
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            Eio.Promise.await second_done);
          assert (phys_equal (Root.Model_inference.context scope) original);
          let release, release_u = Eio.Promise.create () in
          let ready, ready_u = Eio.Promise.create () in
          let denied, denied_u = Eio.Promise.create () in
          let result =
            Root.Model_inference.with_selected_child_target
              (Option.value_exn !root_ref)
              ~target:source_target
              ~selected_context
              ~f:(fun root_context ->
                Eio.Fiber.fork ~sw (fun () ->
                  Eio.Promise.resolve
                    ready_u
                    (phys_equal (Root.Model_inference.context scope) root_context);
                  Eio.Promise.await release;
                  let rejected =
                    Exn.does_raise (fun () ->
                      ignore (Root.Model_inference.context scope : RT.Context.t))
                  in
                  expired_frame_rejected := Some rejected;
                  Eio.Promise.resolve denied_u rejected);
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                  assert (Eio.Promise.await ready));
                assert (phys_equal (Root.Model_inference.context scope) root_context);
                assert (
                  Inference.Request.Target.equal
                    (Inference_runtime.Context.target root_context)
                    selected_target);
                let request = run_provider scope root_context sw in
                derived_prepared
                := Some
                     (Inference_runtime.Context.prepare
                        root_context
                        ~preparation_id:"expired-selected-child"
                        request
                      |> ok);
                Ok "captured child result")
          in
          Eio.Promise.resolve release_u ();
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            assert (Eio.Promise.await denied));
          result)
        ()
    in
    let blobs =
      Agent_store.Blob_store.create
        ~env
        ~temporary_directory:
          (Agent_store.Data_root.temporary_blobs_path (Session_store.data_root session))
        ~durable_directory:
          (Agent_store.Data_root.durable_blobs_path (Session_store.data_root session))
        ~max_upload_bytes:65536L
      |> ok
    in
    Agent_store.Blob_store.bind_developer_runs blobs ~session_store:session store |> ok;
    let disclosure =
      Disclosure.create
        ~runs:store
        ~blobs
        ~policy:(Disclosure.Policy.create ~authorize:(fun _ _ -> true))
        ~limits:
          (Disclosure.Limits.create
             ~max_result_bytes:65536
             ~json:Document_schema.Limits.default
           |> ok)
      |> ok
    in
    let registry =
      Root.Registry.create
        ~sw
        ~store
        ~capacity:(Root_fixture.capacity ())
        ~now:Root_fixture.now
        ~limits:
          (Root.Limits.create
             ~max_active_runs:1
             ~max_staged_jobs:2
             ~max_native_occupancy:2
             ~max_active_workers:1
             ~max_quarantined_workers:1
             ~max_active_inference_attempts:2
             ~max_admitted_call_bindings:8
           |> Or_error.ok_exn)
    in
    let root =
      Root.Registry.admit
        registry
        ~actor
        ~capture:captured
        ~registry:installed.selected
        ~selected:installed.selected
        ~job_policy:Chat_response.One_off_request.default_policy
        ~selected_model:(Some selected_model)
        ~handle
        ~workspace_conflict_domain:"saved-agent-model-test"
      |> ok
    in
    root_ref := Some root;
    let jobs =
      Jobs.create
        ~root
        ~store
        ~disclosure
        ~env
        ~current_actor:(fun () -> actor)
        ~prepare_source:(fun ~parent ~binding ~request ~checkpoint ->
          let open Result.Let_syntax in
          let%bind launch = Jobs.Source_parent.launch parent in
          let%bind () = Jobs.Source_parent.check parent ~binding in
          Chat_response.Agent_runtime.prepare_agent_source
            installed.runtime
            ~binding
            ~launch
            ~request
            ~max_registrations:16
            ~checkpoint)
      |> ok
    in
    let job = Native_fixture.stage_registered_agent_tool jobs in
    Eio.Switch.run (fun runner_sw ->
      let runner =
        Native_fixture.background_runner_with_runtime
          ~agent_runtime:(Some installed.runtime)
          ~sw:runner_sw
          ~root
          ~store
          ~blobs
          ~env
          ~managed_definition:None
          ~jobs
          ~storage:Agent_store.Developer_result_publisher.Storage.Automatic
      in
      let candidate =
        Root.Job_candidate.read_current root ~checkpoint:ignore |> ok |> List.hd_exn
      in
      let completed =
        Agent_server.Developer_background_job_runner.run_candidate runner candidate
        |> ok
        |> Option.value_exn
      in
      assert (P.Id.Job.equal job (P.Developer_job.id completed));
      assert (Option.value_exn !expired_frame_rejected);
      assert (
        match (P.Developer_job.state completed).status with
        | Succeeded -> true
        | Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Failed _
        | Cancelled
        | Interrupted _ -> false));
    assert (Int.equal !saved_attempts 1);
    assert (Int.equal !model_requests 2);
    Root.close_and_wait root |> ok;
    Root.Registry.close_and_wait registry |> ok;
    Run_store.close_and_wait store |> ok;
    Session_store.close session |> ok;
    Runtime.close runtime)
;;

let%test_unit "claimed developer model runner publishes a real offline response" =
  let requests = ref 0 in
  let live = ref true in
  let on_submit = ref ignore in
  let model_actor = Actor.guarded ~principal ~is_current:(fun () -> !live) in
  with_fixture
    ~model_requests:requests
    ~on_model_submit:(fun () -> !on_submit ())
    (fun env sw anchor create _ ->
       let runtime = create () in
       ignore (setup runtime : P.Method_result.t);
       let reads = ref 0 in
       ignore (enroll runtime ~sw "one" "runner-key" reads : DTO.Configuration_result.t);
       let target = capture runtime in
       let selected_model =
         Port.select_model (Runtime.operator_port runtime) ~actor:model_actor ~target
         |> ok
       in
       let session =
         Session_store.create
           ~env
           ~sw
           ~root:(Eio.Path.native_exn Eio.Path.(anchor / "model-runs"))
           ~server_id
           ~process_start_identity:None
           ~lock_nonce:"model-runner-test"
         |> ok
       in
       let store =
         Run_store.open_or_create
           session
           ~sw
           ~limits:
             (Root_fixture.store_limits
                ~max_job_payload_bytes:4096
                ~max_jobs_per_run:5
                ~max_document_bytes:131072
                ~max_inference_bytes_per_run:65536
                ~max_work_steps:4096
                ~max_document_fields:8192
                ~max_document_nodes:16384
                ())
           ~checkpoint:ignore
         |> ok
       in
       let owner =
         match
           P.Execution_owner.developer
             ~host_id:server_id
             ~run_id:(P.Id.Developer_run.of_string "dvr_model_runner" |> ok)
             ~epoch:0
             ~principal_id:principal.id
           |> ok
         with
         | Developer owner -> owner
         | Conversation _ -> failwith "expected developer owner"
       in
       let captured = Root_fixture.capture () in
       let handle =
         Run_store.admit_epoch
           store
           ~owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
           ~previous:None
         |> ok
         |> fst
       in
       let selected =
         Capabilities.create
           ~owner:"model-runner-test"
           ~resource_fingerprint:Digestif.SHA256.(digest_string "model-runner" |> to_hex)
           []
         |> ok
       in
       let capacity =
         Agent_server.Job_capacity.create
           ~limits:
             { Agent_server.Config.Server.daemon_total = 4
             ; per_principal = 4
             ; per_prompt = 4
             ; per_workspace = 4
             ; per_session = 4
             ; per_kind = 4
             ; max_nested_depth = 2
             }
       in
       let registry =
         Root.Registry.create
           ~sw
           ~store
           ~capacity
           ~now:Root_fixture.now
           ~limits:
             (Root.Limits.create
                ~max_active_runs:1
                ~max_staged_jobs:1
                ~max_native_occupancy:1
                ~max_active_workers:2
                ~max_quarantined_workers:2
                ~max_active_inference_attempts:2
                ~max_admitted_call_bindings:8
              |> Or_error.ok_exn)
       in
       let root =
         Root.Registry.admit
           registry
           ~actor:model_actor
           ~capture:captured
           ~registry:selected
           ~selected
           ~selected_model:(Some selected_model)
           ~job_policy:Chat_response.One_off_request.default_policy
           ~handle
           ~workspace_conflict_domain:"model-runner-test"
         |> ok
       in
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(anchor / "local-model.md")
         "<developer>Reply briefly.</developer>";
       let request =
         P.Model_spawn_request.decode
           (`Object
               [ "prompt", `String "local-model.md"
               ; "input", `String "hello"
               ; "is_local", `True
               ])
         |> ok
       in
       let job_id =
         Root.with_root_action
           root
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              let job_id =
                Root.Action.admit_external_model action ~request ~checkpoint:ignore |> ok
              in
              let sealed = Root.Action.seal action ~selected:[] |> ok in
              Ok (job_id, sealed))
         |> ok
       in
       let data_root = Session_store.data_root session in
       let blobs =
         Agent_store.Blob_store.create
           ~env
           ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
           ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
           ~max_upload_bytes:65536L
         |> ok
       in
       Agent_store.Blob_store.bind_developer_runs blobs ~session_store:session store |> ok;
       let results =
         Root.Job_result.bind
           root
           ~blobs
           ~limits:
             (Agent_store.Developer_result_publisher.Limits.create
                ~inline_bytes:1024
                ~max_result_bytes:65536
                ~max_pending:1
              |> ok)
           ~storage:Automatic
         |> ok
       in
       assert (
         Result.is_error
           (Root.Job_result.bind
              root
              ~blobs
              ~limits:
                (Agent_store.Developer_result_publisher.Limits.create
                   ~inline_bytes:1024
                   ~max_result_bytes:65536
                   ~max_pending:1
                 |> ok)
              ~storage:Automatic));
       let runner =
         Agent_server.Developer_model_job_runner.create
           ~root
           ~results
           ~sources:
             (Agent_server.Developer_model_job_runner.Sources.create
                ~env
                ~prompt_dir:anchor
                ~tool_dir:anchor
                ~response_dir:anchor
                ~cache:(Chat_response.Cache.create ~max_size:1 ())
                ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
                ~approval_provider:Shell_runtime.Approval_broker.None_available)
         |> ok
       in
       let candidate =
         Root.Job_candidate.read_current root ~checkpoint:ignore
         |> ok
         |> List.find_exn ~f:(fun candidate ->
           P.Id.Job.equal (P.Developer_job.id (Root.Job_candidate.job candidate)) job_id)
       in
       assert (
         Agent_server.Developer_model_job_runner.Availability.equal
           (Agent_server.Developer_model_job_runner.availability runner candidate |> ok)
           Runnable);
       let scheduler =
         Agent_server.Job_scheduler.start
           ~sw
           ~clock:(Eio.Stdenv.clock env)
           ~registry:(Agent_server.Session_registry.create ())
           ~developer_registry:(Some registry)
           ~capacity
           ~model_job_inference:(fun _ _ ->
             Error (P.Error.invalid_request "unused conversation inference"))
       in
       let await_succeeded job_id =
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           let rec await () =
             let job =
               Run_store.find_job (Run_store.observe handle |> ok) job_id
               |> Option.value_exn
             in
             match (P.Developer_job.state job).status with
             | Succeeded -> job
             | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
               Eio.Fiber.yield ();
               await ()
             | Failed _ | Cancelled | Interrupted _ ->
               failwith "physical scheduler did not complete the developer job"
           in
           await ())
       in
       Exn.protect
         ~finally:(fun () ->
           Agent_server.Job_scheduler.close scheduler;
           Agent_server.Job_scheduler.await_developer_scans_idle scheduler |> ok)
         ~f:(fun () ->
           Agent_server.Job_scheduler.attach_developer_model scheduler ~root ~runner |> ok;
           Agent_server.Job_scheduler.wake_developer scheduler ~root;
           let finished = await_succeeded job_id in
           assert (P.Id.Job.equal (P.Developer_job.id finished) job_id);
           assert (Int.equal !requests 1);
           (match (P.Developer_job.state finished).result with
            | Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded _)) -> ()
            | None | Some _ -> failwith "claimed model did not publish Inline success");
           let prepared =
             Chat_response.One_off_script.prepare_in_domain
               ~env
               ~capabilities:selected
               ~tools:[]
               ~source:"let main input = Task.pure(input)"
               ()
             |> ok
           in
           let script_request =
             Chat_response.Background_request.script
               ~prepared
               ~input:(`String "script answer")
               ~policy:Chat_response.One_off_request.default_policy
             |> ok
           in
           let script_id =
             Root.with_root_action
               root
               ~phase:Run_store.Checkpoint.Active
               (fun ~action ~sw:_ ->
                  let id =
                    Root.Action.stage_background
                      action
                      ~request:script_request
                      ~checkpoint:ignore
                    |> ok
                  in
                  let sealed = Root.Action.seal action ~selected:[ id ] |> ok in
                  Ok (id, sealed))
             |> ok
           in
           let disclosure =
             Disclosure.create
               ~runs:store
               ~blobs
               ~policy:(Disclosure.Policy.create ~authorize:(fun _ _ -> true))
               ~limits:
                 (Disclosure.Limits.create
                    ~max_result_bytes:65536
                    ~json:Document_schema.Limits.default
                  |> ok)
             |> ok
           in
           let jobs =
             Jobs.create
               ~root
               ~store
               ~disclosure
               ~env
               ~current_actor:(fun () -> model_actor)
               ~prepare_source:(fun ~parent:_ ~binding:_ ~request:_ ~checkpoint ->
                 checkpoint ();
                 Ok None)
             |> ok
           in
           let tools =
             Agent_server.Developer_background_job_runner.create
               ~root
               ~results
               ~env
               ~managed_definition:None
               ~agent_runtime:None
               ~jobs
               ~authorize:(fun _ _ -> Ok ())
               ~prepare_output:(function
                 | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
                 | _ -> Error (P.Error.invalid_request "unexpected script output"))
             |> ok
           in
           Agent_server.Job_scheduler.attach_developer scheduler ~root ~runner:tools |> ok;
           Agent_server.Job_scheduler.wake_developer scheduler ~root;
           let script = await_succeeded script_id in
           match (P.Developer_job.state script).result with
           | Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded _)) -> ()
           | None | Some _ -> failwith "shared Tool/Model publisher did not settle script");
       let cap_script =
         Chat_response.One_off_script.prepare_in_domain
           ~env
           ~capabilities:selected
           ~tools:[]
           ~source:"let main input = Task.pure(input)"
           ()
         |> ok
         |> fun prepared ->
         Chat_response.Background_request.script
           ~prepared
           ~input:`Null
           ~policy:Chat_response.One_off_request.default_policy
         |> ok
       in
       let cap_model_id, cap_script_id =
         Root.with_root_action
           root
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              let model_id =
                Root.Action.admit_external_model action ~request ~checkpoint:ignore |> ok
              in
              let script_id =
                Root.Action.stage_background action ~request:cap_script ~checkpoint:ignore
                |> ok
              in
              let sealed = Root.Action.seal action ~selected:[ script_id ] |> ok in
              Ok ((model_id, script_id), sealed))
         |> ok
       in
       let proposed claim =
         let state = P.Developer_job.state (Run_store.Claim.job claim) in
         { state with
           status = P.Job.Succeeded
         ; next_run_at = None
         ; completed_at = Some (Root.native_now root)
         ; result =
             Some
               (P.Developer_job.Result_value.Inline
                  (P.Completion.Succeeded (`String (String.make 2048 'x'))))
         ; retry_failure = None
         }
       in
       let selected_first, selected_first_u = Eio.Promise.create () in
       let release_first, release_first_u = Eio.Promise.create () in
       let first_done, first_done_u = Eio.Promise.create () in
       let first_observation = Run_store.observe handle |> ok in
       Eio.Fiber.fork ~sw (fun () ->
         Eio.Promise.resolve
           first_done_u
           (Root.with_claimed_job
              root
              ~expected:first_observation
              ~lookup:(Run_store.find_job_exact first_observation cap_model_id)
              ~select:(Root.Job_result.select results)
              (fun ~active ~sw:_ ->
                 let claim = Agent_session.Developer_job_scope.claim active in
                 let after = proposed claim in
                 ignore (Root.Job_result.select results ~claim ~proposed:after |> ok);
                 Eio.Promise.resolve selected_first_u ();
                 Eio.Promise.await release_first;
                 Ok after)));
       Eio.Promise.await selected_first;
       let second_observation = Run_store.observe handle |> ok in
       let second =
         Root.with_claimed_job
           root
           ~expected:second_observation
           ~lookup:(Run_store.find_job_exact second_observation cap_script_id)
           (fun ~active ~sw:_ ->
              let claim = Agent_session.Developer_job_scope.claim active in
              let after = proposed claim in
              let rejected = Root.Job_result.select results ~claim ~proposed:after in
              assert (Result.is_error rejected);
              Error (P.Error.invalid_request "shared pending result cap reached"))
       in
       Eio.Promise.resolve release_first_u ();
       assert (Result.is_error second);
       assert (Result.is_ok (Eio.Promise.await first_done));
       let artifact_job =
         Run_store.find_job (Run_store.observe handle |> ok) cap_model_id
         |> Option.value_exn
       in
       (match (P.Developer_job.state artifact_job).result with
        | Some
            (P.Developer_job.Result_value.Artifact
               { outcome = P.Stored_completion.Succeeded; _ }) -> ()
        | None | Some _ -> failwith "shared Model result did not select Artifact");
       let after_effect_job =
         Root.with_root_action
           root
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              let id =
                Root.Action.admit_external_model action ~request ~checkpoint:ignore |> ok
              in
              let sealed = Root.Action.seal action ~selected:[] |> ok in
              Ok (id, sealed))
         |> ok
       in
       (on_submit := fun () -> if Int.equal !requests 2 then live := false);
       let after_effect_candidate =
         Root.Job_candidate.read_current root ~checkpoint:ignore
         |> ok
         |> List.find_exn ~f:(fun candidate ->
           P.Id.Job.equal
             (P.Developer_job.id (Root.Job_candidate.job candidate))
             after_effect_job)
       in
       ignore
         (Agent_server.Developer_model_job_runner.run_candidate
            runner
            after_effect_candidate
          : (P.Developer_job.t option, P.Error.t) result);
       let after_effect_saved =
         Run_store.find_job (Run_store.observe handle |> ok) after_effect_job
         |> Option.value_exn
       in
       (match (P.Developer_job.state after_effect_saved).result with
        | Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded _)) -> ()
        | None | Some _ -> failwith "completed provider effect lost its truthful result");
       assert (not !live);
       assert (Result.is_error (Root.Job_candidate.read_current root ~checkpoint:ignore));
       Root.close_and_wait root |> ok;
       Root.Registry.close_and_wait registry |> ok;
       Run_store.close_and_wait store |> ok;
       Session_store.close session |> ok;
       Runtime.close runtime)
;;

let%test_unit
    "physical model selection admits root/native calls and retains external effects"
  =
  with_fixture (fun env sw anchor create _ ->
    let runtime = create () in
    ignore (setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (enroll runtime ~sw "one" "root-model-key" reads : DTO.Configuration_result.t);
    let port = Runtime.operator_port runtime in
    let target = capture runtime in
    let selected_model = Port.select_model port ~actor ~target |> ok in
    let checkpoint_gate = ref ignore in
    let rename_armed = ref false in
    let sync_blocked = ref false in
    let parent_sync_blocked = ref false in
    let cancel_after_rename = ref (fun () -> ()) in
    let store_env =
      Store_fixture.fault_after_authority_rename_env
        ~after_rename:(fun () ->
          sync_blocked := true;
          !cancel_after_rename ())
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked
    in
    let session =
      Session_store.create
        ~env:store_env
        ~sw
        ~root:(Eio.Path.native_exn Eio.Path.(anchor / "developer-runs"))
        ~server_id
        ~process_start_identity:None
        ~lock_nonce:"model-root-admission"
      |> ok
    in
    let store =
      Run_store.open_or_create
        session
        ~sw
        ~limits:
          (Root_fixture.store_limits
             ~max_job_payload_bytes:4096
             ~max_jobs_per_run:8
             ~max_document_bytes:131072
             ~max_inference_bytes_per_run:65536
             ~max_work_steps:4096
             ~max_document_fields:8192
             ~max_document_nodes:16384
             ())
        ~checkpoint:(fun () -> !checkpoint_gate ())
      |> ok
    in
    let captured = Root_fixture.capture () in
    let owner run_name =
      match
        P.Execution_owner.developer
          ~host_id:server_id
          ~run_id:(P.Id.Developer_run.of_string run_name |> ok)
          ~epoch:0
          ~principal_id:principal.id
        |> ok
      with
      | Developer owner -> owner
      | Conversation _ -> failwith "expected developer owner"
    in
    let admit_epoch owner =
      Run_store.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
      |> fst
    in
    let run_owner = owner "dvr_selected_model" in
    let handle = admit_epoch run_owner in
    let tools =
      Chat_response.Tool_capability.create
        ~owner:"model-root-test"
        ~resource_fingerprint:Digestif.SHA256.(digest_string "model-root" |> to_hex)
        []
      |> ok
    in
    let limits =
      Root.Limits.create
        ~max_active_runs:1
        ~max_staged_jobs:1
        ~max_native_occupancy:1
        ~max_active_workers:1
        ~max_quarantined_workers:1
        ~max_active_inference_attempts:2
        ~max_admitted_call_bindings:32
      |> Or_error.ok_exn
    in
    let registry =
      Root.Registry.create
        ~sw
        ~store
        ~capacity:
          (Agent_server.Job_capacity.create
             ~limits:
               { Agent_server.Config.Server.daemon_total = 8
               ; per_principal = 8
               ; per_prompt = 8
               ; per_workspace = 8
               ; per_session = 8
               ; per_kind = 8
               ; max_nested_depth = 2
               })
        ~now:Root_fixture.now
        ~limits
    in
    let admit_on ?(selected = tools) handle actor selected_model =
      Root.Registry.admit
        registry
        ~actor
        ~capture:captured
        ~registry:selected
        ~selected
        ~selected_model:(Some selected_model)
        ~job_policy:Chat_response.One_off_request.default_policy
        ~handle
        ~workspace_conflict_domain:"model-root-test"
    in
    let admit actor selected_model = admit_on handle actor selected_model in
    let original = admit actor selected_model |> ok in
    assert (Option.value_exn (Root.selected_model original) |> phys_equal selected_model);
    assert (
      Inference.Request.Target.equal (Port.Model_selection.target selected_model) target);
    Root.close_and_wait original |> ok;
    let foreign_actor = Actor.trusted_local principal in
    assert (Result.is_error (admit foreign_actor selected_model));
    (* The failed foreign admission did not consume the physical run claim. *)
    let original_again = admit actor selected_model |> ok in
    Root.close_and_wait original_again |> ok;
    let interrupted actor selected_model revoke =
      let reached, reached_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      (checkpoint_gate
       := fun () ->
            checkpoint_gate := ignore;
            Eio.Promise.resolve reached_u ();
            Eio.Promise.await release);
      let done_, done_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve done_u (admit actor selected_model));
      Eio.Promise.await reached;
      revoke ();
      Eio.Promise.resolve release_u ();
      assert (Result.is_error (Eio.Promise.await done_))
    in
    let live = ref true in
    let expiring = Actor.guarded ~principal ~is_current:(fun () -> !live) in
    let expiring_model = Port.select_model port ~actor:expiring ~target |> ok in
    interrupted expiring expiring_model (fun () -> live := false);
    let after_actor_revoke = admit actor selected_model |> ok in
    let model_request =
      P.Model_spawn_request.decode
        (`Object [ "prompt", `String "synthetic"; "input", `String "hello" ])
      |> ok
    in
    let prepared =
      Chat_response.One_off_script.prepare_in_domain
        ~env
        ~capabilities:tools
        ~tools:[]
        ~source:"let main input = Task.pure(input)"
        ()
      |> ok
    in
    let script_request =
      Chat_response.Background_request.script
        ~prepared
        ~input:`Null
        ~policy:Chat_response.One_off_request.default_policy
      |> ok
    in
    let first_model, second_model, script =
      Root.with_root_action
        after_actor_revoke
        ~phase:Run_store.Checkpoint.Active
        (fun ~action ~sw:_ ->
           let first_model =
             Root.Action.admit_external_model
               action
               ~request:model_request
               ~checkpoint:ignore
             |> ok
           in
           let second_model =
             Root.Action.admit_external_model
               action
               ~request:model_request
               ~checkpoint:ignore
             |> ok
           in
           assert (not (P.Id.Job.equal first_model second_model));
           let script =
             Root.Action.stage_background
               action
               ~request:script_request
               ~checkpoint:ignore
             |> ok
           in
           let selected = Root.Action.seal action ~selected:[ script ] |> ok in
           Ok ((first_model, second_model, script), selected))
      |> ok
    in
    let observed = Run_store.observe handle |> ok in
    List.iter [ first_model; second_model; script ] ~f:(fun id ->
      assert (Option.is_some (Run_store.find_job observed id)));
    let child_model = ref None in
    let expected = Run_store.observe handle |> ok in
    ignore
      (Root.with_claimed_job
         after_actor_revoke
         ~expected
         ~lookup:(Run_store.find_job_exact expected script)
         (fun ~active ~sw:_ ->
            let parent = Root.job_parent after_actor_revoke active |> ok in
            let invocation =
              P.Host_invocation.create_developer
                { id = P.Id.Invocation.of_string "inv_model_child" |> ok
                ; owner = P.Execution_owner.Developer run_owner
                ; origin = P.Invocation.Script
                ; parent_invocation = None
                ; parent_job = Some script
                ; tool_name = "model.child"
                ; implementation_revision = "v1"
                ; capability_fingerprint = "model-test-capability"
                ; input = `Null
                ; created_at = Root_fixture.now ()
                ; deadline = None
                }
              |> ok
            in
            let dispatched = P.Host_invocation.dispatch invocation |> ok in
            let expected = Run_store.observe handle |> ok in
            let lookup =
              Run_store.find_invocation expected (P.Host_invocation.context invocation).id
            in
            let dispatch =
              Run_store.prepare_dispatch handle ~expected ~lookup ~dispatched |> ok
            in
            (match
               Run_store.commit_and_settle
                 handle
                 dispatch
                 (Run_store.Handoff.create ~settle:(fun _ -> ()))
                 ~admit:(fun () -> true)
               |> ok
             with
             | Run_store.Settlement.Installed _ -> ()
             | Absent | Prewrite_conflict | Diverged ->
               failwith "native dispatch not saved");
            Root.with_native_registration
              after_actor_revoke
              ~job_parent:parent
              ~invocation
              (fun ~scope ->
                 let selected =
                   Root.with_native_callback scope ~dispatched (fun ~sw:_ ->
                     child_model
                     := Some
                          (Root.Native_terminal.admit_external_model
                             scope
                             ~request:model_request
                             ~checkpoint:ignore
                             ~check_parent_now:(fun () -> Ok ())
                           |> ok);
                     Root.Native_terminal.seal
                       scope
                       ~outcome:(P.Invocation.Complete `Null)
                       ~selected:[])
                   |> ok
                 in
                 let terminal =
                   P.Host_invocation.resolve dispatched (P.Invocation.Complete `Null)
                   |> ok
                 in
                 ignore
                   (Root.Native_terminal.submit scope ~terminal ~selected:(Some selected)
                    |> ok
                    : int);
                 Ok ())
            |> ok;
            let state =
              P.Developer_job.state
                (Run_store.Claim.job (Agent_session.Developer_job_scope.claim active))
            in
            Ok
              { state with
                status = P.Job.Succeeded
              ; completed_at = Some (Root_fixture.now ())
              ; result =
                  Some
                    (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
              })
       |> ok
       : P.Developer_job.t option);
    let observed = Run_store.observe handle |> ok in
    assert (Option.is_some (Run_store.find_job observed (Option.value_exn !child_model)));
    let model_acknowledgements = ref 0 in
    let model_completions = ref 0 in
    let admitted_nested_calls = ref 0 in
    let nested_effects = ref 0 in
    let escaped_scope = ref None in
    let escaped_prepared = ref None in
    let claimed_children = ref [] in
    let expected = Run_store.observe handle |> ok in
    let failed_model_callback =
      try
        ignore
          (Root.with_claimed_job
             after_actor_revoke
             ~expected
             ~lookup:(Run_store.find_job_exact expected first_model)
             (fun ~active ~sw:_ ->
                Root.Model_inference.with_claimed
                  after_actor_revoke
                  ~active
                  ~f:(fun scope ~sw ->
                    escaped_scope := Some scope;
                    let context = Root.Model_inference.context scope in
                    assert (
                      phys_equal
                        context
                        (Port.Model_selection.context selected_model |> ok));
                    let document_limits = Run_store.document_limits handle |> ok in
                    let measured json =
                      Document_schema.Json.validate_and_measure
                        ~limits:document_limits
                        json
                      |> ok
                    in
                    let before_children = Run_store.observe handle |> ok in
                    let existing_payload_bytes =
                      List.fold
                        [ first_model
                        ; second_model
                        ; script
                        ; Option.value_exn !child_model
                        ]
                        ~init:0
                        ~f:(fun total id ->
                          let job =
                            Run_store.find_job before_children id |> Option.value_exn
                          in
                          total + measured (P.Developer_job.payload job))
                    in
                    let child_envelope =
                      P.Developer_model_call.create
                        ~request:(P.Model_spawn_request.original model_request)
                        ~target
                        ~limits:document_limits
                      |> ok
                      |> P.Developer_model_call.to_json
                    in
                    let aggregate_payload_bytes =
                      existing_payload_bytes + (2 * measured child_envelope)
                    in
                    assert (aggregate_payload_bytes <= 4096);
                    let first_child =
                      Root.Model_inference.admit_external_model
                        scope
                        ~request:model_request
                        ~checkpoint:ignore
                      |> Result.map_error ~f:(fun error ->
                        P.Error.code_to_string error.P.Error.code
                        ^ ": "
                        ^ error.P.Error.message)
                      |> Result.ok_or_failwith
                    in
                    let nested_child = ref None in
                    let physical_recipe
                      : Chat_response.Moderation.Capabilities.model_recipe
                      =
                      { call =
                          (fun ~payload:_ ->
                            failwith
                              "nested developer script made a synchronous model call")
                      ; spawn =
                          (fun ~payload ->
                            let open Result.Let_syntax in
                            let%bind request = P.Model_spawn_request.decode payload in
                            Root.Model_inference.admit_external_model
                              scope
                              ~request
                              ~checkpoint:ignore
                            |> Result.map_error ~f:(fun error -> error.P.Error.message)
                            |> Result.map ~f:(fun id ->
                              nested_child := Some id;
                              P.Id.Job.to_string id))
                      }
                    in
                    let dir = Eio.Stdenv.cwd env in
                    let nested_ctx =
                      Chat_response.Ctx.create
                        ~inference_context:context
                        ~inference_identity:(Root.Model_inference.identity scope)
                        ~on_inference_attempt:(Root.Model_inference.on_attempt scope)
                        ~on_inference_completion:
                          (Root.Model_inference.on_completion scope)
                        ~on_inference_observation:
                          (Root.Model_inference.on_observation scope)
                        ~on_admitted_call:(Root.Model_inference.on_admitted_call scope)
                        ~before_inference_dispatch:
                          (Root.Model_inference.before_dispatch scope)
                        ~agent_prompt_v1_recipe:physical_recipe
                        ~env
                        ~dir
                        ~tool_dir:dir
                        ~cache:(Chat_response.Cache.create ~max_size:1 ())
                        ()
                    in
                    let prompt =
                      Printf.sprintf
                        {|<developer>Admit one selected nested model job.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun child ->
        Task.bind(Runtime.end_session("done"), fun ignored -> Task.pure(st + 1)))
</script>|}
                        (Jsonaf.to_string (P.Model_spawn_request.original model_request))
                    in
                    Chat_response.Driver.run_agent ~ctx:nested_ctx prompt [] |> ignore;
                    let second_child = Option.value_exn !nested_child in
                    assert (not (P.Id.Job.equal first_child second_child));
                    claimed_children := [ first_child; second_child ];
                    let after_children = Run_store.observe handle |> ok in
                    List.iter !claimed_children ~f:(fun id ->
                      let child =
                        Run_store.find_job after_children id |> Option.value_exn
                      in
                      let launch = P.Developer_job.launch child in
                      assert (P.Job.equal_kind (P.Developer_job.kind child) Model_call);
                      assert (
                        match P.Developer_job.Launch.parent launch with
                        | Run_root -> true
                        | Invocation _ -> false);
                      assert (
                        match P.Developer_job.Launch.parent_job launch with
                        | Some (id, attempt) ->
                          P.Id.Job.equal id first_model && Int.equal attempt 1
                        | None -> false);
                      assert (Int.equal (P.Developer_job.Launch.nested_depth launch) 1));
                    let synthetic_context, synthetic_rounds =
                      synthetic_call_context (RT.Context.target context)
                    in
                    let parent_terminal = ref false in
                    let terminal_ready, release_terminal = Eio.Promise.create () in
                    let admitted_token = ref None in
                    let identity = Root.Model_inference.identity scope in
                    let table = String.Table.create () in
                    Hashtbl.set table ~key:"inspect" ~data:(fun ~invocation:_ _ ->
                      Eio.Promise.await terminal_ready;
                      assert !parent_terminal;
                      let token = Option.value_exn !admitted_token in
                      let parent =
                        Chat_response.In_memory_stream.Admitted_call.parent token
                      in
                      let nested_request =
                        Inference.Request.create
                          ~target:(RT.Context.target synthetic_context)
                          ~history:[]
                          ~tools:[]
                          ~assets:[]
                          ~limits:Document_schema.Limits.default
                        |> ok
                      in
                      let prepared =
                        RT.Context.prepare
                          synthetic_context
                          ~preparation_id:"delayed-nested"
                          nested_request
                        |> ok
                      in
                      identity.with_attempt
                        prepared
                        ~relation:(Transcript.Scope.Nested parent)
                        ~f:(fun ~scope:nested_scope ~accounting_id ->
                          let attempt =
                            RT.Prepared.start prepared ~scope:nested_scope ~accounting_id
                            |> ok
                          in
                          Root.Model_inference.on_attempt scope attempt;
                          incr admitted_nested_calls);
                      incr nested_effects;
                      Openai.Responses.Tool_output.Output.Text "done");
                    let allocator =
                      History_entry.Allocator.create
                        ~namespace:"developer-model"
                        ~next_sequence:0
                      |> ok
                    in
                    ignore
                      (Chat_response.In_memory_stream
                       .run_completion_stream_in_memory_entries
                         ~env
                         ~inference_context:synthetic_context
                         ~inference_identity:identity
                         ~on_inference_attempt:(Root.Model_inference.on_attempt scope)
                         ~on_inference_completion:(fun completion ->
                           Root.Model_inference.on_completion scope completion;
                           if not !parent_terminal
                           then (
                             parent_terminal := true;
                             Eio.Promise.resolve release_terminal ()))
                         ~on_inference_observation:
                           (Root.Model_inference.on_observation scope)
                         ~on_admitted_call:(fun token ->
                           assert (not !parent_terminal);
                           Root.Model_inference.on_admitted_call scope token;
                           admitted_token := Some token)
                         ~before_inference_dispatch:
                           (Root.Model_inference.before_dispatch scope)
                         ~allocator
                         ~history:[]
                         ~tools:(Some [])
                         ~tool_tbl:table
                         ~parallel_tool_calls:true
                         ~sw
                         ()
                       : History_entry.t list);
                    assert (!synthetic_rounds = 2);
                    assert (!admitted_nested_calls = 1 && !nested_effects = 1);
                    let request =
                      Inference.Request.create
                        ~target:(RT.Context.target context)
                        ~history:[]
                        ~tools:[]
                        ~assets:[]
                        ~limits:Document_schema.Limits.default
                      |> ok
                    in
                    escaped_prepared
                    := Some
                         (RT.Context.prepare context ~preparation_id:"escaped" request
                          |> ok);
                    ignore
                      (Inference_client.run
                         context
                         ~sw
                         ~identity:(Root.Model_inference.identity scope)
                         ~relation:Transcript.Scope.Root
                         ~request
                         ~before_dispatch:(Root.Model_inference.before_dispatch scope)
                         ~on_attempt:(fun attempt ->
                           let wrong_prepared =
                             RT.Context.prepare
                               context
                               ~preparation_id:"wrong-accounting-attempt"
                               request
                             |> ok
                           in
                           let wrong_accounting =
                             Inference.Observation.Observation_id.of_string "wrong-usage"
                             |> ok
                           in
                           let wrong_attempt =
                             RT.Prepared.start
                               wrong_prepared
                               ~scope:(RT.Attempt.scope attempt)
                               ~accounting_id:wrong_accounting
                             |> ok
                           in
                           let rejected =
                             try
                               Root.Model_inference.on_attempt scope wrong_attempt;
                               false
                             with
                             | _ -> true
                           in
                           assert rejected;
                           let wrong_configuration =
                             RT.Context.prepare
                               context
                               ~preparation_id:"wrong-configuration-attempt"
                               request
                             |> ok
                           in
                           let wrong_configuration_attempt =
                             RT.Prepared.start
                               wrong_configuration
                               ~scope:(RT.Attempt.scope attempt)
                               ~accounting_id:(RT.Attempt.accounting_id attempt)
                             |> ok
                           in
                           assert (
                             not
                               (Inference.Observation.Configuration.equal
                                  (RT.Attempt.configuration wrong_configuration_attempt)
                                  (RT.Attempt.configuration attempt)));
                           assert (
                             try
                               Root.Model_inference.on_attempt
                                 scope
                                 wrong_configuration_attempt;
                               false
                             with
                             | _ -> true);
                           Root.Model_inference.on_attempt scope attempt;
                           incr model_acknowledgements;
                           raise Model_ack_failed)
                         ~on_completion:(fun completion ->
                           Root.Model_inference.on_completion scope completion;
                           incr model_completions)
                         ~on_event:ignore
                         ~on_observation:(Root.Model_inference.on_observation scope)
                       : (RT.Receipt.t, Inference_client.Error.t) result);
                    failwith "strict inference acknowledgement did not fail"))
           |> function
           | Ok value -> value
           | Error error ->
             failwith
               ("model fixture: "
                ^ P.Error.code_to_string error.P.Error.code
                ^ ": "
                ^ error.P.Error.message));
        false
      with
      | Model_ack_failed -> true
    in
    assert failed_model_callback;
    let after_parent_failure = Run_store.observe handle |> ok in
    List.iter !claimed_children ~f:(fun id ->
      assert (Option.is_some (Run_store.find_job after_parent_failure id)));
    assert (!model_acknowledgements = 1 && !model_completions = 1);
    assert (
      try
        Root.Model_inference.before_dispatch
          (Option.value_exn !escaped_scope)
          (Option.value_exn !escaped_prepared);
        false
      with
      | _ -> true);
    let sibling_callbacks = ref 0 in
    let sibling_deferred = ref false in
    let expected = Run_store.observe handle |> ok in
    ignore
      (Root.with_claimed_job
         after_actor_revoke
         ~expected
         ~lookup:(Run_store.find_job_exact expected second_model)
         (fun ~active ~sw:_ ->
            (Root.Model_inference.with_claimed
               after_actor_revoke
               ~active
               ~f:(fun scope ~sw:_ ->
                 let context = Root.Model_inference.context scope in
                 let request =
                   Inference.Request.create
                     ~target:(RT.Context.target context)
                     ~history:[]
                     ~tools:[]
                     ~assets:[]
                     ~limits:Document_schema.Limits.default
                   |> ok
                 in
                 let prepared label =
                   RT.Context.prepare context ~preparation_id:label request |> ok
                 in
                 let identity = Root.Model_inference.identity scope in
                 let older_prepared = prepared "older-sibling" in
                 identity.with_attempt
                   older_prepared
                   ~relation:Transcript.Scope.Root
                   ~f:(fun ~scope:older_scope ~accounting_id:older_accounting ->
                     let older =
                       RT.Prepared.start
                         older_prepared
                         ~scope:older_scope
                         ~accounting_id:older_accounting
                       |> ok
                     in
                     Root.Model_inference.on_attempt scope older;
                     incr sibling_callbacks;
                     let newer_prepared = prepared "newer-sibling" in
                     let uncertain_newer =
                       try
                         identity.with_attempt
                           newer_prepared
                           ~relation:Transcript.Scope.Root
                           ~f:(fun ~scope:newer_scope ~accounting_id:newer_accounting ->
                             let newer =
                               RT.Prepared.start
                                 newer_prepared
                                 ~scope:newer_scope
                                 ~accounting_id:newer_accounting
                               |> ok
                             in
                             let overflow_entered = ref false in
                             let overflow_rejected =
                               try
                                 identity.with_attempt
                                   (prepared "overflow-sibling")
                                   ~relation:Transcript.Scope.Root
                                   ~f:(fun ~scope:_ ~accounting_id:_ ->
                                     overflow_entered := true);
                                 false
                               with
                               | _ -> true
                             in
                             assert (overflow_rejected && not !overflow_entered);
                             rename_armed := true;
                             Root.Model_inference.on_attempt scope newer;
                             incr sibling_callbacks);
                         false
                       with
                       | _ -> true
                     in
                     assert uncertain_newer;
                     assert !sync_blocked;
                     sync_blocked := false;
                     let older_observation =
                       inference_usage ~scope:older_scope ~accounting_id:older_accounting
                     in
                     (sibling_deferred
                      := try
                           Root.Model_inference.on_observation scope older_observation;
                           false
                         with
                         | _ -> true);
                     assert !sibling_deferred))
             |> function
             | Ok value -> value
             | Error error ->
               failwith
                 ("sibling model fixture: "
                  ^ P.Error.code_to_string error.P.Error.code
                  ^ ": "
                  ^ error.P.Error.message));
            let state =
              P.Developer_job.state
                (Run_store.Claim.job (Agent_session.Developer_job_scope.claim active))
            in
            Ok
              { state with
                status = P.Job.Succeeded
              ; completed_at = Some (Root_fixture.now ())
              ; result =
                  Some
                    (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
              })
       |> ok
       : P.Developer_job.t option);
    assert (!sibling_callbacks = 1 && !sibling_deferred);
    let child_model = Option.value_exn !child_model in
    let close_attempts = ref 0 in
    let expected = Run_store.observe handle |> ok in
    let retained_unknown =
      try
        ignore
          (Root.with_claimed_job
             after_actor_revoke
             ~expected
             ~lookup:(Run_store.find_job_exact expected child_model)
             (fun ~active ~sw:_ ->
                Root.Model_inference.with_claimed
                  after_actor_revoke
                  ~active
                  ~f:(fun scope ~sw:_ ->
                    let context = Root.Model_inference.context scope in
                    let request =
                      Inference.Request.create
                        ~target:(RT.Context.target context)
                        ~history:[]
                        ~tools:[]
                        ~assets:[]
                        ~limits:Document_schema.Limits.default
                      |> ok
                    in
                    let prepared =
                      RT.Context.prepare
                        context
                        ~preparation_id:"close-uncertain-model"
                        request
                      |> ok
                    in
                    (Root.Model_inference.identity scope).with_attempt
                      prepared
                      ~relation:Transcript.Scope.Root
                      ~f:(fun ~scope:attempt_scope ~accounting_id ->
                        incr close_attempts;
                        let attempt =
                          RT.Prepared.start prepared ~scope:attempt_scope ~accounting_id
                          |> ok
                        in
                        rename_armed := true;
                        Root.Model_inference.on_attempt scope attempt))
                |> ok;
                let state =
                  P.Developer_job.state
                    (Run_store.Claim.job (Agent_session.Developer_job_scope.claim active))
                in
                Ok
                  { state with
                    status = P.Job.Succeeded
                  ; completed_at = Some (Root_fixture.now ())
                  ; result =
                      Some
                        (P.Developer_job.Result_value.Inline
                           (P.Completion.Succeeded `Null))
                  })
           |> ok);
        false
      with
      | _ -> true
    in
    assert (retained_unknown && !sync_blocked);
    assert (Result.is_error (Root.close_and_wait after_actor_revoke));
    assert (Result.is_error (admit actor selected_model));
    sync_blocked := false;
    Root.close_and_wait after_actor_revoke |> ok;
    assert (!close_attempts = 1);
    let failure_handle = admit_epoch (owner "dvr_model_failed_parent") in
    let managed, selected =
      Native_fixture.managed_job_registry
        ~tool_contract:Chatml_compilation.Tool_contract.Developer_model_spawn
        ~source:
          {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
  Task.bind(Job.get(id), fun status -> Task.pure(`Complete(input))))|}
        env
        (ref 0)
    in
    let failed_run = admit_on ~selected failure_handle actor selected_model |> ok in
    let data_root = Session_store.data_root session in
    let blobs =
      Agent_store.Blob_store.create
        ~env:store_env
        ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
        ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
        ~max_upload_bytes:65536L
      |> ok
    in
    Agent_store.Blob_store.bind_developer_runs blobs ~session_store:session store |> ok;
    let disclosure =
      Disclosure.create
        ~runs:store
        ~blobs
        ~policy:(Disclosure.Policy.create ~authorize:(fun _ _ -> true))
        ~limits:
          (Disclosure.Limits.create
             ~max_result_bytes:65536
             ~json:Document_schema.Limits.default
           |> ok)
      |> ok
    in
    let jobs =
      Jobs.create
        ~root:failed_run
        ~store
        ~disclosure
        ~env
        ~current_actor:(fun () -> actor)
        ~prepare_source:(fun ~parent:_ ~binding:_ ~request:_ ~checkpoint ->
          checkpoint ();
          Ok None)
      |> ok
    in
    let model_payload = P.Model_spawn_request.original model_request in
    let inventory_jobs handle =
      let reader =
        Agent_store.Retention_reader.create
          ~env:store_env
          ~root:(Agent_store.Data_root.path data_root)
          ~max_entries:64
          ~max_bytes:65536
        |> ok
      in
      Run_store.Result_root.rows_for_inventory
        (Run_store.Result_root.of_handle handle |> ok)
        ~reader
        ~checkpoint:ignore
      |> ok
      |> Run_store.Result_root.Inventory_rows.jobs
    in
    let root_compiled =
      Chatml_compilation.compile
        ~env
        ~target:Chatml_compilation.Developer_tool_v1
        ~source:
          {|let run ctx input = Task.bind(
  Task.catch(
    Task.bind(Model.spawn("agent_prompt_v1", input), fun discarded -> Task.fail("discard")),
    fun ignored -> Task.pure("caught")),
  fun ignored -> Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
    Task.bind(Job.get(id), fun status ->
      Task.bind(Job.read_result(id), fun result ->
        Task.bind(Job.cancel(id), fun ignored -> Task.pure(`Complete(input)))))))|}
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let root_returned = ref None in
    let root_script_failed =
      Jobs.with_root_action jobs ~phase:Run_store.Checkpoint.Active (fun ~scope ~sw:_ ->
        let config : Chatml_host_runtime.runtime_config =
          { surface = Chatml.Chatml_extension_surface.developer_tool_v1
          ; operations = Chatml_host_runtime.default_operations ()
          }
        in
        let config = Jobs.Scope.install scope config in
        let value =
          Chatml_host_runtime.run_entrypoint
            ~scoped_async_operation:Chatml_host_runtime.Model_spawn
            config
            root_compiled
            ~entrypoint:"run"
            ~arguments:
              [ Chatml.Chatml_value_codec.import_json `Null
              ; Chatml.Chatml_value_codec.import_json model_payload
              ]
            ()
          |> Result.map_error ~f:P.Error.invalid_request
        in
        match value with
        | Error error -> Error error
        | Ok value ->
          root_returned := Some value;
          Error (P.Error.invalid_request "root failed after compiled Model.spawn"))
    in
    assert (Result.is_error root_script_failed);
    assert (Option.is_some !root_returned);
    let root_models = inventory_jobs failure_handle in
    assert (Int.equal (List.length root_models) 2);
    assert (
      List.for_all root_models ~f:(fun job ->
        P.Job.equal_kind (P.Developer_job.kind job) Model_call));
    assert (
      Int.equal
        (List.count root_models ~f:(fun job ->
           String.equal (P.Job.status_name (P.Developer_job.state job).status) "cancelled"))
        1);
    let queued_model =
      List.find_exn root_models ~f:(fun job ->
        match (P.Developer_job.state job).status with
        | Queued -> true
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ -> false)
    in
    let cancelled_scope = Failure "cancel live model inference scope" in
    let guarded_effects = ref 0 in
    let expected = Run_store.observe failure_handle |> ok in
    (match
       Root.with_claimed_job
         failed_run
         ~expected
         ~lookup:(Run_store.find_job_exact expected (P.Developer_job.id queued_model))
         (fun ~active ~sw:_ ->
            Root.Model_inference.with_claimed failed_run ~active ~f:(fun scope ~sw ->
              let context = Root.Model_inference.context scope in
              let request =
                Inference.Request.create
                  ~target:(RT.Context.target context)
                  ~history:[]
                  ~tools:[]
                  ~assets:[]
                  ~limits:Document_schema.Limits.default
                |> ok
              in
              let prepared =
                RT.Context.prepare context ~preparation_id:"cancelled-scope" request |> ok
              in
              Eio.Switch.fail sw cancelled_scope;
              Eio.Cancel.protect (fun () ->
                assert (
                  try
                    Root.Model_inference.before_dispatch scope prepared;
                    incr guarded_effects;
                    false
                  with
                  | _ -> true)))
            |> ok;
            failwith "cancelled inference scope continued")
     with
     | exception exn when phys_equal exn cancelled_scope -> ()
     | Ok _ | Error _ | (exception _) -> failwith "inference cancellation was replaced");
    assert (!guarded_effects = 0);
    let module Managed = Chat_response.Managed_tool_registry in
    let module H = P.Host_invocation in
    let binding = Capabilities.find selected ~name:"summary" |> ok in
    let reference = Capabilities.reference binding in
    let template =
      Native_fixture.invocation
        (owner "dvr_model_failed_parent")
        selected
        reference
        ~custom:false
    in
    let invocation =
      H.create_developer
        { (H.context template) with
          implementation_revision = Managed.execution_revision managed binding |> ok
        ; input = model_payload
        }
      |> ok
    in
    let executor = Native.create ~root:failed_run |> ok in
    let executor = Native.with_job_service executor jobs |> ok in
    let executor =
      Native.with_managed_definition executor ~definition:managed ~env |> ok
    in
    let terminal =
      Native.run_root
        executor
        ~reference
        ~invocation
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(function
          | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
          | _ -> Error (P.Error.invalid_request "unexpected output"))
      |> ok
    in
    assert (
      P.Invocation.equal_status
        (H.status terminal)
        (P.Invocation.Resolved (P.Invocation.Complete model_payload)));
    let model_rows = inventory_jobs failure_handle in
    assert (Int.equal (List.length model_rows) 3);
    assert (
      List.exists model_rows ~f:(fun job ->
        match P.Developer_job.Launch.parent (P.Developer_job.launch job) with
        | P.Developer_job.Launch.Invocation id ->
          P.Id.Invocation.equal id (H.context invocation).id
        | P.Developer_job.Launch.Run_root -> false));
    let contract =
      Chatml.Chatml_compiler_contract.for_target
        Chatml.Chatml_compiler_contract.Target.Developer_tool_v1
        ~checkpoint:ignore
    in
    let linked =
      Support.input
        contract
        [ ( "entry.chatml"
          , {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id ->
  Task.pure(`Complete(input)))|}
          , [] )
        ]
      |> Support.compiled contract
    in
    let late_failure = Failure "parent failed after external model admission" in
    (match
       Jobs.with_root_action jobs ~phase:Run_store.Checkpoint.Active (fun ~scope ~sw:_ ->
         let config : Chatml_host_runtime.runtime_config =
           { surface = Chatml.Chatml_extension_surface.developer_tool_v1
           ; operations = Chatml_host_runtime.default_operations ()
           }
         in
         let config = Jobs.Scope.install scope config in
         ignore
           (Chat_response.Script_program.run_in_scope
              (Chat_response.Script_program.Prepared linked)
              ~scoped_async_operation:Chatml_host_runtime.Model_spawn
              ~control:None
              ~config
              ~entrypoint:"run"
              ~arguments:
                [ Chatml.Chatml_value_codec.import_json `Null
                ; Chatml.Chatml_value_codec.import_json model_payload
                ]
              ()
            |> Result.map_error ~f:(fun error -> error.Chatml_execution.message)
            |> Result.ok_or_failwith
            : Chatml.Chatml_lang.value);
         raise late_failure)
     with
     | exception exn when phys_equal exn late_failure -> ()
     | Ok _ | Error _ | (exception _) -> failwith "late root failure was replaced");
    let retained = inventory_jobs failure_handle in
    assert (Int.equal (List.length retained) 4);
    assert (
      List.for_all retained ~f:(fun job ->
        P.Job.equal_kind (P.Developer_job.kind job) Model_call));
    Root.close_and_wait failed_run |> ok;
    let call_live = ref true in
    let call_actor = Actor.guarded ~principal ~is_current:(fun () -> !call_live) in
    let call_selection = Port.select_model port ~actor:call_actor ~target |> ok in
    let revoked_run = admit_on failure_handle call_actor call_selection |> ok in
    let before =
      Run_store.Checkpoint.revision
        (Run_store.Observation.checkpoint (Run_store.observe failure_handle |> ok))
    in
    assert (
      Result.is_error
        (Root.with_root_action
           revoked_run
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              Root.Action.admit_external_model
                action
                ~request:model_request
                ~checkpoint:(fun () -> call_live := false)
              |> Result.map ~f:(fun _ -> failwith "revoked call admitted a model"))));
    let after =
      Run_store.Checkpoint.revision
        (Run_store.Observation.checkpoint (Run_store.observe failure_handle |> ok))
    in
    assert (Int64.equal before after);
    Root.close_and_wait revoked_run |> ok;
    let cancelled_run = admit_on failure_handle actor selected_model |> ok in
    let before =
      Run_store.Checkpoint.revision
        (Run_store.Observation.checkpoint (Run_store.observe failure_handle |> ok))
    in
    let cancelled_reason = Failure "cancel root before external model admission" in
    (match
       Eio.Cancel.sub (fun cancel ->
         Root.with_root_action
           cancelled_run
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              Eio.Cancel.cancel cancel cancelled_reason;
              Eio.Cancel.protect (fun () ->
                assert (
                  Result.is_error
                    (Root.Action.admit_external_model
                       action
                       ~request:model_request
                       ~checkpoint:ignore)));
              Eio.Fiber.check ();
              failwith "cancelled root callback continued"))
     with
     | exception Eio.Cancel.Cancelled actual ->
       assert (phys_equal actual cancelled_reason)
     | Ok _ | Error _ | (exception _) -> failwith "root cancellation was replaced");
    let after =
      Run_store.Checkpoint.revision
        (Run_store.Observation.checkpoint (Run_store.observe failure_handle |> ok))
    in
    assert (Int64.equal before after);
    Root.close_and_wait cancelled_run |> ok;
    let uncertain_handle = admit_epoch (owner "dvr_model_uncertain") in
    let uncertain_run = admit_on uncertain_handle actor selected_model |> ok in
    let calls = ref 0 in
    let reason = Failure "cancel after model document rename" in
    (match
       Eio.Cancel.sub (fun cancel ->
         (cancel_after_rename
          := fun () ->
               Eio.Cancel.cancel cancel reason;
               Eio.Fiber.check ());
         Root.with_root_action
           uncertain_run
           ~phase:Run_store.Checkpoint.Active
           (fun ~action ~sw:_ ->
              incr calls;
              rename_armed := true;
              let id =
                Root.Action.admit_external_model
                  action
                  ~request:model_request
                  ~checkpoint:ignore
                |> ok
              in
              failwith ("uncertain model call was acknowledged: " ^ P.Id.Job.to_string id)))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
     | Ok _ | Error _ | (exception _) -> failwith "model cancellation identity was lost");
    assert (Int.equal !calls 1 && !sync_blocked && not !rename_armed);
    assert (Result.is_error (Root.current uncertain_run));
    assert (Result.is_error (Root.close_and_wait uncertain_run));
    sync_blocked := false;
    Root.reconcile_pending uncertain_run |> ok;
    let reader =
      Agent_store.Retention_reader.create
        ~env:store_env
        ~root:(Agent_store.Data_root.path (Session_store.data_root session))
        ~max_entries:64
        ~max_bytes:65536
      |> ok
    in
    let root = Run_store.Result_root.of_handle uncertain_handle |> ok in
    let rows =
      Run_store.Result_root.rows_for_inventory root ~reader ~checkpoint:ignore |> ok
    in
    let jobs = Run_store.Result_root.Inventory_rows.jobs rows in
    assert (List.length jobs = 1);
    assert (P.Job.equal_kind (P.Developer_job.kind (List.hd_exn jobs)) Model_call);
    assert (Int.equal !calls 1);
    Root.close_and_wait uncertain_run |> ok;
    interrupted actor selected_model (fun () -> Port.close port);
    let unowned = Run_store.Run_claim.acquire ~store ~handle |> ok in
    Run_store.Run_claim.release unowned |> ok;
    Root.Registry.close_and_wait registry |> ok;
    Run_store.close_and_wait store |> ok;
    Session_store.close session |> ok;
    Runtime.close runtime)
;;

let%expect_test
    "actual host setup is authorized, explicit, shared and survives lost reply/restart"
  =
  with_fixture (fun _ sw anchor create login_calls ->
    let runtime = create () in
    assert (status runtime).setup_required;
    (match
       Runtime.dispatch
         runtime
         ~actor:denied_actor
         (P.Command.Provider_setup setup_request)
     with
     | Error Denied -> ()
     | _ -> failwith "denied setup created authority");
    let reads = ref 0 in
    assert (
      not (Eio.Path.is_file Eio.Path.(anchor / "authority" / "provider-registry.json")));
    (* Drop the setup response, then reopen the same actual factory. *)
    ignore (setup runtime : P.Method_result.t);
    Runtime.close runtime;
    let reopened = create () in
    assert (not (status reopened).setup_required);
    ignore (setup reopened : P.Method_result.t);
    (match
       Runtime.enroll_private_key
         reopened
         ~actor:denied_actor
         ~profile:(profile "one")
         ~key:(key "denied-input")
         ~source_reference:"synthetic"
         ~sw
         ~read:(fun ~sw:_ ->
           incr reads;
           Error B.Error.Invalid_credential)
     with
     | Error Denied -> ()
     | _ -> failwith "denied input not rejected");
    assert (!reads = 0);
    ignore (enroll reopened ~sw "one" "key-one" reads : DTO.Configuration_result.t);
    assert (!reads = 1);
    assert (String.equal (Inference.Request.Target.profile (capture reopened)) "one");
    Runtime.close reopened;
    let final = create () in
    assert (not (status final).setup_required);
    assert (String.equal (Inference.Request.Target.profile (capture final)) "one");
    assert (!reads = 1 && !login_calls = 0));
  print_endline
    "denied setup/input had no effects; exact setup recovered; protected enrollment \
     survives reopen";
  [%expect
    {| denied setup/input had no effects; exact setup recovered; protected enrollment survives reopen |}]
;;

let%expect_test
    "selection changes fresh capture while held plans fail after credential replacement"
  =
  with_fixture (fun _ sw _ create login_calls ->
    let runtime = create () in
    ignore (setup runtime : P.Method_result.t);
    let reads = ref 0 in
    ignore (enroll runtime ~sw "one" "one-first" reads : DTO.Configuration_result.t);
    ignore (enroll runtime ~sw "two" "two-first" reads : DTO.Configuration_result.t);
    let target = capture runtime in
    let context = Backend.resolve (Runtime.backend runtime) target |> ok in
    let request =
      Inference.Request.create
        ~target
        ~history:[]
        ~tools:[]
        ~assets:[]
        ~limits:Document_schema.Limits.default
      |> ok
    in
    let held = RT.Context.prepare context ~preparation_id:"held" request |> ok in
    let selection = (status runtime).selection |> Option.value_exn in
    ignore
      (Runtime.dispatch
         runtime
         ~actor
         (P.Command.Provider_select
            { profile = profile "two"
            ; expected_revision = selection.revision
            ; idempotency_key = key "select-two"
            })
       |> ok
       : P.Method_result.t);
    assert (String.equal (Inference.Request.Target.profile (capture runtime)) "two");
    let preserved =
      Backend.capture
        (Runtime.backend runtime)
        ~current:(Some target)
        ~model:"synthetic"
        ~settings:[]
      |> ok
    in
    assert (String.equal (Inference.Request.Target.profile preserved) "one");
    ignore (enroll runtime ~sw "one" "one-replacement" reads : DTO.Configuration_result.t);
    let scope =
      Transcript.Scope.create
        ~source:(Transcript.Source_id.of_string "host-test" |> ok)
        ~attempt:(Transcript.Attempt_id.of_string "held" |> ok)
        ~relation:Root
      |> ok
    in
    let attempt =
      RT.Prepared.start
        held
        ~scope
        ~accounting_id:(Inference.Observation.Observation_id.of_string "held" |> ok)
      |> ok
    in
    let terminal =
      RT.Attempt.run attempt ~sw ~on_event:ignore ~on_observation:ignore
      |> ok
      |> RT.Receipt.terminal
    in
    assert (
      Inference.Event.Terminal.equal_delivery
        (Inference.Event.Terminal.delivery terminal)
        Definitely_not_submitted);
    (match Inference.Event.Terminal.outcome terminal with
     | Failed (Authentication Denied) -> ()
     | _ -> failwith "old held plan dispatched");
    assert (!login_calls = 0));
  print_endline
    "selection affects fresh capture; recapture preserves identity; replacement denies \
     old plan before dispatch";
  [%expect
    {| selection affects fresh capture; recapture preserves identity; replacement denies old plan before dispatch |}]
;;

let%expect_test
    "partial original setup publishes remaining profile metadata without replacing \
     registry"
  =
  with_fixture (fun env sw anchor create login_calls ->
    let directory = S.Directory.open_or_create ~sw ~anchor ~components |> ok in
    let intents =
      Provider_operator.Command_intents.create
        directory
        ~host:(id "host")
        ~maximum_records:1024
      |> ok
    in
    let command = P.Command.Provider_setup setup_request in
    let original = id "partial-original-incarnation" in
    (match
       Provider_operator.Command_intents.begin_
         intents
         ~principal:principal.id
         ~key:setup_request.idempotency_key
         ~method_name:(P.Command.method_name command)
         ~params:(P.Command.params command)
         ~operation:original
       |> ok
     with
     | Fresh _ -> ()
     | Existing _ -> failwith "unexpected preexisting setup");
    let secrets = Secret.open_private_files ~sw ~directory ~namespace |> ok in
    let registry =
      C.initialize_new
        ~metadata_admission:C.Metadata_admission.nonblocking
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation:(fun () -> id "unused")
        ~directory
        ~secrets
        ~environment:None
        ~host:(id "host")
        ~incarnation:original
      |> ok
    in
    (* Exact durable prefix of factory setup: intent + registry committed,
       profile metadata absent. This simulates restart after that prefix, not
       an injected filesystem/cancellation failure. *)
    C.close registry;
    Secret.close secrets;
    let runtime = create () in
    assert (status runtime).setup_required;
    (match
       Runtime.dispatch
         runtime
         ~actor
         (P.Command.Provider_setup { idempotency_key = key "different-setup" })
     with
     | Error Submission_uncertain -> ()
     | _ -> failwith "different setup adopted partial authority");
    let result = setup runtime in
    (match result with
     | P.Method_result.Provider_setup result ->
       assert (
         String.equal (DTO.Revision.to_string result.revision) (M.Id.to_string original))
     | _ -> failwith "wrong setup result");
    assert (not (status runtime).setup_required);
    Runtime.close runtime;
    let reopened = create () in
    assert (not (status reopened).setup_required);
    (match Runtime.receipt reopened ~actor command |> ok with
     | Committed (Provider_setup result) ->
       assert (
         String.equal (DTO.Revision.to_string result.revision) (M.Id.to_string original))
     | _ -> failwith "exact original receipt unavailable");
    assert (!login_calls = 0));
  print_endline
    "partial setup recovered original incarnation; unrelated owner operation cannot \
     adopt it";
  [%expect
    {| partial setup recovered original incarnation; unrelated owner operation cannot adopt it |}]
;;

exception Synthetic_request_cancelled

let%expect_test "runtime request cancellation leaves host-owned setup joined by close" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let entered, entered_u = Eio.Promise.create () in
      let blocked, _ = Eio.Promise.create () in
      let cleaned = ref 0 in
      (* Only the explicit Runtime initialization port is substituted here.
         It cannot create an alternate authority or produce a fake Opened. *)
      let runtime =
        Runtime.create
          ~sw
          ~server_id
          ~authorize_setup:authorized
          ~authorize_status:authorized
          ~setup_receipt:(fun ~actor:_ _ -> Ok P.Command_receipt.Missing)
          ~existing:(fun ~sw:_ -> Ok None)
          ~initialize:(fun ~sw:_ ~actor:_ _ ->
            Exn.protect
              ~finally:(fun () -> incr cleaned)
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await blocked;
                Error DTO.Error.Store_unavailable))
        |> ok
      in
      let caller_switch, caller_switch_u = Eio.Promise.create () in
      let caller_done, caller_done_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve caller_done_u ())
          ~f:(fun () ->
            try
              Eio.Switch.run (fun caller_sw ->
                Eio.Promise.resolve caller_switch_u caller_sw;
                ignore (setup runtime : P.Method_result.t))
            with
            | Synthetic_request_cancelled -> ()));
      Eio.Promise.await entered;
      Eio.Switch.fail (Eio.Promise.await caller_switch) Synthetic_request_cancelled;
      Eio.Promise.await caller_done;
      assert (!cleaned = 0);
      Runtime.close runtime;
      assert (!cleaned = 1);
      match Runtime.dispatch runtime ~actor (P.Command.Provider_setup setup_request) with
      | Error Closed -> ()
      | _ -> failwith "closed host admitted setup"));
  print_endline
    "cancelled caller returns; host close cancels and joins exactly one blocked \
     initializer";
  [%expect
    {| cancelled caller returns; host close cancels and joins exactly one blocked initializer |}]
;;

let%expect_test "runtime close joins in-flight provisioning before acknowledging closure" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let setup_done, setup_done_u = Eio.Promise.create () in
      let close_done, close_done_u = Eio.Promise.create () in
      let calls = ref 0 in
      let runtime =
        Runtime.create
          ~sw
          ~server_id
          ~authorize_setup:authorized
          ~authorize_status:authorized
          ~setup_receipt:(fun ~actor:_ _ -> Error DTO.Error.Store_unavailable)
          ~existing:(fun ~sw:_ -> Ok None)
          ~initialize:(fun ~sw:_ ~actor:_ _ ->
            Int.incr calls;
            Eio.Promise.resolve entered_u ();
            Eio.Cancel.protect (fun () -> Eio.Promise.await release);
            Error DTO.Error.Store_unavailable)
        |> ok
      in
      Eio.Fiber.fork ~sw (fun () ->
        let result =
          Runtime.dispatch runtime ~actor (P.Command.Provider_setup setup_request)
        in
        Eio.Promise.resolve setup_done_u result);
      Eio.Promise.await entered;
      Eio.Fiber.fork ~sw (fun () ->
        Runtime.close runtime;
        Eio.Promise.resolve close_done_u ());
      Eio.Fiber.yield ();
      let awaiting_actual_worker = Option.is_none (Eio.Promise.peek close_done) in
      let admission_closed =
        match
          Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None })
        with
        | Error Closed -> true
        | Ok _ | Error _ -> false
      in
      Eio.Promise.resolve release_u ();
      Eio.Promise.await close_done;
      ignore (Eio.Promise.await setup_done : (P.Method_result.t, DTO.Error.t) Result.t);
      Runtime.close runtime;
      print_s
        [%sexp ((awaiting_actual_worker, admission_closed, !calls) : bool * bool * int)]));
  [%expect {| (true true 1) |}]
;;

let%expect_test
    "opened runtime owner remains unavailable and retries exact failed cleanup"
  =
  with_fixture (fun env sw anchor _create _login_calls ->
    let module Service = Provider_operator in
    let directory =
      S.Directory.open_or_create
        ~sw
        ~anchor
        ~components:[ S.Name.create "close-owner" |> ok ]
      |> ok
    in
    let secrets =
      Secret.open_private_files
        ~sw
        ~directory
        ~namespace:(Secret.Namespace.create "close-owner" |> ok)
      |> ok
    in
    let registry =
      C.initialize_new
        ~metadata_admission:C.Metadata_admission.nonblocking
        ~sw
        ~wall_clock:(Eio.Stdenv.clock env)
        ~new_operation:(fun () -> id "operation")
        ~directory
        ~secrets
        ~environment:None
        ~host:(id "host")
        ~incarnation:(id "incarnation")
      |> ok
    in
    let driver =
      Openai.Responses_driver.create
        ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.clock env)
        ()
      |> ok
    in
    let bridge =
      B.create
        driver
        ~registry
        ~mappings:[]
        ~compatible_profiles:[]
        ~approved_profiles:[ "approved" ]
        ~authorize:(fun ~principal:_ ~profile:_ ~operation:_ -> false)
        ~clock:(Eio.Stdenv.mono_clock env)
        ~maximum_wait:(Time_ns.Span.of_sec 1.)
        ~transport_policy:Http_sse
        ~limits:RT.Limits.default
      |> ok
    in
    let approved = profile "approved" in
    let identity =
      M.Identity.api_key
        ~host:(id "host")
        ~provider:"openai"
        ~billing:"api"
        ~account:None
        ~key_reference:(id "binding")
      |> ok
    in
    let template =
      A.Template.create
        ~profile:approved
        ~binding:(id "binding")
        ~revision:(DTO.Revision.of_string "config" |> ok)
        ~authentication:Api_key
        ~expectation:(M.Expectation.exact identity)
        ~expected_account:None
        ~mapping:(fun _ -> Error B.Error.Invalid_mapping)
      |> ok
    in
    A.initialize
      directory
      ~incarnation:(id "incarnation")
      ~templates:[ template ]
      ~default_profile:approved
      ~initial_revision:(DTO.Revision.of_string "initial" |> ok)
    |> ok;
    let profiles =
      A.open_
        directory
        ~incarnation:(id "incarnation")
        ~registry
        ~templates:[ template ]
        ~publish:(B.publish_mapping bridge)
        ~new_revision:(fun () -> DTO.Revision.of_string "next" |> ok)
      |> ok
    in
    let transport =
      Provider_oauth.Transport.create
        ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.mono_clock env)
      |> ok
    in
    Exn.protect
      ~finally:(fun () -> Provider_oauth.Transport.close transport)
      ~f:(fun () ->
        let oauth =
          Provider_oauth_registry.create
            ~transport
            ~policy:
              (Provider_oauth.Policy.direct_codex
                 ~expected_account:None
                 ~callback_port:1455
                 ()
               |> ok)
            ~wall_clock:(Eio.Stdenv.clock env)
        in
        let records =
          Service.Owner_records.create
            directory
            ~incarnation:(id "incarnation")
            ~maximum_records:4
          |> ok
        in
        let intents =
          Service.Command_intents.create directory ~host:(id "host") ~maximum_records:4
          |> ok
        in
        let service =
          Service.create
            ~sw
            ~server_id
            ~host:(id "host")
            ~incarnation:(id "incarnation")
            ~registry
            ~bridge
            ~profiles
            ~oauth
            ~owner_records:records
            ~command_intents:intents
            ~start_login:(fun ~sw:_ ~template:_ ~mode:_ ->
              failwith "close activated login")
            ~authorize:(fun _ ~operation:_ ~profile:_ -> false)
            ~clock:(Eio.Stdenv.mono_clock env)
            ~maximum_wait:(Time_ns.Span.of_sec 1.)
            ~now:(fun () -> P.Timestamp.of_time_ns Time_ns.epoch)
            ~new_operation:(fun () -> id "operation")
            ~limits:DTO.Limits.default
            ~environment:[]
          |> ok
        in
        let attempts = ref 0 in
        let opened =
          Runtime.Opened.create
            ~service
            ~backend:(A.backend profiles ~bridge ~principal:"operator")
            ~bridge:(Some bridge)
            ~incarnation:(DTO.Revision.of_string "incarnation" |> ok)
            ~setup_receipt:(fun ~actor:_ _ -> Error DTO.Error.Store_unavailable)
            ~close:(fun () ->
              Int.incr attempts;
              if Int.equal !attempts 1
              then failwith "original opened owner cleanup failure";
              Service.close service;
              C.close registry)
        in
        let runtime =
          Runtime.create
            ~sw
            ~server_id
            ~authorize_setup:authorized
            ~authorize_status:authorized
            ~setup_receipt:(fun ~actor:_ _ -> Error DTO.Error.Store_unavailable)
            ~existing:(fun ~sw:_ -> Ok (Some opened))
            ~initialize:(fun ~sw:_ ~actor:_ _ -> failwith "close reinitialized owner")
          |> ok
        in
        let primary_preserved =
          try
            Runtime.close runtime;
            false
          with
          | Failure message ->
            String.equal message "original opened owner cleanup failure"
        in
        let admission_closed =
          match
            Runtime.dispatch runtime ~actor (P.Command.Provider_status { profile = None })
          with
          | Error Closed -> true
          | Ok _ | Error _ -> false
        in
        Runtime.close runtime;
        Runtime.close runtime;
        print_s
          [%sexp ((primary_preserved, admission_closed, !attempts) : bool * bool * int)]));
  [%expect {| (true true 2) |}]
;;
