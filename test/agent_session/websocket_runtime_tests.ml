open! Core
open Fixtures
module B = Agent_session.Runtime_builder
module R = Inference.Request
module O = Inference.Observation
module E = Inference.Event
module Runtime = Inference_runtime

let admitted result =
  Result.map_error result ~f:(fun _ -> "graph fixture admission") |> Result.ok_or_failwith
;;

let%expect_test
    "real runtime graph owns transport binding and exposes actual fallback before \
     teardown"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root =
        Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
      in
      List.iter [ "cache"; "session" ] ~f:(fun name ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / name));
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(root / "ws-root.chatmd")
        "<developer>Graph-owned inference.</developer>";
      let definition =
        Agent_session.Prompt_definition.create
          ~id:prompt_id
          ~config_name:"ws-runtime"
          ~root_file:(Eio.Path.native_exn Eio.Path.(root / "ws-root.chatmd"))
          ~allowed_workspaces:[ workspace_id ]
          ~permission_profile:"interactive"
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> store_ok
      in
      let artifact_store =
        Agent_store.Prompt_artifact_store.create
          ~env
          ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
        |> store_ok
      in
      let revision =
        Agent_session.Prompt_revision_builder.build
          ~env
          ~artifact_store
          ~transaction_id
          ~created_at:timestamp
          definition
        |> admitted
      in
      let paths : Agent_session.Runtime_paths.t =
        { tool_dir = root
        ; workspace = root
        ; prompt_dir = root
        ; session_dir = Eio.Path.(root / "session")
        ; cache_dir = Eio.Path.(root / "cache")
        ; home = root
        }
      in
      let target =
        R.Target.create
          ~adapter:"graph-transport"
          ~profile:"selected"
          ~profile_revision:None
          ~account:None
          ~endpoint:"fixture"
          ~model:"model"
          ~settings:[]
          ~limits:Document_schema.Limits.default
        |> admitted
      in
      let opened = ref 0
      and closed = ref 0 in
      let prepare ~policy ~preparation_id request =
        let configuration =
          O.Configuration.of_target
            ~transport_policy:policy
            (R.target request)
            ~preparation_id
            ~transport:Websocket
            ~capabilities:[]
            ~limits:O.Admission.observation
          |> admitted
        in
        Runtime.Plan.create
          ~request
          ~configuration
          ~fingerprint:"captured-graph-fingerprint"
          ~run:
            (fun
              ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation ->
            let selection =
              O.Transport_selection.create
                ~accounting_id
                ~requested:policy
                ~selected:Http_sse
                ~fallback:(Some Connection)
              |> admitted
            in
            let id =
              O.Observation_id.of_string
                ("transport/" ^ O.Observation_id.to_string accounting_id)
              |> admitted
            in
            on_observation
              (O.create
                 ~scope
                 ~id
                 ~revision:0L
                 ~payload:(Transport_selection selection)
                 ~limits:O.Admission.observation
               |> admitted);
            let count = O.Count.create (Actual 0L) |> admitted in
            let counts : O.Usage.counts =
              { input = count
              ; output = count
              ; reported_total = count
              ; cached_input = count
              ; cache_write_input = count
              ; reasoning_output = count
              }
            in
            let usage = O.Usage.create ~counts ~inclusions:[] |> admitted in
            let usage =
              O.create
                ~scope
                ~id:accounting_id
                ~revision:0L
                ~payload:(Usage usage)
                ~limits:O.Admission.observation
              |> admitted
            in
            let terminal =
              E.Terminal.create ~scope ~delivery:Response_started ~outcome:Completed
              |> admitted
            in
            Runtime.Receipt.create
              ~terminal
              ~usage
              ~output:[]
              ~output_coverage:Response_output
              ~limits:Runtime.Limits.default
            |> admitted)
      in
      let adapter =
        Runtime.Adapter.create
          ~preflight_history:(fun ~target:_ _ -> Ok ())
          ~id:"graph-transport"
          ~limits:Runtime.Limits.default
          ~bind:(fun _ -> Ok ())
          ~prepare:(prepare ~policy:Prefer_websocket)
          ~prepare_with_policy:prepare
          ~open_session:(fun owner ~policy ->
            incr opened;
            Runtime.Session.on_release owner (fun () -> incr closed) |> admitted;
            Ok (prepare ~policy))
          ()
        |> admitted
      in
      let context =
        Runtime.Context.create adapter ~target
        |> admitted
        |> fun context -> Runtime.Context.with_transport_policy context Prefer_websocket
      in
      let identity =
        Inference_fixture.create
          ~namespace:"graph-ws"
          ~default_model:"model"
          ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected legacy dispatch")
        |> Inference_fixture.identity
      in
      let attempt = ref None
      and selection = ref None in
      let runtime =
        B.build
          ~sw
          ~env
          ~inference_context:context
          ~inference_identity:identity
          ~on_inference_attempt:(fun value -> attempt := Some value)
          ~on_inference_completion:ignore
          ~on_inference_observation:(fun observation ->
            match O.payload observation with
            | Transport_selection _ -> selection := Some observation
            | _ -> ())
          ~paths
          ~storage_paths:paths
          ~revision
          ~session_id
          ~history_namespace:(Agent_protocol.Id.Session.to_string session_id)
          ~next_history_sequence:1
          ~existing_history:(Some [])
          ~existing_moderator_snapshot:None
          ~moderator_reservation_size:100
          ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
          ~approval_provider:Shell_runtime.Approval_broker.None_available
          ~approval_store:(Shell_access.Approval.create_store ())
          ~permission_profile:
            (permission_policy
               ~tool_default:Allow
               ~fallback:Fallback_deny
               ~evaluator:None
               ~reviewer:None)
          ~review_permission:(fun _ -> failwith "unexpected review")
          ~schedule_services:
            { after_ms = (fun ~delay_ms:_ ~payload:_ -> failwith "unexpected timer")
            ; cancel = (fun ~id:_ -> failwith "unexpected timer")
            }
          ~job_services:
            { spawn_model = (fun ~recipe:_ ~payload:_ -> failwith "unexpected job")
            ; call_model =
                (fun ~recipe:_ ~payload:_ ~execute:_ -> failwith "unexpected job")
            }
        |> protocol_ok
      in
      let execution = B.inference_execution runtime in
      let request =
        R.create
          ~target
          ~history:[]
          ~tools:[]
          ~assets:[]
          ~limits:Document_schema.Limits.default
        |> admitted
      in
      let receipt =
        Inference_client.Execution.run execution ~sw ~request ~on_event:ignore |> admitted
      in
      let configuration = Runtime.Attempt.configuration (Option.value_exn !attempt) in
      let observation = Option.value_exn !selection in
      let terminal = Runtime.Receipt.terminal receipt in
      let view =
        Agent_protocol.Inference_query.Attempt.create
          ~ordinal:1L
          ~generation:1
          ~scope:(E.Terminal.scope terminal)
          ~operation_id:None
          ~invocation_id:None
          ~accounting_id:(O.id (Runtime.Receipt.usage receipt))
          ~state:(Terminal terminal)
          ~usage:(Some (Runtime.Receipt.usage receipt))
          ~context:None
          ~configuration:(Some configuration)
          ~diagnostics:None
          ~omitted_diagnostics:None
        |> admitted
      in
      let view =
        Agent_protocol.Inference_query.Attempt.with_transport_selection
          view
          (Some observation)
        |> admitted
      in
      let restored =
        Agent_protocol.Inference_query.Attempt.to_json view
        |> Agent_protocol.Inference_query.Attempt.of_json
        |> admitted
      in
      let actual =
        Agent_protocol.Inference_query.Attempt.transport_selection restored
        |> Option.value_exn
      in
      assert (O.equal actual observation);
      print_s
        [%sexp (O.Configuration.transport configuration : O.Configuration.transport)];
      (match O.payload actual with
       | Transport_selection actual ->
         print_s
           [%sexp
             (O.Transport_selection.selected actual : O.Transport_selection.transport)
           , (O.Transport_selection.fallback actual
              : O.Transport_selection.fallback_reason option)]
       | _ -> assert false);
      runtime.close ();
      print_s [%sexp (!opened : int), (!closed : int)];
      match Inference_client.Execution.run execution ~sw ~request ~on_event:ignore with
      | Error error -> print_s [%sexp (error : Inference_client.Error.t)]
      | Ok _ -> failwith "disposed graph dispatched"));
  [%expect
    {|
    Websocket
    (Http_sse (Connection))
    (1 1)
    (Preparation Session_closed)
    |}]
;;
