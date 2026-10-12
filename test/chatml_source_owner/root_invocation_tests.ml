open Core
open Support
module R = Agent_session.Prompt_revision
module B = Agent_session.Runtime_builder
module Prep = Agent_session.Runtime_script_preparation
module Role = Chat_response.Script_contract_plan.Program.Role.Moderator_invocation
module State = Agent_session.Session_state

let root_definition root ~role ~enabled =
  Agent_session.Prompt_definition.create
    ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_original" |> ok)
    ~root_invocation:role
    ~config_name:"original"
    ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
    ~allowed_workspaces:[]
    ~permission_profile:"root-invocation"
    ~runtime_policy:None
    ~enabled
    ~description:None
  |> ok
;;

let paths root revision : Agent_session.Runtime_paths.t =
  let root = (root :> Eio.Fs.dir_ty Eio.Path.t) in
  { workspace = root
  ; tool_dir = root
  ; prompt_dir = R.materialized_tree revision
  ; session_dir = Eio.Path.(root / "session")
  ; cache_dir = Eio.Path.(root / "cache")
  ; home = root
  }
;;

let snapshot_state runtime expected =
  let manager = Option.value_exn runtime.B.moderator_manager in
  let snapshot =
    Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
  in
  match snapshot.current_state with
  | Session.Snapshot.Int value -> [%test_eq: int] expected value
  | _ -> failwith "root event lost its actual integer state"
;;

let retained_state env root definition revision target role =
  let module P = Agent_protocol in
  let timestamp = P.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok in
  let session_id = P.Id.Session.of_string "ses_root_contract" |> ok in
  let workspace_instance =
    Agent_session.Workspace_resolver.resolve_current
      ~env
      ~instance_id:(P.Id.Workspace_instance.of_string "wsi_root_contract" |> ok)
      ~path:(Eio.Path.native_exn root)
      ~access:Shared_write
      ~created_at:timestamp
    |> ok
  in
  let protocol =
    P.Session.Spec.create
      ~execution_host:Embedded
      ~prompt:(Catalog definition.Agent_session.Prompt_definition.id)
      ~workspace:Current
      ~liveness:Process_bound
      ~persistence:Transient
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  let inference_target =
    Inference.Selection.captured target ~limits:Document_schema.Limits.default |> ok
  in
  State.create
    ~identity:
      { session_id
      ; display_name = None
      ; creating_principal = None
      ; created_at = timestamp
      ; updated_at = timestamp
      ; labels = []
      ; generation = 0
      ; metadata_revision = 0L
      ; organization = P.Session_organization.Values.empty
      }
    ~spec:
      { protocol
      ; prompt_definition_id = Some definition.id
      ; root_invocation = role
      ; prompt_revision_id = R.id revision
      ; configuration_revision = 0L
      ; inference_target
      ; delegation = None
      ; workspace_instance
      ; permission_profile = "root-invocation"
      ; permission_profile_digest = "fixture"
      ; runtime_policy = None
      ; quota_key = None
      }
    ~initial_history:[]
;;

let build env sw root fixture target (state : State.t) revision ~startup ~snapshot =
  let policy =
    Agent_session.Permission_policy.create
      ~id:"root-invocation"
      ~tool_default:Deny
      ~approval_timeout_ms:None
      ~fallback:Fallback_deny
      ~manifest_authorization:Deny_manifest
      ~evaluator:None
      ~evaluator_revision:None
      ~reviewer:None
    |> ok
  in
  let paths = paths root revision in
  Eio.Path.mkdirs ~perm:0o700 ~exists_ok:true paths.cache_dir;
  B.build
    ~agent_source:None
    ~startup
    ~root_invocation:state.State.spec.root_invocation
    ~script_preparation:(Some (Runtime_preparation_tests.policy ()))
    ~sw
    ~env
    ~inference_context:((Inference_fixture.resolve fixture) target |> ok)
    ~inference_identity:(Inference_fixture.identity fixture)
    ~on_inference_attempt:ignore
    ~on_inference_completion:ignore
    ~on_inference_observation:ignore
    ~paths
    ~storage_paths:paths
    ~revision
    ~session_id:state.identity.session_id
    ~history_namespace:(Agent_protocol.Id.Session.to_string state.identity.session_id)
    ~next_history_sequence:0
    ~existing_history:(Some [])
    ~existing_moderator_snapshot:snapshot
    ~moderator_reservation_size:32
    ~manifest_authorizer:(fun _ -> failwith "root fixture must not authorize shell hooks")
    ~approval_provider:Shell_runtime.Approval_broker.None_available
    ~approval_store:(Shell_access.Approval.create_store ())
    ~permission_profile:policy
    ~review_permission:(fun _ -> failwith "root fixture must not request review")
    ~schedule_services:
      { after_ms = (fun ~delay_ms:_ ~payload:_ -> failwith "unexpected timer")
      ; cancel = (fun ~id:_ -> failwith "unexpected timer")
      }
    ~job_services:
      { spawn_model = (fun ~recipe:_ ~payload:_ -> failwith "unexpected model spawn")
      ; call_model =
          (fun ~recipe:_ ~payload:_ ~execute:_ -> failwith "unexpected model call")
      }
;;

let%expect_test "captured root role survives source removal and real moderator restart" =
  List.iter [ Role.Ui; Headless ] ~f:(fun role ->
    with_workspace (fun env parent root ->
      save
        root
        "root.chatmd"
        {|<script id="owner" language="chatml" kind="moderator" project="chatml.project.json">
let initial_state = Counter.seed
let on_event ctx state event = if ctx.session_id == "ses_root_contract" then Task.pure(state + 1) else Task.fail("wrong context ABI")
</script>|};
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "lib");
      save
        root
        "chatml.project.json"
        {|{"schema_version":1,"namespace":"root_invocation","source_dirs":["lib"]}|};
      save root "lib/counter.chatml" "let seed = 40";
      save root "lib/counter.chatmli" "val seed : int";
      let definition = root_definition root ~role ~enabled:true in
      Eio.Switch.run (fun sw ->
        let warm = catalog env parent in
        let candidate =
          prepare warm sw [ definition ] ~prepare_programs:(fun scope captured ->
            assert (Role.equal role (Owner.Capture.root_invocation captured));
            Preparation_tests.prepare
              env
              scope
              captured
              ~root_role:(Owner.Capture.root_invocation captured))
          |> ok
        in
        Catalog.install_owned warm candidate |> ok;
        let revision = revision warm definition in
        let providers = ref 0 in
        let fixture =
          Inference_fixture.create
            ~namespace:"root-invocation"
            ~default_model:"o3"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              incr providers;
              failwith "root moderator must not infer")
        in
        let target =
          Inference_fixture.capture_config
            fixture
            (Chat_response.Config.of_elements (R.elements revision))
          |> ok
        in
        let retained = retained_state env root definition revision target role in
        let module D = Agent_session.Session_state_document in
        let document =
          D.encode (D.authored retained) ~limits:Document_schema.Limits.default |> ok
        in
        List.iter
          [ "root.chatmd"
          ; "chatml.project.json"
          ; "lib/counter.chatml"
          ; "lib/counter.chatmli"
          ]
          ~f:(fun file -> Eio.Path.unlink Eio.Path.(root / file));
        let current_definition = root_definition root ~role:Ui ~enabled:false in
        let cold = catalog env parent in
        Catalog.install_owned cold (prepare cold sw [ current_definition ] |> ok) |> ok;
        let restored =
          Catalog.restore_revision
            cold
            ~definition_id:definition.id
            ~revision_id:retained.spec.prompt_revision_id
          |> ok
        in
        assert (Role.equal Ui (R.definition restored).root_invocation);
        let retained =
          D.decode ~limits:Document_schema.Limits.default document |> ok |> D.value
        in
        assert (Role.equal role retained.spec.root_invocation);
        let child_effects = ref 0 in
        let wrong_role =
          match role with
          | Ui -> Role.Headless
          | Headless -> Ui
        in
        let rejected =
          Prep.with_revision
            (Some (Runtime_preparation_tests.policy ()))
            restored
            ~env
            ~invocation:(Prep.Invocation.of_root wrong_role)
            ~checkpoint:ignore
            ~f:(fun _ ~compilation_limits:_ ~programs:_ ->
              incr child_effects;
              Ok ())
        in
        assert (Result.is_error rejected);
        [%test_eq: int] 0 !child_effects;
        let first =
          build env sw root fixture target retained restored ~startup:None ~snapshot:None
          |> ok
        in
        let snapshot =
          Fun.protect ~finally:first.close (fun () ->
            let snapshot = first.start_moderator () |> ok in
            snapshot_state first 41;
            snapshot)
        in
        let second =
          build env sw root fixture target retained restored ~startup:None ~snapshot |> ok
        in
        Fun.protect ~finally:second.close (fun () ->
          ignore (second.start_moderator () |> ok : Jsonaf.t option);
          snapshot_state second 42);
        [%test_eq: int] 0 !providers;
        print_s
          [%sexp
            { retained_root = (role : Role.t); start = (41 : int); resume = (42 : int) }])));
  [%expect
    {|
    ((retained_root Ui) (start 41) (resume 42))
    ((retained_root Headless) (start 41) (resume 42))
    |}]
;;
