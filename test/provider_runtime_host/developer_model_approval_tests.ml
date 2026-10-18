open! Core
module P = Agent_protocol
module S = Agent_server
module RH = Runtime_host_tests
module DA = Developer_agent_tests
module DH = Developer_root_owner_test_suite.Developer_host_tests
module R = Agent_store.Developer_command_receipt

let ok = DA.ok

let jobs env daemon run_id =
  let data_root =
    S.Daemon.store daemon
    |> Agent_store.Session_store.data_root
    |> Agent_store.Data_root.path
  in
  let path =
    Eio.Path.(
      Eio.Stdenv.fs env
      / data_root
      / "developer-runs"
      / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
  in
  let fields =
    match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
    | `Object envelope ->
      (match List.Assoc.find_exn envelope ~equal:String.equal "payload" with
       | `Object fields -> fields
       | _ -> failwith "invalid developer run payload")
    | _ -> failwith "invalid developer run envelope"
  in
  match List.Assoc.find_exn fields ~equal:String.equal "jobs" with
  | `Array rows ->
    List.map rows ~f:(fun json ->
      P.Developer_job.of_json json
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
      |> Result.ok_or_failwith)
  | _ -> failwith "invalid developer job rows"
;;

let%test_unit "claimed Model shell approval belongs to its physical Worker" =
  let requests = ref 0 in
  RH.with_fixture ~model_requests:requests (fun env sw anchor create _ ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 50. (fun () ->
      let bootstrap = create () in
      ignore (RH.setup bootstrap : P.Method_result.t);
      let reads = ref 0 in
      ignore
        (RH.enroll bootstrap ~sw "one" "approval-bootstrap-key" reads
         : P.Provider_operator.Configuration_result.t);
      let target = RH.capture bootstrap in
      Provider_runtime.close bootstrap;
      let root = Eio.Path.native_exn anchor in
      let prompt = Filename.concat root "approval-root.chatmd" in
      let stage = Filename.concat root "stage.chatmd" in
      let shared_shell = Filename.concat root "approval-shell.chatmd" in
      let marker = Eio.Path.(Eio.Stdenv.fs env / root / "approval-marker") in
      List.iter [ "input.json"; "output.json" ] ~f:(fun name ->
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / root / name)
          {|{"type":"object"}|});
      let model_payload =
        `Object
          [ "prompt", `String prompt
          ; "input", `String "claimed worker"
          ; "is_local", `True
          ; "source_mode", `String "file"
          ]
        |> Jsonaf.to_string
      in
      let shell_spec =
        sprintf
          {|<shell_access id="approval" cwd="%s">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <write path="%s"/>
  </capabilities>
  <resolver allow_relative_search_path="false">
    <executable id="touch" path="/usr/bin/touch" trusted="true"/>
  </resolver>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="ask"/>
  <approvals provider="ui" unavailable="deny" scopes="once" durable="false"/>
  <audit format="none"/>
</shell_access>|}
          root
          root
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / shared_shell)
        (shell_spec ^ "\n<moderator_runtime shell_runtime=\"approval\"/>");
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / prompt)
        (sprintf
           {|<developer>Run the approved shell probe when a claimed Model worker starts.</developer>
<import src="approval-shell.chatmd"/>
<script id="approval_moderator" language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Item_appended(item) ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Process.run("/usr/bin/touch", [%S]), fun output ->
          Task.pure(st + 1))
      | _ -> Task.pure(st)
</script>|}
           (Filename.concat root "approval-marker"));
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / stage)
        (sprintf
           {|<developer>Stage exactly one persisted Model Job.</developer>
<import src="approval-shell.chatmd"/>
<script id="model_spawn" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun id ->
    Task.pure(`Complete(input)))
</script>
<tool name="model_spawn" type="chatml" script="model_spawn" entrypoint="run"
      input_schema="input.json" output_schema="output.json"/>|}
           model_payload);
      let provider_anchor = Eio.Path.(anchor / "approval-provider") in
      Eio.Path.mkdir ~perm:0o700 provider_anchor;
      let provider_factory ~sw:provider_sw ~server_id:provider_server_id =
        let runtime =
          create
            ~runtime_sw:provider_sw
            ~runtime_server_id:provider_server_id
            ~runtime_anchor:provider_anchor
            ()
        in
        ignore (RH.setup runtime : P.Method_result.t);
        ignore
          (RH.enroll runtime ~sw:provider_sw "one" "approval-daemon-key" reads
           : P.Provider_operator.Configuration_result.t);
        assert (Inference.Request.Target.equal target (RH.capture runtime));
        Ok (Provider_runtime.operator_port runtime)
      in
      let principal =
        Set.add Agent_server_test_support.scopes Developer_live_run
        |> Agent_server_test_support.principal_with_scopes "pri_operator"
      in
      let actor = Operator_authorization.trusted_local principal in
      let base = Agent_server_test_support.config root root prompt in
      let configured ~manifest_grants ~profile =
        let original = List.hd_exn base.prompts in
        let stage_prompt =
          Agent_server.Config.Prompt.
            { original with
              id = "stage.prompt"
            ; path = stage
            ; tool_contract = Chatml_compilation.Tool_contract.Developer_model_spawn
            }
        in
        { base with
          permission_profiles = [ profile ]
        ; manifest_grants
        ; prompts = [ original; stage_prompt ]
        }
      in
      let options =
        let module Approval = S.Daemon.Developer_configuration.Shell_approvals in
        let limits =
          Approval.Limits.create
            ~max_pending:2
            ~max_request_bytes:16384
            ~max_display_bytes:4096
            ~max_decision_reason_bytes:1024
            ~wait_seconds:20.
          |> ok
        in
        let approvals =
          Approval.create
            ~limits
            ~job_read:(fun _ _ -> true)
            ~job_respond:(fun _ _ -> true)
        in
        { S.Daemon.default_options with
          authoring = Some (DH.semantic_authoring_policy env)
        ; developer =
            Some
              (S.Daemon.Developer_configuration.with_shell_approvals
                 (DA.developer_configuration target)
                 approvals)
        ; provider_operator_factory = Some provider_factory
        }
      in
      let inspect =
        let daemon =
          S.Daemon.start
            ~sw
            ~env
            ~config:
              (configured
                 ~manifest_grants:[]
                 ~profile:Agent_server_test_support.permission_profile)
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options
            ()
          |> ok
        in
        let revision name =
          let entry =
            Agent_session.Prompt_catalog.find_by_name (S.Daemon.prompts daemon) name
            |> Option.value_exn
          in
          match entry.availability with
          | Ready revision -> revision
          | Disabled | Unavailable _ -> failwith "approval prompt did not compile"
        in
        let stage_revision = revision "stage.prompt" in
        let worker_revision = revision "restart.prompt" in
        let stage_artifact = Agent_session.Prompt_revision.artifact stage_revision in
        let worker_artifact = Agent_session.Prompt_revision.artifact worker_revision in
        let stage_manifest = Option.value_exn stage_artifact.shell_manifest_sha256 in
        let worker_manifest = Option.value_exn worker_artifact.shell_manifest_sha256 in
        [%test_eq: string] stage_manifest worker_manifest;
        let captured_stage_manifest =
          match
            Chat_response.Agent_runtime.inspect_resources
              ~env
              ~platform:(Chat_response.Agent_runtime.platform ())
              ~prompt_elements:(Agent_session.Prompt_revision.elements stage_revision)
          with
          | Ok inspection -> inspection.manifest
          | Error _ -> failwith "captured stage Shell resource inspection failed"
        in
        [%test_eq: string] stage_manifest captured_stage_manifest.sha256;
        let hashes =
          ( (stage_artifact.root_sha256, stage_manifest)
          , (worker_artifact.root_sha256, worker_manifest) )
        in
        S.Daemon.shutdown daemon |> ok;
        hashes
      in
      let (stage_source, stage_manifest), (worker_source, worker_manifest) = inspect in
      let owned_worker_manifest =
        let dir = Eio.Path.(Eio.Stdenv.fs env / root) in
        let limits =
          Prompt.Chat_markdown.Limits.create
            ~max_input_bytes:(8 * 1024 * 1024)
            ~max_parse_calls:1024
            ~max_tokens:100000
            ~max_depth:128
          |> Or_error.ok_exn
        in
        let parsed =
          Prompt.Chat_markdown.prepare_chat_inputs
            ~source:"approval-root.chatmd"
            ~source_loader:(Source_loader.filesystem ~root:dir)
            ~dir
            ~preprocess:Meta_prompting.Preprocessor.preprocess
            ~limits
            ~checkpoint:Eio.Fiber.check
            (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / prompt))
        in
        match
          Chat_response.Agent_runtime.inspect_resources
            ~env
            ~platform:(Chat_response.Agent_runtime.platform ())
            ~prompt_elements:(Prompt.Chat_markdown.Parsed.elements parsed)
        with
        | Ok inspection -> inspection.manifest
        | Error _ -> failwith "owned Worker Shell resource inspection failed"
      in
      [%test_eq: string] stage_manifest owned_worker_manifest.sha256;
      let profile =
        Agent_server.Config.Permission_profile.
          { Agent_server_test_support.permission_profile with
            manifest_authorization = Require_grant
          }
      in
      let grant ~id ~prompt ~source_sha256 ~manifest_sha256 =
        Agent_server.Config.Manifest_grant.
          { id
          ; prompt
          ; workspaces = [ "restart.workspace" ]
          ; manifest_sha256
          ; source_sha256
          ; principals = [ P.Id.Principal.to_string principal.id ]
          }
      in
      let grants =
        [ grant
            ~id:"claimed-model-stage"
            ~prompt:"stage.prompt"
            ~source_sha256:stage_source
            ~manifest_sha256:stage_manifest
        ; grant
            ~id:"claimed-model-worker"
            ~prompt:"restart.prompt"
            ~source_sha256:worker_source
            ~manifest_sha256:worker_manifest
        ]
      in
      let daemon =
        S.Daemon.start
          ~sw
          ~env
          ~config:(configured ~manifest_grants:grants ~profile)
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options
          ()
        |> ok
      in
      let closed = ref false in
      Fun.protect
        ~finally:(fun () -> if not !closed then S.Daemon.shutdown daemon |> ok)
        (fun () ->
           let connection, expected =
             DA.open_document
               ~contents:
                 {|<script id="s" language="chatml" kind="tool">
let main input =
  Task.bind(Tool.call("model_spawn", input), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))
</script>|}
               daemon
               actor
           in
           let bounds =
             Chatml_developer_capture.Json_bounds.create
               ~max_bytes:4096
               ~max_depth:16
               ~max_nodes:128
             |> Or_error.ok_exn
           in
           let request =
             S.Daemon.Developer.Request.create
               ~route:
                 (Document_script
                    { expected
                    ; selector = P.Authoring.Analysis.Target.selected_script "s" |> ok
                    })
               ~kind:Live_run
               ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
               ~prompt:(Some (S.Catalog_identity.prompt_definition "stage.prompt"))
               ~workspace:
                 (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
               ~entrypoint:"main"
               ~input:(`Object [ "input", `String "go" ])
               ~fixtures:None
               ~requested_execution:None
               ~requested_max_output_bytes:None
               ~bounds
               ~checkpoint:ignore
             |> ok
           in
           let run_id = P.Id.Developer_run.create () in
           let key = P.Idempotency_key.of_string "claimed-model-approval" |> ok in
           let stage_job ~run_id ~key =
             ignore
               (S.Daemon.Developer.submit
                  daemon
                  ~actor
                  ~run_id
                  ~key
                  ~request
                  ~fresh_source:(Some (Document_script { connection }))
                |> ok
                : S.Daemon.Developer.View.t);
             let view =
               S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
             in
             match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
             | Some (Live (Succeeded _)) -> ()
             | value ->
               raise_s [%sexp "managed Model stage failed", (value : R.value option)]
           in
           stage_job ~run_id ~key;
           let rec pending run_id () =
             match S.Daemon.Developer.Approval.pending daemon ~actor ~run_id |> ok with
             | [ item ] -> item
             | [] ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.002;
               pending run_id ()
             | _ -> failwith "unexpected number of Model approvals"
           in
           let approval =
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (pending run_id)
           in
           let module A = S.Daemon.Developer.Approval in
           (match A.Pending.owner approval, A.Pending.execution approval with
            | Claimed_job { job_id; attempt }, Claimed_model_job execution ->
              assert (P.Id.Job.equal job_id execution.job_id);
              [%test_eq: int] attempt execution.attempt;
              let model =
                List.find_exn (jobs env daemon run_id) ~f:(fun job ->
                  P.Id.Job.equal (P.Developer_job.id job) job_id)
              in
              assert (P.Job.equal_kind (P.Developer_job.kind model) Model_call);
              [%test_eq: int] (P.Developer_job.state model).attempt 1
            | _ -> failwith "approval did not belong to claimed Model Worker");
           assert (not (Eio.Path.is_file marker));
           A.respond daemon ~actor ~run_id ~id:(A.Pending.id approval) Approve_once |> ok;
           let rec completed () =
             let models =
               List.filter (jobs env daemon run_id) ~f:(fun job ->
                 P.Job.equal_kind (P.Developer_job.kind job) Model_call)
             in
             match models with
             | [ model ] ->
               (match (P.Developer_job.state model).status with
                | Succeeded -> ()
                | Failed error ->
                  raise_s [%sexp "claimed Model failed", (error : P.Error.t)]
                | Cancelled | Interrupted _ -> failwith "claimed Model was interrupted"
                | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.002;
                  completed ())
             | _ -> failwith "expected exactly one persisted Model Job"
           in
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. completed;
           assert (Eio.Path.is_file marker);
           [%test_eq: int] !requests 1;
           Eio.Path.unlink marker;
           let cancelled_run = P.Id.Developer_run.create () in
           let cancelled_key =
             P.Idempotency_key.of_string "claimed-model-cancelled" |> ok
           in
           stage_job ~run_id:cancelled_run ~key:cancelled_key;
           let waiting =
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (pending cancelled_run)
           in
           (match A.Pending.owner waiting, A.Pending.execution waiting with
            | Claimed_job { attempt = 1; _ }, Claimed_model_job _ -> ()
            | _ -> failwith "second approval did not belong to claimed Model Worker");
           assert (not (Eio.Path.is_file marker));
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
             S.Daemon.shutdown daemon |> ok);
           closed := true;
           let cancelled_model =
             match
               List.filter (jobs env daemon cancelled_run) ~f:(fun job ->
                 P.Job.equal_kind (P.Developer_job.kind job) Model_call)
             with
             | [ model ] -> model
             | _ -> failwith "cancelled run did not retain one Model Job"
           in
           (match (P.Developer_job.state cancelled_model).status with
            | Cancelled -> ()
            | status ->
              raise_s
                [%sexp
                  "claimed Model was not joined at shutdown", (status : P.Job.status)]);
           assert (not (Eio.Path.is_file marker));
           assert (
             Result.is_error
               (A.respond
                  daemon
                  ~actor
                  ~run_id:cancelled_run
                  ~id:(A.Pending.id waiting)
                  Approve_once));
           assert (not (Eio.Path.is_file marker));
           [%test_eq: int] !requests 1)))
;;
