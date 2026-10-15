open Core
open Agent_server_test_support
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Catalog = Agent_session.Prompt_catalog
module Workspace = Agent_session.Workspace_catalog
module Definition = Agent_session.Workspace_definition
module Policy = Agent_session.Permission_policy
module Actor = Agent_session.Session_actor

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected catalog admission fixture rejection"
;;

let with_daemon
      ?(prompt_text = "<developer>Pinned source during creation.</developer>")
      ?(schemas = false)
      ?(tool_contract = Chatml_compilation.Tool_contract.Standard)
      ~policy
      f
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    let path = Eio.Path.(env#fs / root) in
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree path))
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / prompt) prompt_text;
         if schemas
         then (
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(env#fs / root / "input.json")
             {|{"type":"object"}|};
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(env#fs / root / "output.json")
             {|{"type":"string"}|};
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(env#fs / root / "chatml.project.json")
             {|{"schema_version":1,"namespace":"developer-prompt","modules":{"Entry":{"implementation":"handler.chatml"}},"environments":{"default":{"modules":{"Entry":"self.Entry"}}}}|};
           Eio.Path.save
             ~create:(`Exclusive 0o600)
             Eio.Path.(env#fs / root / "handler.chatml")
             {|let run ctx input =
  let unused = fun () -> Model.spawn("agent_prompt_v1", input) in
  Task.pure(`Complete(`String("ok")))|});
         Eio.Switch.run (fun sw ->
           let daemon =
             Daemon.start
               ~sw
               ~env
               ~config:
                 (let config = config root root prompt in
                  { config with
                    prompts =
                      List.map config.prompts ~f:(fun entry ->
                        { entry with tool_contract })
                  })
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options:{ Daemon.default_options with inference_policy = policy }
               ()
             |> protocol_ok
           in
           Fun.protect
             ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
             (fun () -> f env sw root daemon))))
;;

let%test_unit
    "developer publication restores only its captured contract and blocks conversation \
     creation"
  =
  let prompt_text =
    {|<developer>Developer managed handler.</developer>
<script id="handler" kind="tool" language="chatml" src="handler.chatml" project="chatml.project.json"/>
<tool name="managed" type="chatml" script="handler" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
  in
  let captures = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      failwith "unexpected model dispatch")
  in
  let policy =
    { policy with
      capture_inference_target =
        (fun ~prompt_revision_id ~config ->
          incr captures;
          policy.capture_inference_target ~prompt_revision_id ~config)
    }
  in
  with_daemon
    ~prompt_text
    ~schemas:true
    ~tool_contract:Chatml_compilation.Tool_contract.Developer_model_spawn
    ~policy
    (fun env _ root daemon ->
       let entries = Catalog.entries (Daemon.prompts daemon) in
       let entry = List.hd_exn entries in
       let revision =
         match entry.availability with
         | Catalog.Ready revision -> revision
         | Catalog.Unavailable _ | Catalog.Disabled ->
           failwith "developer Prompt did not publish"
       in
       [%test_eq: Chatml_compilation.Tool_contract.t]
         (Agent_session.Prompt_definition.tool_contract entry.definition)
         Chatml_compilation.Tool_contract.Developer_model_spawn;
      assert (Option.is_some (Agent_session.Prompt_revision.programs revision));
       Eio.Path.unlink Eio.Path.(env#fs / root / "parent.chatmd");
       Eio.Path.unlink Eio.Path.(env#fs / root / "input.json");
       Eio.Path.unlink Eio.Path.(env#fs / root / "output.json");
      Eio.Path.unlink Eio.Path.(env#fs / root / "handler.chatml");
      Eio.Path.unlink Eio.Path.(env#fs / root / "chatml.project.json");
       let preparation = Option.value_exn Daemon.default_options.script_preparation in
       (match
          Agent_session.Runtime_script_preparation.with_revision_plan
            ~tool_contract:Chatml_compilation.Tool_contract.Developer_model_spawn
            (Some preparation)
            revision
            ~env
            ~invocation:Agent_session.Runtime_script_preparation.Invocation.Ui
            ~checkpoint:Eio.Fiber.check
           ~f:(fun _ ~compilation_limits:_ ~plan ~programs ->
             Ok
               ( Option.value_map plan ~default:0 ~f:(fun plan ->
                   List.length (Chat_response.Script_contract_plan.programs plan))
               , Option.is_some programs ))
        with
       | Ok (count, true) when count > 0 -> ()
       | Ok (count, restored) ->
         raise_s [%sexp "retained developer graph did not restore", (count : int), (restored : bool)]
       | Error error -> raise_s [%sexp "retained developer restore", (error : Agent_protocol.Error.t)]);
       assert (
         Result.is_error
           (Agent_session.Runtime_script_preparation.with_revision_plan
              ~tool_contract:Chatml_compilation.Tool_contract.Standard
              (Some preparation)
              revision
              ~env
              ~invocation:Agent_session.Runtime_script_preparation.Invocation.Ui
              ~checkpoint:Eio.Fiber.check
              ~f:(fun _ ~compilation_limits:_ ~plan:_ ~programs:_ -> Ok ())));
       (match
          Factory.create_session
            (Daemon.factory daemon)
            ~command_audit:None
            ~principal:(principal ())
            (create_request ())
        with
        | Error { code = Prompt_unavailable; _ } -> ()
        | Error _ | Ok _ -> failwith "conversation admitted a developer-only Prompt");
       [%test_eq: int] !captures 0)
;;

let%test_unit "fresh creation keeps one generation across inference and runtime preflight"
  =
  let captured, captured_u = Eio.Promise.create () in
  let proceed, proceed_u = Eio.Promise.create () in
  let first = ref true in
  let during_runtime = ref ignore in
  let selected_revision = ref None in
  let runtime_calls = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      failwith "unexpected model dispatch")
  in
  let policy =
    { policy with
      capture_inference_target =
        (fun ~prompt_revision_id ~config ->
          if !first
          then (
            first := false;
            selected_revision := Some prompt_revision_id;
            Eio.Promise.resolve captured_u ();
            Eio.Promise.await proceed);
          policy.capture_inference_target ~prompt_revision_id ~config)
    ; runtime_inference_ports =
        (fun actor ->
          incr runtime_calls;
          !during_runtime ();
          policy.runtime_inference_ports actor)
    }
  in
  with_daemon ~policy (fun _ sw root daemon ->
    let factory = Daemon.factory daemon in
    let original = List.hd_exn (Workspace.definitions (Daemon.workspaces daemon)) in
    let replacement = Workspace.create [] |> ok in
    let next =
      Policy.create
        ~id:permission_profile.id
        ~tool_default:Deny
        ~approval_timeout_ms:None
        ~fallback:Fallback_deny
        ~manifest_authorization:Deny_manifest
        ~evaluator:None
        ~evaluator_revision:None
        ~reviewer:None
      |> ok
    in
    let blocked () =
      let polls = ref 0 in
      (match
         Factory.seal_catalogs
           factory
           ~workspaces:replacement
           ~permission_profiles:[ next ]
           ~manifest_grants:[]
           ~checkpoint:(fun () -> incr polls)
       with
       | Error Busy -> ()
       | _ -> assert false);
      [%test_eq: int] !polls 0;
      match Workspace.install (Daemon.workspaces daemon) ~replacement with
      | () -> assert false
      | exception Invalid_argument _ -> ()
    in
    during_runtime := blocked;
    let creation =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Factory.create_session
          factory
          ~command_audit:None
          ~principal:(principal ())
          (create_request ()))
    in
    Eio.Promise.await captured;
    blocked ();
    Eio.Promise.resolve proceed_u ();
    let entry = Eio.Promise.await_exn creation |> protocol_ok in
    Fun.protect ~finally:entry.close (fun () ->
      let state = Actor.state entry.actor |> protocol_ok in
      assert (Option.is_some state.spec.quota_key);
      [%test_eq: Agent_protocol.Id.Prompt_revision.t option]
        !selected_revision
        (Some state.spec.prompt_revision_id);
      [%test_eq: Definition.access] state.spec.workspace_instance.access original.access;
      [%test_eq: string option] state.spec.workspace_instance.configured_root (Some root);
      assert (not (String.equal state.spec.permission_profile_digest next.revision_digest));
      assert (!runtime_calls > 0);
      let planned =
        Factory.seal_catalogs
          factory
          ~workspaces:replacement
          ~permission_profiles:[ next ]
          ~manifest_grants:[]
          ~checkpoint:ignore
        |> ok
      in
      Factory.Catalog_seal.commit planned |> ok;
      (* The admitted old entry is still inspectable; a new root uses the newly
         published missing-workspace result before inference/native preparation. *)
      let pinned = Actor.state entry.actor |> protocol_ok in
      [%test_eq: string]
        pinned.spec.permission_profile_digest
        state.spec.permission_profile_digest;
      match
        Factory.create_session
          factory
          ~command_audit:None
          ~principal:(principal ())
          (create_request ~key:"after-publication" ())
      with
      | Error { code = Workspace_not_found; _ } -> ()
      | _ -> assert false))
;;

let%test_unit "sealed and uncertain catalog admission rejects before the capture callback"
  =
  List.iter [ `Factory; `Prompt ] ~f:(fun owner ->
    let captures = ref 0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
        failwith "unexpected model dispatch")
    in
    let policy =
      { policy with
        capture_inference_target =
          (fun ~prompt_revision_id ~config ->
            incr captures;
            policy.capture_inference_target ~prompt_revision_id ~config)
      }
    in
    with_daemon ~policy (fun _ sw _ daemon ->
      let factory = Daemon.factory daemon in
      let reject expected =
        (match
           Factory.create_session
             factory
             ~command_audit:None
             ~principal:(principal ())
             (create_request ())
         with
         | Error error -> [%test_eq: Agent_protocol.Error.code] error.code expected
         | Ok _ -> assert false);
        [%test_eq: int] !captures 0
      in
      match owner with
      | `Factory ->
        let replacement = Workspace.create [] |> ok in
        let planned =
          Factory.seal_catalogs
            factory
            ~workspaces:replacement
            ~permission_profiles:[]
            ~manifest_grants:[]
            ~checkpoint:ignore
          |> ok
        in
        reject Conflict;
        Factory.Catalog_seal.mark_uncertain planned |> ok;
        reject Invalid_state
      | `Prompt ->
        let prompts = Daemon.prompts daemon in
        let definitions =
          List.map (Catalog.entries prompts) ~f:(fun entry -> entry.definition)
        in
        let source_limits =
          Agent_session.Prompt_source_owner.Limits.create
            ~max_path_components:512
            ~max_runtime_leases:4096
          |> Or_error.ok_exn
        in
        let prepared =
          Catalog.prepare_owned
            prompts
            ~sw
            ~source_limits
            ~max_roots:1
            ~checkpoint:ignore
            ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
            ~created_at:(Agent_protocol.Timestamp.now ())
            definitions
          |> Or_error.ok_exn
        in
        let planned = Catalog.seal prompts prepared ~checkpoint:ignore |> ok in
        reject Conflict;
        Catalog.Seal.mark_uncertain planned |> ok;
        reject Invalid_state;
        Catalog.Seal.retire planned |> ok))
;;
