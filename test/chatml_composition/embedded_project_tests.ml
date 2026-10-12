open Core
open Agent_server_test_support
module P = Agent_protocol
module Embedded = Agent_server.Embedded
module Role = Chat_response.Script_contract_plan.Program.Role.Moderator_invocation

let save env path text =
  Eio.Path.mkdirs
    ~exists_ok:true
    ~perm:0o700
    Eio.Path.(Eio.Stdenv.fs env / Filename.dirname path);
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(Eio.Stdenv.fs env / path) text
;;

let with_directory f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () -> f env root))
;;

let options env root : Embedded.options =
  let workspace = Filename.concat root "workspace" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
  { prompt_file = Filename.concat root "prompt/agent.chatmd"
  ; workspace
  ; tool_dir = root
  ; home = Some root
  ; storage = Embedded_extension_tests.durable_storage (Filename.concat root "store")
  ; start_immediately = true
  ; permission_profile = Embedded.default_permission_profile
  ; attachment_mode = Read_write
  ; event_capacity = 64
  }
;;

let prompt =
  {|<config model="source-model" temperature="0.3"/>
<script id="owner" language="chatml" kind="moderator" project="../external/chatml.project.json" environment="selected">
type state = int
type event = [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]
let initial_state = Counter.seed
let advance state = Task.bind(Turn.prepend_system("captured:" ++ to_string(state)), fun ignored -> Task.pure(state + 1))
let on_event : context -> state -> event -> state task = fun ctx state event ->
  match event with
  | `Session_start -> advance(state)
  | `Session_resume -> advance(state)
  | `Item_appended(_) -> Task.pure(state)
  | `Turn_start -> Task.pure(state)
  | `Turn_end -> Task.pure(state)
</script>|}
;;

let sources env root =
  save env (Filename.concat root "prompt/agent.chatmd") prompt;
  save
    env
    (Filename.concat root "external/chatml.project.json")
    {|{"schema_version":1,"namespace":"embedded.project","source_dirs":["lib"],"environments":{"empty":{"modules":{}},"selected":{"modules":{"Counter":"self.Original"}}},"default_environment":"empty"}|};
  save
    env
    (Filename.concat root "external/lib/original.chatml")
    "let seed = 40 let private_value = false";
  save env (Filename.concat root "external/lib/original.chatmli") "val seed : int"
;;

let has_message (snapshot : P.Public.Snapshot.Fields.t) text =
  let entries =
    snapshot.canonical_history.entries
    @ Option.value_map snapshot.effective_history ~default:[] ~f:(fun history ->
      history.entries)
  in
  List.exists entries ~f:(fun entry ->
    match P.Public.History.full_payload entry with
    | None -> false
    | Some payload ->
      (match
         History_entry.Payload.Semantic.view (History_entry.Payload.semantic payload)
       with
       | Message { content; _ } ->
         List.exists content ~f:(function
           | History_entry.Payload.Content.Text value -> String.equal value.text text
           | _ -> false)
       | Call _ | Result _ | Reasoning _ | Unknown _ -> false))
;;

let snapshot connection id =
  Agent_client.Admin.get_session connection id |> protocol_ok |> P.Public.Snapshot.fields
;;

let await_message env connection id text =
  try
    Background_shell_tests.wait env (fun () -> has_message (snapshot connection id) text)
  with
  | Eio.Time.Timeout ->
    raise_s
      [%sexp
        { expected = (text : string)
        ; snapshot = (snapshot connection id : P.Public.Snapshot.Fields.t)
        }]
;;

let policy ~captures ~resolutions ~providers =
  let base =
    inference_policy
      ~default_model:"host-default-must-not-win"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        incr providers;
        failwith "moderator must not infer")
  in
  { base with
    capture_inference_target =
      (fun ~prompt_revision_id ~config ->
        incr captures;
        [%test_eq: string option] (Some "source-model") config.model;
        let selected =
          base.capture_inference_target ~prompt_revision_id ~config
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
          |> Result.ok_or_failwith
        in
        [%test_eq: string] "source-model" (Inference.Request.Target.model selected);
        Ok selected)
  ; resolve_inference_context =
      (fun target ->
        incr resolutions;
        [%test_eq: string] "source-model" (Inference.Request.Target.model target);
        base.resolve_inference_context target)
  }
;;

let%expect_test
    "embedded configured roots retain selected environment, role and target after cold \
     reopen"
  =
  List.iter [ Role.Ui; Headless ] ~f:(fun role ->
    with_directory (fun env root ->
      sources env root;
      let options = options env root in
      let captures = ref 0
      and resolutions = ref 0
      and providers = ref 0 in
      let daemon_options =
        { Agent_server.Daemon.default_options with
          root_invocation = role
        ; inference_policy = policy ~captures ~resolutions ~providers
        }
      in
      let project_sources : Agent_server.Project_source_configuration.t =
        { external_roots = [ Filename.concat root "external" ]; git = None }
      in
      let id, revision =
        Eio.Switch.run (fun sw ->
          let embedded =
            Embedded.start ~sw ~env ~daemon_options ~project_sources options
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () -> Embedded.close embedded)
            ~f:(fun () ->
              let connection = Embedded.connection embedded in
              let id = Embedded.session_id embedded in
              await_message env connection id "captured:40";
              let before = snapshot connection id in
              (match
                 Agent_client.Connection.request
                   connection
                   (Session_stop
                      { session_id = id
                      ; attachment_id = (Embedded.attachment embedded).id
                      ; mode = Cancel
                      ; idempotency_key =
                          P.Idempotency_key.of_string "embedded-project:stop"
                          |> protocol_ok
                      })
                 |> protocol_ok
               with
               | Non_history value ->
                 (match P.Public_result.Non_history.value value with
                  | Session_stop _ -> ()
                  | _ -> assert false)
               | _ -> assert false);
              id, before.session.prompt_revision))
      in
      [%test_eq: int] 1 !captures;
      assert (!resolutions > 0);
      List.iter
        [ "prompt/agent.chatmd"
        ; "external/chatml.project.json"
        ; "external/lib/original.chatml"
        ; "external/lib/original.chatmli"
        ]
        ~f:(fun path -> Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / path));
      let resumed_options = { options with start_immediately = false } in
      Eio.Switch.run (fun sw ->
        (* Current UI/local-only defaults are deliberately different. The original
           receipt cannot reopen deleted sources and retained Spec must win. *)
        Embedded.with_local_host
          ~sw
          ~env
          ~daemon_options:{ daemon_options with root_invocation = Ui }
          resumed_options
          ~f:(fun host ->
            let connection = Embedded.host_connection host in
            let fields = snapshot connection id in
            assert (
              Option.equal
                P.Id.Prompt_revision.equal
                revision
                fields.session.prompt_revision);
            let attached =
              Embedded.attach_retained
                host
                ~mode:Read_write
                ~expected:
                  (P.Session_lifecycle.Observation.expected
                     (Option.value_exn fields.lifecycle))
              |> protocol_ok
            in
            (match
               Agent_client.Connection.request
                 connection
                 (Session_start
                    { session_id = id
                    ; attachment_id = (Embedded.attachment attached).id
                    ; queue_if_limited = false
                    ; idempotency_key =
                        P.Idempotency_key.of_string "embedded-project:resume"
                        |> protocol_ok
                    })
               |> protocol_ok
             with
             | Non_history value ->
               (match P.Public_result.Non_history.value value with
                | Session_start _ -> ()
                | _ -> assert false)
             | _ -> assert false);
            await_message env connection id "captured:41";
            Ok ())
        |> protocol_ok);
      [%test_eq: int] 1 !captures;
      [%test_eq: int] 0 !providers;
      print_s
        [%sexp
          { role : Role.t
          ; start = (40 : int)
          ; resume = (41 : int)
          ; provider_calls = (!providers : int)
          }]));
  [%expect
    {|
    ((role Ui) (start 40) (resume 41) (provider_calls 0))
    ((role Headless) (start 40) (resume 41) (provider_calls 0))
    |}]
;;

let%test_unit
    "ungranted external project rejects before inference selection or activation"
  =
  with_directory (fun env root ->
    sources env root;
    let options = options env root in
    let captures = ref 0
    and resolutions = ref 0
    and providers = ref 0 in
    Eio.Switch.run (fun sw ->
      let result =
        Embedded.start
          ~sw
          ~env
          ~root_invocation:Headless
          ~daemon_options:
            { Agent_server.Daemon.default_options with
              inference_policy = policy ~captures ~resolutions ~providers
            }
          options
      in
      match result with
      | Error error -> assert (P.Error.equal_code error.code Prompt_unavailable)
      | Ok embedded ->
        Embedded.close embedded;
        failwith "external grant was inferred from a path");
    [%test_eq: int] 0 !captures;
    [%test_eq: int] 0 !resolutions;
    [%test_eq: int] 0 !providers)
;;

let%test_unit "embedded explicit role wins over daemon role and current defaults stay UI" =
  List.iter
    [ None, Role.Ui, true
    ; None, Headless, false
    ; Some Role.Ui, Headless, true
    ; Some Headless, Ui, false
    ]
    ~f:(fun (root_invocation, daemon_role, accepted) ->
      with_directory (fun env root ->
        sources env root;
        let options = options env root in
        save
          env
          options.prompt_file
          (String.substr_replace_first
             prompt
             ~pattern:"let initial_state = Counter.seed"
             ~with_:"let notify = Ui.notify let initial_state = Counter.seed");
        let captures = ref 0
        and resolutions = ref 0
        and providers = ref 0 in
        Eio.Switch.run (fun sw ->
          let result =
            Embedded.start
              ~sw
              ~env
              ?root_invocation
              ~project_sources:
                { external_roots = [ Filename.concat root "external" ]; git = None }
              ~daemon_options:
                { Agent_server.Daemon.default_options with
                  root_invocation = daemon_role
                ; inference_policy = policy ~captures ~resolutions ~providers
                }
              options
          in
          match result with
          | Ok embedded ->
            Exn.protect
              ~finally:(fun () -> Embedded.close embedded)
              ~f:(fun () ->
                assert accepted;
                await_message
                  env
                  (Embedded.connection embedded)
                  (Embedded.session_id embedded)
                  "captured:40";
                [%test_eq: int] 1 !captures)
          | Error error ->
            if accepted
            then
              raise_s
                [%sexp
                  { daemon_role : Role.t
                  ; requested = (root_invocation : Role.t option)
                  ; error : P.Error.t
                  }];
            assert (P.Error.equal_code error.code Invalid_state);
            assert (
              String.is_substring
                error.message
                ~substring:"Unknown configured module 'Ui'");
            (* Selection/context resolution precede this compiler preflight;
               neither is a provider dispatch or initializer effect. *)
            [%test_eq: int] 1 !captures;
            assert (!resolutions > 0));
        [%test_eq: int] 0 !providers))
;;

let reload_configuration env root description =
  let source_file = Filename.concat root "reload.sexp" in
  let text =
    sprintf
      {|(version 1)
(server ((data_dir %S) (unix_socket %S)
         (http ((enabled false)))
         (project_sources ((external_roots (%S))))))
(workspaces (((id project) (source (physical %S)) (access shared_write))))
(prompts (((id owner) (path %S) (description %S) (allowed_workspaces (project))
           (permission_profile deny) (enabled true))))
(permission_profiles (((id deny) (tool_default deny) (approval_fallback deny)
                       (manifest_authorization deny))))|}
      (Filename.concat root "reload-store")
      (Filename.concat root "reload.sock")
      (Filename.concat root "external")
      (Filename.concat root "workspace")
      (Filename.concat root "prompt/agent.chatmd")
      description
  in
  save env source_file text;
  Agent_server.Config_parser.parse_string ~source_file text
  |> Result.bind ~f:(Agent_server.Config_validator.validate ~env)
  |> Result.map_error ~f:(fun errors ->
    Sexp.to_string_hum ([%sexp_of: Agent_server.Config.Diagnostic.t list] errors))
  |> Result.ok_or_failwith
;;

let%test_unit
    "manual reload keeps the trusted headless role on newly captured definitions"
  =
  with_directory (fun env root ->
    sources env root;
    ignore (options env root : Embedded.options);
    let config = reload_configuration env root "before" in
    Eio.Switch.run (fun sw ->
      let daemon =
        Agent_server.Daemon.start
          ~sw
          ~env
          ~config
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options:
            { Agent_server.Daemon.default_options with
              startup_mode = On_demand
            ; root_invocation = Headless
            }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          let revision () =
            let entry =
              Agent_session.Prompt_catalog.find_by_name
                (Agent_server.Daemon.prompts daemon)
                "owner"
              |> Option.value_exn
            in
            assert (Role.equal Headless entry.definition.root_invocation);
            match entry.availability with
            | Ready revision -> revision
            | Unavailable diagnostics ->
              raise_s
                [%sexp
                  (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
            | Disabled -> failwith "enabled fixture became disabled"
          in
          let before = revision () in
          save
            env
            (Filename.concat root "external/lib/original.chatml")
            "let seed = 41 let private_value = false";
          ignore (reload_configuration env root "after" : Agent_server.Config.t);
          let diff =
            Agent_server.Daemon.reload_config daemon
            |> Result.map_error ~f:(fun errors ->
              Sexp.to_string_hum
                ([%sexp_of: Agent_server.Config.Diagnostic.t list] errors))
            |> Result.ok_or_failwith
          in
          assert (List.mem diff.prompts.changed "owner" ~equal:String.equal);
          let after = revision () in
          assert (
            not
              (P.Id.Prompt_revision.equal
                 (Agent_session.Prompt_revision.id before)
                 (Agent_session.Prompt_revision.id after)));
          assert (Option.is_some (Agent_session.Prompt_revision.programs before));
          assert (Option.is_some (Agent_session.Prompt_revision.programs after)))))
;;
