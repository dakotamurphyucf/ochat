open! Core
module P = Agent_protocol
module D = P.Prompt_dependency_selection
module Daemon = Agent_server.Daemon
module Catalog = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Git_fixture = Git_fixture_support

let ok = Agent_server_test_support.protocol_ok
let save = Native_tests.save

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let definition = Agent_server.Catalog_identity.prompt_definition "agent"
let disabled = Agent_server.Catalog_identity.prompt_definition "disabled"
let key text = P.Idempotency_key.of_string text |> ok

let connect ?(drop_update_reply = ref false) daemon principal current =
  let notifications = Eio.Stream.create 256 in
  let actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
  in
  let context =
    Agent_server.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:8
  in
  let connection =
    Agent_client.In_memory.create
      ~notifications
      ~request:(fun command ->
        let response =
          Agent_server.Dispatcher.dispatch_command
            (Daemon.dispatcher daemon)
            ~context
            command
        in
        match command, response with
        | P.Command.Prompt_dependencies_update _, Ok _ when !drop_update_reply ->
          drop_update_reply := false;
          Error
            (P.Error.create
               Interrupted
               ~message:"fixture lost update reply after real dispatch"
               ~retryable:false
               ())
        | _, _ -> response)
      ~close:(fun () -> Daemon.close_connection daemon context)
  in
  Agent_server_test_support.initialize connection;
  connection
;;

let with_workspace f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let path = "/tmp/och230-" ^ P.Id.Transaction.(create () |> to_string) in
    let root = Eio.Path.(env#fs / path) in
    Eio.Path.mkdir ~perm:0o700 root;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      (fun () -> f env root))
;;

let dependency repository commit =
  sprintf
    {|{"source":{"kind":"git","repository":%s,"object_format":"sha1","commit":%s},"project":"chatml.project.json"}|}
    (Jsonaf.to_string (`String repository))
    (Jsonaf.to_string (`String commit))
;;

let commit_project env repository manifest body =
  let git arguments stdin =
    Git_fixture.run_git env repository ~arguments ~stdin |> String.strip
  in
  let declaration = git [ "hash-object"; "-w"; "--stdin" ] manifest in
  let code = git [ "hash-object"; "-w"; "--stdin" ] body in
  let tree =
    git
      [ "mktree" ]
      (sprintf
         "100644 blob %s\tchatml.project.json\n100644 blob %s\tcode.chatml\n"
         declaration
         code)
  in
  let commit = git [ "commit-tree"; tree ] "dependency project\n" in
  ignore (git [ "update-ref"; "refs/heads/main"; commit ] "" : string);
  ignore (git [ "symbolic-ref"; "HEAD"; "refs/heads/main" ] "" : string);
  commit
;;

let inner_manifest =
  {|{"schema_version":1,"namespace":"inner","modules":{"Code":{"implementation":"code.chatml"}}}|}
;;

let outer_manifest repository commit =
  sprintf
    {|{"schema_version":1,"namespace":"outer","modules":{"Code":{"implementation":"code.chatml"}},"environments":{"default":{"modules":{"Own":"self.Code","Nested":"inner.Code"}}},"dependencies":{"inner":%s}}|}
    (dependency repository commit)
;;

let root_manifest repository commit =
  sprintf
    {|{"schema_version":1,"namespace":"root","modules":{"Lookup":{"implementation":"lookup.chatml"}},"environments":{"default":{"modules":{"Own":"self.Lookup","External":"outer.Code"}}},"dependencies":{"outer":%s}}|}
    (dependency repository commit)
;;

let config_text root helpers outer inner =
  let atom text = Sexp.to_string (Sexp.Atom text) in
  sprintf
    {|(version 1)
(server ((data_dir %s) (unix_socket %s)
 (project_sources ((external_roots ()) (git ((helper_directory %s)
 (ca_bundle %s) (memory_bytes_per_process %d)
 (repositories ((https ((repository %s))) (https ((repository %s)))))))))))
(workspaces (((id project) (source (physical %s)) (access shared_write))))
(prompts (((id agent) (path %s) (allowed_workspaces (project))
 (permission_profile unattended) (enabled true))
 ((id disabled) (path %s) (allowed_workspaces (project))
 (permission_profile unattended) (enabled false))))
(permission_profiles (((id unattended) (tool_default allow)
 (manifest_authorization assume_authorized))))|}
    (atom (Eio.Path.native_exn Eio.Path.(root / "data")))
    (atom (Eio.Path.native_exn Eio.Path.(root / "agent.sock")))
    (atom helpers)
    (atom (Eio.Path.native_exn Eio.Path.(root / "ca.pem")))
    (Git_fixture.memory_cap ())
    (atom outer)
    (atom inner)
    (atom (Eio.Path.native_exn root))
    (atom (Eio.Path.native_exn Eio.Path.(root / "root.chatmd")))
    (atom (Eio.Path.native_exn Eio.Path.(root / "root.chatmd")))
;;

let ready daemon =
  match Catalog.find (Daemon.prompts daemon) definition with
  | Some { availability = Ready revision; _ } -> revision
  | _ -> failwith "expected the actual installed ready prompt"
;;

let status connection = Agent_client.Admin.dependency_selection_status connection |> ok

let generation connection =
  match (status connection).D.Status.history with
  | Tracked value -> value
  | Missing | Invalid -> failwith "expected installed dependency history"
;;

let request ~generation ~definitions text =
  D.Update_request.create
    ~definitions
    ~expected_generation:generation
    ~idempotency_key:(key text)
  |> ok
;;

let assert_rejected expected = function
  | Error error -> assert (P.Error.equal_code expected error.P.Error.code)
  | Ok _ -> failwith "dependency admission unexpectedly succeeded"
;;

let create_stopped_session connection =
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog definition)
      ~workspace:
        (Configured (Agent_server.Catalog_identity.workspace_definition "project"))
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  match
    Agent_client.Connection.request
      connection
      (Session_create
         { spec
         ; requested_mode = Some Read_write
         ; subscribe = false
         ; idempotency_key = key "retained-session"
         })
    |> ok
  with
  | Session_create created -> created.session.id
  | _ -> failwith "expected actual stopped session creation"
;;

let session_revision daemon session_id =
  let entry =
    Agent_server.Session_registry.find (Daemon.registry daemon) session_id
    |> Option.value_exn
  in
  (Agent_session.Session_actor.state entry.actor |> ok).spec.prompt_revision_id
;;

let check_saved index generation revision =
  let module B = Agent_session.Prompt_project_baseline in
  let limits =
    B.Limits.create
      ~max_definitions:8
      ~max_references:16
      ~max_identity_bytes:4096
      ~max_work_steps:1_000_000
      ~document:Document_schema.Limits.default
    |> Or_error.ok_exn
  in
  let document =
    Eio.Path.load index |> B.Document.decode ~limits ~checkpoint:ignore |> Support.ok
  in
  [%test_eq: int] generation (B.Document.generation document);
  match B.Document.entries document with
  | [ entry ] ->
    assert (P.Id.Prompt_definition.equal definition (B.Entry.definition_id entry));
    assert (P.Id.Prompt_revision.equal revision (B.Entry.revision_id entry))
  | _ -> failwith "saved index did not retain the actual selected ready revision"
;;

let nested_contents env revision =
  Agent_session.Runtime_script_preparation.with_revision
    Daemon.default_options.script_preparation
    revision
    ~env
    ~invocation:Ui
    ~checkpoint:ignore
    ~f:(fun _ ~compilation_limits:_ ~programs ->
      let programs = Option.value_exn programs in
      let sources =
        Chatml_prompt_programs.Restored.programs programs
        |> List.concat_map ~f:(fun program ->
          Chatml_prepared_program.sources
            (Chatml_prompt_programs.Restored.Program.prepared program))
      in
      let source =
        List.find_exn sources ~f:(fun source ->
          let id = Chatml_source.id source in
          String.equal (Chatml_source.Id.package_namespace id) "inner"
          && String.equal (Chatml_source.Id.relative_path id) "code.chatml")
      in
      Ok (Chatml_source.contents source))
  |> ok
;;

let%expect_test "explicit daemon updates use installed config and retain old session code"
  =
  with_workspace (fun env root ->
    Eio.Switch.run (fun sw ->
      let helpers = Native_tests.helpers root in
      let inner_anchor = Eio.Path.(root / "inner-origin") in
      let outer_anchor = Eio.Path.(root / "outer-origin") in
      List.iter [ inner_anchor; outer_anchor ] ~f:(Eio.Path.mkdir ~perm:0o700);
      let inner_source, _, _, _ = Git_fixture.fixture env inner_anchor Sha1 in
      let outer_source, _, _, _ = Git_fixture.fixture env outer_anchor Sha1 in
      let inner_requests = ref 0 in
      let outer_requests = ref 0 in
      let serve source requests =
        Https_fixture.start env#net ~sw ~handle:(fun request ->
          incr requests;
          Git_fixture.smart_git_response env source request)
        ^ "/repo"
      in
      let inner_url = serve inner_source inner_requests in
      let outer_url = serve outer_source outer_requests in
      let inner_old = "let value = 41\n" in
      let inner_new = "let value = 42\n" in
      let inner_pin = commit_project env inner_source inner_manifest inner_old in
      let outer_body = "let value = 7\n" in
      let outer_pin =
        commit_project env outer_source (outer_manifest inner_url inner_pin) outer_body
      in
      save root "ca.pem" Tls_fixture.certificate;
      save
        root
        "root.chatmd"
        {|<script id="lookup" language="chatml" kind="tool" src="lookup.chatml" project="chatml.project.json"/>|};
      save
        root
        "lookup.chatml"
        {|let run = fun ctx input -> Task.pure(`Complete(`String("captured")))|};
      save root "chatml.project.json" (root_manifest outer_url outer_pin);
      let text = config_text root helpers outer_url inner_url in
      save root "server.sexp" text;
      let config =
        Agent_server.Config_parser.parse_string
          ~source_file:(Eio.Path.native_exn Eio.Path.(root / "server.sexp"))
          text
        |> Support.ok
        |> Agent_server.Config_validator.validate ~env
        |> Support.ok
      in
      let options =
        { Daemon.default_options with
          startup_mode = On_demand
        ; inference_policy =
            Agent_server_test_support.inference_policy
              ~default_model:"fixture-model"
              ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected model dispatch")
        }
      in
      let daemon =
        Daemon.start
          ~sw
          ~env
          ~config
          ~tool_dir:(Eio.Path.native_exn root)
          ~home:(Eio.Path.native_exn root)
          ~options
          ~process_start_identity:None
          ()
        |> ok
      in
      Fun.protect
        ~finally:(fun () -> Daemon.shutdown daemon |> ok)
        (fun () ->
           let current = ref true in
           let drop_update_reply = ref false in
           let connection =
             connect
               ~drop_update_reply
               daemon
               (Agent_server_test_support.principal ())
               current
           in
           let old = ready daemon in
           [%test_eq: string] inner_old (nested_contents env old);
           let session = create_stopped_session connection in
           assert (
             P.Id.Prompt_revision.equal
               (session_revision daemon session)
               (Revision.id old));
           assert (!inner_requests > 0 && !outer_requests > 0);
           let index =
             Eio.Path.(root / "data" / "indexes" / "script-project-baselines.json")
           in
           let network () = !outer_requests, !inner_requests in
           let before_network = network () in
           let saved = Eio.Path.load index in
           let installed_generation = generation connection in
           [%test_eq: int] 1 (D.Generation.to_int installed_generation);
           check_saved index 1 (Revision.id old);
           let inner_pin = commit_project env inner_source inner_manifest inner_new in
           let outer_pin =
             commit_project
               env
               outer_source
               (outer_manifest inner_url inner_pin)
               outer_body
           in
           save root "chatml.project.json" (root_manifest outer_url outer_pin);
           let assert_no_acquisition () =
             [%test_eq: int * int] before_network (network ());
             [%test_eq: string] saved (Eio.Path.load index);
             assert (
               P.Id.Prompt_revision.equal (Revision.id old) (Revision.id (ready daemon)))
           in
           let unauthorized =
             Agent_server_test_support.principal_with_scopes
               "pri_denied"
               (P.Scope.Set.of_list [ List_prompts ])
           in
           let denied = connect daemon unauthorized (ref true) in
           Agent_client.Admin.update_dependencies
             denied
             (request
                ~generation:installed_generation
                ~definitions:[ definition ]
                "denied")
           |> assert_rejected Permission_denied;
           assert_no_acquisition ();
           List.iter
             [ D.Generation.of_int 0 |> ok, [ definition ], "stale"
             ; ( installed_generation
               , [ P.Id.Prompt_definition.of_string "prd_unknown" |> ok ]
               , "unknown" )
             ; installed_generation, [ disabled ], "disabled"
             ]
             ~f:(fun (generation, definitions, key) ->
               Agent_client.Admin.update_dependencies
                 connection
                 (request ~generation ~definitions key)
               |> assert_rejected Invalid_state;
               assert (List.is_empty (Agent_client.Connection.pending_commands connection));
               assert_no_acquisition ());
           print_endline
             "unauthorized, stale, unknown and disabled: unchanged index/catalog; no \
              HTTPS acquisition";
           ignore (Daemon.reload_config daemon |> Support.ok : Agent_server.Config_diff.t);
           [%test_eq: int * int] before_network (network ());
           (match Catalog.find (Daemon.prompts daemon) definition with
            | Some { availability = Unavailable _; _ } -> ()
            | _ -> failwith "ordinary Build reload adopted a changed dependency selection");
           let selected_generation = generation connection in
           check_saved index (D.Generation.to_int selected_generation) (Revision.id old);
           let staged = "(staged configuration is deliberately invalid" in
           save root "server.sexp" staged;
           let update =
             request
               ~generation:selected_generation
               ~definitions:[ definition ]
               "explicit-update"
           in
           assert (List.is_empty (Agent_client.Connection.pending_commands connection));
           drop_update_reply := true;
           Agent_client.Admin.update_dependencies connection update
           |> assert_rejected Interrupted;
           let delivered_network = network () in
           let pending =
             match Agent_client.Connection.pending_commands connection with
             | [ pending ] -> pending
             | _ -> failwith "lost real update reply did not retain one exact intent"
           in
           let result =
             match Agent_client.Connection.reconcile connection pending |> ok with
             | P.Command_receipt.Committed (Prompt_dependencies_updated result) -> result
             | _ -> failwith "real daemon receipt did not resolve the committed update"
           in
           assert (List.is_empty (Agent_client.Connection.pending_commands connection));
           assert (D.Update_result.matches_request result update);
           let replay = Agent_client.Admin.update_dependencies connection update |> ok in
           assert (D.Update_result.equal result replay);
           [%test_eq: int * int] delivered_network (network ());
           let fresh = ready daemon in
           [%test_eq: string] inner_new (nested_contents env fresh);
           [%test_eq: string] inner_old (nested_contents env old);
           check_saved
             index
             (D.Generation.to_int (D.Update_result.generation result))
             (Revision.id fresh);
           assert (not (P.Id.Prompt_revision.equal (Revision.id old) (Revision.id fresh)));
           (match D.Update_result.targets result with
            | [ { definition_id; outcome = Ready revision } ] ->
              assert (P.Id.Prompt_definition.equal definition_id definition);
              assert (P.Id.Prompt_revision.equal revision (Revision.id fresh))
            | _ -> failwith "explicit update did not report its actual ready revision");
           assert (
             !outer_requests > fst before_network && !inner_requests > snd before_network);
           [%test_eq: string] staged (Eio.Path.load Eio.Path.(root / "server.sexp"));
           assert (not (String.equal saved (Eio.Path.load index)));
           assert (
             P.Id.Prompt_revision.equal
               (session_revision daemon session)
               (Revision.id old));
           let artifacts =
             Agent_store.Prompt_artifact_store.open_existing
               ~env
               ~root:
                 (Agent_store.Data_root.prompt_artifacts_path
                    (Agent_store.Session_store.data_root (Daemon.store daemon)))
             |> store_ok
           in
           let retained =
             Agent_store.Prompt_artifact_store.load artifacts (Revision.id old)
             |> store_ok
           in
           assert (
             Agent_store.Prompt_artifact_store.Artifact.same_content
               retained
               (Revision.artifact old));
           let updated_network = network () in
           current := false;
           Agent_client.Admin.update_dependencies connection update
           |> assert_rejected Permission_denied;
           let receipt =
             P.Command_receipt.Request.
               { method_name = "prompt.dependencies.update"
               ; original_params = D.Update_request.to_json update
               }
           in
           Agent_client.Connection.request_without_history
             connection
             (Command_receipt receipt)
           |> assert_rejected Permission_denied;
           [%test_eq: int * int] updated_network (network ());
           print_endline
             "ordinary reload: locked selection; explicit update: both full pins \
              captured from installed config";
           print_endline
             "lost real reply: committed receipt reconciled; exact replay makes no HTTPS \
              request";
           print_endline
             "old session/artifact retained; revoked original actor cannot replay or \
              read receipt")));
  [%expect
    {|
    unauthorized, stale, unknown and disabled: unchanged index/catalog; no HTTPS acquisition
    ordinary reload: locked selection; explicit update: both full pins captured from installed config
    lost real reply: committed receipt reconciled; exact replay makes no HTTPS request
    old session/artifact retained; revoked original actor cannot replay or read receipt
    |}]
;;
