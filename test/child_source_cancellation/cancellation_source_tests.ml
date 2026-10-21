open Core
open Support
module P = Agent_protocol
module S = Agent_session
module Server = Agent_server
module F = Agent_server_test_support
module Factory = Server.Session_factory
module Actor = S.Session_actor
module C = Chat_response.Tool_capability

let%expect_test "File cancellation before reservation preserves source and Factory reuse" =
  with_workspace (fun env home root ->
    save
      root
      "root.chatmd"
      {|<developer>cancellable parent</developer>
<tool name="child" agent="child.chatmd" local persistence="optional"/>|};
    save root "child.chatmd" "<developer>cancellable child</developer>";
    Eio.Switch.run (fun sw ->
      let providers = ref 0 in
      let provider_started, provider_started_u = Eio.Promise.create () in
      let inference =
        F.inference_policy
          ~default_model:"fixture-model"
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            incr providers;
            Eio.Promise.resolve provider_started_u ();
            Stdlib.Seq.empty)
      in
      let options =
        { Server.Daemon.default_options with
          startup_mode = Operator_only
        ; inference_policy = inference
        }
      in
      let configuration =
        F.config
          (Eio.Path.native_exn home)
          (Eio.Path.native_exn root)
          (Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
      in
      let daemon =
        Server.Daemon.start
          ~sw
          ~env
          ~config:configuration
          ~tool_dir:(Eio.Path.native_exn root)
          ~home:(Eio.Path.native_exn home)
          ~process_start_identity:None
          ~options
          ()
        |> F.protocol_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () -> Server.Daemon.shutdown daemon |> F.protocol_ok))
        ~f:(fun () ->
          let store = Server.Daemon.store daemon in
          let data = Agent_store.Session_store.data_root store in
          let policy =
            Server.Project_source_policy.configure
              Server.Project_source_policy.default
              Server.Project_source_configuration.local_only
            |> Or_error.ok_exn
            |> Server.Project_source_policy.Configured.source_preparation
          in
          let cancel = ref (fun () -> ()) in
          let preparations = ref 0 in
          let source =
            S.Captured_agent_runner.Source.create
              ~policy_revision:
                (Chatml_source.Digest.of_contents
                   "fixture.cancel-source.v1"
                   ~checkpoint:ignore)
              ~compilation:(S.Prompt_script_preparation.Policy.compilation policy)
              ~parser_limits:(S.Prompt_revision_builder.capture_limits ())
              ~max_work_steps:(S.Prompt_script_preparation.Policy.max_work_steps policy)
              ~preprocess:Fn.id
              ~prepare_programs:(fun startup ~tool_contract:_ ~invocation scope capture ->
                incr preparations;
                !cancel ();
                S.Prompt_script_preparation.capture_child
                  policy
                  ~startup
                  ~invocation
                  ~host:{ additional_anchors = []; git_store = None; git_grants = [] }
                  ~scope
                  ~capture
                  ~operation:(fun _ ->
                    Chatml_project_build.Operation.Build_without_previous)
                  ~provided:[]
                  ~env
                  ~checkpoint:(fun () ->
                    Chat_response.Startup_compilation.checkpoint startup)
                  ()
                |> Result.map ~f:S.Prompt_script_preparation.Prepared.programs)
            |> Or_error.ok_exn
          in
          let permission =
            S.Permission_policy.create
              ~id:F.permission_profile.id
              ~tool_default:Allow
              ~approval_timeout_ms:None
              ~fallback:Fallback_deny
              ~manifest_authorization:Assume_authorized
              ~evaluator:None
              ~evaluator_revision:None
              ~reviewer:None
            |> F.protocol_ok
          in
          let factory =
            Factory.create
              ~sw
              ~env
              ~store
              ~registry:(Server.Daemon.registry daemon)
              ~idempotency_store:
                (Agent_store.Idempotency_store.open_or_create
                   ~env
                   ~path:
                     (Filename.concat
                        (Agent_store.Data_root.indexes_path data)
                        "idempotency.sexp")
                 |> ok)
              ~blob_store:(Server.Daemon.blob_store daemon)
              ~prompts:(Server.Daemon.prompts daemon)
              ~workspaces:(Server.Daemon.workspaces daemon)
              ~permission_profiles:[ permission ]
              ~manifest_grants:[]
              ~quota_manager:
                (S.Quota_manager.create
                   ~limits:options.quota_limits
                   ~workspace_leases:(S.Workspace_lease.create ())
                 |> F.protocol_ok)
              ~job_capacity:
                (Server.Job_capacity.create ~limits:configuration.server.job_limits)
              ~tool_dir:(Eio.Path.native_exn root)
              ~home:(Some (Eio.Path.native_exn home))
              ~inference_policy:inference
              ~qualify_chatml_extensions:true
              ~session_helpers:[]
              ~independent_lifetime_policy:None
              ~chatml_runtime_policy:options.chatml_runtime_policy
              ~authoring_validation_host:options.authoring_validation_host
              ~script_preparation:options.script_preparation
              ~content_limits:options.content_limits
              ~child_sources:
                (Some
                   (Factory.Child_source_policy.create
                      ~source
                      ~runtime_policy_revision:
                        (Chatml_source.Digest.of_contents
                           "fixture.cancel-runtime.v1"
                           ~checkpoint:ignore)))
              ~durability:Agent_store.Journal_segment.Flush
              ~limits:options.factory_limits
          in
          let parent =
            Factory.create_session
              factory
              ~command_audit:None
              ~principal:(F.principal ())
              (F.create_request ())
            |> F.protocol_ok
          in
          let initial = Actor.state parent.actor |> F.protocol_ok in
          Server.Session_registry.add
            (Server.Daemon.registry daemon)
            ~session_id:initial.identity.session_id
            parent
          |> F.protocol_ok;
          let attachment, _ =
            Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> F.protocol_ok
          in
          Actor.start parent.actor ~attachment_id:attachment.id |> F.protocol_ok |> ignore;
          let current = Actor.state parent.actor |> F.protocol_ok in
          let now = current.identity.created_at in
          let deadline = P.Timestamp.of_string "2099-01-01T00:00:00Z" |> F.protocol_ok in
          let job : P.Job.t =
            { id = P.Id.Job.create ()
            ; session_id = current.identity.session_id
            ; generation = current.identity.generation
            ; kind = Async_tool
            ; payload = `Object []
            ; status = Queued
            ; retry_policy = Never
            ; attempt = 0
            ; created_at = now
            ; started_at = None
            ; next_run_at = None
            ; completed_at = None
            ; result = None
            ; delivery = Not_required
            ; launch = None
            ; progress = None
            }
          in
          Actor.add_job parent.actor job |> F.protocol_ok |> ignore;
          let job =
            Actor.claim_job parent.actor ~job_id:job.id ~generation:job.generation
            |> F.protocol_ok
            |> Option.value_exn
          in
          let ledger () =
            Agent_store.Delegation_store.with_records
              (Agent_store.Session_store.delegations store)
              ~max_records:8
              ~max_bytes:1048576
              ~f:(fun records -> Ok records)
            |> ok
          in
          let invoke () =
            Server.Runtime_owner.with_background_runtime parent.runtime (fun runtime ->
              let native = Option.value_exn runtime.native_runtime in
              let registry =
                Lazy.force native.capabilities
                |> Result.map_error ~f:(fun error -> error.C.message)
                |> Result.ok_or_failwith
              in
              let reference =
                C.find registry ~name:"child"
                |> Result.map_error ~f:(fun error -> error.C.message)
                |> Result.ok_or_failwith
                |> C.reference
              in
              let invocation =
                P.Invocation.create
                  { id = P.Id.Invocation.create ()
                  ; session_id = current.identity.session_id
                  ; generation = current.identity.generation
                  ; origin = Script
                  ; provider_call_id = None
                  ; call_entry_id = None
                  ; parent_invocation = None
                  ; parent_job = Some job.id
                  ; tool_name = reference.name
                  ; implementation_revision = reference.implementation_revision
                  ; capability_fingerprint = C.fingerprint registry
                  ; input =
                      `Object
                        [ "input", `String "retain this request"
                        ; "mode", `String "persistent"
                        ]
                  ; created_at = now
                  ; deadline = Some deadline
                  }
                |> F.protocol_ok
              in
              Actor.with_job_invocations
                parent.actor
                ~job_id:job.id
                ~generation:job.generation
                ~attempt:job.attempt
                ~deadline:(Some deadline)
                (fun ~job:_ ~execute ->
                   S.Native_tool_invocation.run_scoped
                     ~execute
                     ~registry:(fun () -> registry)
                     ~reference
                     ~invocation
                     ~is_halted:(fun () -> false)
                     ~authorize:(fun _ _ -> Ok ())
                     ~prepare_output:(function
                       | Text text -> Ok (`String text)
                       | _ -> assert false)))
          in
          let sentinel = Failure "pre-reservation source cancellation" in
          (match
             Eio.Cancel.sub (fun context ->
               (cancel := fun () -> Eio.Cancel.cancel context sentinel);
               invoke ())
           with
           | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason sentinel)
           | result ->
             raise_s
               [%sexp
                 "cancel fixture returned before callback"
               , (result : (P.Invocation.t, P.Error.t) Result.t)
               , (!preparations : int)]);
          [%test_eq: int] !preparations 1;
          assert (List.is_empty (ledger ()));
          [%test_eq: int] (List.length (Agent_store.Session_store.list_sessions store)) 1;
          [%test_eq: int] !providers 0;
          cancel := ignore;
          let resolved = invoke () |> F.protocol_ok in
          (match resolved.status with
           | Resolved (Complete _) -> ()
           | _ -> raise_s [%sexp (resolved : P.Invocation.t)]);
          [%test_eq: int] !preparations 2;
          [%test_eq: int] (List.length (ledger ())) 1;
          [%test_eq: int] (List.length (Agent_store.Session_store.list_sessions store)) 2;
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            Eio.Promise.await provider_started);
          [%test_eq: int] !providers 1;
          print_endline
            "original cancellation; no reservation/child/provider; same Factory and \
             source reused")));
  [%expect
    {|original cancellation; no reservation/child/provider; same Factory and source reused|}]
;;
