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

(* Cleanup cannot replace the original assertion, cancellation or selected
   writer failure. A successful body still has to complete cleanup successfully. *)
let protect_cleanup ~finally f =
  match f () with
  | value ->
    Eio.Cancel.protect finally;
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let with_max_root_agents (config : Agent_server.Config.t) ~max_root_agents =
  { config with
    workspaces =
      List.map config.workspaces ~f:(fun workspace ->
        { workspace with
          Agent_server.Config.Workspace.prompt_limits =
            List.map workspace.prompt_limits ~f:(fun limit ->
              { limit with Agent_server.Config.Workspace.max_root_agents })
        })
  }
;;

let with_daemon
      ?(prompt_text = "<developer>Pinned source during creation.</developer>")
      ?(schemas = false)
      ?(sources = [])
      ?(qualify_chatml_extensions = false)
      ?(profile = permission_profile)
      ?(tool_contract = Chatml_compilation.Tool_contract.Standard)
      ?(prepare_sources = fun _ _ -> ())
      ?(wrap_env = Fn.id)
      ?(max_root_agents = 1)
      ~policy
      f
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    let path = Eio.Path.(env#fs / root) in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree path)
      (fun () ->
         let prompt = Filename.concat root "parent.chatmd" in
         Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / prompt) prompt_text;
         List.iter sources ~f:(fun (name, source) ->
           Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / root / name) source);
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
         prepare_sources env root;
         Eio.Switch.run (fun sw ->
           let daemon =
             Daemon.start
               ~sw
               ~env:(wrap_env (env :> Eio_unix.Stdenv.base))
               ~config:
                 (let config =
                    with_max_root_agents
                      (config ~profile root root prompt)
                      ~max_root_agents
                  in
                  { config with
                    prompts =
                      List.map config.prompts ~f:(fun entry ->
                        { entry with tool_contract })
                  })
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options:
                 { Daemon.default_options with
                   inference_policy = policy
                 ; qualify_chatml_extensions
                 }
               ()
             |> protocol_ok
           in
           protect_cleanup
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
          raise_s
            [%sexp
              "retained developer graph did not restore", (count : int), (restored : bool)]
        | Error error ->
          raise_s [%sexp "retained developer restore", (error : Agent_protocol.Error.t)]);
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

let%test_unit "legacy Factory startup owns native calls and saves its emitted suffix" =
  let module P = Agent_protocol in
  let module Owner = Agent_server.Runtime_owner in
  let module Checkpoint = Agent_session.Moderator_checkpoint in
  let module Queue = Agent_session.Moderator_queue in
  let prompt_text =
    {|<developer>Offline constructor callback fixture.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<tool name="append_to_file"/>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Session_start ->
  Task.bind(Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("startup.txt")}])), fun result ->
    match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) ->
      Task.bind(Runtime.emit(`String("constructor suffix")), fun ignored ->
        Task.bind(Schedule.after_ms(60000, `String("constructor timer")), fun timer ->
          Task.bind(Ui.notify("constructor notification"), fun ignored ->
            Task.pure(state + 1)))))
| `Session_resume ->
  Task.bind(Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("resume.txt")}])), fun result ->
    match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) ->
      Task.bind(Tool.call("append_to_file", `Object([{key = "path"; value = `String("prefix-marker.txt")}, {key = "content"; value = `String("fresh-resume")}])), fun appended ->
        match appended with
        | `Error(code) -> Task.fail(code)
        | `Ok(value) ->
          Task.bind(Runtime.emit(`String("resume suffix")), fun ignored ->
            Task.pure(state + 1))))
| `String("constructor suffix") ->
  Task.bind(Tool.call("append_to_file", `Object([{key = "path"; value = `String("prefix-marker.txt")}, {key = "content"; value = `String("forbidden-old-head")}])), fun result ->
    match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) -> Task.pure(state + 1))
| _ -> Task.pure(state)
</script>|}
  in
  List.iter [ false; true ] ~f:(fun qualify_chatml_extensions ->
    let model_calls = ref 0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
        incr model_calls;
        failwith "constructor fixture must not dispatch a provider")
    in
    with_daemon ~prompt_text ~qualify_chatml_extensions ~policy (fun env _ root daemon ->
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(env#fs / root / "startup.txt")
        "actual startup native result";
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(env#fs / root / "resume.txt")
        "actual cold resume native result";
      let marker = Eio.Path.(env#fs / root / "prefix-marker.txt") in
      Eio.Path.save ~create:(`Exclusive 0o600) marker "sentinel";
      let entry =
        Factory.create_session
          (Daemon.factory daemon)
          ~command_audit:None
          ~principal:(principal ())
          (create_request ())
        |> protocol_ok
      in
      let closed = ref false in
      let close () =
        if not !closed
        then (
          entry.close ();
          closed := true)
      in
      Fun.protect ~finally:close (fun () ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
          let state = Actor.state entry.actor |> protocol_ok in
          assert (
            Option.is_none
              (Agent_server.Session_registry.find
                 (Daemon.registry daemon)
                 state.identity.session_id));
          assert (
            Agent_session.Session_state.Runtime_initialization.equal
              state.runtime_initialization
              Ready);
          [%test_eq: P.Session.desired_state] state.lifecycle.desired Stopped;
          assert (Option.is_none state.active_operation);
          assert (Option.is_none state.failure);
          let receipt =
            match state.moderator_executions with
            | [ receipt ] -> receipt
            | receipts ->
              raise_s
                [%sexp
                  "expected one constructor receipt"
                , (receipts : P.Moderator_execution.t list)]
          in
          assert (P.Moderator_execution.equal_phase receipt.context.phase Session_start);
          let events =
            match
              Agent_session.Durable_event_log.replay
                entry.durable_events
                ~after_sequence:0L
                ~through_sequence:state.counters.event_sequence
            with
            | Available events -> events
            | Snapshot_required ->
              failwith "constructor events exceeded the replay window"
          in
          let notifications =
            List.filter_map events ~f:(fun event ->
              match
                P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                |> protocol_ok
              with
              | Moderator_notification payload -> Some payload
              | _ -> None)
          in
          assert (
            List.equal
              Jsonaf.exactly_equal
              notifications
              [ `Object
                  [ "message", `String "constructor notification"
                  ; ( "moderator_execution_id"
                    , P.Id.Moderator_execution.to_json receipt.context.id )
                  ]
              ]);
          (match receipt.status with
           | Completed _ -> ()
           | Running | Failed _ | Interrupted _ ->
             raise_s
               [%sexp "constructor did not complete", (receipt : P.Moderator_execution.t)]);
          let invocation = List.hd_exn state.invocations in
          [%test_eq: int] (List.length state.invocations) 1;
          [%test_eq: P.Id.Moderator_execution.t option]
            invocation.parent_event
            (Some receipt.context.id);
          let observation = Option.value_exn invocation.observation in
          assert (P.Invocation.equal_observer observation.observer receipt.context.source);
          (match invocation.status with
           | Resolved (Complete result) ->
             assert (
               String.is_substring
                 (Jsonaf.to_string result)
                 ~substring:"actual startup native result")
           | Admitted | Dispatching | Published _
           | Resolved (Pending _ | Fail _ | Cancelled _) ->
             raise_s [%sexp "unexpected native outcome", (invocation : P.Invocation.t)]);
          let snapshot =
            Checkpoint.decode state.moderator |> protocol_ok |> Option.value_exn
          in
          assert (
            Jsonaf.exactly_equal
              (Session.Snapshot.to_jsonaf snapshot.current_state)
              (Session.Snapshot.to_jsonaf (Int 1)));
          assert (
            List.equal
              (fun left right ->
                 Jsonaf.exactly_equal
                   (Session.Snapshot.to_jsonaf left)
                   (Session.Snapshot.to_jsonaf right))
              snapshot.queued_internal_events
              [ Variant ("String", [ String "constructor suffix" ]) ]);
          let queue =
            Checkpoint.queue state.moderator |> protocol_ok |> Option.value_exn
          in
          [%test_eq: int64] (Queue.head_ordinal queue) 0L;
          [%test_eq: int64] (Queue.next_ordinal queue) 1L;
          let occurrence = List.hd_exn (Queue.entries queue) in
          assert (
            Option.equal
              Agent_session.Queue_producer.equal
              (Queue.Entry.producer occurrence)
              (Some (Event receipt.context.id)));
          assert (
            Option.equal
              P.Inference_origin.equal
              (Queue.Entry.origin occurrence)
              (Some Unbound));
          assert (
            P.Invocation.equal_observer
              (Queue.source queue).observer
              receipt.context.source);
          let timer = List.hd_exn state.schedules in
          [%test_eq: int] (List.length state.schedules) 1;
          (match timer.status with
           | Scheduled -> ()
           | Delivering | Delivered | Cancelled | Failed _ ->
             raise_s [%sexp "unexpected constructor timer status", (timer : P.Schedule.t)]);
          let provenance = Option.value_exn timer.inference_provenance in
          assert (
            P.Run_source.equal
              (P.Schedule_provenance.source provenance)
              (Queue.source queue));
          assert (
            Option.equal
              P.Inference_origin.equal
              (P.Schedule_provenance.origin provenance)
              (Queue.Entry.origin occurrence));
          [%test_eq: int] !model_calls 0;
          assert (not (Owner.is_loaded entry.runtime));
          let resume_live = Atomic.make true in
          let resume_original =
            Operator_authorization.guarded
              ~principal:(principal ())
              ~is_current:(fun () -> Atomic.get resume_live)
          in
          (match
             Owner.ensure_loaded_authenticated entry.runtime ~actor:resume_original
           with
           | Ok () -> ()
           | Error failure ->
             let failed = Actor.state entry.actor |> protocol_ok in
             let calls =
               List.filter failed.invocations ~f:(fun invocation ->
                 String.equal invocation.context.tool_name "append_to_file")
             in
             raise_s
               [%sexp
                 "cold Resume failed"
               , (failure : P.Error.t)
               , (calls : P.Invocation.t list)]);
          assert (Owner.is_loaded entry.runtime);
          let resumed = Actor.state entry.actor |> protocol_ok in
          [%test_eq: P.Session.desired_state] resumed.lifecycle.desired Stopped;
          assert (
            Agent_session.Session_state.Runtime_initialization.equal
              resumed.runtime_initialization
              Ready);
          assert (Option.is_none resumed.failure);
          let resume_receipt =
            match
              List.filter resumed.moderator_executions ~f:(fun candidate ->
                not
                  (P.Id.Moderator_execution.equal candidate.context.id receipt.context.id))
            with
            | [ receipt ] -> receipt
            | receipts ->
              raise_s
                [%sexp
                  "expected one cold Resume receipt"
                , (receipts : P.Moderator_execution.t list)]
          in
          [%test_eq: int] (List.length resumed.moderator_executions) 2;
          assert (
            List.exists
              resumed.moderator_executions
              ~f:(P.Moderator_execution.equal receipt));
          assert (
            P.Moderator_execution.equal_phase resume_receipt.context.phase Session_resume);
          assert (
            P.Invocation.equal_observer
              resume_receipt.context.source
              receipt.context.source);
          assert (Option.is_none resume_receipt.context.operation_id);
          assert (Option.is_none resumed.active_operation);
          (match resume_receipt.status with
           | Completed _ -> ()
           | Running | Failed _ | Interrupted _ ->
             raise_s
               [%sexp
                 "cold Resume did not complete"
               , (resume_receipt : P.Moderator_execution.t)]);
          let resumed_native =
            match
              List.filter resumed.invocations ~f:(fun candidate ->
                Option.equal
                  P.Id.Moderator_execution.equal
                  candidate.parent_event
                  (Some resume_receipt.context.id)
                && String.equal candidate.context.tool_name "read_file")
            with
            | [ invocation ] -> invocation
            | invocations ->
              raise_s
                [%sexp
                  "expected one cold Resume native call"
                , (invocations : P.Invocation.t list)]
          in
          [%test_eq: int] 3 (List.length resumed.invocations);
          let resumed_append =
            match
              List.filter resumed.invocations ~f:(fun candidate ->
                Option.equal
                  P.Id.Moderator_execution.equal
                  candidate.parent_event
                  (Some resume_receipt.context.id)
                && String.equal candidate.context.tool_name "append_to_file")
            with
            | [ invocation ] -> invocation
            | invocations ->
              raise_s
                [%sexp
                  "expected one cold Resume append", (invocations : P.Invocation.t list)]
          in
          (match resumed_append.status with
           | Resolved (Complete _) -> ()
           | Admitted | Dispatching | Published _
           | Resolved (Pending _ | Fail _ | Cancelled _) ->
             raise_s [%sexp (resumed_append : P.Invocation.t)]);
          [%test_eq: string] "sentinel\nfresh-resume" (Eio.Path.load marker);
          assert (List.exists resumed.invocations ~f:(P.Invocation.equal invocation));
          [%test_eq: P.Id.Moderator_execution.t option]
            resumed_native.parent_event
            (Some resume_receipt.context.id);
          (match resumed_native.status with
           | Resolved (Complete result) ->
             assert (
               String.is_substring
                 (Jsonaf.to_string result)
                 ~substring:"actual cold resume native result")
           | Admitted | Dispatching | Published _
           | Resolved (Pending _ | Fail _ | Cancelled _) ->
             raise_s
               [%sexp
                 "unexpected cold Resume native outcome"
               , (resumed_native : P.Invocation.t)]);
          let resumed_snapshot =
            Checkpoint.decode resumed.moderator |> protocol_ok |> Option.value_exn
          in
          assert (
            Jsonaf.exactly_equal
              (Session.Snapshot.to_jsonaf resumed_snapshot.current_state)
              (Session.Snapshot.to_jsonaf (Int 2)));
          assert (
            List.equal
              (fun left right ->
                 Jsonaf.exactly_equal
                   (Session.Snapshot.to_jsonaf left)
                   (Session.Snapshot.to_jsonaf right))
              resumed_snapshot.queued_internal_events
              [ Variant ("String", [ String "constructor suffix" ])
              ; Variant ("String", [ String "resume suffix" ])
              ]);
          let resumed_queue =
            Checkpoint.queue resumed.moderator |> protocol_ok |> Option.value_exn
          in
          assert (P.Run_source.equal (Queue.source queue) (Queue.source resumed_queue));
          [%test_eq: int64] (Queue.head_ordinal resumed_queue) (Queue.head_ordinal queue);
          [%test_eq: int64] (Queue.next_ordinal resumed_queue) 2L;
          let raw_entries checkpoint =
            let raw = Option.value_exn checkpoint in
            let queue = Jsonaf.member "queue_provenance" raw |> Option.value_exn in
            match Jsonaf.member "entries" queue with
            | Some (`Array entries) -> entries
            | _ -> failwith "expected actual raw queue entries"
          in
          assert (
            Jsonaf.exactly_equal
              (List.hd_exn (raw_entries state.moderator))
              (List.hd_exn (raw_entries resumed.moderator)));
          (match Queue.entries resumed_queue with
           | [ old; fresh ] ->
             assert (Queue.Entry.equal old occurrence);
             [%test_eq: int64] (Queue.Entry.ordinal fresh) 1L;
             assert (
               Option.equal
                 Agent_session.Queue_producer.equal
                 (Queue.Entry.producer fresh)
                 (Some (Event resume_receipt.context.id)));
             assert (
               Option.equal
                 P.Inference_origin.equal
                 (Queue.Entry.origin fresh)
                 (Some Unbound))
           | _ -> failwith "cold Resume changed the queued prefix");
          assert (
            List.equal
              (fun left right ->
                 Jsonaf.exactly_equal (P.Schedule.to_json left) (P.Schedule.to_json right))
              state.schedules
              resumed.schedules);
          assert (Operator_authorization.is_current resume_original);
          Owner.ensure_loaded_authenticated entry.runtime ~actor:resume_original
          |> protocol_ok;
          let repeated = Actor.state entry.actor |> protocol_ok in
          (match
             Agent_session.Session_state_document.equal_values
               resumed
               ~limits:Document_schema.Limits.default
               repeated
           with
           | Ok true -> ()
           | Ok false -> failwith "already loaded runtime replayed cold construction"
           | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]);
          let writer, _ =
            Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          Actor.start ~actor:resume_original entry.actor ~attachment_id:writer.id
          |> protocol_ok
          |> ignore;
          let rec await_idle () =
            let current = Actor.state entry.actor |> protocol_ok in
            match
              current.active_operation, current.lifecycle.observed, current.failure
            with
            | None, Idle, None -> current
            | _, _, Some failure -> raise_s [%sexp (failure : P.Error.t)]
            | _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              await_idle ()
          in
          let running = await_idle () in
          [%test_eq: P.Session.desired_state] Running running.lifecycle.desired;
          (* A fresh constructor can use this exact tool, but reopening does not
             lend its authority to the retained queue prefix. *)
          (match Owner.drain_idle_moderator entry.runtime with
           | Error failure ->
             [%test_eq: P.Error.code] Invalid_state failure.code;
             assert (
               String.is_substring
                 failure.message
                 ~substring:"invocation.admission_failed")
           | Ok progress ->
             raise_s [%sexp "old prefix unexpectedly drained", (progress : bool)]);
          let state = Actor.state entry.actor |> protocol_ok in
          let failed =
            match
              List.filter state.moderator_executions ~f:(fun candidate ->
                not
                  (List.exists running.moderator_executions ~f:(fun previous ->
                     P.Id.Moderator_execution.equal
                       candidate.context.id
                       previous.context.id)))
            with
            | [ failed ] -> failed
            | receipts ->
              raise_s
                [%sexp
                  "expected one failed old queue head"
                , (receipts : P.Moderator_execution.t list)]
          in
          assert (P.Moderator_execution.equal_phase failed.context.phase Internal_event);
          assert (Option.is_none failed.context.operation_id);
          (match failed.status with
           | Failed failure -> [%test_eq: string] "event.handler_failed" failure.code
           | Running | Completed _ | Interrupted _ ->
             raise_s [%sexp (failed : P.Moderator_execution.t)]);
          let consumed = Option.value_exn failed.queue_occurrence in
          [%test_eq: int64]
            (Queue.head_ordinal resumed_queue)
            (P.Moderator_execution.Queue_occurrence.ordinal consumed);
          assert (
            P.Run_source.equal
              (Queue.source resumed_queue)
              (P.Moderator_execution.Queue_occurrence.source consumed));
          assert (P.Invocation.equal_observer failed.context.source receipt.context.source);
          assert (
            Option.equal
              P.Inference_origin.equal
              (P.Moderator_execution.Queue_occurrence.origin consumed)
              (Queue.Entry.origin occurrence));
          assert (Option.equal Jsonaf.exactly_equal repeated.moderator state.moderator);
          [%test_eq: string] "sentinel\nfresh-resume" (Eio.Path.load marker);
          [%test_eq: bool] false (Owner.drain_idle_moderator entry.runtime |> protocol_ok);
          let blocked = Actor.state entry.actor |> protocol_ok in
          assert (
            List.equal
              P.Moderator_execution.equal
              state.moderator_executions
              blocked.moderator_executions);
          assert (Option.equal Jsonaf.exactly_equal state.moderator blocked.moderator);
          [%test_eq: string] "sentinel\nfresh-resume" (Eio.Path.load marker);
          [%test_eq: int] !model_calls 0;
          let snapshot_directory =
            entry.store_handle
            |> Option.value_exn
            |> Agent_store.Session_store.Handle.snapshot_directory
          in
          (* Direct Factory admission leaves registration and owner closure to the
         caller. Close this exact entry to join its runtime and install the latest
         journal-backed checkpoint before inspecting CURRENT. *)
          close ();
          let installed =
            match
              Agent_store.Snapshot.load_current
                ~env
                ~directory:snapshot_directory
                ~max_payload_length:(64 * 1024 * 1024)
            with
            | Ok installed -> Option.value_exn installed
            | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
          in
          let saved =
            match
              Agent_session.Session_state_document.decode
                ~limits:Document_schema.Limits.default
                installed.snapshot.payload
            with
            | Ok saved -> Agent_session.Session_state_document.value saved
            | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
          in
          assert (Option.equal Jsonaf.exactly_equal saved.moderator state.moderator);
          assert (List.equal P.Invocation.equal saved.invocations state.invocations);
          assert (
            List.equal
              (fun left right ->
                 Jsonaf.exactly_equal (P.Schedule.to_json left) (P.Schedule.to_json right))
              saved.schedules
              state.schedules);
          [%test_eq: int] !model_calls 0))))
;;

let%test_unit
    "installed legacy Factory callbacks own foreground and idle Tool and Model calls"
  =
  let module P = Agent_protocol in
  let module Owner = Agent_server.Runtime_owner in
  let module Checkpoint = Agent_session.Moderator_checkpoint in
  let module Queue = Agent_session.Moderator_queue in
  let module Res = Openai.Responses in
  let module Mode = struct
    type t =
      | Ordinary
      | Named_run
  end
  in
  let prompt_text ~named_run =
    Printf.sprintf
      {|<developer>525_ROOT_PROMPT. Use the requested native tool, then answer.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<tool name="recipe_source" agent="recipe.chatmd" local/>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let named_run = %b
let read file = Tool.call("read_file", `Object([
  {key = "root"; value = `String("data")},
  {key = "file"; value = `String(file)}
]))
let phase state name file =
  let* native = read(file) in
  match native with
  | `Error(code) -> Task.fail(code)
  | `Ok(value) ->
    let* model = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("recipe.chatmd")},
      {key = "input"; value = `String(name)},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    (match model with
    | `Ok(value) -> Task.pure(state + 1)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message))
let on_event ctx state event = match event with
| `Session_start ->
  let* result = read("constructor.txt") in
  (match result with
  | `Ok(value) -> Task.pure(state + 1)
  | `Error(code) -> Task.fail(code))
| `Turn_start ->
  if state == 1 then
    let* next = phase(state, "525_PHASE_TURN", "turn.txt") in
    if named_run then Task.pure(next)
    else
      let* timer = Schedule.after_ms(0, `String("idle-queued")) in
      Task.pure(next)
  else Task.pure(state)
| `Pre_tool_call(p) ->
  if state == 2 then
    if p.name == "read_file" then phase(state, "525_PHASE_TOOL", "pretool.txt")
    else Task.pure(state)
  else Task.pure(state)
| `String("idle-queued") ->
  if state == 3 then phase(state, "525_PHASE_IDLE", "idle.txt")
  else Task.fail("idle callback did not follow the foreground")
| _ -> Task.pure(state)
</script>|}
      named_run
  in
  let answer ~id text =
    let message : Res.Output_message.t =
      { role = Assistant
      ; id
      ; status = "completed"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      ; phase = None
      ; _type = "message"
      }
    in
    let item = Res.Response_stream.Item.Output_message message in
    [ Res.Response_stream.Output_item_added
        { item; output_index = 0; type_ = "response.output_item.added" }
    ; Output_text_delta
        { item_id = id
        ; output_index = 0
        ; content_index = 0
        ; delta = text
        ; type_ = "response.output_text.delta"
        }
    ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
    ]
    |> Stdlib.List.to_seq
  in
  let root_tool () =
    let open Res.Response_stream in
    let args = {|{"root":"data","file":"root.txt"}|} in
    let item arguments status =
      Item.Function_call
        { name = "read_file"
        ; arguments
        ; call_id = "525-root-call"
        ; _type = "function_call"
        ; id = Some "525-root-tool-item"
        ; status
        }
    in
    [ Output_item_added
        { item = item "" None; output_index = 0; type_ = "response.output_item.added" }
    ; Function_call_arguments_done
        { arguments = args
        ; item_id = "525-root-tool-item"
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ; Output_item_done
        { item = item args (Some "completed")
        ; output_index = 0
        ; type_ = "response.output_item.done"
        }
    ]
    |> Stdlib.List.to_seq
  in
  let modes = [ false, Mode.Ordinary; true, Ordinary; false, Named_run ] in
  List.iter modes ~f:(fun (qualify_chatml_extensions, mode) ->
    let named_run =
      match mode with
      | Ordinary -> false
      | Named_run -> true
    in
    let admitted_jobs = ref [] in
    let constructor_live = Atomic.make true in
    let foreground_live = Atomic.make true in
    let original live =
      Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
        Atomic.get live)
    in
    let constructor = original constructor_live in
    let foreground = original foreground_live in
    let child_calls = Hashtbl.create (module String) in
    let root_calls = ref 0 in
    let calls phase = Option.value (Hashtbl.find child_calls phase) ~default:0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
        let transcript =
          `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
        in
        let child =
          List.exists inputs ~f:(fun item ->
            let json = Res.Item.jsonaf_of_t item in
            match Jsonaf.member "role" json with
            | Some (`String "developer") ->
              String.is_substring (Jsonaf.to_string json) ~substring:"525_CHILD_PROMPT"
            | _ -> false)
        in
        assert (not (Operator_authorization.is_current constructor));
        assert (Operator_authorization.is_current foreground);
        if child
        then (
          let phase =
            List.filter
              [ "525_PHASE_TURN"; "525_PHASE_TOOL"; "525_PHASE_IDLE" ]
              ~f:(fun phase -> String.is_substring transcript ~substring:phase)
            |> function
            | [ phase ] -> phase
            | phases ->
              raise_s [%sexp "unexpected child provider input", (phases : string list)]
          in
          Hashtbl.incr child_calls phase;
          answer ~id:(phase ^ "-answer") (phase ^ "-ok"))
        else (
          assert (String.is_substring transcript ~substring:"525_ROOT_REQUEST");
          incr root_calls;
          [%test_eq: int] 1 (calls "525_PHASE_TURN");
          match named_run, !root_calls with
          | true, 1 -> answer ~id:"525-root-answer" "525-root-completed"
          | false, 1 -> root_tool ()
          | false, 2 ->
            [%test_eq: int] 1 (calls "525_PHASE_TOOL");
            answer ~id:"525-root-answer" "525-root-completed"
          | _, count -> raise_s [%sexp "unexpected root provider count", (count : int)]))
    in
    let policy =
      if not named_run
      then policy
      else
        { policy with
          runtime_inference_ports =
            (fun actor ->
              Result.map
                (policy.runtime_inference_ports actor)
                ~f:(fun (upstream : Agent_server.Graph_tracking.Upstream.t) ->
                  { upstream with
                    on_admitted =
                      (fun ~scope ~accounting_id ->
                        let state = Actor.state actor |> protocol_ok in
                        let active_jobs =
                          List.filter state.jobs ~f:(fun job ->
                            P.Job.equal_kind job.kind Model_call
                            &&
                            match job.status with
                            | Running -> true
                            | _ -> false)
                          |> List.map ~f:(fun job -> job.id)
                        in
                        upstream.on_admitted ~scope ~accounting_id;
                        admitted_jobs
                        := (scope, accounting_id, active_jobs) :: !admitted_jobs)
                  }))
        }
    in
    with_daemon
      ~prompt_text:(prompt_text ~named_run)
      ~sources:
        [ ( "recipe.chatmd"
          , "<developer>525_CHILD_PROMPT. Answer the phase marker.</developer>" )
        ]
      ~qualify_chatml_extensions
      ~policy
      (fun env _ root daemon ->
         List.iter
           [ "constructor.txt"; "turn.txt"; "pretool.txt"; "idle.txt"; "root.txt" ]
           ~f:(fun name ->
             Eio.Path.save
               ~create:(`Exclusive 0o600)
               Eio.Path.(env#fs / root / name)
               ("525-native-" ^ name));
         let entry =
           Factory.create_session_authenticated
             (Daemon.factory daemon)
             ~command_audit:None
             ~actor:constructor
             (create_request ~start_immediately:true ())
           |> protocol_ok
         in
         let closed = ref false in
         let close () =
           if not !closed
           then (
             entry.close ();
             closed := true)
         in
         Fun.protect ~finally:close (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
             let rec await_idle () =
               let state = Actor.state entry.actor |> protocol_ok in
               match state.active_operation, state.lifecycle.observed, state.failure with
               | None, Idle, None -> state
               | _, _, Some error -> raise_s [%sexp (error : P.Error.t)]
               | _ ->
                 Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                 await_idle ()
             in
             let initial = await_idle () in
             assert (
               Option.is_none
                 (Agent_server.Session_registry.find
                    (Daemon.registry daemon)
                    initial.identity.session_id));
             [%test_eq: P.Session.desired_state] Running initial.lifecycle.desired;
             assert (
               Agent_session.Session_state.Runtime_initialization.equal
                 initial.runtime_initialization
                 Ready);
             [%test_eq: int] 0 !root_calls;
             [%test_eq: int] 0 (Hashtbl.length child_calls);
             Atomic.set constructor_live false;
             assert (not (Operator_authorization.is_current constructor));
             let writer, _ =
               Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             Actor.start ~actor:foreground entry.actor ~attachment_id:writer.id
             |> protocol_ok
             |> ignore;
             let history_id =
               Agent_session.History_id_source.allocate entry.history_ids |> protocol_ok
             in
             let message =
               Owner.parse_user_content
                 entry.runtime
                 ~id:history_id
                 { kind = Plain_text; text = "525_ROOT_REQUEST"; attachments = [] }
               |> protocol_ok
               |> Agent_session.History_codec.to_protocol
             in
             let operation_id, run_receipt =
               match mode with
               | Ordinary ->
                 let submission =
                   Actor.submit_message_authenticated
                     entry.actor
                     ~actor:foreground
                     ~attachment_id:writer.id
                     message
                   |> protocol_ok
                 in
                 Option.value_exn submission.operation_id, None
               | Named_run ->
                 let before = Actor.state entry.actor |> protocol_ok in
                 let observer =
                   Checkpoint.observer before.moderator |> protocol_ok |> Option.value_exn
                 in
                 let scope =
                   Agent_session.Run_admission.Scope.create_authenticated
                     ~actor:foreground
                     ~observer
                     ~startup_pending:(fun () -> false)
                     ~authorize:(fun _ -> Ok ())
                   |> protocol_ok
                 in
                 let request =
                   P.Run_start.create
                     ~session_id:before.identity.session_id
                     ~attachment_id:writer.id
                     ~generation:before.identity.generation
                     ~expected_revision:before.counters.revision
                     ~mode:Single_turn
                     ~input:
                       (User_submission
                          { kind = Plain_text
                          ; text = "525_ROOT_REQUEST"
                          ; attachments = []
                          })
                     ~key:(P.Idempotency_key.of_string "525-named-run" |> protocol_ok)
                   |> protocol_ok
                 in
                 let session =
                   P.Session_ref.create
                     ~server_id:
                       (Daemon.store daemon |> Agent_store.Session_store.server_id)
                     ~session_id:before.identity.session_id
                 in
                 let receipt =
                   Actor.admit_run
                     entry.actor
                     ~scope
                     ~request
                     ~session
                     ~request_sha256:
                       (P.Run_start.to_json request
                        |> Jsonaf.to_string
                        |> Digestif.SHA256.digest_string
                        |> Digestif.SHA256.to_hex)
                     ~entry:(Some message)
                   |> protocol_ok
                 in
                 let admitted = Actor.state entry.actor |> protocol_ok in
                 let run =
                   Agent_session.Run_state.find
                     (Option.value_exn admitted.run_state)
                     receipt.run_id
                   |> Option.value_exn
                 in
                 let operations =
                   run.owned_work
                   @ List.map run.terminal_work ~f:(fun (item : P.Run_work.Terminal.t) ->
                     item.work)
                   |> List.filter_map ~f:(fun (work : P.Run_work.t) ->
                     match work.key with
                     | Operation id -> Some id
                     | Retained _ -> None)
                   |> List.dedup_and_sort ~compare:P.Id.Operation.compare
                 in
                 let operation =
                   match operations with
                   | [ id ] -> id
                   | ids ->
                     raise_s
                       [%sexp
                         "expected one admitted Run operation"
                       , (ids : P.Id.Operation.t list)]
                 in
                 operation, Some receipt
             in
             let foreground_state = await_idle () in
             let events =
               match
                 Agent_session.Durable_event_log.replay
                   entry.durable_events
                   ~after_sequence:initial.counters.event_sequence
                   ~through_sequence:foreground_state.counters.event_sequence
               with
               | Available events -> events
               | Snapshot_required ->
                 failwith "installed fixture exceeded event replay window"
             in
             if
               not
                 (List.exists events ~f:(fun event ->
                    match
                      P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                      |> protocol_ok
                    with
                    | Operation_completed { id; state = Completed; _ } ->
                      P.Id.Operation.equal id operation_id
                    | _ -> false))
             then (
               let operations =
                 List.filter events ~f:(fun event ->
                   match
                     P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                     |> protocol_ok
                   with
                   | Operation_started _ | Operation_completed _ | Operation_failed _ ->
                     true
                   | _ -> false)
               in
               raise_s
                 [%sexp
                   "installed foreground operation did not complete"
                 , (operation_id : P.Id.Operation.t)
                 , (operations : P.Event.Durable.t list)]);
             [%test_eq: int] (if named_run then 1 else 2) !root_calls;
             [%test_eq: int] 1 (calls "525_PHASE_TURN");
             [%test_eq: int] (if named_run then 0 else 1) (calls "525_PHASE_TOOL");
             [%test_eq: int] 0 (calls "525_PHASE_IDLE");
             assert (Option.is_none foreground_state.failure);
             assert (not foreground_state.halted);
             let state_value state =
               let snapshot =
                 Checkpoint.decode state.Agent_session.Session_state.moderator
                 |> protocol_ok
                 |> Option.value_exn
               in
               Session.Snapshot.to_jsonaf snapshot.current_state
             in
             assert (
               Jsonaf.exactly_equal
                 (state_value foreground_state)
                 (Session.Snapshot.to_jsonaf (Int (if named_run then 2 else 3))));
             let final, queue =
               if named_run
               then foreground_state, None
               else (
                 let observed, due = Actor.due_schedules entry.actor |> protocol_ok in
                 (match observed with
                  | Idle -> ()
                  | _ -> raise_s [%sexp (observed : P.Session.observed_state)]);
                 let timer =
                   match due with
                   | [ timer ] -> timer
                   | timers -> raise_s [%sexp (timers : P.Schedule.t list)]
                 in
                 let claimed =
                   Actor.claim_schedule
                     entry.actor
                     ~schedule_id:timer.id
                     ~generation:timer.generation
                   |> protocol_ok
                   |> Option.value_exn
                 in
                 Owner.deliver_schedule entry.runtime claimed |> protocol_ok;
                 let delivered = Actor.state entry.actor |> protocol_ok in
                 let queue =
                   Checkpoint.queue delivered.moderator |> protocol_ok |> Option.value_exn
                 in
                 [%test_eq: int64] 0L (Queue.head_ordinal queue);
                 [%test_eq: int64] 1L (Queue.next_ordinal queue);
                 let occurrence =
                   match Queue.entries queue with
                   | [ occurrence ] -> occurrence
                   | _ -> failwith "timer did not append exactly one occurrence"
                 in
                 (match Queue.Entry.producer occurrence with
                  | Some (Schedule_delivery actual) ->
                    assert (P.Id.Schedule.equal actual.id timer.id);
                    [%test_eq: int] timer.generation actual.generation;
                    [%test_eq: int] 1 actual.delivery_count
                  | _ -> failwith "idle head did not retain the actual Schedule producer");
                 assert (
                   Option.equal
                     P.Inference_origin.equal
                     (Queue.Entry.origin occurrence)
                     (Some Unbound));
                 let provenance = Option.value_exn timer.inference_provenance in
                 assert (
                   P.Run_source.equal
                     (P.Schedule_provenance.source provenance)
                     (Queue.source queue));
                 assert (Owner.drain_idle_moderator entry.runtime |> protocol_ok);
                 let final = Actor.state entry.actor |> protocol_ok in
                 assert (
                   Jsonaf.exactly_equal
                     (state_value final)
                     (Session.Snapshot.to_jsonaf (Int 4)));
                 [%test_eq: int] 1 (calls "525_PHASE_IDLE");
                 [%test_eq: int] 2 !root_calls;
                 assert (not (Owner.drain_idle_moderator entry.runtime |> protocol_ok));
                 let repeated = Actor.state entry.actor |> protocol_ok in
                 assert (
                   Option.equal Jsonaf.exactly_equal repeated.moderator final.moderator);
                 [%test_eq: int] 2 !root_calls;
                 List.iter
                   [ "525_PHASE_TURN"; "525_PHASE_TOOL"; "525_PHASE_IDLE" ]
                   ~f:(fun phase -> [%test_eq: int] 1 (calls phase));
                 let final_queue =
                   Checkpoint.queue final.moderator |> protocol_ok |> Option.value_exn
                 in
                 [%test_eq: int64] 1L (Queue.head_ordinal final_queue);
                 [%test_eq: int64] 1L (Queue.next_ordinal final_queue);
                 assert (List.is_empty (Queue.entries final_queue));
                 final, Some queue)
             in
             let expected_origin =
               Option.value_map
                 run_receipt
                 ~default:P.Inference_origin.Unbound
                 ~f:(fun receipt -> P.Inference_origin.Run receipt.run_id)
             in
             let check_native file phase expected_operation =
               let invocation =
                 List.filter final.invocations ~f:(fun invocation ->
                   String.equal invocation.context.tool_name "read_file"
                   && Option.equal
                        Jsonaf.exactly_equal
                        (Jsonaf.member "file" invocation.context.input)
                        (Some (`String file)))
                 |> function
                 | [ invocation ] -> invocation
                 | invocations -> raise_s [%sexp (invocations : P.Invocation.t list)]
               in
               assert (P.Invocation.equal_origin invocation.context.origin Moderator);
               let parent = Option.value_exn invocation.parent_event in
               let receipt =
                 List.find_exn final.moderator_executions ~f:(fun receipt ->
                   P.Id.Moderator_execution.equal receipt.context.id parent)
               in
               assert (P.Moderator_execution.equal_phase receipt.context.phase phase);
               assert (
                 Option.equal
                   P.Id.Operation.equal
                   receipt.context.operation_id
                   expected_operation);
               (match receipt.status with
                | Completed _ -> ()
                | _ -> raise_s [%sexp (receipt : P.Moderator_execution.t)]);
               (match invocation.status with
                | Resolved (Complete result) | Published (Complete result) ->
                  assert (
                    String.is_substring
                      (Jsonaf.to_string result)
                      ~substring:("525-native-" ^ file))
                | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   invocation.inference_origin
                   (Some
                      (if P.Moderator_execution.equal_phase phase Session_start
                       then Unbound
                       else expected_origin)));
               receipt
             in
             ignore
               (check_native "constructor.txt" Session_start None
                : P.Moderator_execution.t);
             ignore
               (check_native "turn.txt" Turn_start (Some operation_id)
                : P.Moderator_execution.t);
             if not named_run
             then (
               ignore
                 (check_native "pretool.txt" Pre_tool_call (Some operation_id)
                  : P.Moderator_execution.t);
               let root_invocation =
                 List.filter final.invocations ~f:(fun invocation ->
                   Option.equal
                     String.equal
                     invocation.context.provider_call_id
                     (Some "525-root-call"))
                 |> function
                 | [ invocation ] -> invocation
                 | invocations -> raise_s [%sexp (invocations : P.Invocation.t list)]
               in
               [%test_eq: string] "read_file" root_invocation.context.tool_name;
               assert (P.Invocation.equal_origin root_invocation.context.origin Model);
               (match root_invocation.status with
                | Resolved (Complete result) | Published (Complete result) ->
                  assert (
                    String.is_substring
                      (Jsonaf.to_string result)
                      ~substring:"525-native-root.txt")
                | _ -> raise_s [%sexp (root_invocation : P.Invocation.t)]);
               let idle_receipt = check_native "idle.txt" Internal_event None in
               let consumed = Option.value_exn idle_receipt.queue_occurrence in
               [%test_eq: int64]
                 0L
                 (P.Moderator_execution.Queue_occurrence.ordinal consumed);
               assert (
                 P.Run_source.equal
                   (P.Moderator_execution.Queue_occurrence.source consumed)
                   (Queue.source (Option.value_exn queue)));
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   (P.Moderator_execution.Queue_occurrence.origin consumed)
                   (Some Unbound)));
             let model_jobs =
               List.filter final.jobs ~f:(fun job -> P.Job.equal_kind job.kind Model_call)
             in
             [%test_eq: int] (if named_run then 1 else 3) (List.length model_jobs);
             List.iter
               (if named_run
                then [ "525_PHASE_TURN" ]
                else [ "525_PHASE_TURN"; "525_PHASE_TOOL"; "525_PHASE_IDLE" ])
               ~f:(fun phase ->
                 let job =
                   List.filter model_jobs ~f:(fun job ->
                     Option.equal
                       Jsonaf.exactly_equal
                       (Jsonaf.member "payload" job.payload
                        |> Option.bind ~f:(Jsonaf.member "input"))
                       (Some (`String phase)))
                   |> function
                   | [ job ] -> job
                   | jobs -> raise_s [%sexp (jobs : P.Job.t list)]
                 in
                 [%test_eq: int] 1 job.attempt;
                 (match job.status, job.delivery with
                  | Succeeded, Not_required -> ()
                  | _ -> raise_s [%sexp (job : P.Job.t)]);
                 assert (
                   Option.equal
                     P.Inference_origin.equal
                     job.inference_origin
                     (Some expected_origin));
                 let result = Option.value_exn job.result in
                 assert (
                   Option.equal
                     Jsonaf.exactly_equal
                     (Jsonaf.member "final_text" result)
                     (Some (`String (phase ^ "-ok")))));
             Option.iter run_receipt ~f:(fun (receipt : P.Run_receipt.t) ->
               let module Ledger = Agent_session.Inference_ledger in
               let module Observation = Inference.Observation in
               let run =
                 Agent_session.Run_state.find
                   (Option.value_exn final.run_state)
                   receipt.run_id
                 |> Option.value_exn
               in
               (match run.lifecycle with
                | Terminal (Completed _) -> ()
                | _ -> raise_s [%sexp (run : P.Run.t)]);
               assert (P.Run_source.equal run.source receipt.source);
               let child = List.hd_exn model_jobs in
               let child_key =
                 P.Run_work.Key.Retained
                   (P.Session_work.Key.Job { id = child.id; attempt = child.attempt })
               in
               assert (
                 List.exists
                   run.terminal_work
                   ~f:(fun (terminal : P.Run_work.Terminal.t) ->
                     P.Run_work.Key.equal terminal.work.key child_key
                     && P.Run_work.Terminal.equal_outcome terminal.outcome Succeeded));
               let turn =
                 List.filter final.moderator_executions ~f:(fun event ->
                   P.Moderator_execution.equal_phase event.context.phase Turn_start
                   && Option.equal
                        P.Id.Operation.equal
                        event.context.operation_id
                        (Some operation_id))
                 |> function
                 | [ event ] -> event
                 | events -> raise_s [%sexp (events : P.Moderator_execution.t list)]
               in
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   (Agent_session.Invocation_inference_provenance.of_event
                      ~runs:final.run_state
                      ~jobs:final.jobs
                      turn
                    |> protocol_ok)
                   (Some expected_origin));
               let child_admission =
                 List.filter !admitted_jobs ~f:(fun (_, _, jobs) ->
                   List.mem jobs child.id ~equal:P.Id.Job.equal)
                 |> function
                 | [ admission ] -> admission
                 | _ -> failwith "actual child lacks its unique acknowledged scope"
               in
               let child_scope, child_accounting, active_jobs = child_admission in
               [%test_eq: P.Id.Job.t list] [ child.id ] active_jobs;
               let rows = Ledger.rows final.inference_ledger in
               let child_row =
                 List.filter rows ~f:(fun row ->
                   Transcript.Scope.equal
                     (Ledger.Handle.scope (Ledger.Row.handle row))
                     child_scope)
                 |> function
                 | [ row ] -> row
                 | _ -> failwith "child scope lacks its unique retained ledger row"
               in
               let require_completed label record =
                 match Observation.Attempt_record.state record with
                 | Terminal terminal ->
                   assert (
                     Transcript.Scope.equal
                       (Observation.Attempt_record.scope record)
                       (Inference.Event.Terminal.scope terminal));
                   (match Inference.Event.Terminal.outcome terminal with
                    | Completed -> ()
                    | (Refused | Incomplete _ | Failed _) as outcome ->
                      raise_s
                        [%sexp
                          (label : string), (outcome : Inference.Event.Terminal.outcome)])
                 | Prepared | Running | Interrupted _ ->
                   failwith (label ^ " attempt has no acknowledged completed terminal")
               in
               let handle = Ledger.Row.handle child_row in
               let record = Ledger.Row.record child_row in
               (* The actual Inference_job service supplies no operation or
                  invocation association; its on_admitted scope is recorded while
                  this exact synchronous child is Running. *)
               assert (Option.is_none (Ledger.Handle.operation_id handle));
               assert (Option.is_none (Ledger.Handle.invocation_id handle));
               assert (
                 Transcript.Scope.equal
                   child_scope
                   (Observation.Attempt_record.scope record));
               assert (
                 Observation.Observation_id.equal
                   child_accounting
                   (Ledger.Handle.accounting_id handle));
               assert (
                 Observation.Observation_id.equal
                   child_accounting
                   (Observation.Attempt_record.accounting_id record));
               assert (
                 P.Id.Session.equal
                   final.identity.session_id
                   (Ledger.Handle.session_id handle));
               [%test_eq: int] child.generation (Ledger.Handle.generation handle);
               require_completed "child" record;
               let root_rows =
                 List.filter rows ~f:(fun row ->
                   Option.equal
                     P.Id.Operation.equal
                     (Ledger.Handle.operation_id (Ledger.Row.handle row))
                     (Some operation_id))
               in
               assert (not (List.is_empty root_rows));
               List.iter root_rows ~f:(fun row ->
                 let root_handle = Ledger.Row.handle row in
                 let acknowledged =
                   List.filter !admitted_jobs ~f:(fun (scope, accounting, jobs) ->
                     Transcript.Scope.equal scope (Ledger.Handle.scope root_handle)
                     && Observation.Observation_id.equal
                          accounting
                          (Ledger.Handle.accounting_id root_handle)
                     && List.is_empty jobs)
                 in
                 [%test_eq: int] 1 (List.length acknowledged);
                 assert (
                   not
                     (Transcript.Scope.equal
                        child_scope
                        (Ledger.Handle.scope root_handle)));
                 assert (
                   Transcript.Scope.equal
                     (Ledger.Handle.scope root_handle)
                     (Observation.Attempt_record.scope (Ledger.Row.record row)));
                 assert (
                   Observation.Observation_id.equal
                     (Ledger.Handle.accounting_id root_handle)
                     (Observation.Attempt_record.accounting_id (Ledger.Row.record row)));
                 assert (Option.is_none (Ledger.Handle.invocation_id root_handle));
                 require_completed "root" (Ledger.Row.record row));
               let answers =
                 Agent_session.History_codec.all_of_protocol
                   final.conversation.canonical_history
                 |> protocol_ok
                 |> List.filter_map ~f:(fun entry ->
                   match
                     History_entry.payload entry
                     |> History_entry.Payload.semantic
                     |> History_entry.Payload.Semantic.view
                   with
                   | Message { role = Assistant; content = [ Text { text; _ } ]; _ } ->
                     Some text
                   | _ -> None)
               in
               [%test_eq: string list] [ "525-root-completed" ] answers);
             assert (not (Operator_authorization.is_current constructor));
             assert (Operator_authorization.is_current foreground);
             let directory =
               Option.value_exn entry.store_handle
               |> Agent_store.Session_store.Handle.snapshot_directory
             in
             close ();
             let installed =
               Agent_store.Snapshot.load_current
                 ~env
                 ~directory
                 ~max_payload_length:(64 * 1024 * 1024)
               |> function
               | Ok value -> Option.value_exn value
               | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
             in
             let saved =
               Agent_session.Session_state_document.decode
                 ~limits:Document_schema.Limits.default
                 installed.snapshot.payload
               |> function
               | Ok value -> Agent_session.Session_state_document.value value
               | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
             in
             if named_run
             then (
               assert (
                 Option.equal
                   Agent_session.Run_state.equal
                   saved.run_state
                   final.run_state);
               let ledger_json ledger =
                 Agent_session.Inference_ledger.to_document ledger
                 |> function
                 | Ok document -> Document_schema.Document.json document
                 | Error error ->
                   raise_s [%sexp (error : Agent_session.Inference_ledger.Error.t)]
               in
               assert (
                 Jsonaf.exactly_equal
                   (ledger_json saved.inference_ledger)
                   (ledger_json final.inference_ledger)));
             assert (Option.equal Jsonaf.exactly_equal saved.moderator final.moderator);
             assert (List.equal P.Invocation.equal saved.invocations final.invocations);
             assert (
               List.equal
                 (fun left right ->
                    Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
                 saved.jobs
                 final.jobs);
             assert (
               List.equal
                 (fun left right ->
                    Jsonaf.exactly_equal
                      (P.Schedule.to_json left)
                      (P.Schedule.to_json right))
                 saved.schedules
                 final.schedules)))))
;;

let%test_unit "legacy Factory initialization owns dedicated Model call and spawn" =
  let module P = Agent_protocol in
  let module Checkpoint = Agent_session.Moderator_checkpoint in
  let module Res = Openai.Responses in
  let prompt_text =
    {|<developer>Offline dedicated constructor Model fixture.</developer>
<tool name="recipe_source" agent="init-recipe.chatmd" local/>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let payload input = `Object([
  {key = "prompt"; value = `String("init-recipe.chatmd")},
  {key = "input"; value = `String(input)},
  {key = "is_local"; value = `Bool(true)}
])
let on_event ctx state event = match event with
| `Session_start ->
  let* called = Model.call("agent_prompt_v1", payload("525_INIT_CALL")) in
  (match called with
  | `Ok(value) ->
    let* spawned = Model.spawn("agent_prompt_v1", payload("525_INIT_SPAWN")) in
    Task.pure(state + 1)
  | `Refused(message) -> Task.fail(message)
  | `Error(message) -> Task.fail(message))
| _ -> Task.pure(state)
</script>|}
  in
  List.iter [ false; true ] ~f:(fun qualify_chatml_extensions ->
    let original_live = Atomic.make true in
    let original =
      Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
        Atomic.get original_live)
    in
    let provider_calls = ref 0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
        assert (Operator_authorization.is_current original);
        let transcript =
          `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
        in
        assert (String.is_substring transcript ~substring:"525_INIT_RECIPE");
        assert (String.is_substring transcript ~substring:"525_INIT_CALL");
        assert (not (String.is_substring transcript ~substring:"525_INIT_SPAWN"));
        incr provider_calls;
        let message : Res.Output_message.t =
          { role = Assistant
          ; id = "525-init-answer"
          ; status = "completed"
          ; content =
              [ { annotations = []; text = "525-init-call-result"; _type = "output_text" }
              ]
          ; phase = None
          ; _type = "message"
          }
        in
        let item = Res.Response_stream.Item.Output_message message in
        [ Res.Response_stream.Output_item_added
            { item; output_index = 0; type_ = "response.output_item.added" }
        ; Output_text_delta
            { item_id = message.id
            ; output_index = 0
            ; content_index = 0
            ; delta = "525-init-call-result"
            ; type_ = "response.output_text.delta"
            }
        ; Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
        ]
        |> Stdlib.List.to_seq)
    in
    with_daemon
      ~prompt_text
      ~sources:
        [ ( "init-recipe.chatmd"
          , "<developer>525_INIT_RECIPE. Return the fixture result.</developer>" )
        ]
      ~qualify_chatml_extensions
      ~policy
      (fun env _ _root daemon ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
           let entry =
             Factory.create_session_authenticated
               (Daemon.factory daemon)
               ~command_audit:None
               ~actor:original
               (create_request ())
             |> protocol_ok
           in
           let closed = ref false in
           let close () =
             if not !closed
             then (
               entry.close ();
               closed := true)
           in
           Fun.protect
             ~finally:(fun () -> Eio.Cancel.protect close)
             (fun () ->
                let state = Actor.state entry.actor |> protocol_ok in
                assert (
                  Agent_session.Session_state.Runtime_initialization.equal
                    state.runtime_initialization
                    Ready);
                [%test_eq: P.Session.desired_state] state.lifecycle.desired Stopped;
                assert (Option.is_none state.active_operation);
                assert (Option.is_none state.failure);
                let receipt =
                  match state.moderator_executions with
                  | [ receipt ] -> receipt
                  | receipts ->
                    raise_s
                      [%sexp
                        "expected dedicated constructor receipt"
                      , (receipts : P.Moderator_execution.t list)]
                in
                assert (
                  P.Moderator_execution.equal_phase receipt.context.phase Session_start);
                assert (Option.is_none receipt.context.operation_id);
                (match receipt.status with
                 | Completed _ -> ()
                 | Running | Failed _ | Interrupted _ ->
                   raise_s [%sexp (receipt : P.Moderator_execution.t)]);
                let snapshot =
                  Checkpoint.decode state.moderator |> protocol_ok |> Option.value_exn
                in
                assert (
                  Jsonaf.exactly_equal
                    (Session.Snapshot.to_jsonaf snapshot.current_state)
                    (Session.Snapshot.to_jsonaf (Int 1)));
                let find_job input =
                  match
                    List.filter state.jobs ~f:(fun job ->
                      Option.equal
                        Jsonaf.exactly_equal
                        (Jsonaf.member "payload" job.P.Job.payload
                         |> Option.bind ~f:(Jsonaf.member "input"))
                        (Some (`String input)))
                  with
                  | [ job ] -> job
                  | jobs ->
                    raise_s
                      [%sexp
                        "unexpected dedicated constructor jobs", (jobs : P.Job.t list)]
                in
                [%test_eq: int] (List.length state.jobs) 2;
                let called = find_job "525_INIT_CALL" in
                let spawned = find_job "525_INIT_SPAWN" in
                assert (not (P.Id.Job.equal called.id spawned.id));
                List.iter [ called; spawned ] ~f:(fun job ->
                  assert (P.Job.equal_kind job.kind Model_call);
                  assert (
                    Option.equal
                      P.Inference_origin.equal
                      job.inference_origin
                      (Some Unbound)));
                [%test_eq: int] called.attempt 1;
                (match called.status, called.delivery with
                 | Succeeded, Not_required -> ()
                 | _ ->
                   raise_s
                     [%sexp "dedicated Init call did not finish", (called : P.Job.t)]);
                assert (
                  Option.equal
                    Jsonaf.exactly_equal
                    (Jsonaf.member "final_text" (Option.value_exn called.result))
                    (Some (`String "525-init-call-result")));
                [%test_eq: int] spawned.attempt 0;
                (match spawned.status, spawned.delivery with
                 | Queued, Pending -> ()
                 | _ ->
                   raise_s
                     [%sexp "dedicated Init spawn was not queued", (spawned : P.Job.t)]);
                assert (Option.is_none spawned.result);
                assert (Option.is_none spawned.started_at);
                assert (Operator_authorization.is_current original);
                [%test_eq: int] !provider_calls 1;
                let directory =
                  Option.value_exn entry.store_handle
                  |> Agent_store.Session_store.Handle.snapshot_directory
                in
                close ();
                let installed =
                  match
                    Agent_store.Snapshot.load_current
                      ~env
                      ~directory
                      ~max_payload_length:(64 * 1024 * 1024)
                  with
                  | Ok value -> Option.value_exn value
                  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
                in
                let saved =
                  match
                    Agent_session.Session_state_document.decode
                      ~limits:Document_schema.Limits.default
                      installed.snapshot.payload
                  with
                  | Ok value -> Agent_session.Session_state_document.value value
                  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
                in
                assert (Option.equal Jsonaf.exactly_equal saved.moderator state.moderator);
                assert (
                  List.equal
                    P.Moderator_execution.equal
                    saved.moderator_executions
                    state.moderator_executions);
                assert (
                  List.equal
                    (fun left right ->
                       Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
                    saved.jobs
                    state.jobs);
                [%test_eq: int] !provider_calls 1))))
;;

let%test_unit "cold Ready Running legacy constructor settles actual session approval" =
  let module P = Agent_protocol in
  let module Owner = Agent_server.Runtime_owner in
  let module Checkpoint = Agent_session.Moderator_checkpoint in
  let profile =
    { permission_profile with tool_default = Ask; approval_fallback = Deny }
  in
  let prompt_text =
    {|<developer>Offline interactive constructor fixture.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state)
| `Session_resume ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("approved-resume.txt")}
  ])) in
  (match result with
  | `Ok(value) -> Task.pure(state + 1)
  | `Error(message) -> Task.fail(message))
| _ -> Task.pure(state)
</script>|}
  in
  let provider_calls = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr provider_calls;
      failwith "native approval must not dispatch a provider")
  in
  with_daemon ~prompt_text ~profile ~policy (fun env _ root daemon ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(env#fs / root / "approved-resume.txt")
        "actual approved cold native result";
      let initial_live = Atomic.make true in
      let original live =
        Operator_authorization.guarded ~principal:(principal ()) ~is_current:(fun () ->
          Atomic.get live)
      in
      let initial_original = original initial_live in
      let entry =
        Factory.create_session_authenticated
          (Daemon.factory daemon)
          ~command_audit:None
          ~actor:initial_original
          (create_request ~start_immediately:true ())
        |> protocol_ok
      in
      let closed = ref false in
      let close () =
        if not !closed
        then (
          entry.close ();
          closed := true)
      in
      let protect_body f =
        match f () with
        | value ->
          Eio.Cancel.protect close;
          value
        | exception primary ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          eprintf
            "primary Ready/Running constructor failure: %s\n"
            (Exn.to_string primary);
          (try Eio.Cancel.protect close with
           | cleanup ->
             eprintf
               "secondary Ready/Running constructor cleanup failure: %s\n"
               (Exn.to_string cleanup));
          Exn.raise_with_original_backtrace primary backtrace
      in
      protect_body (fun () ->
        let rec await_idle () =
          let state = Actor.state entry.actor |> protocol_ok in
          match state.lifecycle.observed, state.active_operation, state.failure with
          | Idle, None, None -> state
          | _, _, Some error -> raise_s [%sexp (error : P.Error.t)]
          | _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
            await_idle ()
        in
        let initial = await_idle () in
        assert (
          Agent_session.Session_state.Runtime_initialization.equal
            initial.runtime_initialization
            Ready);
        [%test_eq: P.Session.desired_state] initial.lifecycle.desired Running;
        assert (List.is_empty initial.permissions);
        assert (List.is_empty initial.invocations);
        let writer, _ =
          Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
        in
        Owner.unload entry.runtime |> protocol_ok;
        assert (not (Owner.is_loaded entry.runtime));
        Atomic.set initial_live false;
        let resume_live = Atomic.make true in
        let resume_original = original resume_live in
        Eio.Switch.run (fun sw ->
          let loading =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Owner.ensure_loaded_authenticated entry.runtime ~actor:resume_original)
          in
          let rec await_permission () =
            let state = Actor.state entry.actor |> protocol_ok in
            match
              List.filter state.permissions ~f:(fun permission ->
                P.Permission.equal_state permission.state Pending)
            with
            | [ permission ] -> state, permission
            | [] ->
              (match Eio.Promise.peek loading with
               | Some _ ->
                 Eio.Promise.await_exn loading |> protocol_ok;
                 failwith "cold constructor returned without its actual Ask"
               | None ->
                 Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
                 await_permission ())
            | permissions ->
              raise_s
                [%sexp
                  "unexpected constructor permissions"
                , (permissions : P.Permission.t list)]
          in
          let waiting, permission = await_permission () in
          (match waiting.lifecycle.observed with
           | Waiting_for_permission id -> assert (P.Id.Permission.equal id permission.id)
           | observed ->
             raise_s
               [%sexp
                 "constructor is not waiting for approval"
               , (observed : P.Session.observed_state)]);
          assert (Option.is_none waiting.active_operation);
          assert (Operator_authorization.is_current resume_original);
          assert (not (Operator_authorization.is_current initial_original));
          let invocation_id =
            match permission.owner with
            | Invocation id -> id
            | Operation _ -> failwith "constructor Ask fabricated a foreground operation"
          in
          let native =
            List.find_exn waiting.invocations ~f:(fun invocation ->
              P.Id.Invocation.equal invocation.context.id invocation_id)
          in
          [%test_eq: string] native.context.tool_name "read_file";
          let event_id = Option.value_exn native.parent_event in
          let resume =
            List.find_exn waiting.moderator_executions ~f:(fun receipt ->
              P.Id.Moderator_execution.equal receipt.context.id event_id)
          in
          assert (P.Moderator_execution.equal_phase resume.context.phase Session_resume);
          (match resume.status with
           | Running -> ()
           | Completed _ | Failed _ | Interrupted _ ->
             raise_s [%sexp (resume : P.Moderator_execution.t)]);
          let approved =
            Actor.respond_permission
              entry.actor
              ~attachment_id:writer.id
              ~principal_id:(Some (principal ()).id)
              ~permission_id:permission.id
              ~permission_generation:permission.generation
              ~choice:Approve_session
              ~reason:None
            |> function
            | Ok approved -> approved
            | Error error ->
              raise_s
                [%sexp "actual constructor approval response failed", (error : P.Error.t)]
          in
          assert (P.Permission.equal_state approved.state Approved);
          (match approved.resolution with
           | Some { choice = Approve_session; principal_id = Some id; _ } ->
             assert (P.Id.Principal.equal id (principal ()).id)
           | resolution -> raise_s [%sexp (resolution : P.Permission.resolution option)]);
          (match Eio.Promise.await_exn loading with
           | Ok () -> ()
           | Error error ->
             raise_s
               [%sexp "actual approved cold construction failed", (error : P.Error.t)]);
          assert (Owner.is_loaded entry.runtime);
          let final = await_idle () in
          [%test_eq: P.Session.desired_state] final.lifecycle.desired Running;
          assert (
            Agent_session.Session_state.Runtime_initialization.equal
              final.runtime_initialization
              Ready);
          let completed =
            List.find_exn final.moderator_executions ~f:(fun receipt ->
              P.Id.Moderator_execution.equal receipt.context.id event_id)
          in
          (match completed.status with
           | Completed _ -> ()
           | Running | Failed _ | Interrupted _ ->
             raise_s [%sexp (completed : P.Moderator_execution.t)]);
          let native =
            List.find_exn final.invocations ~f:(fun invocation ->
              P.Id.Invocation.equal invocation.context.id invocation_id)
          in
          (match native.status with
           | Resolved (Complete result) ->
             assert (
               String.is_substring
                 (Jsonaf.to_string result)
                 ~substring:"actual approved cold native result")
           | Admitted | Dispatching | Published _
           | Resolved (Pending _ | Fail _ | Cancelled _) ->
             raise_s [%sexp (native : P.Invocation.t)]);
          let snapshot =
            Checkpoint.decode final.moderator |> protocol_ok |> Option.value_exn
          in
          assert (
            Jsonaf.exactly_equal
              (Session.Snapshot.to_jsonaf snapshot.current_state)
              (Session.Snapshot.to_jsonaf (Int 1)));
          let grant =
            match final.grants with
            | [ grant ] -> grant
            | grants -> raise_s [%sexp (grants : P.Grant.t list)]
          in
          [%test_eq: P.Grant.scope] grant.scope Exact_session;
          [%test_eq: P.Grant.state] grant.state Active;
          assert (P.Id.Principal.equal grant.principal_id (principal ()).id);
          [%test_eq: string] grant.tool_name permission.tool_name;
          [%test_eq: string]
            grant.identity_digest
            (Option.value_exn permission.runtime_identity);
          let events =
            match
              Agent_session.Durable_event_log.replay
                entry.durable_events
                ~after_sequence:initial.counters.event_sequence
                ~through_sequence:final.counters.event_sequence
            with
            | Available events -> events
            | Snapshot_required ->
              failwith "interactive constructor events exceeded replay window"
          in
          let requests, resolutions =
            List.fold events ~init:([], []) ~f:(fun (requests, resolutions) event ->
              match
                P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                |> protocol_ok
              with
              | Permission_requested value
                when P.Id.Permission.equal value.id permission.id ->
                value :: requests, resolutions
              | Permission_resolved value
                when P.Id.Permission.equal value.id permission.id ->
                requests, value :: resolutions
              | _ -> requests, resolutions)
          in
          [%test_eq: int] (List.length requests) 1;
          [%test_eq: int] (List.length resolutions) 1;
          assert (
            Jsonaf.exactly_equal
              (P.Permission.to_json (List.hd_exn requests))
              (P.Permission.to_json permission));
          assert (
            Jsonaf.exactly_equal
              (P.Permission.to_json (List.hd_exn resolutions))
              (P.Permission.to_json approved));
          (match
             Actor.respond_permission
               entry.actor
               ~attachment_id:writer.id
               ~principal_id:(Some (principal ()).id)
               ~permission_id:permission.id
               ~permission_generation:permission.generation
               ~choice:Approve_session
               ~reason:None
           with
           | Error { code = Already_resolved; _ } -> ()
           | Error error -> raise_s [%sexp (error : P.Error.t)]
           | Ok _ -> failwith "constructor permission resolved twice");
          Owner.ensure_loaded_authenticated entry.runtime ~actor:resume_original
          |> protocol_ok;
          let repeated = Actor.state entry.actor |> protocol_ok in
          (match
             Agent_session.Session_state_document.equal_values
               final
               ~limits:Document_schema.Limits.default
               repeated
           with
           | Ok true -> ()
           | Ok false -> failwith "loaded constructor repeated approval or Resume"
           | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]);
          assert (Operator_authorization.is_current resume_original);
          [%test_eq: int] !provider_calls 0))))
;;
