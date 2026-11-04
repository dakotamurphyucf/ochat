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
      ?(qualify_chatml_extensions = false)
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
               ~options:
                 { Daemon.default_options with
                   inference_policy = policy
                 ; qualify_chatml_extensions
                 }
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
  let module Checkpoint = Agent_session.Moderator_checkpoint in
  let module Queue = Agent_session.Moderator_queue in
  let prompt_text =
    {|<developer>Offline constructor callback fixture.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Session_start ->
  Task.bind(Tool.call("read_file", `Object([{key = "root"; value = `String("data")}; {key = "file"; value = `String("startup.txt")}])), fun result ->
    match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) ->
      Task.bind(Runtime.emit(`String("constructor suffix")), fun ignored ->
        Task.bind(Schedule.after_ms(60000, `String("constructor timer")), fun timer ->
          Task.bind(Ui.notify("constructor notification"), fun ignored ->
            Task.pure(state + 1)))))
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
          | Snapshot_required -> failwith "constructor events exceeded the replay window"
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
        let queue = Checkpoint.queue state.moderator |> protocol_ok |> Option.value_exn in
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
          P.Invocation.equal_observer (Queue.source queue).observer receipt.context.source);
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
        [%test_eq: int] !model_calls 0)))
;;
