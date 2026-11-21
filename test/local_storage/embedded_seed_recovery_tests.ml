open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded

let prompt =
  {|<developer>Leave the imported transcript unchanged.</developer>
<script id="seed_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let connection host =
  let connection = E.host_connection host in
  let initialized =
    C.Session_handle.initialize
      connection
      ~implementation_name:"local-seed-recovery"
      ~implementation_version:"dev"
    |> protocol_ok
  in
  connection, initialized.server_id
;;

let snapshot connection id =
  C.Admin.get_session connection id |> protocol_ok |> P.Public.Snapshot.fields
;;

let expected (fields : P.Public.Snapshot.Fields.t) =
  P.Session_lifecycle.Observation.expected (Option.value_exn fields.lifecycle)
;;

let insist_stopped (fields : P.Public.Snapshot.Fields.t) =
  assert (P.Session.equal_desired_state fields.session.desired_state Stopped);
  match fields.session.observed_state with
  | Stopped -> ()
  | _ -> failwith "enrollment started the seeded session"
;;

let script_selection host fields =
  let selected =
    E.session_script_selection
      host
      ~expected:(expected fields)
      ~checkpoint:Eio.Fiber.check
    |> protocol_ok
  in
  (match selected.moderator with
   | Some (Standard Moderator_v1) -> ()
   | _ -> failwith "seed lost the original versioned authored selection");
  selected.session
;;

let stored_state env storage id =
  let module S = Agent_store in
  let store_ok result =
    Result.map_error result ~f:S.Store_error.to_protocol_error |> protocol_ok
  in
  let module Persistence = Agent_session.Session_persistence in
  let max_payload_length =
    Agent_server.Daemon.default_options.factory_limits.snapshot_payload_limit
  in
  let root =
    S.Data_root.open_existing ~env ~path:(Agent_server.Local_storage.Root.path storage)
    |> store_ok
  in
  let directory = Filename.concat (S.Data_root.session_path root id) "snapshot" in
  let snapshot =
    S.Snapshot.load_current ~env ~directory ~max_payload_length
    |> store_ok
    |> Option.value_exn
  in
  let limits =
    Agent_session.Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:(fun _ -> "invalid fixture limits")
    |> Result.ok_or_failwith
  in
  Persistence.restore_snapshot ~limits snapshot.snapshot
  |> store_ok
  |> Persistence.Restored.state
;;

let%expect_test "stopped local seed enrolls exact history before the authored run" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "seed.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          prompt;
        let storage =
          Agent_server.Local_storage.Root.create ~path:(Filename.concat root "store") ()
          |> protocol_ok
        in
        let options : E.options =
          { prompt_file
          ; workspace = root
          ; tool_dir = root
          ; home = Some root
          ; storage = Durable storage
          ; start_immediately = false
          ; permission_profile = E.default_permission_profile
          ; attachment_mode = Owner_read_write
          ; event_capacity = 32
          }
        in
        let daemon_options =
          { Agent_server.Daemon.default_options with
            inference_policy =
              inference_policy
                ~default_model:"fixture-model"
                ~post_stream:(fun ~sw:_ ~inputs:_ ->
                  failwith "seed enrollment or authored fixture invoked a provider")
          }
        in
        let with_host f =
          Eio.Switch.run (fun sw ->
            E.with_local_host
              ~sw
              ~env
              ~daemon_options
              ~root_invocation:Headless
              options
              ~f:(fun host -> Ok (f sw host))
            |> protocol_ok)
        in
        let original = "Précis retained before startup." in
        let module History = Agent_session.History_codec in
        let id =
          History_entry.Id.create ~namespace:"imported.seed" ~sequence:0
          |> Result.ok_or_failwith
        in
        let row = History.user_text ~id original |> History.to_protocol in
        let transcript =
          P.Session_seed.Transcript.create
            [ row ]
            ~next_history_sequence:1L
            ~limits:Document_schema.Limits.default
            ~max_entries:8
            ~max_work_steps:100000
            ~checkpoint:Eio.Fiber.check
          |> protocol_ok
        in
        let read connection scope =
          let citation =
            P.Recovery_window.Citation.create
              scope
              ~occurrence:row.id
              ~version:(P.Recovery_item_version.canonical row.content_revision)
            |> protocol_ok
          in
          let range =
            P.Recovery_text_range.create
              citation
              ~part:0
              ~start:0
              ~stop:(String.length original)
              ()
            |> protocol_ok
          in
          let reply = C.Recovery_windows.read_text connection range |> protocol_ok in
          assert (String.equal reply.text original)
        in
        let attach sw connection id =
          C.Session_handle.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            ~session_id:id
            ~mode:Owner_read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let id, scoped =
          with_host (fun sw host ->
            let connection, server_id = connection host in
            let spec = E.session_spec options |> protocol_ok in
            let definition_id =
              match spec.prompt with
              | Catalog id -> id
              | _ -> failwith "embedded seed lacks catalog source"
            in
            let definition =
              C.Catalog.get_prompt connection definition_id |> protocol_ok
            in
            let code =
              P.Session_seed.Code_selection.Catalog
                { definition_id
                ; expected_revision = Option.value_exn definition.current_revision
                }
            in
            let request =
              P.Session_seed.Request.create
                spec
                ~code
                ~transcript
                ~idempotency_key:
                  (P.Idempotency_key.of_string "local-recovery-seed" |> protocol_ok)
              |> protocol_ok
            in
            let id =
              (C.Session_handle.seed connection request |> protocol_ok).session_id
            in
            let initial = snapshot connection id in
            insist_stopped initial;
            ignore (script_selection host initial : P.Session.t);
            let handle = attach sw connection id in
            let session = P.Session_ref.create ~server_id ~session_id:id in
            let selected =
              C.Recovery_windows.enable
                connection
                { session
                ; attachment_id = (C.Session_handle.attachment handle).id
                ; expected_generation = initial.session.generation
                }
              |> protocol_ok
            in
            read connection selected;
            C.Session_handle.close handle;
            insist_stopped (snapshot connection id);
            id, selected)
        in
        let check_history initialization =
          let state = stored_state env storage id in
          assert (
            Agent_session.Session_state.Runtime_initialization.equal
              state.runtime_initialization
              initialization);
          assert (
            List.equal P.History.equal_entry state.conversation.canonical_history [ row ]);
          assert (Option.is_none state.recovery_installed_originals)
        in
        check_history (Pending { fresh_history = false });
        with_host (fun sw host ->
          let connection, server_id = connection host in
          let before = snapshot connection id in
          insist_stopped before;
          E.select_session host (expected before) |> protocol_ok;
          let handle = attach sw connection id in
          let session = P.Session_ref.create ~server_id ~session_id:id in
          let page =
            C.Recovery_windows.list
              connection
              { session
              ; generation = None
              ; state = None
              ; page = P.Page.Request.create ~limit:2 () |> protocol_ok
              }
            |> protocol_ok
          in
          (match page.items with
           | [ window ] -> assert (P.Recovery_window.Scope.equal window.scope scoped)
           | _ -> failwith "reopened seed did not retain one enrolled window");
          read connection scoped;
          let selected = script_selection host (snapshot connection id) in
          let receipt =
            C.Session_handle.start_run
              handle
              ~generation:selected.generation
              ~expected_revision:selected.revision
              ~mode:Workflow
              ~input:Authored_start
            |> protocol_ok
          in
          let views = C.Run_views.create connection ~server_id in
          let rec await_terminal () =
            match
              C.Run_views.lookup views { session; run_id = receipt.run_id } |> protocol_ok
            with
            | Available view ->
              (match view.run.lifecycle with
               | Terminal (Completed _) -> ()
               | Terminal _ -> failwith "authored seed start failed"
               | Admitted | Active | Waiting _ ->
                 Eio.Fiber.yield ();
                 await_terminal ())
            | Unavailable _ -> failwith "authored seed run lost its original receipt"
          in
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. await_terminal;
          read connection scoped;
          C.Session_handle.close handle);
        check_history Ready));
  print_endline
    "same imported occurrence across local enrollment, reopen and authored start";
  [%expect
    {| same imported occurrence across local enrollment, reopen and authored start |}]
;;
