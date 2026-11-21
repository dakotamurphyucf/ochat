open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded
module A = Agent_session
module S = Agent_store
module Ledger = A.Recovery_window_ledger

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

let daemon_options =
  { Agent_server.Daemon.default_options with
    inference_policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
        failwith "seed enrollment or authored fixture invoked a provider")
  }
;;

let with_host env options ~f =
  Eio.Switch.run (fun sw ->
    E.with_local_host
      ~sw
      ~env
      ~daemon_options
      ~root_invocation:Headless
      options
      ~f:(fun host ->
        let connection = E.host_connection host in
        initialize connection;
        Ok (f sw host connection))
    |> protocol_ok)
;;

let with_writer env sw connection id ~f =
  let handle =
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
  Exn.protect ~f:(fun () -> f handle) ~finally:(fun () -> C.Session_handle.close handle)
;;

let lifecycle_anchor connection id ~stopped =
  let fields =
    C.Admin.get_session connection id |> protocol_ok |> P.Public.Snapshot.fields
  in
  if stopped
  then (
    match fields.session.desired_state, fields.session.observed_state with
    | Stopped, Stopped -> ()
    | _ -> failwith "seed no longer has its stopped lifecycle anchor");
  fields.lifecycle |> Option.value_exn |> P.Session_lifecycle.Observation.expected
;;

let script_selection host expected =
  let selected =
    E.session_script_selection host ~expected ~checkpoint:Eio.Fiber.check |> protocol_ok
  in
  match selected.moderator with
  | Some (Standard Moderator_v1) -> selected.session
  | _ -> failwith "seed lost the original versioned authored selection"
;;

let seed options connection row =
  let spec = E.session_spec options |> protocol_ok in
  let definition_id =
    match spec.prompt with
    | Catalog id -> id
    | _ -> failwith "embedded seed lacks catalog source"
  in
  let definition = C.Catalog.get_prompt connection definition_id |> protocol_ok in
  let code =
    P.Session_seed.Code_selection.Catalog
      { definition_id; expected_revision = Option.value_exn definition.current_revision }
  in
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
  let idempotency_key =
    P.Idempotency_key.of_string "local-recovery-seed" |> protocol_ok
  in
  let request =
    P.Session_seed.Request.create spec ~code ~transcript ~idempotency_key |> protocol_ok
  in
  (C.Session_handle.seed connection request |> protocol_ok).session_id
;;

let stored_state env storage id =
  let store_ok result =
    Result.map_error result ~f:S.Store_error.to_protocol_error |> protocol_ok
  in
  let module Persistence = A.Session_persistence in
  let max_payload_length = daemon_options.factory_limits.snapshot_payload_limit in
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
    A.Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:(fun _ -> "invalid fixture limits")
    |> Result.ok_or_failwith
  in
  Persistence.restore_snapshot ~limits snapshot.snapshot
  |> store_ok
  |> Persistence.Restored.state
;;

let await_authored_run env connection session run_id =
  let views =
    C.Run_views.create connection ~server_id:(P.Session_ref.server_id session)
  in
  let rec await_terminal () =
    match C.Run_views.lookup views { session; run_id } |> protocol_ok with
    | Available view ->
      (match view.run.lifecycle with
       | Terminal (Completed None) -> ()
       | Terminal _ -> failwith "authored seed start failed"
       | Admitted | Active | Waiting _ ->
         Eio.Fiber.yield ();
         await_terminal ())
    | Unavailable _ -> failwith "authored seed run lost its original receipt"
  in
  with_stage_timeout
    ~clock:(Eio.Stdenv.clock env)
    ~seconds:20.
    ~stage:"authored seed terminal"
    await_terminal
;;

let check_seed env storage scope row initialization =
  let id = P.Recovery_window.Scope.session scope |> P.Session_ref.session_id in
  let state = stored_state env storage id in
  let ledger = Option.value_exn state.recovery_windows in
  assert (
    A.Session_state.Runtime_initialization.equal
      state.runtime_initialization
      initialization);
  assert (List.equal P.History.equal_entry state.conversation.canonical_history [ row ]);
  assert (Option.is_none state.recovery_installed_originals);
  (match Ledger.windows ledger with
   | [ window ] ->
     assert (P.Recovery_window.Scope.equal (Ledger.Window.scope window) scope)
   | _ -> failwith "seed did not retain exactly one enrolled window");
  state.run_state
;;

let exercise_seed env storage options =
  let original = "Précis retained before startup." in
  let id =
    History_entry.Id.create ~namespace:"imported.seed" ~sequence:0
    |> Result.ok_or_failwith
  in
  let row = A.History_codec.user_text ~id original |> A.History_codec.to_protocol in
  let read connection range =
    let reply = C.Recovery_windows.read_text connection range |> protocol_ok in
    assert (String.equal reply.text original)
  in
  let scope, range =
    with_host env options ~f:(fun sw host connection ->
      let id = seed options connection row in
      let initial = lifecycle_anchor connection id ~stopped:true in
      ignore (script_selection host initial : P.Session.t);
      let scope =
        with_writer env sw connection id ~f:(fun handle ->
          C.Recovery_windows.enable
            connection
            { session = P.Session_lifecycle.Expected.reference initial
            ; attachment_id = (C.Session_handle.attachment handle).id
            ; expected_generation = P.Session_lifecycle.Expected.generation initial
            }
          |> protocol_ok)
      in
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
      read connection range;
      ignore
        (lifecycle_anchor connection id ~stopped:true : P.Session_lifecycle.Expected.t);
      scope, range)
  in
  assert (
    Option.is_none (check_seed env storage scope row (Pending { fresh_history = false })));
  let session = P.Recovery_window.Scope.session scope in
  let id = P.Session_ref.session_id session in
  let run_id =
    with_host env options ~f:(fun sw host connection ->
      E.select_session host (lifecycle_anchor connection id ~stopped:true) |> protocol_ok;
      read connection range;
      with_writer env sw connection id ~f:(fun handle ->
        let selected =
          script_selection host (lifecycle_anchor connection id ~stopped:false)
        in
        let receipt =
          C.Session_handle.start_run
            handle
            ~generation:selected.generation
            ~expected_revision:selected.revision
            ~mode:Workflow
            ~input:Authored_start
          |> protocol_ok
        in
        await_authored_run env connection session receipt.run_id;
        read connection range;
        receipt.run_id))
  in
  match
    check_seed env storage scope row Ready |> Option.value_exn |> A.Run_state.runs
  with
  | [ run ] -> assert (P.Id.Run.equal run.id run_id)
  | _ -> failwith "seed admitted more than its one explicit authored startup"
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
        exercise_seed
          env
          storage
          { prompt_file
          ; workspace = root
          ; tool_dir = root
          ; home = Some root
          ; storage = Durable storage
          ; start_immediately = false
          ; permission_profile = E.default_permission_profile
          ; attachment_mode = Owner_read_write
          ; event_capacity = 32
          }));
  print_endline
    "same imported occurrence across local enrollment, reopen and authored start";
  [%expect
    {| same imported occurrence across local enrollment, reopen and authored start |}]
;;
