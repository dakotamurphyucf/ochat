open Core
module F = Crash_recovery_fixture
module Config_fixture = Support.Config_fixture
module Process_manager = Support.Process_manager

let configure env fixture =
  F.write
    env
    (Config_fixture.prompt_path fixture)
    "<developer>Invoke append_to_file once.</developer>\n\
     <tool name=\"append_to_file\"><write path=\"${workspace}\"/></tool>";
  let config =
    Config_fixture.configuration fixture ()
    |> String.substr_replace_all
         ~pattern:"(tool_default deny)"
         ~with_:"(tool_default allow)"
  in
  F.write env (Config_fixture.config_path fixture) config
;;

let start_session client created =
  let attachment =
    (Option.value_exn created.Agent_protocol.Public.Result.Create.attachment).attachment
  in
  ignore
    (F.request
       client
       (Session_start
          { session_id = created.session.id
          ; attachment_id = attachment.id
          ; queue_if_limited = false
          ; idempotency_key = F.key "crash-effect:start"
          })
     : Agent_protocol.Method_result.t);
  attachment
;;

let send client session_id attachment_id =
  match
    F.request
      client
      (Session_send_message
         { session_id
         ; attachment_id
         ; content =
             { kind = Plain_text; text = "append the marker once"; attachments = [] }
         ; idempotency_key = F.key "crash-effect:send"
         ; timing = Agent_protocol.Pending_input.Timing.Safe_boundary
         })
  with
  | Session_send_message sent -> sent
  | _ -> F.fail "side-effect message returned wrong result"
;;

let with_host env environment fixture marker f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"side-effect"
        ~arguments:[ "side-effect"; Config_fixture.config_path fixture; marker ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "crash-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let assert_unknown (snapshot : Agent_protocol.Public.Snapshot.Fields.t) =
  F.require
    (Option.is_some snapshot.session.active_operation)
    "unknown effect had no durable active operation";
  F.require
    (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
       Support.Public_view.has_header entry (Call Function)))
    "tool intent was not committed before its side effect";
  F.require
    (not
       (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
          Support.Public_view.has_header entry (Result Function))))
    "tool completion was committed before the crash"
;;

let assert_no_replay env child client session_id marker =
  for _ = 1 to 20 do
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
    F.require
      (String.equal (F.read env marker) "\nexecuted")
      "unknown side effect was replayed";
    F.require
      (not
         (String.is_substring
            (Process_manager.stdout child).contents
            ~substring:"crash-provider-invoked"))
      "recovery automatically invoked the provider again"
  done;
  let recovered = F.get client session_id in
  F.require
    (Option.is_none recovered.session.active_operation)
    "recovery retained a nonterminal foreground operation";
  F.require
    (List.is_empty recovered.deferred_entries)
    "recovery queued the acknowledged message for replay";
  recovered
;;

let persisted_events env fixture session_id =
  let directory = Filename.concat (F.session_directory fixture session_id) "journal" in
  Eio.Path.read_dir (F.path env directory)
  |> List.filter ~f:(String.is_suffix ~suffix:".log")
  |> List.concat_map ~f:(fun filename ->
    let id = Agent_store.Journal_segment.Id.of_filename filename |> F.store_ok in
    let segment =
      Agent_store.Journal_segment.open_existing ~env ~directory ~id |> F.store_ok
    in
    let scan =
      Agent_store.Journal_segment.scan ~env ~max_payload_length:(64 * 1024 * 1024) segment
      |> F.store_ok
    in
    scan.entries
    |> List.concat_map ~f:(fun entry ->
      if Agent_store.Frame.flags entry.frame <> 0
      then []
      else
        Agent_store.Frame.payload entry.frame
        |> Agent_store.Transaction.decode
        |> F.store_ok
        |> Agent_session.Session_persistence.durable_events
             ~limits:Document_schema.Limits.default
        |> F.store_ok))
;;

let assert_interrupted env fixture (before : Agent_protocol.Public.Snapshot.Fields.t) =
  let operation = Option.value_exn before.session.active_operation in
  let interrupted =
    persisted_events env fixture before.session.id
    |> List.filter_map ~f:(fun event ->
      let payload =
        Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
        |> F.protocol_ok
      in
      match payload with
      | Operation_interrupted interrupted
        when Agent_protocol.Id.Operation.compare interrupted.id operation.id = 0 ->
        Some interrupted
      | _ -> None)
  in
  match interrupted with
  | [ ({ state = Interrupted { reason; _ }; _ } as operation) ] ->
    F.require (not (String.is_empty reason)) "persisted interruption has no explanation";
    operation
  | _ ->
    F.fail
      "recovery did not durably classify the exact operation as interrupted exactly once"
;;

let retained_interruption env fixture session_id operation_id =
  let installed =
    Agent_store.Snapshot.load_current
      ~env
      ~directory:(Filename.concat (F.session_directory fixture session_id) "snapshot")
      ~max_payload_length:(64 * 1024 * 1024)
    |> F.store_ok
    |> Option.value_exn
  in
  let state =
    Agent_session.Session_persistence.restore_snapshot
      ~limits:Document_schema.Limits.default
      installed.snapshot
    |> F.store_ok
    |> Agent_session.Session_persistence.Restored.state
  in
  F.require (Option.is_none state.active_operation) "checkpoint retained active operation";
  let ledger =
    Agent_session.Inference_ledger.to_document state.inference_ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum ([%sexp_of: Agent_session.Inference_ledger.Error.t] error))
    |> Result.ok_or_failwith
    |> Document_schema.Document.payload
  in
  let turns =
    match Jsonaf.member_exn "turns" ledger with
    | `Array turns -> turns
    | _ -> F.fail "validated inference ledger has no retained turns"
  in
  let exact =
    List.filter_map turns ~f:(fun turn ->
      let operation =
        Jsonaf.member_exn "operation" turn
        |> Agent_protocol.Operation.of_json
        |> F.protocol_ok
      in
      Option.some_if
        (Agent_protocol.Id.Operation.equal operation.id operation_id)
        operation)
  in
  match exact with
  | [ ({ state = Interrupted { reason; _ }; _ } as operation) ] ->
    F.require (not (String.is_empty reason)) "checkpoint interruption has no explanation";
    operation
  | _ -> F.fail "checkpoint lost the exact retained operation interruption"
;;

let test env environment =
  let fixture = F.fixture env environment "crash-unknown-side-effect" in
  configure env fixture;
  let marker =
    Filename.concat (Config_fixture.physical_workspace fixture) "crash-effect-marker"
  in
  let before =
    with_host env environment fixture marker (fun child client ->
      let created = F.create_session client in
      let attachment = start_session client created in
      let sent = send client created.session.id attachment.id in
      F.await_marker env child "crash-side-effect-written";
      F.require
        (String.equal (F.read env marker) "\nexecuted")
        "tool side effect did not happen exactly once";
      let snapshot = F.get client created.session.id in
      assert_unknown snapshot;
      F.require
        (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
           Agent_protocol.History.Id.compare entry.id sent.history_id = 0))
        "acknowledged message ID is absent at crash";
      F.kill env child;
      snapshot)
  in
  let recovered_history = ref None in
  let durable_interruption = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture marker (fun child client ->
      let recovered = assert_no_replay env child client before.session.id marker in
      let prefix, appended =
        List.split_n
          recovered.canonical_history.entries
          (List.length before.canonical_history.entries)
      in
      F.require_equal
        "unknown-effect original canonical history"
        [%sexp_of: Agent_protocol.Public.History.Window.t]
        before.canonical_history
        { recovered.canonical_history with entries = prefix };
      (* Recovery closes the persisted invocation without replaying its uncertain
         effect. The cancellation acknowledgement is appended once, never a
         fabricated successful result or a replacement for the original call. *)
      (match appended with
       | [ entry ] ->
         let payload = Support.Public_view.full_payload entry in
         (match
            History_entry.Payload.Semantic.view (History_entry.Payload.semantic payload)
          with
          | Result { kind = Function; output = Text encoded; _ }
            when History_entry.Payload.Presence.equal
                   String.equal
                   (History_entry.Payload.Semantic.metadata
                      (History_entry.Payload.semantic payload))
                     .call_id
                   (Value "crash-unknown-call") ->
            (match
               Agent_protocol.Invocation.outcome_of_json (Jsonaf.of_string encoded)
               |> F.protocol_ok
             with
             | Cancelled reason ->
               F.require
                 (not (String.is_empty reason))
                 "unknown invocation cancellation has no explanation"
             | _ -> F.fail "unknown invocation recovery fabricated a tool result")
          | _ -> F.fail "recovery output belongs to another tool call")
       | _ -> F.fail "recovery must append exactly one invocation interruption");
      (match !recovered_history with
       | None -> recovered_history := Some recovered.canonical_history
       | Some history ->
         F.require_equal
           "unknown-effect repeated recovery history"
           [%sexp_of: Agent_protocol.Public.History.Window.t]
           history
           recovered.canonical_history);
      F.kill env child);
    let operation = Option.value_exn before.session.active_operation in
    let retained = retained_interruption env fixture before.session.id operation.id in
    if reopen = 1
    then (
      let interrupted = assert_interrupted env fixture before in
      F.require
        (Jsonaf.exactly_equal
           (Agent_protocol.Operation.to_json interrupted)
           (Agent_protocol.Operation.to_json retained))
        "checkpoint differs from the exact committed interruption";
      durable_interruption := Some retained)
    else (
      (* The two-checkpoint retention floor may prune the original interruption
         journal segment; its exact terminal turn remains in the checkpoint. *)
      F.require
        (Jsonaf.exactly_equal
           (Agent_protocol.Operation.to_json (Option.value_exn !durable_interruption))
           (Agent_protocol.Operation.to_json retained))
        "second physical reopen changed the retained terminal operation";
      F.require
        (not
           (List.exists
              (persisted_events env fixture before.session.id)
              ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
                Agent_protocol.Event.Durable.equal_kind event.kind Operation_interrupted)))
        "second recovery appended another operation interruption")
  done
;;
