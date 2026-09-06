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
    (Option.value_exn created.Agent_protocol.Method_result.Create.attachment).attachment
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

let assert_unknown (snapshot : Agent_protocol.Snapshot.t) =
  F.require
    (Option.is_some snapshot.session.active_operation)
    "unknown effect had no durable active operation";
  F.require
    (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
       Agent_protocol.History.equal_kind entry.kind Tool_call))
    "tool intent was not committed before its side effect";
  F.require
    (not
       (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
          Agent_protocol.History.equal_kind entry.kind Tool_output)))
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
        |> F.store_ok))
;;

let assert_interrupted env fixture (before : Agent_protocol.Snapshot.t) =
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
  | [ { state = Interrupted { reason; _ }; _ } ] ->
    F.require (not (String.is_empty reason)) "persisted interruption has no explanation"
  | _ ->
    F.fail
      "recovery did not durably classify the exact operation as interrupted exactly once"
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
  for _ = 1 to 2 do
    with_host env environment fixture marker (fun child client ->
      let recovered = assert_no_replay env child client before.session.id marker in
      F.require_equal
        "unknown-effect canonical history"
        [%sexp_of: Agent_protocol.History.Window.t]
        before.canonical_history
        recovered.canonical_history;
      F.kill env child);
    assert_interrupted env fixture before
  done
;;
