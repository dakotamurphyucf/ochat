open! Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module State = Agent_session.Session_state
module Registry = Agent_server.Session_registry
module Catalog = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Bundle = Agent_store.Prompt_program_bundle

(* Saved immutable observations, not a runtime, original proof or execution
   capability. The second host must independently admit the real client. *)
type saved =
  { session_id : P.Id.Session.t
  ; revision_id : P.Id.Prompt_revision.t
  ; programs : Bundle.Manifest.t
  ; component_state : Session.Snapshot.t
  ; status_calls : int
  }

let component_state (state : State.t) =
  let checkpoint =
    Agent_session.Moderator_checkpoint.decode state.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  checkpoint.current_state
;;

let status_calls (state : State.t) =
  List.count state.invocations ~f:(fun invocation ->
    String.equal invocation.P.Invocation.context.tool_name "instructions_status")
;;

let program_manifest daemon revision_id =
  Catalog.find_revision (Agent_server.Daemon.prompts daemon) revision_id
  |> Option.value_exn
  |> Revision.programs
  |> Option.value_exn
  |> Chatml_prompt_programs.to_storage
  |> Bundle.manifest
;;

let capture daemon (state : State.t) =
  { session_id = state.identity.session_id
  ; revision_id = state.spec.prompt_revision_id
  ; programs = program_manifest daemon state.spec.prompt_revision_id
  ; component_state = component_state state
  ; status_calls = status_calls state
  }
;;

type terminal =
  | Completed
  | Failed

let wait_terminal env (entry : Registry.entry) ~after_sequence ~kind ~terminal =
  let module Log = Agent_session.Durable_event_log in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
    let rec wait () =
      let state = A.state entry.actor |> protocol_ok in
      let events =
        match
          Log.replay
            entry.durable_events
            ~after_sequence
            ~through_sequence:state.counters.event_sequence
        with
        | Available events -> events
        | Snapshot_required -> failwith "composed example terminal evidence expired"
      in
      let operation =
        List.find_map events ~f:(fun (event : P.Event.Durable.t) ->
          match
            P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
          with
          | Operation_completed operation
          | Operation_failed operation
          | Operation_interrupted operation
          | Operation_cancelled operation
            when P.Operation.equal_kind operation.kind kind -> Some operation
          | _ -> None)
      in
      match operation, state.active_operation with
      | Some operation, None ->
        (match terminal, operation.state with
         | Completed, Completed | Failed, Failed _ -> state
         | ( Completed
           , (Starting | Running | Cancelling | Failed _ | Cancelled | Interrupted _) )
         | ( Failed
           , (Starting | Running | Cancelling | Completed | Cancelled | Interrupted _) )
           ->
           raise_s
             [%sexp "unexpected composed example terminal", (operation : P.Operation.t)])
      | None, _ | Some _, Some _ ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        wait ()
    in
    wait ())
;;

let assert_checkpoint saved (state : State.t) ~calls =
  [%test_eq: Session.Snapshot.t] saved.component_state (component_state state);
  [%test_eq: int] calls (status_calls state);
  assert (P.Id.Prompt_revision.equal saved.revision_id state.spec.prompt_revision_id)
;;

let send env handle entry ~terminal text =
  let before = A.state entry.Registry.actor |> protocol_ok in
  H.send_message handle { kind = Plain_text; text; attachments = [] }
  |> protocol_ok
  |> ignore;
  wait_terminal
    env
    entry
    ~after_sequence:before.counters.event_sequence
    ~kind:(Turn User_submit)
    ~terminal
;;

let exercise saved ~sources ~sw ~env ~root ~configuration =
  (* The first daemon's joined shutdown has released every owner and store lock.
     Remove all ordinary example/library files, not just the original entry. *)
  List.iter sources ~f:(fun (member, _) ->
    Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / member));
  let requests = ref 0 in
  let summaries = ref 0 in
  let provider_failure = ref None in
  let post_stream ~sw:_ ~inputs =
    let text = Instruction_runtime_tests.wire_text inputs in
    if
      String.is_substring
        text
        ~substring:"your task is to compact it to save on context space"
    then (
      incr summaries;
      Authoring_compaction_tests.summary_events ())
    else (
      incr requests;
      if !requests = 2
      then failwith "controlled composed-example provider failure"
      else (
        (try
           assert (String.is_substring text ~substring:"# Composed lifecycle decisions")
         with
         | exn ->
           provider_failure := Some exn;
           raise exn);
        call_events []))
  in
  let daemon =
    Agent_server.Daemon.start
      ~sw
      ~env
      ~config:configuration
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Agent_server.Daemon.default_options with
          inference_policy = inference_policy ~default_model:"fixture-model" ~post_stream
        }
      ()
    |> protocol_ok
  in
  Exn.protect
    ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
    ~f:(fun () ->
      let entry =
        Registry.load (Agent_server.Daemon.registry daemon) saved.session_id
        |> protocol_ok
      in
      assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
      let loaded = A.state entry.actor |> protocol_ok in
      assert_checkpoint saved loaded ~calls:saved.status_calls;
      let client = Instruction_runtime_tests.authenticated_connection daemon in
      initialize client;
      let handle =
        H.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection:client
          ~session_id:saved.session_id
          ~mode:Read_write
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () ->
          H.close handle;
          Agent_client.Connection.close client)
        ~f:(fun () ->
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          let resumed =
            send env handle entry ~terminal:Completed "Resume the installed review."
          in
          Option.iter !provider_failure ~f:raise;
          assert_checkpoint saved resumed ~calls:(saved.status_calls + 1);
          assert (
            List.exists resumed.moderator_executions ~f:(fun receipt ->
              match receipt.P.Moderator_execution.context.phase, receipt.status with
              | Session_resume, Completed _ -> true
              | _ -> false));
          assert (
            Bundle.Manifest.equal
              saved.programs
              (program_manifest daemon saved.revision_id));
          H.compact handle ~expected_revision:(Some resumed.counters.revision)
          |> protocol_ok
          |> ignore;
          let compacted =
            wait_terminal
              env
              entry
              ~after_sequence:resumed.counters.event_sequence
              ~kind:Compaction
              ~terminal:Completed
          in
          [%test_eq: int] 1 !summaries;
          [%test_eq: int]
            (resumed.conversation.compaction_generation + 1)
            compacted.conversation.compaction_generation;
          assert (not (List.is_empty compacted.conversation.compaction_archives));
          assert_checkpoint saved compacted ~calls:(saved.status_calls + 1);
          let failed =
            send env handle entry ~terminal:Failed "Observe a provider failure."
          in
          [%test_eq: int] 2 !requests;
          assert_checkpoint saved failed ~calls:(saved.status_calls + 1);
          let recovered =
            send env handle entry ~terminal:Completed "Retry with a fresh turn."
          in
          Option.iter !provider_failure ~f:raise;
          [%test_eq: int] 3 !requests;
          assert_checkpoint saved recovered ~calls:(saved.status_calls + 2)))
;;
