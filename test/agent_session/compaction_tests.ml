open Core
open Fixtures

let%expect_test "compaction atomically replaces history and advances its generation" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let attachment_id =
        Agent_protocol.Id.Attachment.of_string "att_actor_compaction" |> protocol_ok
      in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id = (fun () -> attachment_id)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      Agent_session.Session_actor.start actor ~attachment_id:attachment.id
      |> protocol_ok
      |> ignore;
      let initial_entry =
        Agent_session.History_codec.user_text ~id:history_id "remember this"
        |> Agent_session.History_codec.to_protocol
      in
      Agent_session.Session_actor.append_history
        actor
        ~attachment_id:attachment.id
        [ initial_entry ]
      |> protocol_ok
      |> ignore;
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let stale_revision_rejected =
        Agent_session.Session_actor.compact
          actor
          ~attachment_id:attachment.id
          ~expected_revision:(Some Int64.(before.counters.revision - 1L))
        |> Result.is_error
      in
      Agent_session.Session_actor.compact
        actor
        ~attachment_id:attachment.id
        ~expected_revision:(Some before.counters.revision)
      |> protocol_ok
      |> ignore;
      let state = await_idle actor in
      let reminder = List.hd_exn state.conversation.canonical_history in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_revision_rejected : bool
          ; history_count = (List.length state.conversation.canonical_history : int)
          ; compaction_generation = (state.conversation.compaction_generation : int)
          ; high_water = (state.conversation.next_history_sequence : int64)
          ; reminder_namespace = (History_entry.Id.namespace reminder.id : string)
          ; active_operation = (Option.is_some state.active_operation : bool)
          }]));
  [%expect
    {|
    ((stale_revision_rejected true) (history_count 1) (compaction_generation 1)
     (high_water 1) (reminder_namespace ses_agent_session_test)
     (active_operation false))
    |}]
;;

let compaction_cancel_state workspace_instance =
  let initial =
    actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
  in
  let entry =
    Agent_session.History_codec.user_text ~id:history_id "preserve cancelled history"
    |> Agent_session.History_codec.to_protocol
  in
  { initial with
    conversation = { initial.conversation with canonical_history = [ entry ] }
  }
;;

let compaction_cancel_actor ~sw env workspace_instance state_committed =
  let services : Agent_session.Session_actor.services =
    { now = (fun () -> timestamp)
    ; create_attachment_id =
        (fun () ->
          Agent_protocol.Id.Attachment.of_string "att_compact_cancel" |> protocol_ok)
    ; create_reclaim_token = (fun () -> "compaction-cancel-reclaim")
    ; state_committed
    }
  in
  Agent_session.Session_actor.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~mailbox_capacity:32
    ~compaction_env:None
    ~initial_state:(compaction_cancel_state workspace_instance)
    ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
    ~operation_worker:None
    ~services
;;

let compaction_cancel_events recorded started _state events =
  recorded := !recorded @ events;
  List.iter events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> protocol_ok
    with
    | Operation_started operation ->
      Eio.Promise.resolve started operation.id;
      Eio.Fiber.yield ()
    | _ -> ())
;;

let compaction_cancel_on_start ~sw actor attachment_id started =
  Eio.Fiber.fork ~sw (fun () ->
    let operation_id = Eio.Promise.await started in
    ignore
      (Agent_session.Session_actor.cancel_operation actor ~attachment_id ~operation_id
       |> protocol_ok
       : Agent_protocol.Session.t))
;;

let compaction_cancel_run ~sw env actor started =
  let attachment, _subscriber =
    Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
    |> protocol_ok
  in
  ignore
    (Agent_session.Session_actor.start actor ~attachment_id:attachment.id |> protocol_ok
     : Agent_protocol.Session.t);
  compaction_cancel_on_start ~sw actor attachment.id started;
  ignore
    (Agent_session.Session_actor.compact
       actor
       ~attachment_id:attachment.id
       ~expected_revision:None
     |> protocol_ok
     : Agent_protocol.Session.t);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () -> await_idle actor)
;;

let compaction_cancel_terminals events =
  List.filter_map events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> protocol_ok
    with
    | Operation_completed operation
    | Operation_cancelled operation
    | Operation_failed operation -> Some (event.kind, operation.state)
    | _ -> None)
;;

let compaction_cancel_report before after events =
  let history state =
    [%sexp_of: Agent_protocol.History.entry list]
      state.Agent_session.Session_state.conversation.canonical_history
  in
  print_s
    [%sexp
      { history_preserved = (Sexp.equal (history before) (history after) : bool)
      ; compaction_generation = (after.conversation.compaction_generation : int)
      ; terminals =
          (compaction_cancel_terminals events
           : (Agent_protocol.Event.Durable.kind * Agent_protocol.Operation.state) list)
      ; history_replaced =
          (List.exists events ~f:(fun event ->
             Agent_protocol.Event.Durable.equal_kind
               event.Agent_protocol.Event.Durable.kind
               History_replaced)
           : bool)
      ; active_operation = (Option.is_some after.active_operation : bool)
      }]
;;

let%expect_test "compaction cancellation before worker readiness preserves history" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let recorded = ref [] in
      let started, notify_started = Eio.Promise.create () in
      let actor =
        compaction_cancel_actor
          ~sw
          env
          workspace_instance
          (compaction_cancel_events recorded notify_started)
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let after = compaction_cancel_run ~sw env actor started in
      Agent_session.Session_actor.shutdown actor;
      compaction_cancel_report before after !recorded));
  [%expect
    {|
    ((history_preserved true) (compaction_generation 0)
     (terminals ((Operation_cancelled Cancelled))) (history_replaced false)
     (active_operation false))
    |}]
;;

let%expect_test "actor consumes late compaction success without changing cancelled state" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let recorded = ref [] in
      let started, notify_started = Eio.Promise.create () in
      let actor =
        compaction_cancel_actor
          ~sw
          env
          workspace_instance
          (compaction_cancel_events recorded notify_started)
      in
      let cancelled = compaction_cancel_run ~sw env actor started in
      let events_before = !recorded in
      let history =
        [ Agent_session.History_codec.user_text
            ~id:history_id
            "late summary must never replace history"
        ]
      in
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
        Agent_session.Session_actor.For_testing.deliver_compaction_result
          actor
          ~operation_id:(Eio.Promise.await started)
          ~history
        |> protocol_ok);
      let after = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { exact_state_unchanged =
              (Sexp.equal
                 ([%sexp_of: Agent_session.Session_state.t] cancelled)
                 ([%sexp_of: Agent_session.Session_state.t] after)
               : bool)
          ; exact_events_unchanged =
              (Sexp.equal
                 ([%sexp_of: Agent_protocol.Event.Durable.t list] events_before)
                 ([%sexp_of: Agent_protocol.Event.Durable.t list] !recorded)
               : bool)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect {| ((exact_state_unchanged true) (exact_events_unchanged true)) |}]
;;

let%expect_test
    "compaction archive survives memory event retention and failures terminate safely"
  =
  with_actor_workspace (fun env workspace_instance ->
    List.iter [ false; true ] ~f:(fun reject_archive ->
      Eio.Switch.run (fun sw ->
        let actor, backend =
          audit_actor ~sw ~env ~workspace_instance ~reject_archive ()
        in
        let writer, _ =
          Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
          |> protocol_ok
        in
        ignore
          (Agent_session.Session_actor.start actor ~attachment_id:writer.id |> protocol_ok
           : Agent_protocol.Session.t);
        ignore
          (Agent_session.Session_actor.compact
             actor
             ~attachment_id:writer.id
             ~expected_revision:None
           |> protocol_ok
           : Agent_protocol.Session.t);
        let state =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () -> await_idle actor)
        in
        for _ = 1 to 70 do
          let attachment, _ =
            Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:false
            |> protocol_ok
          in
          Agent_session.Session_actor.detach actor attachment.id |> protocol_ok
        done;
        let original =
          match state.conversation.compaction_archives with
          | [] ->
            List.exists state.conversation.canonical_history ~f:(fun entry ->
              Agent_protocol.History.Id.compare entry.id history_id = 0)
          | reference :: _ ->
            Option.is_some
              (Agent_session.Memory_backend.archived_state
                 backend
                 ~revision:reference.revision)
        in
        printf
          "failure=%b archives=%d original_preserved=%b idle=%b\n"
          reject_archive
          (List.length state.conversation.compaction_archives)
          original
          (Option.is_none state.active_operation);
        Agent_session.Session_actor.shutdown actor)));
  [%expect
    {|
    failure=false archives=1 original_preserved=true idle=true
    failure=true archives=0 original_preserved=true idle=true
    |}]
;;
