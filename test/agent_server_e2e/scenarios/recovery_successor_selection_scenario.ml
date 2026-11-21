open! Core
module P = Agent_protocol
module A = Agent_session
module D = Agent_server.Daemon
module H = Agent_client.Session_handle
module Shares = Agent_server.Session_factory.Recovery_note_shares
module Notes = Recovery_note_provider_scenario
module Checkpoint = Recovery_checkpoint_scenario
module Catalog = Recovery_notes.Catalog
module Ref = A.Recovery_rollover_hint_ref
module Provider = Support.Tui_stream_provider

let ok = Agent_server_test_support.protocol_ok
let key = Crash_recovery_fixture.key
let shared_text = "handoff from the source session"
let selected_passage = "résumé"

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/><developer>Use the selected excerpts.</developer>
<tool name="recovery_note_write"/><tool name="context_checkpoint"/><tool name="context_new_window"/>
<script id="select-excerpts" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state : json = `Null
let on_event : context -> json -> moderator_event -> json task =
  fun ctx state event -> match event with
  | `Item_appended(item) ->
    (match Json.get_path(item.value, ["role"]) with
     | `Some(`String("user")) ->
       (match Json.get_path(item.value, ["content", "0", "text"]) with
        | `Some(`String(text)) -> (match Json.parse_opt(text) with
            | `Some(`Array(refs)) -> Task.pure(`Array(refs))
            | _ -> Task.pure(state))
        | _ -> Task.pure(state))
     | _ -> Task.pure(state))
  | _ -> Task.pure(state)
let on_context_rollover :
  context -> json -> context_rollover_event -> (json * context_rollover_decision) task =
  fun ctx state event -> match event with
  | `Before_context_rollover(_) -> (match state with
      | `Array(refs) -> Task.pure((|state, `Before(`Proceed({ hints = `Some(refs); continuation = `None }))|))
      | _ -> Task.fail("expected user-selected sources"))
  | `After_context_rollover(_) -> Task.pure((|state, `After(`Continue)|))
</script>|}
;;

let run env environment =
  let auth ~sw:_ _ = Openai.Responses_driver.Auth.bearer "local-selected-source-test" in
  Notes.with_fixture
    env
    environment
    ~prompt_xml:prompt
    ~recovery_capacity:(Recovery_successor_guard_scenario.config ~input:6000L ())
    ~max_sessions:2
    ~auth
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let factory = D.factory daemon in
       let session () =
         let h = Notes.open_session sw env client ~mode:Owner_read_write in
         let entry =
           Agent_server.Session_registry.find (D.registry daemon) (H.session_id h)
           |> Option.value_exn
         in
         let owner =
           P.Session_ref.create
             ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
             ~session_id:(H.session_id h)
         in
         let check _ =
           Agent_server.Session_factory.check_registered_owner
             factory
             entry
             ~session:owner
         in
         h, entry, owner, check
       in
       let src, source, owner, source_check = session () in
       let dst, target, recipient, target_check = session () in
       let state entry =
         A.Session_actor.state entry.Agent_server.Session_registry.actor |> ok
       in
       let start h = ignore (H.start h ~queue_if_limited:false |> ok : P.Session.t) in
       start src;
       let access =
         A.Recovery_note_access.operator
           proof
           ~owner
           ~generation:(state source).identity.generation
           ~attachment_id:(Some (H.attachment src).id)
           ~authorize:source_check
       in
       let note =
         A.Session_actor.mutate_recovery_note_operator
           source.actor
           ~access
           ~key:(key "create-source")
           (Create
              { expected_catalog_revision = Catalog.Catalog_revision.of_int64 0L |> ok
              ; path = Catalog.Note_path.of_string "source/memo" |> ok
              ; content = Catalog.Content.of_string shared_text |> ok
              })
         |> ok
       in
       let grant =
         Shares.create
           factory
           ~source
           ~access
           ~recipient
           ~key:(key "share-source")
           { expected_revision = Shares.management_revision factory ~source ~access |> ok
           ; expected_catalog_revision = note.catalog_revision
           ; selection = note.selection
           ; expires_at = P.Timestamp.add_ms (P.Timestamp.now ()) 120_000 |> ok
           }
         |> ok
         |> A.Recovery_note_share.Receipt.reference
       in
       let scope =
         A.Session_actor.enable_recovery_windows
           target.actor
           ~actor:proof
           ~attachment_id:(H.attachment dst).id
           ~authorize:target_check
         |> ok
       in
       start dst;
       let send text =
         H.send_message dst { kind = Plain_text; text; attachments = [] } |> ok
       in
       let row = send ("Before " ^ selected_passage ^ " after") in
       let request = Provider.await_request provider env in
       Provider.reply_summary (request 0) "Source recorded.";
       let idle () =
         Support.Tui_fixture.await env (fun () ->
           let current = state target in
           Option.some_if (Option.is_none current.active_operation) current)
       in
       let entry =
         List.find_exn (idle ()).conversation.canonical_history ~f:(fun item ->
           P.History.Id.equal row.history_id item.id)
       in
       let citation =
         P.Recovery_window.Citation.create
           scope
           ~occurrence:entry.id
           ~version:(P.Recovery_item_version.canonical entry.content_revision)
         |> ok
       in
       let start_offset = String.length "Before " in
       let range =
         P.Recovery_text_range.create
           citation
           ~part:0
           ~start:start_offset
           ~stop:(start_offset + String.length selected_passage)
           ()
         |> ok
       in
       let selection =
         Ref.Selection.create [ Ref.shared_note grant; Ref.canonical_text range |> ok ]
         |> ok
       in
       ignore
         (send (Ref.Selection.to_json selection |> Jsonaf.to_string)
          : P.Method_result.Send_message.t);
       Provider.reply_summary (request 1) "Selection captured.";
       ignore (idle () : A.Session_state.t);
       ignore (send "Use my earlier selection." : P.Method_result.Send_message.t);
       let checkpoint =
         Checkpoint.write_and_register request ~start:2 scope ~id:"select-sources"
       in
       Checkpoint.request_transition checkpoint scope ~id:"rotate";
       let successor = request 5 in
       let wire = Provider.body successor in
       Checkpoint.require_hints
         (Jsonaf.member_exn "input" wire |> Jsonaf.list_exn)
         [ Checkpoint.excerpt "shared_note" shared_text
         ; Checkpoint.excerpt "history" selected_passage
         ];
       Crash_recovery_fixture.require
         (not
            (String.is_substring
               (Jsonaf.to_string wire)
               ~substring:(P.Id.Session.to_string (H.session_id src))))
         "successor exposed the source session locator";
       Provider.reply_summary successor "Selected references received.";
       ignore (idle () : A.Session_state.t);
       H.close dst;
       H.close src)
;;
