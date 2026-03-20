open Core

module CM = Prompt.Chat_markdown
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Session = Session

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let show_snapshot (snapshot : Session.Moderator_snapshot.t) =
  print_s [%sexp (snapshot : Session.Moderator_snapshot.t)]
;;

let script_source =
  {|
    type state = { count : int }
    type event = [ `Session_start | `Internal_done(string) ]

    let initial_state = { count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Session_start ->
          Task.bind(Turn.prepend_system("policy"), fun ignored_turn ->
          Task.bind(Runtime.emit(`Internal_done("queued")), fun ignored_emit ->
          Task.pure({ count = st.count + 1 })))
        | `Internal_done(_) ->
          Task.bind
            (Turn.append_message
               ({ id = "synthetic-1"
                ; role = "assistant"
                ; content = "queued"
                ; meta = `Null
                }),
             fun ignored_turn ->
             Task.pure({ count = st.count + 1 }))
  |}
;;

let script =
  CM.
    { id = "main"
    ; language = "chatml"
    ; kind = "moderator"
    ; source = Inline script_source
    }
;;

let artifact () =
  let registry, compiled = ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) in
  print_s [%sexp (Manager.Registry.artifact_count registry : int)];
  compiled
;;

let capabilities = Moderation.Capabilities.default

let%expect_test "registry caches compiled moderator scripts by source hash" =
  let registry, _ = ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) in
  let registry, _ = ok_or_fail (Manager.Registry.compile_script registry script) in
  print_s [%sexp (Manager.Registry.artifact_count registry : int)];
  [%expect {| 1 |}]
;;

let%expect_test "manager snapshots, restores, and drains internal events" =
  let artifact = artifact () in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  let outcome =
    ok_or_fail
      (Manager.handle_event
         manager
         ~session_id:"session-1"
         ~now_ms:1
         ~history:[]
         ~available_tools:[]
         ~session_meta:`Null
         ~event:Moderation.Event.Session_start)
  in
  print_s [%sexp (outcome.overlay_ops : Moderation.Overlay.op list)];
  let drained =
    ok_or_fail
      (Manager.drain_internal_events
         manager
         ~session_id:"session-1"
         ~now_ms:2
         ~history:[]
         ~available_tools:[]
         ~session_meta:`Null)
  in
  print_s [%sexp (List.map drained ~f:(fun outcome -> outcome.overlay_ops) : Moderation.Overlay.op list list)];
  let messages = Manager.effective_messages manager [] in
  List.iter messages ~f:(fun message ->
    print_endline (Printf.sprintf "%s %s %S" message.id message.role message.content));
  let snapshot = ok_or_fail (Manager.snapshot manager) in
  show_snapshot snapshot;
  let restored = ok_or_fail (Manager.create ~artifact ~capabilities ~snapshot ()) in
  show_snapshot (ok_or_fail (Manager.snapshot restored));
  [%expect
    {|
    1
    ((Prepend_system policy))
    (((Append_message
       ((id synthetic-1) (role assistant) (content queued) (meta Null)))))
    moderation-overlay-1 system "policy"
    synthetic-1 assistant "queued"
    ((script_id main) (script_source_hash c371fe2893a6ff72c1e3fddce9be73ee)
     (current_state (Record ((count (Int 2))))) (queued_internal_events ())
     (halted false)
     (overlay
      ((prepended_system_messages
        (((id moderation-overlay-1) (role system) (content policy)
          (meta (Variant Null ())))))
       (appended_messages
        (((id synthetic-1) (role assistant) (content queued)
          (meta (Variant Null ())))))
       (replacements ()) (deleted_message_ids ()) (halted_reason ()))))
    ((script_id main) (script_source_hash c371fe2893a6ff72c1e3fddce9be73ee)
     (current_state (Record ((count (Int 2))))) (queued_internal_events ())
     (halted false)
     (overlay
      ((prepended_system_messages
        (((id moderation-overlay-1) (role system) (content policy)
          (meta (Variant Null ())))))
       (appended_messages
        (((id synthetic-1) (role assistant) (content queued)
          (meta (Variant Null ())))))
       (replacements ()) (deleted_message_ids ()) (halted_reason ()))))
    |}]
;;

let%expect_test "manager rejects mismatched persisted script metadata" =
  let artifact = ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd in
  let snapshot = ok_or_fail (Manager.snapshot (ok_or_fail (Manager.create ~artifact ~capabilities ()))) in
  let bad_snapshot =
    { snapshot with script_source_hash = "other-hash" }
  in
  (match Manager.create ~artifact ~capabilities ~snapshot:bad_snapshot () with
   | Ok _ -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  [%expect
    {| Moderator snapshot source hash "other-hash" does not match prompt source hash "c371fe2893a6ff72c1e3fddce9be73ee". |}]
;;
