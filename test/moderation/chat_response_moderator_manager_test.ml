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
  print_s
    [%sexp
      ([ "script_id", snapshot.script_id
       ; "script_source_hash", snapshot.script_source_hash
       ; ( "queued_internal_events"
         , Int.to_string (List.length snapshot.queued_internal_events) )
       ; ( "prepended_items"
         , Int.to_string (List.length snapshot.overlay.prepended_system_items) )
       ; "appended_items", Int.to_string (List.length snapshot.overlay.appended_items)
       ; "replacements", Int.to_string (List.length snapshot.overlay.replacements)
       ; "deleted_items", Int.to_string (List.length snapshot.overlay.deleted_item_ids)
       ]
       : (string * string) list)]
;;

let input_text text = Openai.Responses.Input_message.Text { text; _type = "input_text" }

let output_text text =
  Openai.Responses.Output_message.{ annotations = []; text; _type = "output_text" }
;;

let printable_item (item : Moderation.Item.t) : string =
  let response_item = ok_or_fail (Moderation.Item.to_response_item item) in
  match response_item with
  | Openai.Responses.Item.Input_message msg ->
    let role = Openai.Responses.Input_message.role_to_string msg.role in
    let content =
      List.filter_map msg.content ~f:(function
        | Openai.Responses.Input_message.Text { text; _ } -> Some text
        | Openai.Responses.Input_message.Image { image_url; _ } ->
          Some (Printf.sprintf "<image src=\"%s\" />" image_url))
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s %s %S" item.id role content
  | Openai.Responses.Item.Output_message msg ->
    let content =
      List.map msg.content ~f:(fun part -> part.text) |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s assistant %S" item.id content
  | Openai.Responses.Item.Function_call call ->
    Printf.sprintf
      "%s assistant %S"
      item.id
      (Printf.sprintf "%s %s" call.name call.arguments)
  | Openai.Responses.Item.Custom_tool_call call ->
    Printf.sprintf "%s assistant %S" item.id (Printf.sprintf "%s %s" call.name call.input)
  | Openai.Responses.Item.Function_call_output out ->
    let content =
      match out.output with
      | Openai.Responses.Tool_output.Output.Text text -> text
      | Content parts ->
        List.map parts ~f:(function
          | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
          | Input_image { image_url; _ } ->
            Printf.sprintf "<image src=\"%s\" />" image_url)
        |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s tool %S" item.id content
  | Openai.Responses.Item.Custom_tool_call_output out ->
    let content =
      match out.output with
      | Openai.Responses.Tool_output.Output.Text text -> text
      | Content parts ->
        List.map parts ~f:(function
          | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
          | Input_image { image_url; _ } ->
            Printf.sprintf "<image src=\"%s\" />" image_url)
        |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s tool %S" item.id content
  | other ->
    Printf.sprintf
      "%s json %S"
      item.id
      (Jsonaf.to_string (Openai.Responses.Item.jsonaf_of_t other))
;;

let print_items (items : Moderation.Item.t list) =
  List.iter items ~f:(fun item -> print_endline (printable_item item))
;;

let overlay_op_name (op : Moderation.Overlay.op) : string =
  match op with
  | Prepend_system _ -> "prepend_system"
  | Append_item _ -> "append_item"
  | Replace_item _ -> "replace_item"
  | Delete_item _ -> "delete_item"
  | Halt _ -> "halt"
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
          Task.bind(Turn.append_item(Item.output_text_message("synthetic-1", "queued")), fun ignored_turn ->
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
  let registry, compiled =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script)
  in
  print_s [%sexp (Manager.Registry.artifact_count registry : int)];
  compiled
;;

let capabilities = Moderation.Capabilities.default

let%expect_test "registry caches compiled moderator scripts by source hash" =
  let registry, _ =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script)
  in
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
  print_s [%sexp (List.map outcome.overlay_ops ~f:overlay_op_name : string list)];
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
  print_s
    [%sexp
      (List.map drained ~f:(fun outcome ->
         List.map outcome.overlay_ops ~f:overlay_op_name)
       : string list list)];
  let items = Manager.effective_items manager [] in
  print_items items;
  let snapshot = ok_or_fail (Manager.snapshot manager) in
  show_snapshot snapshot;
  let restored = ok_or_fail (Manager.create ~artifact ~capabilities ~snapshot ()) in
  show_snapshot (ok_or_fail (Manager.snapshot restored));
  [%expect
    {|
    1
    (prepend_system)
    ((append_item))
    moderation-overlay-1 system "policy"
    synthetic-1 assistant "queued"
    ((script_id main) (script_source_hash 720bb598084b1f2609fb3ce5ac5d8787)
     (queued_internal_events 0) (prepended_items 1) (appended_items 1)
     (replacements 0) (deleted_items 0))
    ((script_id main) (script_source_hash 720bb598084b1f2609fb3ce5ac5d8787)
     (queued_internal_events 0) (prepended_items 1) (appended_items 1)
     (replacements 0) (deleted_items 0))
    |}]
;;

let%expect_test "manager rejects mismatched persisted script metadata" =
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let snapshot =
    ok_or_fail (Manager.snapshot (ok_or_fail (Manager.create ~artifact ~capabilities ())))
  in
  let bad_snapshot = { snapshot with script_source_hash = "other-hash" } in
  (match Manager.create ~artifact ~capabilities ~snapshot:bad_snapshot () with
   | Ok _ -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  [%expect
    {| Moderator snapshot source hash "other-hash" does not match prompt source hash "720bb598084b1f2609fb3ce5ac5d8787". |}]
;;

let%expect_test "manager restores queued internal events from persisted snapshots" =
  let artifact = artifact () in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  ignore
    (ok_or_fail
       (Manager.handle_event
          manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_start)
     : Moderation.Outcome.t);
  let snapshot = ok_or_fail (Manager.snapshot manager) in
  print_s [%sexp (List.length snapshot.queued_internal_events : int)];
  let restored = ok_or_fail (Manager.create ~artifact ~capabilities ~snapshot ()) in
  let drained =
    ok_or_fail
      (Manager.drain_internal_events
         restored
         ~session_id:"session-1"
         ~now_ms:2
         ~history:[]
         ~available_tools:[]
         ~session_meta:`Null)
  in
  print_s
    [%sexp
      (List.map drained ~f:(fun outcome ->
         List.map outcome.overlay_ops ~f:overlay_op_name)
       : string list list)];
  print_items (Manager.effective_items restored []);
  print_s
    [%sexp ((ok_or_fail (Manager.snapshot restored)).current_state : Session.Snapshot.t)];
  [%expect
    {|
    1
    1
    ((append_item))
    moderation-overlay-1 system "policy"
    synthetic-1 assistant "queued"
    (Record ((count (Int 2))))
    |}]
;;

let%expect_test "manager applies prepend, replace, delete, and append overlay ops" =
  let history =
    [ Openai.Responses.Item.Input_message
        { role = Openai.Responses.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ; Openai.Responses.Item.Output_message
        { role = Openai.Responses.Output_message.Assistant
        ; id = "msg-1"
        ; content = [ output_text "Done" ]
        ; status = "completed"
        ; phase = None
        ; _type = "message"
        }
    ]
  in
  let source =
    {|
      type state = { seen : int }
      type event = [ `Turn_start ]

      let initial_state = { seen = 0 }

      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Turn_start ->
            Task.bind(Turn.prepend_system("policy"), fun ignored_prepend ->
            Task.bind
              (Turn.replace_item
                 ("msg-1", Item.output_text_message("msg-1", "rewritten")),
               fun ignored_replace ->
               Task.bind(Turn.delete_item("host-message-1"), fun ignored_delete ->
               Task.bind
                 (Turn.append_item(Item.output_text_message("synthetic-1", "after")),
                  fun ignored_append ->
                  Task.pure(st)))))
    |}
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  ignore
    (ok_or_fail
       (Manager.handle_event
          manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  print_items (Manager.effective_items manager history);
  [%expect
    {|
    moderation-overlay-1 system "policy"
    msg-1 assistant "rewritten"
    synthetic-1 assistant "after"
    |}]
;;

let%expect_test "manager item helpers expose structured item accessors" =
  let history =
    [ Openai.Responses.Item.Input_message
        { role = Openai.Responses.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let source =
    {|
      type state = { seen : int }
      type event = [ `Turn_start ]

      let initial_state = { seen = 0 }

      let first_text : string array -> string =
        fun parts ->
          if Array.length(parts) == 0 then "" else Array.get(parts, 0)

      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Turn_start ->
            let item = ctx.items[0] in
            let summary =
              Item.id(item)
              ++ ":"
              ++ Option.get_or(Item.kind(item), "missing")
              ++ ":"
              ++ Option.get_or(Item.role(item), "missing")
              ++ ":"
              ++ first_text(Item.text_parts(item))
            in
            Task.bind(Turn.append_item(Item.create("copy-1", Item.value(item))), fun ignored_copy ->
            Task.bind(Turn.append_item(Item.output_text_message("summary-1", summary)), fun ignored_summary ->
            Task.bind(Turn.append_item(Item.input_text_message("summary-2", "system", "guard")), fun ignored_guard ->
            Task.pure(st))))
    |}
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  ignore
    (ok_or_fail
       (Manager.handle_event
          manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  print_items (Manager.effective_items manager history);
  [%expect
    {|
    host-message-1 user "Hello"
    copy-1 user "Hello"
    summary-1 assistant "host-message-1:message:user:Hello"
    summary-2 system "guard"
    |}]
;;

let%expect_test "manager rolls back overlay and state after failed moderation tasks" =
  let history =
    [ Openai.Responses.Item.Input_message
        { role = Openai.Responses.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let source =
    {|
      type state = { count : int }
      type event = [ `Turn_start ]

      let initial_state = { count = 0 }

      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Turn_start ->
            Task.bind(Turn.prepend_system("policy"), fun ignored_turn ->
            Task.bind(Tool.call("echo", `String("payload")), fun ignored_call ->
            Task.pure({ count = st.count + 1 })))
    |}
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  (match
     Manager.handle_event
       manager
       ~session_id:"session-1"
       ~now_ms:1
       ~history
       ~available_tools:[]
       ~session_meta:`Null
       ~event:Moderation.Event.Turn_start
   with
   | Ok _ -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  print_items (Manager.effective_items manager history);
  print_s
    [%sexp ((ok_or_fail (Manager.snapshot manager)).current_state : Session.Snapshot.t)];
  [%expect
    {|
    Tool.call is not configured
    host-message-1 user "Hello"
    (Record ((count (Int 0))))
    |}]
;;

let entry_history allocator items =
  List.map items ~f:(History_entry.create ~allocator) |> Result.all |> ok_or_fail
;;

let%expect_test "entry moderation preserves target identity and persists provenance" =
  let allocator =
    History_entry.Allocator.create ~namespace:"moderation-session" ~next_sequence:0
    |> ok_or_fail
  in
  let history =
    entry_history
      allocator
      [ Openai.Responses.Item.Input_message
          { role = Openai.Responses.Input_message.User
          ; content = [ input_text "Hello" ]
          ; _type = "message"
          }
      ]
  in
  let target = History_entry.id (List.hd_exn history) |> History_entry.Id.to_string in
  let source =
    Printf.sprintf
      {|
        type state = { seen : int }
        type event = [ `Turn_start ]
        let initial_state = { seen = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.prepend_system("policy"), fun ignored_prepend ->
              Task.bind(Turn.replace_item("%s", Item.output_text_message("provider-label", "rewritten")), fun ignored_replace ->
              Task.pure(st)))
      |}
      target
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  ignore
    (ok_or_fail
       (Manager.handle_event_entries
          manager
          ~session_id:"moderation-session"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  let effective = Manager.effective_entries manager history in
  List.iter effective ~f:(fun effective ->
    print_s
      [%sexp
        (( History_entry.Id.to_string (History_entry.id effective.entry)
         , effective.provenance )
         : string * Moderation.Effective_entry.provenance)]);
  let snapshot = ok_or_fail (Manager.identity_snapshot manager) in
  print_s
    [%sexp
      (( snapshot.revision
       , snapshot.next_change_id
       , List.length snapshot.prepended_items
       , List.length snapshot.replacements
       , History_entry.Allocator.next_sequence allocator )
       : int * int * int * int * int)];
  [%expect
    {|
    (18:moderation-session:1 (Moderator_inserted (change_id 0)))
    (18:moderation-session:0
     (Moderator_replacement (target_id 18:moderation-session:0) (change_id 1)))
    (1 2 1 1 2)
    |}]
;;

let%expect_test "failed identity validation commits no runtime or overlay state" =
  let allocator =
    History_entry.Allocator.create ~namespace:"moderation-session" ~next_sequence:0
    |> ok_or_fail
  in
  let unknown =
    History_entry.Id.create ~namespace:"moderation-session" ~sequence:99
    |> ok_or_fail
    |> History_entry.Id.to_string
  in
  let source =
    Printf.sprintf
      {|
        type state = { count : int }
        type event = [ `Turn_start ]
        let initial_state = { count = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.prepend_system("must-not-commit"), fun ignored_prepend ->
              Task.bind(Turn.delete_item("%s"), fun ignored_delete ->
              Task.pure({ count = st.count + 1 })))
      |}
      unknown
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  (match
     Manager.handle_event_entries
       manager
       ~session_id:"moderation-session"
       ~now_ms:1
       ~history:[]
       ~available_tools:[]
       ~session_meta:`Null
       ~event:Moderation.Event.Turn_start
   with
   | Ok _ -> print_endline "unexpected success"
   | Error error -> print_endline error);
  let snapshot = ok_or_fail (Manager.identity_snapshot manager) in
  print_s
    [%sexp
      (( snapshot.current_state
       , snapshot.queued_internal_events
       , snapshot.revision
       , snapshot.next_change_id
       , List.length snapshot.prepended_items
       , List.length snapshot.tombstones
       , History_entry.Allocator.next_sequence allocator )
       : Session.Snapshot.t * Session.Snapshot.t list * int * int * int * int * int)];
  [%expect
    {|
    Unknown canonical moderation target "18:moderation-session:99".
    ((Record ((count (Int 0)))) () 0 0 0 0 0)
    |}]
;;

let%expect_test "failed resumed identity validation commits no suspended transaction" =
  let allocator =
    History_entry.Allocator.create ~namespace:"moderation-session" ~next_sequence:0
    |> ok_or_fail
  in
  let unknown =
    History_entry.Id.create ~namespace:"moderation-session" ~sequence:99
    |> ok_or_fail
    |> History_entry.Id.to_string
  in
  let source =
    Printf.sprintf
      {|
        type state = { count : int }
        type event = [ `Turn_start ]
        let initial_state = { count = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.prepend_system("must-not-commit"), fun ignored_prepend ->
              Task.bind(Approval.ask_text("continue?"), fun answer ->
              Task.bind(Turn.delete_item("%s"), fun ignored_delete ->
              Task.pure({ count = st.count + 1 }))))
      |}
      unknown
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail
      (Manager.Registry.compile_script
         ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
         Manager.Registry.empty
         script)
    |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  ignore
    (ok_or_fail
       (Manager.handle_event_entries
          manager
          ~session_id:"moderation-session"
          ~now_ms:1
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  (match Manager.resume_ui_request manager ~response:"yes" with
   | Ok _ -> print_endline "unexpected success"
   | Error error -> print_endline error);
  let snapshot = ok_or_fail (Manager.identity_snapshot manager) in
  print_s
    [%sexp
      (( snapshot.current_state
       , snapshot.queued_internal_events
       , snapshot.revision
       , snapshot.next_change_id
       , List.length snapshot.prepended_items
       , List.length snapshot.tombstones
       , History_entry.Allocator.next_sequence allocator )
       : Session.Snapshot.t * Session.Snapshot.t list * int * int * int * int * int)];
  [%expect
    {|
    Unknown canonical moderation target "18:moderation-session:99".
    ((Record ((count (Int 0)))) () 0 0 0 0 0)
    |}]
;;

let operation_name = function
  | Moderation.Overlay_change.Inserted _ -> "inserted"
  | Replaced _ -> "replaced"
  | Deleted _ -> "deleted"
  | Halted _ -> "halted"
;;

let%expect_test "committed identity moderation emits one atomic change" =
  let allocator =
    History_entry.Allocator.create ~namespace:"change-session" ~next_sequence:0
    |> ok_or_fail
  in
  let history =
    entry_history
      allocator
      [ Openai.Responses.Item.Input_message
          { role = Openai.Responses.Input_message.User
          ; content = [ input_text "Hello" ]
          ; _type = "message"
          }
      ]
  in
  let target = History_entry.id (List.hd_exn history) |> History_entry.Id.to_string in
  let source =
    Printf.sprintf
      {|type state = { seen : int }
        type event = [ `Turn_start ]
        let initial_state = { seen = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.prepend_system("policy"), fun ignored_prepend ->
              Task.bind(Turn.replace_item("%s", Item.output_text_message("label", "rewritten")), fun ignored_replace ->
              Task.pure(st)))|}
      target
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  let wakeups = ref 0 in
  let subscription =
    Manager.subscribe_committed_changes manager ~on_wakeup:(fun () -> Int.incr wakeups)
  in
  ignore
    (ok_or_fail
       (Manager.handle_event_entries
          manager
          ~session_id:"change-session"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  let changes = Manager.drain_committed_changes subscription in
  List.iter changes ~f:(fun change ->
    print_s
      [%sexp
        (( change.revision
         , List.map change.operations ~f:operation_name
         , List.map change.affected_entry_ids ~f:History_entry.Id.to_string
         , List.map change.allocated_inserted_ids ~f:History_entry.Id.to_string
         , change.script_id
         , Moderation.Phase.to_string change.phase
         , change.visible_history_changed )
         : int * string list * string list * string list * string * string * bool)]);
  print_s [%sexp ((!wakeups, Manager.overlay_revision manager) : int * int)];
  [%expect
    {|
    (1 (inserted replaced) (14:change-session:1 14:change-session:0)
     (14:change-session:1) main turn_start true)
    (1 1)
    |}]
;;

let%expect_test "failed identity transaction emits no committed change" =
  let allocator =
    History_entry.Allocator.create ~namespace:"change-session" ~next_sequence:0
    |> ok_or_fail
  in
  let unknown =
    History_entry.Id.create ~namespace:"change-session" ~sequence:99
    |> ok_or_fail
    |> History_entry.Id.to_string
  in
  let source =
    Printf.sprintf
      {|type state = { count : int }
        type event = [ `Turn_start ]
        let initial_state = { count = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.delete_item("%s"), fun ignored -> Task.pure(st))|}
      unknown
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  let subscription =
    Manager.subscribe_committed_changes manager ~on_wakeup:(fun () -> failwith "wakeup")
  in
  ignore
    (Manager.handle_event_entries
       manager
       ~session_id:"change-session"
       ~now_ms:1
       ~history:[]
       ~available_tools:[]
       ~session_meta:`Null
       ~event:Moderation.Event.Turn_start
     : (Moderation.Outcome.t, string) result);
  print_s
    [%sexp
      (( List.length (Manager.drain_committed_changes subscription)
       , Manager.overlay_revision manager
       , History_entry.Allocator.next_sequence allocator )
       : int * int * int)];
  [%expect {| (0 0 0) |}]
;;

let%expect_test "approval resume emits one combined change and restore does not replay" =
  let allocator =
    History_entry.Allocator.create ~namespace:"approval-change" ~next_sequence:0
    |> ok_or_fail
  in
  let source =
    {|type state = { approved : bool }
      type event = [ `Turn_start ]
      let initial_state = { approved = false }
      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Turn_start ->
            Task.bind(Turn.prepend_system("before"), fun ignored_before ->
            Task.bind(Approval.ask_text("continue?"), fun answer ->
            Task.bind(Turn.prepend_system(answer), fun ignored_after ->
            Task.pure({ approved = true }))))|}
  in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail
      (Manager.Registry.compile_script
         ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
         Manager.Registry.empty
         script)
    |> snd
  in
  let manager =
    ok_or_fail (Manager.create_entries ~artifact ~capabilities ~allocator ())
  in
  let subscription =
    Manager.subscribe_committed_changes manager ~on_wakeup:(fun () -> ())
  in
  ignore
    (ok_or_fail
       (Manager.handle_event_entries
          manager
          ~session_id:"approval-change"
          ~now_ms:1
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Turn_start)
     : Moderation.Outcome.t);
  print_s
    [%sexp
      (( List.length (Manager.drain_committed_changes subscription)
       , Option.is_some (Manager.pending_ui_request manager)
       , Manager.overlay_revision manager )
       : int * bool * int)];
  ignore
    (ok_or_fail (Manager.resume_ui_request manager ~response:"after")
     : Moderation.Outcome.t list);
  let changes = Manager.drain_committed_changes subscription in
  print_s
    [%sexp
      (( List.length changes
       , List.map changes ~f:(fun change -> List.length change.operations)
       , Manager.overlay_revision manager )
       : int * int list * int)];
  let snapshot = ok_or_fail (Manager.identity_snapshot manager) in
  let restored_allocator =
    History_entry.Allocator.create
      ~namespace:"approval-change"
      ~next_sequence:(History_entry.Allocator.next_sequence allocator)
    |> ok_or_fail
  in
  let restored =
    ok_or_fail
      (Manager.create_entries
         ~artifact
         ~capabilities
         ~allocator:restored_allocator
         ~snapshot
         ())
  in
  let restored_subscription =
    Manager.subscribe_committed_changes restored ~on_wakeup:(fun () -> ())
  in
  print_s
    [%sexp
      (( List.length (Manager.drain_committed_changes restored_subscription)
       , Manager.overlay_revision restored
       , List.length (Manager.effective_entries restored []) )
       : int * int * int)];
  [%expect
    {|
    (0 true 0)
    (1 (2) 1)
    (0 1 2)
    |}]
;;

let serialized_script =
  let source =
    {|type state = { count : int }
      type event = [ `Session_start | `Turn_start ]
      let initial_state = { count = 0 }
      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Session_start ->
            Task.bind(Process.run("gate", []), fun ignored_gate ->
            Task.pure({ count = st.count + 1 }))
          | `Turn_start ->
            Task.pure({ count = st.count + 10 })|}
  in
  CM.
    { id = "serialized"; language = "chatml"; kind = "moderator"; source = Inline source }
;;

let handle_serialized_event manager event =
  Manager.handle_event
    manager
    ~session_id:"serialized"
    ~now_ms:1
    ~history:[]
    ~available_tools:[]
    ~session_meta:`Null
    ~event
;;

let%expect_test "manager serializes events while a process handler is suspended" =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let first_done, first_done_u = Eio.Promise.create () in
  let second_done, second_done_u = Eio.Promise.create () in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty serialized_script)
    |> snd
  in
  let on_process_run _session ~command:_ ~args:_ =
    Eio.Promise.resolve entered_u ();
    Eio.Promise.await release;
    Ok "released"
  in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ~on_process_run ()) in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Promise.resolve
      first_done_u
      (handle_serialized_event manager Moderation.Event.Session_start));
  Eio.Promise.await entered;
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Promise.resolve
      second_done_u
      (handle_serialized_event manager Moderation.Event.Turn_start));
  Eio.Fiber.yield ();
  print_s [%sexp (Eio.Promise.is_resolved second_done : bool)];
  Eio.Promise.resolve release_u ();
  ignore (ok_or_fail (Eio.Promise.await first_done) : Moderation.Outcome.t);
  ignore (ok_or_fail (Eio.Promise.await second_done) : Moderation.Outcome.t);
  let snapshot = ok_or_fail (Manager.snapshot manager) in
  print_s [%sexp (snapshot.current_state : Session.Snapshot.t)];
  [%expect
    {|
    false
    (Record ((count (Int 11))))
    |}]
;;
