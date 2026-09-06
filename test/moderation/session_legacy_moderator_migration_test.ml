open Core
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses

let ok_exn = function
  | Ok value -> value
  | Error error -> failwith error
;;

let snapshot_item id =
  let item =
    Res.Item.Output_message
      { role = Res.Output_message.Assistant
      ; id
      ; content = []
      ; status = "completed"
      ; phase = None
      ; _type = "message"
      }
  in
  let value =
    Chatml.Chatml_value_codec.jsonaf_to_value (Res.Item.jsonaf_of_t item)
    |> Chatml.Chatml_value_codec.Snapshot.of_value
    |> ok_exn
  in
  Session.Moderator_snapshot.Item.{ id; value }
;;

let snapshot ~replacements ~deleted_item_ids =
  let overlay =
    { Session.Moderator_snapshot.Overlay.empty with replacements; deleted_item_ids }
  in
  Session.Moderator_snapshot.create
    ~script_id:"main"
    ~script_source_hash:"hash"
    ~current_state:(Record [])
    ~queued_internal_events:[]
    ~halted:false
    ~overlay
    ()
;;

let v3 ~history ~moderator_snapshot : Session.Legacy.V3.t =
  { version = 3
  ; id = "legacy-moderation"
  ; prompt_file = "prompt.chatmd"
  ; local_prompt_copy = None
  ; history
  ; tasks = []
  ; moderator_snapshot = Some moderator_snapshot
  ; kv_store = []
  ; vfs_root = "vfs"
  }
;;

let reasoning id =
  Res.Item.Reasoning { summary = []; _type = "reasoning"; id; status = None }
;;

let replacement target_id =
  let module Overlay = Session.Moderator_snapshot.Overlay in
  Overlay.{ target_id; item = snapshot_item "replacement-label" }
;;

let print_result = function
  | Ok _ -> print_endline "ok"
  | Error error -> print_endline error
;;

let%expect_test "legacy moderator targets resolve only unique history occurrences" =
  let migrated =
    Session.V4.of_v3
      (v3
         ~history:[ reasoning "first"; reasoning "second" ]
         ~moderator_snapshot:
           (snapshot ~replacements:[ replacement "second" ] ~deleted_item_ids:[ "first" ]))
    |> ok_exn
  in
  let identity = Option.value_exn migrated.moderator_state.identity_snapshot in
  let replacement =
    let replacement : Session.V4.Moderator_state.Identity_snapshot.Replacement.t =
      List.hd_exn identity.replacements
    in
    replacement.target_id
  in
  let tombstone =
    let tombstone : Session.V4.Moderator_state.Identity_snapshot.Tombstone.t =
      List.hd_exn identity.tombstones
    in
    tombstone.target_id
  in
  print_s
    [%sexp
      ((History_entry.Id.to_string replacement, History_entry.Id.to_string tombstone)
       : string * string)];
  [%expect {| (17:legacy-moderation:1 17:legacy-moderation:0) |}]
;;

let%expect_test "legacy moderator migration rejects unknown targets" =
  let history = [ reasoning "known" ] in
  print_result
    (Session.V4.of_v3
       (v3
          ~history
          ~moderator_snapshot:
            (snapshot ~replacements:[ replacement "unknown" ] ~deleted_item_ids:[])));
  print_result
    (Session.V4.of_v3
       (v3
          ~history
          ~moderator_snapshot:(snapshot ~replacements:[] ~deleted_item_ids:[ "unknown" ])));
  [%expect
    {|
    Unknown legacy moderator target "unknown".
    Unknown legacy moderator target "unknown".
    |}]
;;

let%expect_test "legacy moderator migration rejects ambiguous targets" =
  let history = [ reasoning "same"; reasoning "same" ] in
  print_result
    (Session.V4.of_v3
       (v3
          ~history
          ~moderator_snapshot:
            (snapshot ~replacements:[ replacement "same" ] ~deleted_item_ids:[])));
  print_result
    (Session.V4.of_v3
       (v3
          ~history
          ~moderator_snapshot:(snapshot ~replacements:[] ~deleted_item_ids:[ "same" ])));
  [%expect
    {|
    Ambiguous legacy moderator target "same" matches 2 history entries.
    Ambiguous legacy moderator target "same" matches 2 history entries.
    |}]
;;

let allocator namespace =
  History_entry.Allocator.create ~namespace ~next_sequence:0 |> ok_exn
;;

let artifact () =
  let script =
    Prompt.Chat_markdown.
      { id = "main"
      ; language = "chatml"
      ; kind = "moderator"
      ; source =
          Inline
            {|
              type state = { count : int }
              type event = [ `Session_start ]
              let initial_state = { count = 0 }
              let on_event : context -> state -> event -> state task =
                fun ignored_context state ignored_event -> Task.pure(state)
            |}
      }
  in
  Manager.Registry.compile_script Manager.Registry.empty script |> ok_exn |> snd
;;

let%expect_test "Chat-TUI requires the moderator to share its live allocator" =
  let history_allocator = allocator "session" in
  let manager =
    Manager.create_entries
      ~artifact:(artifact ())
      ~capabilities:Moderation.Capabilities.default
      ~allocator:history_allocator
      ()
    |> ok_exn
  in
  let moderator =
    Chat_response.In_memory_stream.
      { manager
      ; session_id = "session"
      ; session_meta = `Null
      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
      }
  in
  let check history_allocator =
    Chat_tui.App_runtime.validate_moderator_allocator
      ~moderator:(Some moderator)
      ~history_allocator
    |> Result.is_ok
  in
  print_s
    [%sexp
      { shared = (check history_allocator : bool)
      ; reconstructed = (check (allocator "session") : bool)
      }];
  [%expect {| ((shared true) (reconstructed false)) |}]
;;
