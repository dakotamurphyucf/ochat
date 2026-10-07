open! Core
module P = History_entry.Payload
module H = Agent_protocol.Public.History
module C = Chat_tui.Conversation
module Row = Chat_tui.Projected_message

let ok = Result.ok_or_failwith

let protocol_ok result =
  Result.map_error result ~f:(fun (error : Agent_protocol.Error.t) -> error.message) |> ok
;;

let id sequence = History_entry.Id.create ~namespace:"neutral-ui" ~sequence |> ok
let payload view = P.Semantic.create view ~metadata:P.Metadata.empty |> ok |> P.authored

let text role value =
  payload
    (Message
       { form = Input
       ; role
       ; content = [ Text { text = value; annotations = []; logprobs = Absent } ]
       ; phase = Absent
       })
;;

let entry sequence value = History_entry.create_with_id ~id:(id sequence) value

let public_full sequence value =
  H.full (entry sequence value) ~provenance:Canonical |> protocol_ok
;;

let row value = C.project_public_entries [ value ] |> C.rows |> List.hd_exn
let disclose value = P.semantic value |> H.Visible.of_semantic |> Option.value_exn

let%expect_test "neutral families preserve Developer and inspectable unknown evidence" =
  let values =
    [ text System "system"
    ; text Developer "developer"
    ; text User "user"
    ; text Assistant "assistant"
    ; payload
        (Call
           { kind = Custom
           ; name = "script"
           ; namespace = Absent
           ; input_bytes = "exact\nbytes"
           ; async = Absent
           })
    ; payload
        (Result
           { relation = Unresolved
           ; kind = Custom
           ; output =
               Content
                 [ Refusal "refused"; Image { uri = "image://real"; detail = Null } ]
           })
    ; payload (Reasoning { readable_summary = [ "one"; "two" ] })
    ; payload (Unknown { provider_kind = "future_family" })
    ]
  in
  List.iter values ~f:(fun value ->
    let role, text = C.Rendered.of_payload value |> C.Rendered.message in
    printf
      "%s: %s\n"
      role
      (if String.equal role "unknown"
       then
         "inspectable="
         ^ Bool.to_string (String.is_substring text ~substring:"future_family")
       else text));
  [%expect
    {|
    system: system
    developer: developer
    user: user
    assistant: assistant
    tool: script(exact
    bytes)
    tool_output: refused
    <image src="image://real" />
    reasoning: one two
    unknown: inspectable=true |}]
;;

let%expect_test "visible parts redact opaque data without fabricating assistant context" =
  let value =
    payload
      (Message
         { form = Input
         ; role = Developer
         ; content =
             [ Text
                 { text = "readable"
                 ; annotations = [ `String "secret annotation" ]
                 ; logprobs = Value (`String "secret probability")
                 }
             ; Unknown
                 { kind = "future_part"; raw = `Object [ "secret", `String "hidden" ] }
             ]
         ; phase = Null
         })
  in
  let visible = H.visible (id 1) ~provenance:Canonical (disclose value) |> protocol_ok in
  let redacted =
    H.redacted
      (id 2)
      ~provenance:Canonical
      (H.Redaction.create ~disclosed_header:(Some (Call Function)))
    |> protocol_ok
  in
  let rows = C.project_public_entries [ visible; redacted ] |> C.rows in
  List.iter rows ~f:(fun row ->
    print_s
      [%sexp
        (row.message : string * string), (Option.is_some (Row.editing_text row) : bool)]);
  let exported = Agent_session.Chatmd_export.render_public [ visible; redacted ] in
  assert (not (String.is_substring exported ~substring:"secret"));
  assert (not (String.is_substring exported ~substring:"hidden"));
  assert (String.is_substring exported ~substring:"ochat-public-view");
  assert (not (String.is_substring exported ~substring:"<assistant"));
  [%expect
    {|
    ((developer  "readable\
                \n[Redacted content: future_part]") false)
    ((tool "[Content redacted]") false)
    |}]
;;

let%expect_test "identity and exact edit text are independent of disclosure and display" =
  let value = text User "  original spaces  " in
  let full = public_full 0 value in
  let visible = H.visible (id 0) ~provenance:Canonical (disclose value) |> protocol_ok in
  [%test_eq: string option]
    (C.Rendered.of_visible (disclose value) |> C.Rendered.copy_text)
    (Some "  original spaces  ");
  let redacted =
    H.redacted (id 0) ~provenance:Canonical (H.Redaction.create ~disclosed_header:None)
    |> protocol_ok
  in
  List.iter [ full; visible; redacted ] ~f:(fun value ->
    let row = row value in
    print_s
      [%sexp
        (row.message : string * string)
      , (Row.editing_text row : string option)
      , (Option.map (Row.deletion_target row) ~f:History_entry.Id.to_string
         : string option)]);
  let replaced =
    H.full (entry 3 value) ~provenance:(Moderator_replaced (id 0)) |> protocol_ok |> row
  in
  assert (Row.Id.equal replaced.id (Row.Id.canonical (id 0)));
  assert (Option.is_none (Row.deletion_target replaced));
  assert (Option.is_none (Row.editing_text replaced));
  [%expect
    {|
    ((user "original spaces") ("  original spaces  ") (10:neutral-ui:0))
    ((user "original spaces") () (10:neutral-ui:0))
    ((redacted "[Content redacted]") () (10:neutral-ui:0)) |}]
;;

let scope source attempt relation =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string source |> ok)
    ~attempt:(Transcript.Attempt_id.of_string attempt |> ok)
    ~relation
  |> ok
;;

let item scope key entry_id header =
  Transcript.Item.create
    ~scope
    ~id:(Transcript.Item_id.of_string key |> ok)
    ~entry_id
    ~header
    ~call_name:None
  |> ok
;;

let event view = Transcript.Stream.create view ~limits:Transcript.Admission.default |> ok
let apply drafts view = Chat_tui.Stream.apply drafts (event view) |> ok

let%expect_test
    "self described drafts retain gaps and unavailable header without defaults"
  =
  let scope = scope "source" "attempt" Root in
  let item = item scope "provider-local" None None in
  let part =
    Transcript.Part.create
      ~item
      ~id:(Transcript.Part_id.of_string "part" |> ok)
      ~index:None
      ~kind:Text
    |> ok
  in
  let drafts =
    apply
      (Chat_tui.Stream.create ())
      (Changed { target = Content part; change = Append "suffix" })
  in
  let row = Chat_tui.Stream.rows drafts |> List.hd_exn in
  print_s
    [%sexp
      (row.message : string * string)
    , (Option.is_none row.entry_id : bool)
    , (Option.is_none (Row.editing_text row) : bool)
    , (Option.is_none (Row.deletion_target row) : bool)];
  [%expect
    {|
    ((unavailable  "[Earlier live content unavailable]\
                  \nsuffix") true true true)
    |}]
;;

let%expect_test "source attempts and nested drafts do not alias canonical occurrences" =
  let root = scope "same" "first" Root in
  let retry = scope "same" "second" Root in
  let child =
    scope
      "child"
      "first"
      (Nested
         { scope = Transcript.Scope.key root
         ; call_entry_id = Some (id 10)
         ; call_alias = Some "alias"
         })
  in
  let descriptors =
    [ item root "same-item" (Some (id 1)) (Some (Message Developer))
    ; item retry "same-item" None (Some (Message Developer))
    ; item child "same-item" (Some (id 1)) (Some (Message Developer))
    ]
  in
  let drafts =
    List.fold descriptors ~init:(Chat_tui.Stream.create ()) ~f:(fun drafts item ->
      apply drafts (Item_announced item))
  in
  let rows = Chat_tui.Stream.rows drafts in
  assert (
    List.length
      (List.dedup_and_sort
         (List.map rows ~f:(fun row -> row.Row.id))
         ~compare:Row.Id.compare)
    = 3);
  assert (List.for_all rows ~f:(fun row -> Option.is_none (Row.deletion_target row)));
  let retired = Chat_tui.Stream.remove_committed drafts (id 1) |> Chat_tui.Stream.rows in
  printf
    "distinct=%d retained-after-root-commit=%d\n"
    (List.length rows)
    (List.length retired);
  [%expect {| distinct=3 retained-after-root-commit=2 |}]
;;

let%expect_test
    "finalized actual payload renders exactly like canonical replay without append \
     authority"
  =
  let scope = scope "source" "attempt" Root in
  let descriptor = item scope "item" (Some (id 1)) (Some (Message Developer)) in
  let actual = entry 1 (text Developer "final exact") in
  let drafts =
    apply
      (Chat_tui.Stream.create ())
      (Item_finalized { item = descriptor; entry = actual })
  in
  let row = Chat_tui.Stream.rows drafts |> List.hd_exn in
  let canonical = C.project_entries [ actual ] |> C.rows |> List.hd_exn in
  assert (String.equal (fst row.message) (fst canonical.message));
  assert (String.equal (snd row.message) (snd canonical.message));
  assert (Row.Id.equal row.id canonical.id);
  assert (Option.is_none (Row.deletion_target row));
  print_s [%sexp (row.message : string * string)];
  [%expect {| (developer "final exact") |}]
;;

let%expect_test "neutral export preserves bound host relation and unknown JSON" =
  let call =
    payload
      (Call
         { kind = Function
         ; name = "f"
         ; namespace = Absent
         ; input_bytes = "opaque exact"
         ; async = Null
         })
  in
  let output =
    payload
      (Result { relation = Bound (id 1); kind = Function; output = Text "complete" })
  in
  let unknown = payload (Unknown { provider_kind = "future" }) in
  let exported =
    Agent_session.Chatmd_export.render
      [ entry 0 (text Developer "distinct")
      ; entry 1 call
      ; entry 2 output
      ; entry 3 unknown
      ]
  in
  assert (String.is_substring exported ~substring:"role=\"developer\"");
  assert (
    String.is_substring exported ~substring:"ochat-call-entry-id=\"10:neutral-ui:1\"");
  assert (String.is_substring exported ~substring:"ochat-unknown-item");
  assert (String.is_substring exported ~substring:"history.payload");
  print_endline "neutral role, relation, unknown evidence and host IDs retained";
  [%expect {| neutral role, relation, unknown evidence and host IDs retained |}]
;;

let%expect_test "cancellation repair binds synthetic output to actual call host identity" =
  let metadata = { P.Metadata.empty with call_id = Value "same-alias" } in
  let make_call sequence =
    P.Semantic.create
      (Call
         { kind = Function
         ; name = "f"
         ; namespace = Absent
         ; input_bytes = "{}"
         ; async = Absent
         })
      ~metadata
    |> ok
    |> P.authored
    |> entry sequence
  in
  let first, second = make_call 0, make_call 1 in
  let actual_output =
    P.Semantic.create
      (Result { relation = Bound (id 1); kind = Function; output = Text "second done" })
      ~metadata
    |> ok
    |> P.authored
    |> entry 2
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"neutral-ui" ~next_sequence:3 |> ok
  in
  let repaired =
    Chat_tui.App_reducer.Cancellation_repair.repair
      ~allocator
      ~error:"cancelled"
      [ first; second; actual_output ]
    |> ok
  in
  let outputs =
    List.filter_map repaired ~f:(fun entry ->
      match P.Semantic.view (P.semantic (History_entry.payload entry)) with
      | Result { relation = Bound id; _ } -> Some (History_entry.Id.sequence id)
      | Message _ | Call _ | Result { relation = Unresolved; _ } | Reasoning _ | Unknown _
        -> None)
  in
  print_s [%sexp (outputs : int list)];
  [%expect {| (0 1) |}]
;;

let activity_model () =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:"draft"
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Insert
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

module Activity = Agent_protocol.Activity
module Model = Chat_tui.Model

let activity_key source alias parent =
  Activity.Key.create ~scope:(Transcript.Scope.key source) ~call_alias:alias ~parent
  |> protocol_ok
;;

let activity_summary key ~classified ~text ~state =
  let descriptor =
    Activity.Tool.descriptor
      key
      ~call_entry_id:None
      ~name:key.Activity.Key.call_alias
      ~kind:Function
      ~input:"{}"
      ~classification:(if classified then Some Subagent else None)
    |> protocol_ok
  in
  Activity.Tool.summary
    key
    ~descriptor:(Some descriptor)
    ~channels:
      (Option.to_list
         (Option.map text ~f:(fun text ->
            Activity.Tool.{ channel = Assistant; text; complete = true })))
    ~state
  |> protocol_ok
;;

let%test_unit
    "absolute activity replacement removes omitted channels and finished evidence"
  =
  let model = activity_model () in
  let key = activity_key (scope "activity" "first" Root) "worker" None in
  let summary =
    activity_summary
      key
      ~classified:true
      ~text:(Some "old")
      ~state:(Finished { outcome = Returned; output = Some (Text "old output") })
  in
  Model.reconcile_agent_activity model [ summary ] ~operation_ended:false;
  let first = List.hd_exn (Model.active_agent_calls model) in
  assert (Option.is_some (Model.agent_call_outcome first));
  [%test_eq: string list]
    (Model.agent_call_progress_entries first |> List.map ~f:Model.progress_entry_text)
    [ "old" ];
  let replacement = activity_summary key ~classified:true ~text:None ~state:Running in
  Model.reconcile_agent_activity model [ replacement ] ~operation_ended:false;
  let second = List.hd_exn (Model.active_agent_calls model) in
  [%test_eq: string] (Model.agent_call_id first) (Model.agent_call_id second);
  assert (Option.is_none (Model.agent_call_outcome second));
  assert (List.is_empty (Model.agent_call_progress_entries second));
  Model.reconcile_agent_activity model [] ~operation_ended:false;
  assert (List.is_empty (Model.active_agent_calls model));
  [%test_eq: string] (Model.input_line model) "draft"
;;

let%test_unit "nested-first activity uses the actual parent scope even for the same alias"
  =
  let model = activity_model () in
  let root_scope = scope "root" "first" Root in
  let root_key = activity_key root_scope "same" None in
  let parent =
    Activity.Key.{ scope = Transcript.Scope.key root_scope; call_alias = "same" }
  in
  let child_scope =
    scope
      "child"
      "first"
      (Nested { scope = parent.scope; call_entry_id = None; call_alias = Some "same" })
  in
  let child_key = activity_key child_scope "same" (Some parent) in
  let root = activity_summary root_key ~classified:true ~text:None ~state:Running in
  let child =
    activity_summary child_key ~classified:false ~text:(Some "child only") ~state:Running
  in
  Model.reconcile_agent_activity model [ child ] ~operation_ended:false;
  assert (List.is_empty (Model.active_agent_calls model));
  Model.reconcile_agent_activity model [ child; root ] ~operation_ended:false;
  let call = List.hd_exn (Model.active_agent_calls model) in
  [%test_eq: int] (List.length (Model.active_agent_calls model)) 1;
  [%test_eq: string]
    (Model.agent_call_id call)
    (Activity.Key.sexp_of_t root_key |> Sexp.to_string_mach);
  let nested =
    Model.agent_call_progress_entries call
    |> List.filter_map ~f:Model.progress_entry_tool_view
  in
  assert (
    List.exists nested ~f:(fun (key, _, _, _, progress, _, _) ->
      String.equal key (Activity.Key.sexp_of_t child_key |> Sexp.to_string_mach)
      && List.exists progress ~f:(fun (_, text) -> String.equal text "child only")));
  assert (List.is_empty (Model.history_items model))
;;
