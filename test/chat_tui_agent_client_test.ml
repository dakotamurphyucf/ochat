open Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let history_id =
  History_entry.Id.create ~namespace:"tui-agent" ~sequence:0 |> Result.ok_or_failwith
;;

let session_id =
  Agent_protocol.Id.Session.of_string "ses_tui_agent_projection" |> protocol_ok
;;

let principal_id =
  Agent_protocol.Id.Principal.of_string "pri_tui_agent_projection" |> protocol_ok
;;

let grant_id = Agent_protocol.Id.Grant.of_string "grt_tui_agent_projection" |> protocol_ok
let timestamp = Agent_protocol.Timestamp.of_string "2026-08-16T12:00:00Z" |> protocol_ok

let item text =
  Openai.Responses.Item.Input_message
    { role = User; content = [ Text { text; _type = "input_text" } ]; _type = "message" }
;;

let history_entry text =
  Agent_protocol.History.
    { id = history_id
    ; role = User
    ; kind = Message
    ; payload = Openai.Responses.Item.jsonaf_of_t (item text)
    ; provenance = Canonical
    ; redacted = false
    }
;;

let session () =
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Local_path "/prompt.chatmd")
      ~workspace:(Local_path "/workspace")
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:true
      ~labels:[]
      ()
    |> protocol_ok
  in
  Agent_protocol.Session.
    { id = session_id
    ; creator = Some principal_id
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec
    ; desired_state = Running
    ; observed_state = Idle
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 1L
    ; latest_event_sequence = 1L
    }
;;

let projection text =
  let window =
    Agent_protocol.History.Window.
      { entries = [ history_entry text ]
      ; previous_cursor = None
      ; next_cursor = None
      ; reached_start = true
      ; reached_end = true
      ; structurally_complete = true
      }
  in
  let snapshot =
    Agent_protocol.Snapshot.
      { session = session ()
      ; canonical_history = window
      ; archived_revisions = []
      ; effective_history = None
      ; deferred_entries = []
      ; permissions = []
      ; grants = []
      ; jobs = []
      ; extension_status = []
      ; schedules = []
      ; active_tool_calls = []
      ; active_agent_calls = []
      ; halted = false
      ; halt_reason = None
      ; failure = None
      ; revision = 1L
      ; latest_event_sequence = 1L
      }
  in
  Agent_client.Projection.install_snapshot snapshot
  |> Chat_tui.Agent_projection.of_client_projection
  |> protocol_ok
;;

let model () =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:"unsent draft"
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:12
    ~selection_anchor:None
    ~mode:Insert
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let damage_name = function
  | Chat_tui.Model.No_damage -> "no_damage"
  | Below_viewport -> "below_viewport"
  | Visible_damage -> "visible_damage"
  | Above_viewport -> "above_viewport"
  | Unknown_damage -> "unknown_damage"
;;

let permission () : Agent_protocol.Permission.t =
  { id = Agent_protocol.Id.Permission.of_string "per_tui_render" |> protocol_ok
  ; session_id
  ; generation = 0
  ; owner =
      Operation (Agent_protocol.Id.Operation.of_string "op_tui_render" |> protocol_ok)
  ; call_id = "manual-fork-1"
  ; tool_name = "fork"
  ; runtime_identity = None
  ; invocation_display = "fork(<redacted>)"
  ; rationale = None
  ; effects = []
  ; choices = [ Approve_once; Approve_session; Deny ]
  ; created_at = timestamp
  ; expires_at = None
  ; state = Pending
  ; resolution = None
  }
;;

let rendered_screen model ~size =
  let image, _ = Chat_tui.Renderer.render_full ~size ~model in
  let buffer = Buffer.create 4096 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) size image;
  Buffer.contents buffer
;;

let audit_model count =
  let model = model () in
  let records =
    List.init count ~f:(fun index ->
      Agent_protocol.Audit.
        { sequence = Int64.of_int (100 + index)
        ; timestamp
        ; level = Info
        ; name = "protocol.command.succeeded"
        ; session_id = Some session_id
        ; principal_id = Some principal_id
        ; payload = `Object []
        ; redacted = true
        })
  in
  let page =
    Chat_tui.Agent_security_projection.audit_page
      ~session_id
      { Agent_protocol.Page.items = records; next_cursor = None }
  in
  let generation = Chat_tui.Model.begin_shell_management_load model in
  assert (Chat_tui.Model.finish_shell_management_load model ~generation page);
  Chat_tui.Model.set_active_page model Shell_security;
  Chat_tui.Model.set_shell_security_tab model Audit;
  model, page
;;

let%test_unit "security tabs retain spacing and audit details distinguish records" =
  let model, page = audit_model 24 in
  assert (
    List.equal
      String.equal
      (List.map page.requests ~f:(fun request -> request.request_id))
      (List.init 24 ~f:(fun index ->
         sprintf "protocol.command.succeeded:%d" (123 - index))));
  List.iter [ 40; 100; 140; 40; 140 ] ~f:(fun width ->
    let screen = rendered_screen model ~size:(width, 45) in
    assert (String.is_substring screen ~substring:" Audit ");
    assert (not (String.is_substring screen ~substring:"GrantsAudit"));
    assert (String.is_substring screen ~substring:"Selected request");
    assert (String.is_substring screen ~substring:"#123");
    let flattened = String.filter screen ~f:(Fn.non Char.is_whitespace) in
    assert (String.is_substring flattened ~substring:"protocol.command.succeeded:123");
    Chat_tui.Model.move_shell_audit_selection model 1;
    let screen = rendered_screen model ~size:(width, 45) in
    assert (String.is_substring screen ~substring:"Request ID");
    Chat_tui.Model.move_shell_audit_selection model (-1));
  for index = 0 to 23 do
    let screen = rendered_screen model ~size:(40, 45) in
    let flattened = String.filter screen ~f:(Fn.non Char.is_whitespace) in
    assert (
      String.is_substring
        flattened
        ~substring:(sprintf "protocol.command.succeeded:%d" (123 - index)));
    Chat_tui.Model.move_shell_audit_selection model 1
  done
;;

let audit_row_positions screen =
  String.split_lines screen
  |> List.filter_mapi ~f:(fun index line ->
    Option.map (String.substr_index line ~pattern:"#") ~f:(fun start ->
      index, String.sub line ~pos:start ~len:4))
;;

let%test_unit "a fitting audit list stays fixed through selection and wraparound" =
  let model, _ = audit_model 3 in
  let positions () = rendered_screen model ~size:(150, 53) |> audit_row_positions in
  let initial = positions () in
  assert (List.length initial = 3);
  List.iter [ 1; 1; 1; -1; -1; -1 ] ~f:(fun delta ->
    Chat_tui.Model.move_shell_audit_selection model delta;
    assert (Poly.equal (positions ()) initial))
;;

let%test_unit "audit overflow scrolls only at the edge and restores on resize" =
  let model, _ = audit_model 24 in
  let positions size = rendered_screen model ~size |> audit_row_positions in
  let size = 150, 53 in
  let initial = positions size in
  let visible = List.length initial in
  assert (visible > 2 && visible < 24);
  for _ = 1 to visible - 1 do
    Chat_tui.Model.move_shell_audit_selection model 1;
    assert (Poly.equal (positions size) initial)
  done;
  Chat_tui.Model.move_shell_audit_selection model 1;
  let shifted = positions size in
  assert (List.length shifted = visible);
  assert (String.equal (snd (List.hd_exn shifted)) "#122");
  Chat_tui.Model.move_shell_audit_selection model (-1);
  assert (Poly.equal (positions size) shifted);
  let expanded = positions (150, 110) in
  assert (List.length expanded = 24);
  assert (String.equal (snd (List.hd_exn expanded)) "#123");
  let narrowed = positions (40, 30) in
  assert (
    List.exists narrowed ~f:(fun (_, label) ->
      String.equal label (sprintf "#%d" (124 - visible))));
  Chat_tui.Model.move_shell_audit_selection model (24 - visible);
  let last = positions size in
  assert (List.length last = visible);
  assert (String.equal (snd (List.last_exn last)) "#100");
  Chat_tui.Model.move_shell_audit_selection model 1;
  assert (Poly.equal (positions size) initial);
  Chat_tui.Model.move_shell_audit_selection model (-1);
  assert (Poly.equal (positions size) last)
;;

let%test_unit "actual agent permission prompt renders multiline details and choices" =
  let model = model () in
  let snapshot = Chat_tui.Agent_projection.snapshot (projection "permission") in
  let projection =
    Agent_client.Projection.install_snapshot
      { snapshot with permissions = [ permission () ] }
    |> Chat_tui.Agent_projection.of_client_projection
    |> protocol_ok
  in
  ignore
    (Chat_tui.Agent_permission_view.sync model ~current:None projection
     : Agent_protocol.Permission.t option);
  List.iter
    [ 40, 30; 100, 30; 140, 40 ]
    ~f:(fun size ->
      let rendered = rendered_screen model ~size in
      List.iter
        [ "Tool permission requested"
        ; "Tool: fork"
        ; "Invocation: fork(<redacted>)"
        ; "approve once"
        ; "approve session"
        ; "deny"
        ]
        ~f:(fun substring -> assert (String.is_substring rendered ~substring)))
;;

let%test_unit "multiline moderator input keeps its cursor on the response row" =
  List.iter
    [ 40, 30; 100, 30 ]
    ~f:(fun size ->
      let model = model () in
      Chat_tui.Model.open_moderator_modal
        model
        (Chat_response.In_memory_stream.Ask_text
           { prompt =
               "First prompt line\n\n\
                Last prompt line with enough words to wrap narrowly\n"
           });
      let modal = Chat_tui.Model.moderator_modal model |> Option.value_exn in
      modal.response <- "yes";
      modal.cursor <- 2;
      let image, cursor = Chat_tui.Renderer.render_full ~size ~model in
      let buffer = Buffer.create 32 in
      Notty.Render.to_buffer buffer Notty.Cap.dumb cursor (1, 1) image;
      assert (String.is_substring (Buffer.contents buffer) ~substring:"▏");
      let rendered = rendered_screen model ~size in
      List.iter [ "First prompt line"; "narrowly"; "Enter submit" ] ~f:(fun substring ->
        assert (String.is_substring rendered ~substring)))
;;

let%test_unit "moderator labels sanitize controls and validation errors keep newlines" =
  let model = model () in
  Chat_tui.Model.open_moderator_modal
    model
    (Chat_response.In_memory_stream.Ask_choice
       { prompt = "Choose\n\nDetails\tvalue\027\000"
       ; choices = [| "allow\nonce"; "deny" |]
       });
  let modal = Chat_tui.Model.moderator_modal model |> Option.value_exn in
  modal.validation_error <- Some "First error\nSecond error";
  let rendered = rendered_screen model ~size:(100, 30) in
  List.iter
    [ "Choose"; "Details    value"; "allow once"; "First error"; "Second error" ]
    ~f:(fun substring -> assert (String.is_substring rendered ~substring))
;;

let%expect_test "agent projection decodes canonical history with stable identity" =
  let projection = projection "hello from daemon" in
  let ids =
    Chat_tui.Agent_projection.canonical_history projection
    |> List.map ~f:(fun entry -> History_entry.id entry |> History_entry.Id.to_string)
  in
  print_s
    [%sexp
      { ids : string list
      ; messages = (List.length (Chat_tui.Agent_projection.messages projection) : int)
      }];
  [%expect {| ((ids (9:tui-agent:0)) (messages 1)) |}]
;;

let%expect_test "projection replacement preserves the local draft" =
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let damage =
    Chat_tui.Agent_event_apply.apply
      applier
      ~model
      ~viewport_height:20
      (projection "server history")
    |> protocol_ok
  in
  print_s
    [%sexp
      { draft = (Chat_tui.Model.input_line model : string)
      ; history = (List.length (Chat_tui.Model.history_items model) : int)
      ; damage = (damage_name damage : string)
      }];
  [%expect {| ((draft "unsent draft") (history 1) (damage visible_damage)) |}]
;;

let trace_operation = Agent_protocol.Id.Operation.of_string "op_tui_trace" |> protocol_ok

let trace_id =
  History_entry.Id.create ~namespace:"tui-agent" ~sequence:1 |> Result.ok_or_failwith
;;

let trace_event sequence kind payload : Agent_protocol.Event.Recoverable.t =
  { session_id
  ; operation_id = trace_operation
  ; operation_sequence = Int64.of_int sequence
  ; anchor_sequence = 1L
  ; timestamp
  ; kind
  ; payload
  }
;;

let trace_stream sequence kind event =
  trace_event
    sequence
    kind
    (`Object
        [ "entry_id", `String (History_entry.Id.to_string trace_id)
        ; "parent_call_id", `Null
        ; "event", Openai.Responses.Response_stream.jsonaf_of_t event
        ])
;;

let trace_reasoning text : Openai.Responses.Reasoning.t =
  { id = "trace-reasoning"
  ; _type = "reasoning"
  ; status = Some "completed"
  ; summary = [ { text; _type = "summary_text" } ]
  }
;;

let trace_events () =
  let added =
    Openai.Responses.Response_stream.Output_item_added
      { item = Reasoning (trace_reasoning "")
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  in
  let delta =
    Openai.Responses.Response_stream.Reasoning_summary_text_delta
      { item_id = "trace-reasoning"
      ; output_index = 0
      ; summary_index = 0
      ; delta = "think"
      ; type_ = "response.reasoning_summary_text.delta"
      }
  in
  [ trace_stream 1 History_correlated_stream added
  ; trace_stream 2 Sourced_stream added
  ; trace_stream 3 History_correlated_stream delta
  ; trace_stream 4 Sourced_stream delta
  ; trace_event
      5
      Tool_started
      (`Object
          [ "call_id", `String "fork-call"
          ; "name", `String "fork"
          ; "kind", `String "function"
          ; "payload", `String "{}"
          ; "agent_page_kind", `String "subagent"
          ])
  ; trace_event
      6
      Tool_progress
      (`Object
          [ "call_id", `String "fork-call"
          ; ( "progress"
            , `Object
                [ "channel", `String "assistant"
                ; "update", `String "append"
                ; "text", `String "progress"
                ] )
          ])
  ]
;;

let trace_projection revision ~committed events =
  let base = Chat_tui.Agent_projection.snapshot (projection "base") in
  let entries =
    if not committed
    then base.canonical_history.entries
    else
      base.canonical_history.entries
      @ [ { (history_entry "") with
            id = trace_id
          ; role = Assistant
          ; kind = Reasoning
          ; payload =
              Openai.Responses.Item.jsonaf_of_t (Reasoning (trace_reasoning "think"))
          }
        ]
  in
  let snapshot =
    { base with
      revision
    ; session = { base.session with revision }
    ; canonical_history = { base.canonical_history with entries }
    }
  in
  List.fold
    events
    ~init:(Agent_client.Projection.install_snapshot snapshot)
    ~f:(fun projection event ->
      Agent_client.Projection.apply_live_event projection event |> protocol_ok)
  |> Chat_tui.Agent_projection.of_client_projection
  |> protocol_ok
;;

let%expect_test "live text and tool progress survive durable rebuild without duplication" =
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let events = trace_events () in
  List.iter
    [ 1L, false; 2L, false; 3L, true; 3L, true ]
    ~f:(fun (revision, committed) ->
      ignore
        (Chat_tui.Agent_event_apply.apply
           applier
           ~model
           ~viewport_height:20
           (trace_projection revision ~committed events)
         |> protocol_ok
         : Chat_tui.Model.projection_damage);
      [%test_eq: (string * string) list]
        (Chat_tui.Model.messages model)
        [ "user", "base"; "reasoning", "think" ];
      let call = Chat_tui.Model.active_agent_calls model |> List.hd_exn in
      [%test_eq: string list]
        (Chat_tui.Model.agent_call_progress_entries call
         |> List.map ~f:Chat_tui.Model.progress_entry_text)
        [ "progress" ]);
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test
    "same-revision overlay events replace visible history and preserve canonical draft"
  =
  let module P = Agent_client.Projection in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let base = projection "canonical" |> Chat_tui.Agent_projection.snapshot in
  let current = ref (P.install_snapshot base) in
  let apply () =
    let projection =
      Chat_tui.Agent_projection.of_client_projection !current |> protocol_ok
    in
    ignore
      (Chat_tui.Agent_event_apply.apply applier ~model ~viewport_height:20 projection
       |> protocol_ok
       : Chat_tui.Model.projection_damage)
  in
  let event sequence payload =
    let event =
      Agent_protocol.Event.Durable.of_payload
        ~session_id
        ~sequence
        ~revision:2L
        ~timestamp
        (Moderator_overlay_changed payload)
    in
    current := P.apply_event !current event |> protocol_ok;
    apply ()
  in
  apply ();
  List.iter
    [ 2L, "first"; 3L, "second" ]
    ~f:(fun (sequence, text) ->
      let window = { base.canonical_history with entries = [ history_entry text ] } in
      event
        sequence
        (`Object
            [ "halted", `True
            ; "halt_reason", `String "fixture"
            ; "effective_history", Agent_protocol.History.Window.to_json window
            ]);
      [%test_eq: (string * string) list] (Chat_tui.Model.messages model) [ "user", text ];
      [%test_eq: Sexp.t]
        ([%sexp_of: Agent_protocol.History.Window.t]
           (P.snapshot !current).canonical_history)
        ([%sexp_of: Agent_protocol.History.Window.t] base.canonical_history));
  event 4L (`Object [ "halted", `False ]);
  [%test_eq: (string * string) list]
    (Chat_tui.Model.messages model)
    [ "user", "canonical" ];
  [%test_eq: bool] (P.snapshot !current).halted false;
  [%test_eq: string option] (P.snapshot !current).halt_reason None;
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test
    "durable cancellation closes a call when terminal live progress is coalesced away"
  =
  let module P = Agent_client.Projection in
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let operation =
    Agent_protocol.Operation.
      { id = trace_operation
      ; generation = 0
      ; kind = Turn User_submit
      ; state = Running
      ; started_at = timestamp
      ; updated_at = timestamp
      }
  in
  let base =
    { base with session = { base.session with active_operation = Some operation } }
  in
  let current =
    List.fold
      (trace_events ())
      ~init:(P.install_snapshot base)
      ~f:(fun projection event -> P.apply_live_event projection event |> protocol_ok)
  in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let apply current =
    let projection =
      Chat_tui.Agent_projection.of_client_projection current |> protocol_ok
    in
    ignore
      (Chat_tui.Agent_event_apply.apply applier ~model ~viewport_height:20 projection
       |> protocol_ok
       : Chat_tui.Model.projection_damage)
  in
  apply current;
  let event =
    Agent_protocol.Event.Durable.of_payload
      ~session_id
      ~sequence:2L
      ~revision:2L
      ~timestamp
      (Operation_cancelled { operation with state = Cancelled })
  in
  let terminal = P.apply_event current event |> protocol_ok in
  [%test_eq: int] (List.length (P.live_events terminal)) 0;
  List.iter [ terminal; terminal ] ~f:apply;
  let call = List.hd_exn (Chat_tui.Model.active_agent_calls model) in
  assert (
    Poly.equal
      (Chat_tui.Model.agent_call_outcome call)
      (Some Ochat_function.Trace.Cancelled));
  [%test_eq: string list]
    (Chat_tui.Model.agent_call_progress_entries call
     |> List.map ~f:Chat_tui.Model.progress_entry_text)
    [ "progress" ];
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test "daemon security grants and audit records project into the TUI page" =
  let grant =
    Agent_protocol.Grant.
      { id = grant_id
      ; session_id
      ; principal_id
      ; tool_name = "shell"
      ; identity_digest = "sha256:command"
      ; scope = Durable_exact
      ; state = Active
      ; created_at = timestamp
      ; expires_at = None
      ; revoked_at = None
      ; revocation_reason = None
      }
  in
  let projection =
    let base = projection "security" |> Chat_tui.Agent_projection.snapshot in
    { base with grants = [ grant ] }
  in
  let security =
    Chat_tui.Agent_security_projection.snapshot
      ~current:Chat_tui.Shell_security_page_state.empty_snapshot
      projection
  in
  let audit =
    Agent_protocol.Audit.
      { sequence = 7L
      ; timestamp
      ; level = Info
      ; name = "grant.revoked"
      ; session_id = Some session_id
      ; principal_id = Some principal_id
      ; payload = `Object [ "request_id", `String "req-security" ]
      ; redacted = true
      }
  in
  let audit_page =
    Chat_tui.Agent_security_projection.audit_page
      ~session_id
      { Agent_protocol.Page.items = [ audit ]; next_cursor = None }
  in
  let projected_grant = List.hd_exn security.grants in
  let projected_audit = List.hd_exn audit_page.requests in
  print_s
    [%sexp
      { grants = (List.length security.grants : int)
      ; grant_id = (projected_grant.Session.Shell_state.Approval_grant.grant_id : string)
      ; runtime_id = (projected_grant.runtime_id : string)
      ; audit_integrity = (audit_page.integrity : string)
      ; audit_request_id = (projected_audit.request_id : string)
      ; last_sequence = (audit_page.last_sequence : int64 option)
      }];
  [%expect
    {|
    ((grants 1) (grant_id grt_tui_agent_projection) (runtime_id shell)
     (audit_integrity "verified; redacted") (audit_request_id req-security)
     (last_sequence (7)))
    |}]
;;

let%expect_test
    "replacement snapshots restore active calls once and clear completed calls"
  =
  let module P = Agent_client.Projection in
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let operation =
    Agent_protocol.Operation.
      { id = trace_operation
      ; generation = 0
      ; kind = Turn User_submit
      ; state = Running
      ; started_at = timestamp
      ; updated_at = timestamp
      }
  in
  let started =
    trace_events ()
    |> List.find_exn ~f:(fun event ->
      Agent_protocol.Event.Recoverable.equal_kind event.kind Tool_started)
  in
  let snapshot =
    { base with
      session = { base.session with active_operation = Some operation }
    ; active_tool_calls = [ Agent_protocol.Event.Recoverable.to_json started ]
    ; active_agent_calls = [ Agent_protocol.Event.Recoverable.to_json started ]
    }
  in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let apply client =
    let projected =
      Chat_tui.Agent_projection.of_client_projection client |> protocol_ok
    in
    ignore
      (Chat_tui.Agent_event_apply.apply applier ~model ~viewport_height:20 projected
       |> protocol_ok
       : Chat_tui.Model.projection_damage)
  in
  let client = P.install_snapshot snapshot in
  apply client;
  apply client;
  [%test_eq: int] (List.length (Chat_tui.Model.active_agent_calls model)) 1;
  let finished =
    trace_event
      7
      Tool_finished
      (`Object
          [ "call_id", `String "fork-call"
          ; "outcome", `String "returned"
          ; "output", `Null
          ])
  in
  let client = P.apply_live_event client finished |> protocol_ok in
  apply client;
  [%test_eq: int] (List.length (P.snapshot client).active_tool_calls) 0;
  let call = List.hd_exn (Chat_tui.Model.active_agent_calls model) in
  assert (
    Poly.equal
      (Chat_tui.Model.agent_call_outcome call)
      (Some Ochat_function.Trace.Returned));
  apply (P.install_snapshot base);
  [%test_eq: int] (List.length (Chat_tui.Model.active_agent_calls model)) 0;
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test "redacted transcript entries render as placeholders" =
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let entry =
    { (history_entry "") with
      kind = Tool_call
    ; role = Assistant
    ; redacted = true
    ; payload = `Object []
    }
  in
  let base =
    { base with canonical_history = { base.canonical_history with entries = [ entry ] } }
  in
  let view =
    Agent_client.Projection.install_snapshot base
    |> Chat_tui.Agent_projection.of_client_projection
    |> protocol_ok
  in
  [%test_eq: (string * string) list]
    (Chat_tui.Agent_projection.messages view)
    [ "assistant", "[Tool content redacted]" ];
  [%expect {| |}]
;;
