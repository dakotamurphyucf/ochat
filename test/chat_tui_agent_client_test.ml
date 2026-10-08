open Core
open Chat_tui_projection_support

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

let equal_position (left_index, left_label) (right_index, right_label) =
  Int.equal left_index right_index && String.equal left_label right_label
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
    assert (List.equal equal_position (positions ()) initial))
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
    assert (List.equal equal_position (positions size) initial)
  done;
  Chat_tui.Model.move_shell_audit_selection model 1;
  let shifted = positions size in
  assert (List.length shifted = visible);
  assert (String.equal (snd (List.hd_exn shifted)) "#122");
  Chat_tui.Model.move_shell_audit_selection model (-1);
  assert (List.equal equal_position (positions size) shifted);
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
  assert (List.equal equal_position (positions size) initial);
  Chat_tui.Model.move_shell_audit_selection model (-1);
  assert (List.equal equal_position (positions size) last)
;;

let%test_unit "actual agent permission prompt renders multiline details and choices" =
  let model = model () in
  let snapshot = Chat_tui.Agent_projection.snapshot (projection "permission") in
  let projection =
    Agent_client.Projection.install_snapshot
      (Agent_protocol.Public.Snapshot.create
         { (Agent_protocol.Public.Snapshot.fields snapshot) with
           permissions = [ permission () ]
         }
       |> protocol_ok)
    |> Chat_tui.Agent_projection.of_client_projection
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
    |> List.map ~f:(fun entry ->
      entry.Agent_protocol.Public.History.id |> History_entry.Id.to_string)
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
  [%expect {| ((draft "unsent draft") (history 0) (damage visible_damage)) |}]
;;

module P = Agent_protocol
module Payload = History_entry.Payload

let trace_operation = P.Id.Operation.of_string "op_tui_trace" |> protocol_ok

let trace_id =
  History_entry.Id.create ~namespace:"tui-agent" ~sequence:1 |> Result.ok_or_failwith
;;

let trace_scope =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string "tui-trace" |> Result.ok_or_failwith)
    ~attempt:(Transcript.Attempt_id.of_string "attempt" |> Result.ok_or_failwith)
    ~relation:Root
  |> Result.ok_or_failwith
;;

let trace_item =
  Transcript.Item.create
    ~scope:trace_scope
    ~id:(Transcript.Item_id.of_string "reasoning" |> Result.ok_or_failwith)
    ~entry_id:(Some trace_id)
    ~header:(Some Reasoning)
    ~call_name:None
  |> Result.ok_or_failwith
;;

let trace_part =
  Transcript.Part.create
    ~item:trace_item
    ~id:(Transcript.Part_id.of_string "summary" |> Result.ok_or_failwith)
    ~index:None
    ~kind:Reasoning_summary
  |> Result.ok_or_failwith
;;

let activity_key =
  P.Activity.Key.create
    ~scope:(Transcript.Scope.key trace_scope)
    ~call_alias:"fork-call"
    ~parent:None
  |> protocol_ok
;;

let activity_descriptor =
  P.Activity.Tool.descriptor
    activity_key
    ~call_entry_id:None
    ~name:"fork"
    ~kind:Function
    ~input:"{}"
    ~classification:(Some Subagent)
  |> protocol_ok
;;

let operation =
  P.Operation.
    { id = trace_operation
    ; generation = 0
    ; kind = Turn User_submit
    ; state = Running
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let with_fields snapshot ~f =
  P.Public.Snapshot.create (f (P.Public.Snapshot.fields snapshot)) |> protocol_ok
;;

let trace_event sequence payload =
  P.Event.Recoverable.create
    ~session_id
    ~operation_id:trace_operation
    ~operation_sequence:(Int64.of_int sequence)
    ~anchor_sequence:1L
    ~timestamp
    ~invocation_id:None
    ~parent_invocation_id:None
    payload
  |> protocol_ok
;;

let observation view =
  Transcript.Stream.create view ~limits:Transcript.Admission.default
  |> Result.ok_or_failwith
;;

let trace_events () =
  [ trace_event 1 (Transcript (observation (Item_announced trace_item)))
  ; trace_event 2 (Transcript (observation (Part_announced trace_part)))
  ; trace_event
      3
      (Transcript
         (observation (Changed { target = Content trace_part; change = Append "think" })))
  ; trace_event 4 (Tool_activity (Started activity_descriptor))
  ; trace_event
      5
      (Tool_activity
         (Progress
            { key = activity_key
            ; progress = { channel = Assistant; update = Append "progress" }
            }))
  ]
;;

let trace_entry =
  Payload.Semantic.create
    (Reasoning { readable_summary = [ "think" ] })
    ~metadata:Payload.Metadata.empty
  |> Result.ok_or_failwith
  |> Payload.authored
  |> History_entry.create_with_id ~id:trace_id
  |> fun entry -> P.Public.History.full entry ~provenance:Canonical |> protocol_ok
;;

let trace_projection revision ~committed events =
  let base = Chat_tui.Agent_projection.snapshot (projection "base") in
  let snapshot =
    with_fields base ~f:(fun fields ->
      { fields with
        revision
      ; session = { fields.session with revision; active_operation = Some operation }
      ; canonical_history =
          { fields.canonical_history with
            entries =
              (fields.canonical_history.entries
               @ if committed then [ trace_entry ] else [])
          }
      })
  in
  List.fold
    events
    ~init:(Agent_client.Projection.install_snapshot snapshot)
    ~f:(fun projection event ->
      Agent_client.Projection.apply_live_event projection event |> protocol_ok)
  |> Chat_tui.Agent_projection.of_client_projection
;;

let apply_projection applier model view =
  Chat_tui.Agent_event_apply.apply applier ~model ~viewport_height:20 view
  |> protocol_ok
  |> ignore
;;

let%expect_test
    "live text and absolute tool progress survive durable rebuild without duplication"
  =
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  List.iter
    [ 1L, false; 2L, false; 3L, true; 3L, true ]
    ~f:(fun (revision, committed) ->
      apply_projection
        applier
        model
        (trace_projection revision ~committed (trace_events ()));
      [%test_eq: (string * string) list]
        (Chat_tui.Model.messages model)
        [ "user", "base"; "reasoning", "think" ];
      let call = Chat_tui.Model.active_agent_calls model |> List.hd_exn in
      [%test_eq: string list]
        (Chat_tui.Model.agent_call_progress_entries call
         |> List.map ~f:Chat_tui.Model.progress_entry_text)
        [ "progress" ]);
  [%test_eq: int] (List.length (Chat_tui.Model.history_items model)) 0;
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let public_event sequence payload internal =
  let envelope =
    P.Event.Durable.of_payload ~session_id ~sequence ~revision:2L ~timestamp internal
  in
  P.Public.Durable.of_internal_envelope
    envelope
    ~body:(Full payload)
    ~extension_status:None
    ~replacement_snapshot:None
  |> protocol_ok
;;

let%expect_test
    "same-revision typed overlays replace read history without canonical import"
  =
  let module Client = Agent_client.Projection in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let base = projection "canonical" |> Chat_tui.Agent_projection.snapshot in
  let current = ref (Client.install_snapshot base) in
  let apply () =
    apply_projection
      applier
      model
      (Chat_tui.Agent_projection.of_client_projection !current)
  in
  apply ();
  List.iter
    [ 2L, "first"; 3L, "second" ]
    ~f:(fun (sequence, text) ->
      let window =
        { (P.Public.Snapshot.fields base).canonical_history with
          entries = [ history_entry text ]
        }
      in
      let overlay =
        P.Public.Durable.
          { effective_history = Some window; halted = true; halt_reason = Some "fixture" }
      in
      current
      := Client.apply_event
           !current
           (public_event
              sequence
              (Moderator_overlay_changed overlay)
              (Moderator_overlay_changed (`Object [])))
         |> protocol_ok;
      apply ();
      [%test_eq: (string * string) list] (Chat_tui.Model.messages model) [ "user", text ];
      assert (
        Sexp.equal
          (P.Public.History.Window.sexp_of_t
             (P.Public.Snapshot.fields (Client.snapshot !current)).canonical_history)
          (P.Public.History.Window.sexp_of_t
             (P.Public.Snapshot.fields base).canonical_history)));
  let overlay =
    P.Public.Durable.{ effective_history = None; halted = false; halt_reason = None }
  in
  current
  := Client.apply_event
       !current
       (public_event
          4L
          (Moderator_overlay_changed overlay)
          (Moderator_overlay_changed (`Object [])))
     |> protocol_ok;
  apply ();
  [%test_eq: (string * string) list]
    (Chat_tui.Model.messages model)
    [ "user", "canonical" ];
  [%test_eq: int] (List.length (Chat_tui.Model.history_items model)) 0;
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test "operation cancellation cannot fabricate an unobserved tool outcome" =
  let module Client = Agent_client.Projection in
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let base =
    with_fields base ~f:(fun fields ->
      { fields with session = { fields.session with active_operation = Some operation } })
  in
  let current =
    List.fold
      (trace_events ())
      ~init:(Client.install_snapshot base)
      ~f:(fun projection event -> Client.apply_live_event projection event |> protocol_ok)
  in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let apply client =
    apply_projection applier model (Chat_tui.Agent_projection.of_client_projection client)
  in
  apply current;
  let payload =
    P.Public.Durable.Shared_payload.of_internal
      (Operation_cancelled { operation with state = Cancelled })
    |> protocol_ok
  in
  let terminal =
    Client.apply_event
      current
      (public_event
         2L
         (Shared payload)
         (Operation_cancelled { operation with state = Cancelled }))
    |> protocol_ok
  in
  List.iter [ terminal; terminal ] ~f:apply;
  let call = List.hd_exn (Chat_tui.Model.active_agent_calls model) in
  assert (Option.is_none (Chat_tui.Model.agent_call_outcome call));
  assert (
    List.exists (Chat_tui.Model.agent_call_render_blocks call) ~f:(fun block ->
      match Chat_tui.Model.agent_render_block_view block with
      | Outcome_unavailable -> true
      | _ -> false));
  [%test_eq: string list]
    (Chat_tui.Model.agent_call_progress_entries call
     |> List.map ~f:Chat_tui.Model.progress_entry_text)
    [ "progress" ];
  [%expect {| |}]
;;

let%test_unit "an observed tool completion stays exact after operation cancellation" =
  let module Client = Agent_client.Projection in
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let base =
    with_fields base ~f:(fun fields ->
      { fields with session = { fields.session with active_operation = Some operation } })
  in
  let current =
    List.fold
      (trace_events ())
      ~init:(Client.install_snapshot base)
      ~f:(fun projection event -> Client.apply_live_event projection event |> protocol_ok)
  in
  let finished =
    trace_event
      6
      (Tool_activity
         (Finished
            { key = activity_key
            ; outcome = Returned
            ; output = Some (Text "actual output")
            }))
  in
  let current = Client.apply_live_event current finished |> protocol_ok in
  let cancelled : P.Event.Durable.Payload.t =
    Operation_cancelled { operation with state = Cancelled }
  in
  let shared = P.Public.Durable.Shared_payload.of_internal cancelled |> protocol_ok in
  let terminal =
    Client.apply_event current (public_event 2L (Shared shared) cancelled) |> protocol_ok
  in
  let model = model () in
  apply_projection
    (Chat_tui.Agent_event_apply.create ())
    model
    (Chat_tui.Agent_projection.of_client_projection terminal);
  let call = List.hd_exn (Chat_tui.Model.active_agent_calls model) in
  (match Chat_tui.Model.agent_call_outcome call with
   | Some Returned -> ()
   | Some (Raised | Cancelled) | None -> failwith "actual tool outcome was lost");
  assert (
    not
      (List.exists (Chat_tui.Model.agent_call_render_blocks call) ~f:(fun block ->
         match Chat_tui.Model.agent_render_block_view block with
         | Outcome_unavailable -> true
         | Invocation _ | Truncation | Waiting | Progress _ | Status _ -> false)))
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
    with_fields base ~f:(fun fields -> { fields with grants = [ grant ] })
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

let%expect_test "replacement snapshots replace absolute activity and clear omitted calls" =
  let module Client = Agent_client.Projection in
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let summary =
    P.Activity.Tool.summary
      activity_key
      ~descriptor:(Some activity_descriptor)
      ~channels:[ { channel = Assistant; text = "old"; complete = true } ]
      ~state:Running
    |> protocol_ok
  in
  let snapshot =
    with_fields base ~f:(fun fields ->
      { fields with
        session = { fields.session with active_operation = Some operation }
      ; active_tool_calls = [ summary ]
      ; active_agent_calls = [ summary ]
      })
  in
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let apply client =
    apply_projection applier model (Chat_tui.Agent_projection.of_client_projection client)
  in
  let current = Client.install_snapshot snapshot in
  apply current;
  apply current;
  [%test_eq: int] (List.length (Chat_tui.Model.active_agent_calls model)) 1;
  let summary =
    P.Activity.Tool.summary
      activity_key
      ~descriptor:(Some activity_descriptor)
      ~channels:[]
      ~state:Running
    |> protocol_ok
  in
  let snapshot =
    with_fields snapshot ~f:(fun fields ->
      { fields with active_tool_calls = [ summary ]; active_agent_calls = [ summary ] })
  in
  apply (Client.install_snapshot snapshot);
  [%test_eq: int]
    (List.length
       (Chat_tui.Model.agent_call_progress_entries
          (List.hd_exn (Chat_tui.Model.active_agent_calls model))))
    0;
  apply (Client.install_snapshot base);
  [%test_eq: int] (List.length (Chat_tui.Model.active_agent_calls model)) 0;
  [%test_eq: string] (Chat_tui.Model.input_line model) "unsent draft";
  [%expect {| |}]
;;

let%expect_test "redacted transcript keeps only its disclosed actual header" =
  let base = projection "base" |> Chat_tui.Agent_projection.snapshot in
  let entry =
    P.Public.History.redacted
      history_id
      ~provenance:Canonical
      (P.Public.History.Redaction.create ~disclosed_header:(Some (Call Function)))
    |> protocol_ok
  in
  let base =
    with_fields base ~f:(fun fields ->
      { fields with
        canonical_history = { fields.canonical_history with entries = [ entry ] }
      })
  in
  let view =
    Agent_client.Projection.install_snapshot base
    |> Chat_tui.Agent_projection.of_client_projection
  in
  [%test_eq: (string * string) list]
    (Chat_tui.Agent_projection.messages view)
    [ "tool", "[Content redacted]" ];
  [%expect {| |}]
;;
