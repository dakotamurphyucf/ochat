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

let history_entry text =
  let module Payload = History_entry.Payload in
  let payload =
    Payload.Semantic.create
      (Message
         { form = Input
         ; role = User
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:Payload.Metadata.empty
    |> Result.ok_or_failwith
    |> Payload.authored
  in
  Agent_protocol.Public.History.full
    (History_entry.create_with_id ~id:history_id payload)
    ~provenance:Canonical
  |> protocol_ok
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
    ; metadata_revision = 0L
    ; organization = Agent_protocol.Session_organization.Values.empty
    ; latest_event_sequence = 1L
    ; inference_summary = History_entry.Payload.Presence.Absent
    }
;;

let projection text =
  let window =
    Agent_protocol.Public.History.Window.
      { entries = [ history_entry text ]
      ; previous_cursor = None
      ; next_cursor = None
      ; reached_start = true
      ; reached_end = true
      ; structurally_complete = true
      }
  in
  let snapshot =
    Agent_protocol.Public.Snapshot.Fields.
      { session = session ()
      ; lifecycle = None
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
  Agent_protocol.Public.Snapshot.create snapshot
  |> protocol_ok
  |> Agent_client.Projection.install_snapshot
  |> Chat_tui.Agent_projection.of_client_projection
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
