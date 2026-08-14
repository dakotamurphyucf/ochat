open Core
module Model = Chat_tui.Model
module Projected_message = Chat_tui.Projected_message

let make_model ?(history = []) messages =
  Model.create
    ~history_items:history
    ~messages
    ~input_line:""
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
    ~mode:Normal
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let history_id sequence =
  History_entry.Id.create ~namespace:"identity-interactions" ~sequence
  |> Result.ok_or_failwith
;;

let row sequence text =
  Projected_message.canonical_row ~entry_id:(history_id sequence) ("assistant", text)
;;

let selected model =
  Model.selected_projected_id model |> Option.map ~f:Projected_message.Id.to_string
;;

let%expect_test "selection preserves identity and falls back to nearest survivor" =
  let model = make_model [] in
  let a = row 0 "a" in
  let b = row 1 "b" in
  let c = row 2 "c" in
  Model.reconcile_projected_rows model [ a; b; c ];
  Model.reconcile_messages model [ a.message; b.message; c.message ];
  Model.select_projected model (Some b.id);
  Model.reconcile_projected_rows model [ c; b; a ];
  Model.reconcile_messages model [ c.message; b.message; a.message ];
  let preserved = selected model in
  Model.reconcile_projected_rows model [ c; a ];
  Model.reconcile_messages model [ c.message; a.message ];
  let middle_fallback = selected model in
  Model.select_projected model (Some a.id);
  Model.reconcile_projected_rows model [ c ];
  Model.reconcile_messages model [ c.message ];
  let tail_fallback = selected model in
  Model.reconcile_projected_rows model [];
  Model.reconcile_messages model [];
  print_s
    [%sexp
      ((preserved, middle_fallback, tail_fallback, selected model)
       : string option * string option * string option * string option)];
  [%expect
    {|
    ((21:identity-interactions:1) (21:identity-interactions:0)
     (21:identity-interactions:2) ())
    |}]
;;

let%expect_test "search returns and selects a stable row ID" =
  let model = make_model [] in
  let a = row 0 "first" in
  let b = row 1 "needle" in
  let c = row 2 "last" in
  Model.reconcile_projected_rows model [ a; b; c ];
  Model.reconcile_messages model [ a.message; b.message; c.message ];
  let found =
    Chat_tui.Controller_history_search.find_next ~model ~query:"needle" ~dir:Forward
    |> Option.value_exn
  in
  ignore
    (Chat_tui.Controller_history_search.select_and_reveal
       ~model
       ~term:(Obj.magic 0)
       ~id:found
     : Chat_tui.Controller_types.reaction);
  Model.reconcile_projected_rows model [ c; a; b ];
  Model.reconcile_messages model [ c.message; a.message; b.message ];
  print_s
    [%sexp
      (( Projected_message.Id.to_string found
       , selected model
       , Model.selected_msg model
       , Option.map
           (Model.take_projected_reveal_request model)
           ~f:Projected_message.Id.to_string )
       : string * string option * int option * string option)];
  [%expect
    {|
    (21:identity-interactions:1 (21:identity-interactions:1) (2)
     (21:identity-interactions:1))
    |}]
;;

let%expect_test "stream buffers and tool metadata follow row identity" =
  let model = make_model [] in
  let id = history_id 4 in
  let id_string = History_entry.Id.to_string id in
  ignore
    (Model.apply_patches
       model
       [ Ensure_buffer { id = id_string; role = "tool_output" }
       ; Append_text { id = id_string; role = "tool_output"; text = "one" }
       ; Set_function_name { id = id_string; name = "read_file" }
       ; Set_function_output { id = id_string; output = "contents" }
       ]
     : Model.t);
  let buffer = Hashtbl.find_exn (Model.msg_buffers model) id_string in
  let streamed_row = Projected_message.canonical_row ~entry_id:id ("tool", "contents") in
  let prepended = row 0 "prepended" in
  Model.reconcile_projected_rows model [ prepended; streamed_row ];
  Model.reconcile_messages model [ prepended.message; streamed_row.message ];
  ignore
    (Model.set_tool_output_kind_for_row
       model
       ~id:buffer.row_id
       (Read_file { path = None })
     : bool);
  Model.reconcile_projected_rows model [ streamed_row; prepended ];
  let kind =
    match Model.tool_output_for_row model ~id:buffer.row_id with
    | Some (Read_file { path }) -> "read_file", path
    | Some Apply_patch -> "apply_patch", None
    | Some (Read_directory { path }) -> "read_directory", path
    | Some (Other { name }) -> Option.value name ~default:"other", None
    | None -> "none", None
  in
  print_s
    [%sexp
      (( Projected_message.Id.to_string buffer.row_id
       , Model.render_index_by_id model ~id:buffer.row_id
       , kind )
       : string * int option * (string * string option))];
  [%expect {| (21:identity-interactions:4 (0) (read_file ())) |}]
;;

let output_message text =
  Openai.Responses.Item.Output_message
    { role = Openai.Responses.Output_message.Assistant
    ; id = "provider"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let reasoning =
  Openai.Responses.Item.Reasoning
    { id = "reasoning"; summary = []; status = None; _type = "reasoning" }
;;

let%expect_test "delete is ID-addressed and noncanonical command rejection is visible" =
  let entry_a = History_entry.create_with_id ~id:(history_id 0) (output_message "a") in
  let hidden = History_entry.create_with_id ~id:(history_id 2) reasoning in
  let entry_b = History_entry.create_with_id ~id:(history_id 1) (output_message "b") in
  let model = make_model ~history:[ entry_a; hidden; entry_b ] [] in
  let a = row 0 "a" in
  let b = row 1 "b" in
  let notice_id =
    Projected_message.Id.local ~namespace:"notice" ~local_id:"one"
    |> Result.ok_or_failwith
  in
  let notice =
    Projected_message.
      { id = notice_id
      ; entry_id = None
      ; message = "system", "notice"
      ; provenance = Placeholder
      ; source = Placeholder { local_id = "one"; kind = "notice" }
      ; revision = 0
      }
  in
  Model.reconcile_projected_rows model [ notice; a; b ];
  Model.reconcile_messages model [ notice.message; a.message; b.message ];
  Model.select_projected model (Some b.id);
  let delete_reaction = Chat_tui.Controller_cmdline.execute_command model "delete" in
  ignore
    (Chat_tui.App_runtime.refresh_messages (Chat_tui.App_runtime.create ~model ())
     : Chat_tui.Model.projection_damage);
  let remaining =
    Model.history_items model
    |> List.map ~f:(fun entry -> History_entry.Id.to_string (History_entry.id entry))
  in
  let remaining_row = row 0 "a" in
  Model.reconcile_projected_rows model [ notice; remaining_row ];
  Model.reconcile_messages model [ notice.message; remaining_row.message ];
  Model.select_projected model (Some notice.id);
  ignore (Chat_tui.Controller_cmdline.execute_command model "delete");
  let last_message =
    List.last (Model.messages model)
    |> Option.map ~f:(fun (role, text) -> role ^ ": " ^ text)
  in
  print_s
    [%sexp
      (( remaining
       , Poly.equal delete_reaction Chat_tui.Controller_types.Refresh_messages
       , last_message
       , List.length (Model.history_items model) )
       : string list * bool * string option * int)];
  [%expect
    {|
    ((21:identity-interactions:0 21:identity-interactions:2) true
     ("system: Cannot delete a transient UI row.") 2)
    |}]
;;
