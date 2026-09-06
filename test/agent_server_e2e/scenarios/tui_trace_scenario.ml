open Core
module F = Support.Tui_fixture
module Config = Support.Config_fixture
module Temp = Support.Temporary_environment
module Client = Chat_tui.Agent_session_client
module Projection = Chat_tui.Agent_projection
module Apply = Chat_tui.Agent_event_apply
module Model = Chat_tui.Model

let embedded_options fixture : Agent_server.Embedded.options =
  let roots = Temp.roots (Config.environment fixture) in
  { prompt_file = Config.prompt_path fixture
  ; workspace = Config.physical_workspace fixture
  ; tool_dir = Config.physical_workspace fixture
  ; home = roots.home
  ; data_root = Some roots.data
  ; start_immediately = true
  ; permission_profile = Agent_server.Embedded.default_permission_profile
  ; attachment_mode = Read_write
  ; event_capacity = 4096
  }
;;

let with_embedded_provider env fixture f =
  Eio.Switch.run (fun sw ->
    let host = Agent_server.Embedded.start ~sw ~env (embedded_options fixture) |> F.ok in
    Exn.protect
      ~f:(fun () ->
        let connection = Agent_server.Embedded.connect host in
        let client =
          Client.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            ~session_id:(Agent_server.Embedded.session_id host)
            ~mode:Read_write
            ()
          |> F.ok
        in
        Exn.protect
          ~f:(fun () -> f client (Agent_server.Embedded.connection host))
          ~finally:(fun () -> Client.close client))
      ~finally:(fun () -> Agent_server.Embedded.close host))
;;

let with_embedded env fixture f =
  with_embedded_provider (F.offline_environment env) fixture f
;;

let connected ~sw env fixture http =
  if http
  then
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" (Config.http_port fixture)))
      ~bearer_token:(Some (Config.admin_token fixture))
      ~notification_capacity:4096
    |> F.ok
  else Support.Unix_driver.connect ~sw ~env ~socket_path:(Config.unix_socket fixture)
;;

let with_connected env fixture http f =
  F.with_daemon env fixture (fun sw observer ->
    let connection = connected ~sw env fixture http in
    Exn.protect
      ~f:(fun () ->
        let client =
          Client.create
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            { prompt = "smoke"
            ; workspace = "physical"
            ; liveness = Detached
            ; permission_profile = None
            ; display_name = None
            ; labels = []
            ; mode = Read_write
            }
          |> F.ok
        in
        Exn.protect
          ~f:(fun () -> f client observer)
          ~finally:(fun () -> Client.close client))
      ~finally:(fun () -> Agent_client.Connection.close connection))
;;

let model draft =
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:draft
    ~auto_follow:false
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:3
    ~selection_anchor:(Some 1)
    ~mode:Insert
    ~draft_mode:Raw_xml
    ~selected_msg:None
    ~undo_stack:[ "undo-local", 2 ]
    ~redo_stack:[ "redo-local", 1 ]
    ~cmdline:"local-command"
    ~cmdline_cursor:4
;;

let local_state model =
  [%sexp
    (Model.input_line model : string)
  , (Model.cursor_pos model : int)
  , (Model.selection_anchor model : int option)
  , (Model.undo_stack model : (string * int) list)
  , (Model.redo_stack model : (string * int) list)
  , (Model.cmdline model : string)
  , (Model.cmdline_cursor model : int)
  , (Poly.equal (Model.mode model) Insert : bool)
  , (Poly.equal (Model.draft_mode model) Raw_xml : bool)
  , (Model.auto_follow model : bool)]
;;

let apply applier model projection =
  ignore
    (Apply.apply applier ~model ~viewport_height:20 projection |> F.ok
     : Model.projection_damage)
;;

let canonical_json entries =
  `Array (List.map entries ~f:Agent_protocol.History.entry_to_json) |> Jsonaf.to_string
;;

let assert_identity model snapshot sent =
  let canonical =
    List.map (Model.history_items model) ~f:Agent_session.History_codec.to_protocol
  in
  F.require
    (String.equal
       (canonical_json canonical)
       (canonical_json snapshot.Agent_protocol.Snapshot.canonical_history.entries))
    "TUI canonical IDs or payloads diverged";
  F.require
    (List.count canonical ~f:(fun entry ->
       Agent_protocol.History.Id.compare
         entry.id
         sent.Agent_protocol.Method_result.Send_message.history_id
       = 0)
     = 1)
    "TUI duplicated or lost its acknowledged history ID";
  F.assert_user canonical "trace-canonical-message"
;;

let final_projection env client observer sent =
  let session_id = (Projection.snapshot (Client.projection client)).session.id in
  let authoritative =
    F.await env (fun () ->
      let snapshot = Agent_client.Admin.get_session observer session_id |> F.ok in
      if snapshot.halted && Option.is_none snapshot.session.active_operation
      then Some snapshot
      else None)
  in
  let projection =
    F.await env (fun () ->
      let projection = Client.projection client in
      let snapshot = Projection.snapshot projection in
      if Int64.(snapshot.latest_event_sequence >= authoritative.latest_event_sequence)
      then Some projection
      else None)
  in
  F.require
    Int64.(
      authoritative.latest_event_sequence
      >= sent.Agent_protocol.Method_result.Send_message.mutation.latest_event_sequence)
    "authoritative projection predates message acceptance";
  authoritative, projection
;;

let assert_scroll_isolation first second =
  let before = Notty_scroll_box.scroll (Model.scroll_box first) in
  let second_scroll = Notty_scroll_box.scroll (Model.scroll_box second) in
  F.require
    (Model.chat_max_scroll first ~viewport_height:1 > 0)
    "TUI fixture has no scrollable history";
  let delta = if before > 0 then -1 else 1 in
  ignore (Model.scroll_chat_by first ~viewport_height:1 delta : bool);
  F.require
    (Notty_scroll_box.scroll (Model.scroll_box first) <> before)
    "TUI scroll did not move";
  F.require
    (Notty_scroll_box.scroll (Model.scroll_box second) = second_scroll)
    "TUI scroll leaked to another model"
;;

let render_history model width =
  let size = width, 20 in
  Chat_tui.Renderer_page_chat.prepare_startup_history ~size ~model;
  Chat_tui.Renderer_page_chat.relayout_history_synchronously ~size ~model;
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size ~model;
  F.require
    (Chat_tui.Renderer_page_chat.publish_startup_history ~size ~model)
    "TUI history was not fully published"
;;

let assert_rendering_isolation first second =
  List.iter
    [ first, 64; second, 90 ]
    ~f:(fun (model, width) -> render_history model width);
  let first_cache = (Model.chat_page first).msg_img_cache in
  let second_cache = (Model.chat_page second).msg_img_cache in
  let second_count = Hashtbl.length second_cache in
  F.require (not (phys_equal first_cache second_cache)) "TUI row caches are shared";
  F.require
    (Hashtbl.length first_cache > 0 && second_count > 0)
    "TUI row caches were not exercised";
  F.require
    (not
       (Option.equal
          Int.equal
          (Model.active_history_width first)
          (Model.active_history_width second)))
    "TUI widths were not independently rendered";
  assert_scroll_isolation first second;
  Model.clear_all_img_caches first;
  F.require
    (Hashtbl.is_empty first_cache && Hashtbl.length second_cache = second_count)
    "invalidating one TUI cache affected another"
;;

let assert_editor_isolation first second before_first before_second authoritative =
  F.require
    (Sexp.equal before_first (local_state first))
    "first editor state was overwritten by server projection";
  F.require
    (Sexp.equal before_second (local_state second))
    "second editor state was overwritten by server projection";
  F.require
    (not (phys_equal (Model.kv_store first) (Model.kv_store second)))
    "TUI models share local caches";
  let encoded = Agent_protocol.Snapshot.to_json authoritative |> Jsonaf.to_string in
  List.iter
    [ "first-private-draft"
    ; "second-private-draft"
    ; "local-command"
    ; "undo-local"
    ; "redo-local"
    ]
    ~f:(fun private_text ->
      F.require
        (not (String.is_substring encoded ~substring:private_text))
        "local editor state leaked into the session")
;;

let assert_snapshot_unchanged observer authoritative =
  let after =
    Agent_client.Admin.get_session
      observer
      authoritative.Agent_protocol.Snapshot.session.id
    |> F.ok
  in
  let encode snapshot = Agent_protocol.Snapshot.to_json snapshot |> Jsonaf.to_string in
  F.require
    (String.equal (encode authoritative) (encode after))
    "presentation changes mutated the server snapshot"
;;

let exercise ~presentation env client observer =
  let first, second = model "first-private-draft", model "second-private-draft" in
  let first_apply, second_apply = Apply.create (), Apply.create () in
  apply first_apply first (Client.projection client);
  apply second_apply second (Client.projection client);
  let before_first, before_second = local_state first, local_state second in
  let sent = Client.send_text client "trace-canonical-message" |> F.ok in
  let authoritative, projected = final_projection env client observer sent in
  List.iter
    [ first_apply, first; second_apply, second ]
    ~f:(fun (applier, model) ->
      apply applier model projected;
      apply applier model projected;
      assert_identity model authoritative sent);
  if presentation
  then (
    assert_editor_isolation first second before_first before_second authoritative;
    assert_rendering_isolation first second;
    assert_snapshot_unchanged observer authoritative);
  Model.messages first
;;

let across_transports ~presentation env temporary =
  let make name = F.create env temporary name in
  let local = with_embedded env (make "trace-local") (exercise ~presentation env) in
  let unix = with_connected env (make "trace-unix") false (exercise ~presentation env) in
  let http = with_connected env (make "trace-http") true (exercise ~presentation env) in
  F.require
    (List.equal [%equal: string * string] local unix)
    "Unix TUI messages differ from embedded";
  F.require
    (List.equal [%equal: string * string] local http)
    "HTTP TUI messages differ from embedded"
;;

let messages = across_transports ~presentation:false
let presentation = across_transports ~presentation:true
