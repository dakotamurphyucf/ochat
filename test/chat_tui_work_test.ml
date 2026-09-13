open Core
open Chat_tui_projection_support
module P = Agent_protocol
module M = Chat_tui.Model
module V = Chat_tui.Agent_work_view

let job id : P.Job.t =
  { id = P.Id.Job.of_string id |> protocol_ok
  ; session_id
  ; generation = 0
  ; kind = Async_tool
  ; payload = `String "PRIVATE-ARGUMENT"
  ; status = Running
  ; retry_policy = Never
  ; attempt = 1
  ; created_at = timestamp
  ; started_at = Some timestamp
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; delivery = Pending
  ; launch = None
  ; progress =
      Some
        { sequence = 3
        ; channels =
            [ { channel = Stderr; text = "PRIVATE-PROGRESS"; truncated = false } ]
        }
  }
;;

let status kind id state =
  P.Extension_status.of_json
    (`Object
        [ "version", `Number "1"
        ; "kind", `String kind
        ; "id", `String id
        ; "generation", `Number "0"
        ; "state", `String state
        ])
  |> protocol_ok
;;

let snapshot jobs =
  let base = projection "existing conversation" |> Chat_tui.Agent_projection.snapshot in
  { base with
    P.Snapshot.jobs
  ; extension_status =
      [ status "invocation" "inv_ack" "published.pending"
      ; status "subscription" "sub_wait" "active"
      ]
  }
;;

let apply applier model client =
  Chat_tui.Agent_projection.of_client_projection client
  |> protocol_ok
  |> Chat_tui.Agent_event_apply.apply applier ~model ~viewport_height:10
  |> protocol_ok
;;

let render model (w, h) =
  let image, _ = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
  assert (Notty.I.width image <= Int.max 0 w && Notty.I.height image <= Int.max 0 h);
  let buffer = Buffer.create 512 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (w, h) image;
  Buffer.contents buffer
;;

let statuses model =
  (Option.value_exn (M.session_work model)).rows
  |> Array.to_list
  |> List.map ~f:(fun row -> row.V.label, row.status)
;;

let%expect_test
    "job events update work without rewriting acknowledgements or the chat draft"
  =
  let model = model () in
  let applier = Chat_tui.Agent_event_apply.create () in
  let running = job "job_work" in
  let client = Agent_client.Projection.install_snapshot (snapshot [ running ]) in
  apply applier model client |> ignore;
  let history = M.history_items model in
  let text = M.input_line model in
  Chat_tui.Controller_cmdline.execute_command model "work" |> ignore;
  (match M.active_page model with
   | Work -> ()
   | _ -> failwith "work command did not open page");
  let initial_output = render model (100, 16) in
  assert (String.is_substring initial_output ~substring:"progress update 3");
  assert (not (String.is_substring initial_output ~substring:"PRIVATE"));
  let terminal =
    { running with
      status = Succeeded
    ; completed_at = Some timestamp
    ; progress = None
    ; result = Some (P.Completion.to_json (Succeeded (`String "PRIVATE-RESULT")))
    }
  in
  let event sequence value =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence
      ~revision:sequence
      ~timestamp
      (Job_state_changed value)
  in
  let completed =
    Agent_client.Projection.apply_event client (event 2L terminal) |> protocol_ok
  in
  apply applier model completed |> ignore;
  print_s [%sexp (statuses model : (string * string) list)];
  let retired =
    { terminal with delivery = Discarded { at = timestamp; reason = Authority_changed } }
  in
  let completed =
    Agent_client.Projection.apply_event completed (event 3L retired) |> protocol_ok
  in
  apply applier model completed |> ignore;
  let output = render model (100, 16) in
  assert (String.is_substring output ~substring:"completion discarded: authority changed");
  assert (not (String.is_substring output ~substring:"PRIVATE"));
  M.set_connection_status model (Some (Chat_tui.Connection_status.disconnected ()));
  assert (
    String.is_substring (render model (160, 16)) ~substring:"last known state (offline)");
  let restored =
    Agent_client.Projection.snapshot completed
    |> P.Snapshot.to_json
    |> P.Snapshot.of_json
    |> protocol_ok
    |> Agent_client.Projection.install_snapshot
  in
  let before = Option.value_exn (M.session_work model) in
  apply applier model restored |> ignore;
  assert (V.equal before (Option.value_exn (M.session_work model)));
  let operation : P.Operation.t =
    { id = P.Id.Operation.of_string "op_work_followup" |> protocol_ok
    ; generation = 0
    ; kind = Turn Idle_followup
    ; state = Running
    ; started_at = timestamp
    ; updated_at = timestamp
    }
  in
  let next_turn =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence:4L
      ~revision:4L
      ~timestamp
      (Operation_started operation)
  in
  let started = Agent_client.Projection.apply_event restored next_turn |> protocol_ok in
  apply applier model started |> ignore;
  (match M.active_page model with
   | Work -> ()
   | _ -> failwith "new turn closed work page");
  List.iter
    [ 0, 0; 1, 1; 12, 3; 30, 7; 80, 20 ]
    ~f:(fun size -> render model size |> ignore);
  Chat_tui.Controller_work.For_testing.handle_key
    ~model
    ~size:(fun () -> 80, 20)
    (`Key (`Escape, []))
  |> ignore;
  (match M.active_page model with
   | Chat -> ()
   | _ -> failwith "escape did not return to chat");
  [%test_eq: string] text (M.input_line model);
  assert (
    Sexp.equal
      ([%sexp_of: History_entry.t list] history)
      ([%sexp_of: History_entry.t list] (M.history_items model)));
  print_endline
    "retirement survives reconnect; metadata-only rendering preserves draft and history";
  [%expect
    {|
    ((Subscription "Waiting for completion")
     ("Tool job" "Succeeded \194\183 completion pending")
     (Invocation "Background work acknowledged"))
    retirement survives reconnect; metadata-only rendering preserves draft and history
    |}]
;;

let%expect_test
    "work page retains its scroll anchor and clears scoped data on replacement"
  =
  let model = model () in
  let jobs = List.init 80 ~f:(fun i -> job (Printf.sprintf "job_%03d" i)) in
  let initial = snapshot jobs in
  let applier = Chat_tui.Agent_event_apply.create () in
  apply applier model (Agent_client.Projection.install_snapshot initial) |> ignore;
  Chat_tui.Controller_cmdline.execute_command model "jobs" |> ignore;
  let key key =
    Chat_tui.Controller_work.For_testing.handle_key ~model ~size:(fun () -> 80, 10) key
    |> ignore
  in
  key (`Key (`Page `Down, []));
  key (`Key (`ASCII 'j', []));
  let before = Option.value_exn (M.session_work model) in
  let anchor = before.rows.(M.work_offset model).key in
  let terminal =
    { (List.hd_exn jobs) with
      status = Cancelled
    ; completed_at = Some timestamp
    ; delivery = Not_required
    }
  in
  let next = { initial with jobs = terminal :: List.tl_exn jobs } in
  apply applier model (Agent_client.Projection.install_snapshot next) |> ignore;
  let after = Option.value_exn (M.session_work model) in
  [%test_eq: string] anchor after.rows.(M.work_offset model).key;
  key (`Key (`End, []));
  render model (80, 10) |> ignore;
  assert (M.work_offset model > 0);
  let observer =
    P.Principal.create
      ~id:principal_id
      ~authentication_kind:"fixture"
      ~scopes:(P.Scope.Set.of_list [ View_session_transcript ])
      ~attributes:[]
    |> protocol_ok
  in
  let narrowed = Agent_server.Principal_projection.snapshot observer next in
  apply applier model (Agent_client.Projection.install_snapshot narrowed) |> ignore;
  let visible = Option.value_exn (M.session_work model) in
  [%test_eq: int] 0 (Array.length visible.rows);
  assert (not (String.is_substring (render model (80, 10)) ~substring:"job_"));
  let reset = { initial with session = { initial.session with generation = 1 } } in
  apply applier model (Agent_client.Projection.install_snapshot reset) |> ignore;
  [%test_eq: int] 0 (Array.length (Option.value_exn (M.session_work model)).rows);
  [%test_eq: int] 0 (M.work_offset model);
  print_endline "anchor preserved; permission and generation replacement remove old work";
  [%expect {| anchor preserved; permission and generation replacement remove old work |}]
;;
