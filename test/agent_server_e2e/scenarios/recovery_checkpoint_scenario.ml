open! Core
module P = Agent_protocol
module Q = P.Recovery_query
module Scope = P.Recovery_window.Scope
module Provider = Support.Tui_stream_provider
module Notes = Recovery_note_provider_scenario
module Stock = Recovery_capacity_scenario
module Fixture = Crash_recovery_fixture
module Rollover = Agent_session.Recovery_rollover

let protocol_ok = Fixture.protocol_ok
let require = Fixture.require
let result request id = Notes.output request ~id
let string value = `String value
let checkpoint_name = "context_checkpoint"
let note_body = "Private handoff note from the loopback agent."

let write_prompt env fixture ~select =
  let checkpoint = if select then {|<tool name="context_checkpoint"/>|} else "" in
  Fixture.write
    env
    (Support.Config_fixture.prompt_path fixture)
    ({|<config model="gpt-4.1" max_tokens="100"/>
<developer>Use the selected local context tools only.</developer>
<tool name="recovery_note_write"/><tool name="context_new_window"/>|}
     ^ checkpoint)
;;

let send client (session : P.Session.t) attachment_id ~key text =
  match
    Fixture.request
      client
      (Session_send_message
         { session_id = session.id
         ; attachment_id
         ; timing = Safe_boundary
         ; content = { kind = Plain_text; text; attachments = [] }
         ; idempotency_key = Fixture.key key
         })
  with
  | Session_send_message receipt when Option.is_some receipt.operation_id -> ()
  | _ -> Fixture.fail "checkpoint fixture did not admit the foreground operation"
;;

let await_finished env client (session : P.Session.t) =
  let rec wait () =
    let current = Fixture.get client session.id in
    match current.session.observed_state, current.session.active_operation with
    | Idle, None -> Ok ()
    | Failed failure, _ -> Error failure
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      wait ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. wait
;;

let await_idle env client session = await_finished env client session |> protocol_ok

let retained_rollover env fixture summary attachment_id =
  let state =
    Support.Background_fixture.checkpoint env fixture { summary; attachment_id }
    |> Option.value_exn ~message:"checkpoint session storage was unavailable"
  in
  state.recovery_rollover
;;

let retained_checkpoint env fixture summary attachment_id =
  Option.bind (retained_rollover env fixture summary attachment_id) ~f:Rollover.checkpoint
;;

let enabled_session client connection ~server_id name =
  let session, attachment =
    Support.Data_integrity_http.create ~mode:Owner_read_write client name
  in
  let scope = Stock.enable connection ~server_id session attachment.id in
  Stock.start client session attachment.id (name ^ "-start");
  session, attachment.id, scope
;;

let assert_status request id expected =
  match result request id with
  | Complete body ->
    require
      (Document_schema.Json.equal body (`Object [ "status", string expected ]))
      "checkpoint receipt did not match the expected body-free status"
  | outcome -> raise_s [%sexp "checkpoint tool failed", (outcome : P.Invocation.outcome)]
;;

let tools request =
  Jsonaf.member_exn "tools" (Provider.body request)
  |> Jsonaf.list_exn
  |> List.map ~f:(fun tool -> Jsonaf.member_exn "name" tool |> Jsonaf.string_exn)
  |> String.Set.of_list
;;

let checkpoint request scope ~id ~key ~readback =
  let notes =
    if readback then [] else [ `Object [ "id", string "1"; "revision", string "1" ] ]
  in
  Notes.function_call
    request
    ~id
    ~name:checkpoint_name
    (`Object
        [ "expected_window", P.Id.Context_window.to_json (Scope.window scope)
        ; "key", string key
        ; "mode", string (if readback then "readback" else "register")
        ; "notes", `Array notes
        ])
;;

let request_transition request scope ~id =
  Notes.function_call
    request
    ~id
    ~name:"context_new_window"
    (`Object [ "expected_window", P.Id.Context_window.to_json (Scope.window scope) ])
;;

let list_windows connection scope =
  let page = P.Page.Request.create ~limit:10 () |> protocol_ok in
  (Agent_client.Recovery_windows.list
     connection
     { session = Scope.session scope
     ; generation = Some (Scope.generation scope)
     ; state = None
     ; page
     }
   |> protocol_ok)
    .items
;;

let require_successor connection previous =
  let windows = list_windows connection previous in
  require
    (List.exists windows ~f:(fun row ->
       Q.State.equal row.state Sealed && Scope.equal row.scope previous)
     && List.exists windows ~f:(fun row ->
       Q.State.equal row.state Open && not (Scope.equal row.scope previous)))
    "the actual settled foreground did not publish the archive-backed successor"
;;

let require_previous connection scope =
  match list_windows connection scope with
  | [ row ] when Q.State.equal row.state Open && Scope.equal row.scope scope -> ()
  | _ -> Fixture.fail "stale checkpoint unexpectedly changed the active window"
;;

let write_and_register request ~start scope ~id =
  let first = request start in
  require (Set.mem (tools first) checkpoint_name) "authored checkpoint is unselected";
  let write_id = id ^ "-write" in
  Notes.function_call
    first
    ~id:write_id
    ~name:"recovery_note_write"
    (`Object
        [ "kind", string "create"
        ; "expected", `Null
        ; "path", string "handoff/context"
        ; "expected_catalog_revision", string "0"
        ; "content", string note_body
        ]);
  let second = request (start + 1) in
  (match result second write_id with
   | Complete `Null -> ()
   | outcome ->
     raise_s [%sexp "selected note write failed", (outcome : P.Invocation.outcome)]);
  checkpoint second scope ~id ~key:id ~readback:false;
  let acknowledgment = request (start + 2) in
  assert_status acknowledgment id "accepted";
  acknowledgment
;;

let run env environment =
  let fixture = Fixture.fixture env environment "authored-checkpoint" in
  write_prompt env fixture ~select:true;
  let config_path =
    String.substr_replace_first
      Stock.policy
      ~pattern:"(mode automatic)"
      ~with_:"(mode manual)"
    |> Stock.configuration fixture
    |> Support.Config_fixture.write_configuration fixture ~name:"authored-checkpoint.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let request index = Provider.await_request provider env index in
    let saved, saved_scope, registration =
      Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
        let session, attachment, scope =
          enabled_session client connection ~server_id "checkpoint-then-rollover"
        in
        send
          client
          session
          attachment
          ~key:"start-checkpoint"
          "record and start a new local context";
        let acknowledged =
          write_and_register request ~start:0 scope ~id:"success-proof"
        in
        request_transition acknowledged scope ~id:"successful-transition";
        let successor = request 3 in
        require_successor connection scope;
        let input = Jsonaf.member_exn "input" (Provider.body successor) |> Jsonaf.list_exn in
        require
          (not
             (List.exists input ~f:(fun item ->
                Option.equal
                  Document_schema.Json.equal
                  (Jsonaf.member "call_id" item)
                  (Some (string "successful-transition")))))
          "the successor replayed the old rollover call";
        let open_window =
          list_windows connection scope
          |> List.find_exn ~f:(fun row -> Q.State.equal row.state Open)
        in
        (match
           Option.bind
             (retained_rollover env fixture session attachment)
             ~f:(Rollover.foreground ~scope:open_window.scope)
         with
         | Some { phase = Admitted _; _ } -> ()
         | _ -> Fixture.fail "the observed request has no committed successor admission");
        Provider.reply_summary successor "Continuing in the new local window.";
        await_idle env client session;
        let saved, attachment, saved_scope =
          enabled_session client connection ~server_id "checkpoint-before-restart"
        in
        send client saved attachment ~key:"save-checkpoint" "save an exact checkpoint";
        let acknowledged =
          write_and_register request ~start:4 saved_scope ~id:"saved-proof"
        in
        Provider.reply_summary acknowledged "Another substantive assistant message.";
        await_idle env client saved;
        require_previous connection saved_scope;
        let registration =
          retained_checkpoint env fixture saved attachment
          |> Option.value_exn ~message:"the registered cut was not durable"
        in
        saved, saved_scope, registration)
    in
    write_prompt env fixture ~select:false;
    Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
      let attachment = Stock.reattach client saved "checkpoint-reopen-attachment" in
      Stock.start client saved attachment "checkpoint-reopen-start";
      send
        client
        saved
        attachment
        ~key:"follow-checkpoint"
        "more user input after the checkpoint";
      let first = request 7 in
      checkpoint first saved_scope ~id:"readback" ~key:"saved-proof" ~readback:true;
      let readback = request 8 in
      assert_status readback "readback" "accepted";
      checkpoint readback saved_scope ~id:"retry" ~key:"saved-proof" ~readback:false;
      let replayed = request 9 in
      assert_status replayed "retry" "accepted";
      request_transition replayed saved_scope ~id:"stale-transition";
      let resumed = request 10 in
      let transition_id =
        match result resumed "stale-transition" with
        | Complete body ->
          require
            (String.equal (Stock.string (Stock.member body "status") "kind") "pending")
            "stale checkpoint did not reach actual rollover settlement";
          P.Id.Operation.of_json (Stock.member body "id") |> protocol_ok
        | outcome ->
          raise_s
            [%sexp "stale rollover was not admitted", (outcome : P.Invocation.outcome)]
      in
      Provider.reply_summary resumed "The original checkpoint is stale.";
      await_idle env client saved;
      require_previous connection saved_scope;
      (match
         Option.bind
           (retained_rollover env fixture saved attachment)
           ~f:(Rollover.find ~id:transition_id)
       with
       | Some { status = Terminal (Superseded Checkpoint_required); _ } -> ()
       | _ -> Fixture.fail "rollover did not reject the stale checkpoint at settlement");
      require
        (Option.exists
           (retained_checkpoint env fixture saved attachment)
           ~f:(Rollover.Checkpoint.equal registration))
        "an exact replay changed the persisted checkpoint cut";
      let other, attachment, scope =
        enabled_session client connection ~server_id "checkpoint-unselected-agent"
      in
      send
        client
        other
        attachment
        ~key:"unselected-checkpoint"
        "the checkpoint capability is unselected";
      let first = request 11 in
      require
        (not (Set.mem (tools first) checkpoint_name))
        "unselected tool was advertised";
      checkpoint first scope ~id:"unselected" ~key:"unselected" ~readback:false;
      Eio.Fiber.first
        (fun () -> ignore (await_finished env client other : (unit, P.Error.t) result))
        (fun () ->
           let followup = request 12 in
           (match result followup "unselected" with
            | Fail _ -> ()
            | Complete _ | Pending _ | Cancelled _ ->
              Fixture.fail "unselected checkpoint was admitted");
           Provider.reply_summary followup "No checkpoint was recorded.";
           await_idle env client other);
      require
        (Option.is_none (retained_checkpoint env fixture other attachment))
        "unselected checkpoint was recorded in the journal";
      require_previous connection scope))
;;
