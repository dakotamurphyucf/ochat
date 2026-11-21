open Core
module P = Agent_protocol
module A = Agent_session
module Stock = Recovery_capacity_scenario
module Checkpoint = Recovery_checkpoint_scenario
module Fixture = Crash_recovery_fixture
module Notes = Recovery_note_provider_scenario
module Provider = Support.Tui_stream_provider
module Config = Support.Config_fixture
module H = Agent_client.Session_handle
module D = Agent_server.Daemon

let ok = Fixture.protocol_ok
let require = Fixture.require
let advisory = "This request is nearing its configured context limit."
let required = "Preserve the exact required ordinary input."
let developer = "This is required developer guidance."

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/><developer>|} ^ developer ^ "</developer>"
;;

let entries request = Jsonaf.member_exn "input" (Provider.body request) |> Jsonaf.list_exn

let has_entry request ~role ~substring =
  List.exists (entries request) ~f:(fun entry ->
    Option.equal
      Document_schema.Json.equal
      (Jsonaf.member "role" entry)
      (Some (`String role))
    && String.is_substring (Jsonaf.to_string entry) ~substring)
;;

let check_request request ~reminder =
  require
    (has_entry request ~role:"developer" ~substring:developer)
    "developer input lost";
  require (has_entry request ~role:"user" ~substring:required) "required user input lost";
  require
    (Bool.equal (has_entry request ~role:"user" ~substring:advisory) reminder)
    "unexpected User-priority reminder at the physical provider boundary";
  require
    (not (has_entry request ~role:"developer" ~substring:advisory))
    "optional reminder gained developer authority"
;;

let rows (state : A.Session_state.t) =
  Option.value_map state.recovery_reminders ~default:[] ~f:(fun ledger ->
    A.Recovery_reminder.to_json ledger |> Jsonaf.list_exn)
;;

let receipts (state : A.Session_state.t) ~pending ~accepted =
  let pending_rows, accepted_rows =
    List.partition_tf (rows state) ~f:(fun row ->
      Document_schema.Json.equal (Jsonaf.member_exn "attempt" row) `Null)
  in
  require (Int.equal (List.length pending_rows) pending) "unexpected reminder claim count";
  require
    (Int.equal (List.length accepted_rows) accepted)
    "unexpected reminder acceptance count";
  let document =
    A.Session_state_document.encode
      (A.Session_state_document.authored state)
      ~limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string (Document_schema.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  require
    (Bool.equal
       (List.mem
          (Document_schema.Document.required_semantics document)
          P.Session_storage_semantics.recovery_reminders
          ~equal:String.equal)
       (pending + accepted > 0))
    "reminder receipts were stored without their required semantics";
  require
    (not
       (String.is_substring
          (Document_schema.Document.payload document |> Jsonaf.to_string)
          ~substring:advisory))
    "reminder text escaped into the durable session"
;;

let stock_config fixture ~lead name =
  Stock.policy
  |> String.substr_replace_first ~pattern:"(input 6000)" ~with_:"(input 1200)"
  |> String.substr_replace_first
       ~pattern:"(warning_lead 256)"
       ~with_:(sprintf "(warning_lead %d)" lead)
  |> Stock.configuration fixture
  |> Config.write_configuration fixture ~name
;;

let run_stock env environment =
  let fixture = Fixture.fixture env environment "recovery-reminder" in
  Fixture.write env (Config.prompt_path fixture) prompt;
  let initial = stock_config fixture ~lead:676 "reminder-initial.sexp" in
  let changed = stock_config fixture ~lead:675 "reminder-revised.sexp" in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let send ?(complete = true) client session attachment_id ~key ~reminder =
      let index = Provider.request_count provider in
      Checkpoint.send client session attachment_id ~key required;
      let request = Provider.await_request provider env index in
      check_request request ~reminder;
      if complete
      then (
        Provider.reply_summary request "Preserved.";
        Checkpoint.await_idle env client session)
    in
    let retained session attachment_id =
      Support.Background_fixture.checkpoint
        env
        fixture
        { summary = session; attachment_id }
      |> Option.value_exn
    in
    let stop_without_response env daemon =
      let module Daemon = Support.Daemon_process in
      Daemon.signal daemon Stdlib.Sys.sigkill;
      let result = Support.Tui_fixture.await env (fun () -> Daemon.result daemon) in
      require
        (Support.Process_manager.equal_exit result.exit (Signaled Stdlib.Sys.sigkill))
        "reminder fixture daemon did not stop at the claimed process cut"
    in
    let session, first_scope =
      Stock.with_stock
        ~stop:stop_without_response
        env
        fixture
        ~port
        ~config_path:initial
        (fun client connection server_id ->
           let session, attachment_id, scope =
             Checkpoint.enabled_session client connection ~server_id "near-limit"
           in
           (* The provider has the actual request but never replies. The receipt
           must already be Accepted before the daemon is stopped. *)
           send
             ~complete:false
             client
             session
             attachment_id
             ~key:"near-first"
             ~reminder:true;
           receipts (retained session attachment_id) ~pending:0 ~accepted:1;
           session, scope)
    in
    Stock.with_stock env fixture ~port ~config_path:initial (fun client _ _ ->
      let attachment_id = Stock.reattach client session "near-same-attachment" in
      Stock.start client session attachment_id "near-same-start";
      send client session attachment_id ~key:"near-same" ~reminder:false;
      receipts (retained session attachment_id) ~pending:0 ~accepted:1);
    Stock.with_stock env fixture ~port ~config_path:changed (fun client _ _ ->
      let attachment_id = Stock.reattach client session "near-revised-attachment" in
      Stock.start client session attachment_id "near-revised-start";
      send client session attachment_id ~key:"near-revised" ~reminder:true;
      send client session attachment_id ~key:"near-revised-repeat" ~reminder:false;
      (match
         Fixture.request
           client
           (Session_stop
              { session_id = session.id
              ; attachment_id
              ; mode = Graceful
              ; idempotency_key = Fixture.key "near-fresh-stop"
              })
       with
       | Session_stop _ -> ()
       | _ -> failwith "recovery generation reset could not stop the session");
      let before =
        Support.Tui_fixture.await env (fun () ->
          let current = Fixture.get client session.id in
          match current.session.observed_state with
          | Stopped -> Some current
          | _ -> None)
      in
      let reset =
        Fixture.request
          client
          (Session_reset
             { session_id = session.id
             ; attachment_id
             ; expected_revision = before.revision
             ; keep_history = false
             ; keep_tasks = true
             ; keep_cache = true
             ; keep_workspace = true
             ; keep_grants = true
             ; keep_labels = true
             ; idempotency_key = Fixture.key "near-fresh-reset"
             })
      in
      let fresh =
        match reset with
        | Session_reset reset -> reset.session
        | _ -> failwith "fresh recovery generation was not admitted"
      in
      let ledger = retained fresh attachment_id in
      let fresh_scope =
        Option.value_exn ledger.recovery_windows
        |> A.Recovery_window_ledger.active
        |> A.Recovery_window_ledger.Window.scope
      in
      require
        (not (P.Recovery_window.Scope.equal first_scope fresh_scope))
        "reset did not select a fresh recovery window";
      Stock.start client fresh attachment_id "near-fresh-start";
      send client fresh attachment_id ~key:"near-fresh" ~reminder:true;
      receipts (retained fresh attachment_id) ~pending:0 ~accepted:3))
;;

let capacity ~input ~reserve ~lead ~margin =
  let module C = Agent_server.Recovery_capacity_configuration in
  C.create
    ~mode:Manual
    ~host_total:8192L
    ~targets:
      [ C.Target.
          { prompt = "restart.prompt"
          ; profile = "generated-loopback-fixture"
          ; model = "gpt-4.1"
          ; encoder = Openai_responses_v1
          ; total = 16384L
          ; input = Some input
          ; output = Some 500L
          ; session_total = Some 7000L
          ; omitted_output_default = Some 60L
          ; checkpoint_reserve = reserve
          ; warning_lead = lead
          ; estimate_margin = margin
          }
      ]
  |> ok
;;

let enabled_actor daemon proof handle =
  let session_id = H.session_id handle in
  let entry =
    Agent_server.Session_registry.find (D.registry daemon) session_id |> Option.value_exn
  in
  let owner =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
      ~session_id
  in
  let authorize _state =
    Agent_server.Session_factory.check_registered_owner
      (D.factory daemon)
      entry
      ~session:owner
  in
  A.Session_actor.enable_recovery_windows
    entry.actor
    ~actor:proof
    ~attachment_id:(H.attachment handle).id
    ~authorize
  |> ok
  |> ignore;
  entry.actor
;;

let send_handle handle =
  H.send_message handle { kind = Plain_text; text = required; attachments = [] }
  |> ok
  |> fun (receipt : P.Method_result.Send_message.t) ->
  Option.value_exn receipt.operation_id
;;

let await_inactive env actor =
  Support.Tui_fixture.await env (fun () ->
    let state = A.Session_actor.state actor |> ok in
    Option.some_if (Option.is_none state.active_operation) state)
;;

let run_cut env environment =
  let cut, at_auth = Eio.Promise.create () in
  let block, _ = Eio.Promise.create () in
  let hold_first = ref true in
  let withdraw = ref false in
  let checks = ref 0 in
  let auth ~sw:_ _ =
    if !hold_first
    then (
      hold_first := false;
      Eio.Promise.resolve at_auth ();
      Eio.Promise.await block);
    let module Auth = Openai.Responses_driver.Auth in
    let%bind.Result lease = Auth.bearer "recovery-reminder-cut" in
    let check_current () =
      if !withdraw
      then (
        Int.incr checks;
        if !checks >= 2 then Error Auth.Denied else Ok ())
      else Ok ()
    in
    Auth.with_identity lease ~owner:"reminder-cut" ~generation:0L ~check_current
  in
  let config = capacity ~input:1200L ~reserve:512L ~lead:676L ~margin:100L in
  Notes.with_fixture
    ~prompt_xml:prompt
    ~recovery_capacity:config
    env
    environment
    ~auth
    ~max_sessions:2
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let handle = Notes.open_session sw env client ~mode:Owner_read_write in
       let actor = enabled_actor daemon proof handle in
       H.start handle ~queue_if_limited:false |> ok |> ignore;
       let operation = send_handle handle in
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         Eio.Promise.await cut);
       receipts (A.Session_actor.state actor |> ok) ~pending:1 ~accepted:0;
       H.cancel_operation handle operation |> ok |> ignore;
       receipts (await_inactive env actor) ~pending:1 ~accepted:0;
       require
         (Int.equal (Provider.request_count provider) 0)
         "canceled request crossed the write cut";
       ignore (send_handle handle : P.Id.Operation.t);
       let request = Provider.await_request provider env 0 in
       check_request request ~reminder:true;
       receipts (A.Session_actor.state actor |> ok) ~pending:0 ~accepted:1;
       Provider.reply_summary request "Preserved.";
       ignore (await_inactive env actor : A.Session_state.t);
       let handle = Notes.open_session sw env client ~mode:Owner_read_write in
       let actor = enabled_actor daemon proof handle in
       H.start handle ~queue_if_limited:false |> ok |> ignore;
       withdraw := true;
       ignore (send_handle handle : P.Id.Operation.t);
       receipts (await_inactive env actor) ~pending:1 ~accepted:0;
       require (!checks >= 2) "withdrawal did not reach the final transport check";
       require
         (Int.equal (Provider.request_count provider) 1)
         "credential withdrawal crossed the application-write boundary")
;;

let run_tight env environment =
  let preparations = ref 0 in
  let auth ~sw:_ _ = Openai.Responses_driver.Auth.bearer "recovery-reminder-tight" in
  let config = capacity ~input:103L ~reserve:20L ~lead:70L ~margin:0L in
  Notes.with_fixture
    ~prompt_xml:prompt
    ~recovery_capacity:config
    ~on_preparation:(fun () -> Int.incr preparations)
    env
    environment
    ~auth
    ~max_sessions:1
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let handle = Notes.open_session sw env client ~mode:Owner_read_write in
       let actor = enabled_actor daemon proof handle in
       H.start handle ~queue_if_limited:false |> ok |> ignore;
       ignore (send_handle handle : P.Id.Operation.t);
       let request = Provider.await_request provider env 0 in
       check_request request ~reminder:false;
       require
         (Int.equal !preparations 2)
         "the offered reminder did not undergo fresh preparation";
       receipts (A.Session_actor.state actor |> ok) ~pending:0 ~accepted:0;
       Provider.reply_summary request "Preserved.";
       ignore (await_inactive env actor : A.Session_state.t))
;;
