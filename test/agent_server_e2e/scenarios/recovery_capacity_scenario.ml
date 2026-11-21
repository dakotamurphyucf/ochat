open! Core
module P = Agent_protocol
module Config = Support.Config_fixture
module Daemon = Support.Daemon_process
module Http = Support.Http_driver
module Provider = Support.Tui_stream_provider
module Fixture = Crash_recovery_fixture

let protocol_ok = Fixture.protocol_ok
let key = Fixture.key
let require = Fixture.require
let member json name = Jsonaf.member_exn name json
let string json name = member json name |> Jsonaf.string_exn
let provider_model = "gpt-4.1"
let tool_name = "context_remaining"

let policy =
  {|(recovery_capacity
      ((mode automatic)
       (host_total 8192)
       (targets
        (((prompt smoke)
          (profile first-party-openai-responses)
          (model gpt-4.1)
          (encoder openai_responses_v1)
          (total 16384)
          (input 6000)
          (output 500)
          (session_total 7000)
          (omitted_output_default 60)
          (checkpoint_reserve 512)
          (warning_lead 256)
          (estimate_margin 100))))))|}
;;

let configuration fixture recovery =
  Config.configuration fixture ()
  |> String.substr_replace_first
       ~pattern:"(tool_default deny)"
       ~with_:"(tool_default allow)"
  |> String.substr_replace_first
       ~pattern:"(max_root_agents 2)"
       ~with_:"(max_root_agents 3)"
  |> String.substr_replace_first
       ~pattern:"(unix_socket"
       ~with_:(recovery ^ "\n (unix_socket")
;;

let with_stock ?(stop = Fixture.stop) env fixture ~port ~config_path f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon.start_with_environment_overrides
        ~sw
        ~env
        ~fixture
        ~config_path
        ~environment_overrides:
          [ "API_URL", sprintf "http://127.0.0.1:%d" port
          ; "OPENAI_API_KEY", "recovery-capacity-loopback-fixture"
          ]
    in
    Exn.protect
      ~finally:(fun () -> stop env daemon)
      ~f:(fun () ->
        Fixture.wait_ready env daemon;
        let client =
          Http.create
            ~sw
            ~env
            ~port:(Config.http_port fixture)
            ~token:(Some (Config.admin_token fixture))
          |> Result.ok_or_failwith
        in
        Exn.protect
          ~finally:(fun () -> Http.shutdown client)
          ~f:(fun () ->
            let connection = Support.Data_integrity_http.connection client in
            let initialized =
              Agent_client.Session_handle.initialize_with_features
                connection
                ~implementation_name:"recovery-capacity-e2e"
                ~implementation_version:"dev"
                ~features:[ P.Recovery_query.Features.windows ]
              |> protocol_ok
            in
            f client connection initialized.P.Initialize.Response.server_id)))
;;

let start client (session : P.Session.t) attachment_id name =
  match
    Fixture.request
      client
      (Session_start
         { session_id = session.id
         ; attachment_id
         ; queue_if_limited = false
         ; idempotency_key = key name
         })
  with
  | Session_start _ -> ()
  | _ -> failwith "capacity fixture could not start its session"
;;

let enable connection (session : P.Session.t) ~server_id attachment_id =
  Agent_client.Recovery_windows.enable
    connection
    { session = P.Session_ref.create ~server_id ~session_id:session.id
    ; attachment_id
    ; expected_generation = session.generation
    }
  |> protocol_ok
;;

let reattach client (session : P.Session.t) name =
  match
    Http.request
      client
      (Session_attach
         { session_id = session.id
         ; requested_mode = Owner_read_write
         ; subscribe = false
         ; after_sequence = None
         ; reclaim_token = None
         ; idempotency_key = key name
         })
    |> protocol_ok
  with
  | { result = Session_attach result; _ } -> result.attachment.id
  | _ -> failwith "capacity fixture could not reattach the writer"
;;

let emit_remaining request call_id =
  let open Openai.Responses.Response_stream in
  let call arguments status =
    Item.Function_call
      { name = tool_name
      ; arguments
      ; call_id
      ; _type = "function_call"
      ; id = Some call_id
      ; status
      }
  in
  let arguments = "{}" in
  Provider.emit
    request
    [ Provider.added (call "" None)
    ; Function_call_arguments_done
        { arguments
        ; item_id = call_id
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ; Provider.done_ (call arguments (Some "completed"))
    ];
  Provider.finish request
;;

let result request ~call_id =
  member (Provider.body request) "input"
  |> Jsonaf.list_exn
  |> List.find_exn ~f:(fun item ->
    Option.equal
      Document_schema.Json.equal
      (Jsonaf.member "type" item)
      (Some (`String "function_call_output"))
    && String.equal (string item "call_id") call_id)
  |> fun row ->
  match
    string row "output" |> Jsonaf.of_string |> P.Invocation.outcome_of_json |> protocol_ok
  with
  | Complete page -> page
  | outcome ->
    raise_s [%sexp "context_remaining failed", (outcome : P.Invocation.outcome)]
;;

let observe
      ?(output = 100)
      env
      provider
      ~index
      client
      (session : P.Session.t)
      attachment_id
  =
  let call_id = sprintf "capacity-call-%d" index in
  (match
     Fixture.request
       client
       (Session_send_message
          { session_id = session.id
          ; attachment_id
          ; timing = Safe_boundary
          ; content =
              { kind = Plain_text; text = "Check ordinary context"; attachments = [] }
          ; idempotency_key = key call_id
          })
   with
   | Session_send_message receipt when Option.is_some receipt.operation_id -> ()
   | _ -> failwith "capacity fixture did not admit a foreground operation");
  let request = Provider.await_request provider env index in
  let body = Provider.body request in
  require
    (not
       (String.is_substring
          (Jsonaf.to_string body)
          ~substring:"This request is nearing its configured context limit."))
    "off, unknown or below-warning capacity produced an advisory";
  require (String.equal (string body "model") provider_model) "wrong actual request model";
  require
    (Document_schema.Json.equal
       (member body "max_output_tokens")
       (`Number (Int.to_string output)))
    "wrong actual output reserve";
  let names =
    member body "tools" |> Jsonaf.list_exn |> List.map ~f:(fun tool -> string tool "name")
  in
  require
    (List.equal String.equal names [ tool_name ])
    "the authored prompt did not select exactly one capacity tool";
  emit_remaining request call_id;
  let followup = Provider.await_request provider env (index + 1) in
  let result = result followup ~call_id in
  Provider.reply_summary followup "Ochat recovery capacity checked.";
  let finish () =
    let rec wait () =
      let state = Fixture.get client session.id in
      match state.session.observed_state, state.session.active_operation with
      | Idle, None -> ()
      | Failed error, _ ->
        raise_s [%sexp "capacity foreground failed", (error : P.Error.t)]
      | _ ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
        wait ()
    in
    wait ()
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. finish;
  result
;;

let assert_status ?window result status =
  let visible = member result "window" in
  (match window with
   | None -> require (Document_schema.Json.equal visible `Null) "unexpected window opt-in"
   | Some expected ->
     let actual = P.Recovery_window.Scope.of_json visible |> protocol_ok in
     require
       (P.Recovery_window.Scope.equal expected actual)
       "durable window opt-in was lost");
  let capacity = member result "capacity" in
  require
    (String.equal (string capacity "status") status)
    "unexpected reported capacity status";
  capacity
;;

let assert_manual_fallback capacity =
  require
    (String.equal (string capacity "requested_mode") "automatic"
     && String.equal (string capacity "effective_mode") "manual")
    "unqualified stock automation did not remain Manual"
;;

let validate_rejects env fixture =
  Eio.Switch.run (fun sw ->
    let invalid =
      String.substr_replace_first
        ~pattern:"(checkpoint_reserve 512)"
        ~with_:"(checkpoint_reserve 7000)"
        policy
      |> configuration fixture
      |> Config.write_configuration fixture ~name:"capacity-invalid.sexp"
    in
    let result =
      Daemon.run_cli ~sw ~env ~fixture ~arguments:[ "-config"; invalid; "-validate-only" ]
    in
    require
      (String.is_substring result.stderr.contents ~substring:"config.recovery_capacity")
      "stock configuration accepted reserves that exhaust the declared input budget")
;;

let run env environment =
  let fixture = Fixture.fixture env environment "recovery-capacity" in
  Fixture.write
    env
    (Config.prompt_path fixture)
    {|<config model="gpt-4.1" max_tokens="100"/><developer>Check context.</developer>
<tool name="context_remaining"/>|};
  validate_rejects env fixture;
  let default_config =
    configuration fixture ""
    |> Config.write_configuration fixture ~name:"capacity-off.sexp"
  in
  let enabled_config =
    configuration fixture policy
    |> Config.write_configuration fixture ~name:"capacity-enabled.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let session, window =
      with_stock
        env
        fixture
        ~port
        ~config_path:default_config
        (fun client connection server_id ->
           let session, attachment =
             Support.Data_integrity_http.create
               ~mode:Owner_read_write
               client
               "capacity-opted-in"
           in
           let window = enable connection ~server_id session attachment.id in
           start client session attachment.id "capacity-start-default";
           let observed = observe env provider ~index:0 client session attachment.id in
           ignore (assert_status ~window observed "disabled" : Jsonaf.t);
           session, window)
    in
    with_stock env fixture ~port ~config_path:enabled_config (fun client _ _ ->
      let attachment_id = reattach client session "context-capacity-reattach" in
      start client session attachment_id "capacity-start-after-restart";
      let observed = observe env provider ~index:2 client session attachment_id in
      let capacity = assert_status ~window observed "current" in
      assert_manual_fallback capacity;
      require
        (String.equal (string capacity "evidence") "estimated"
         && String.equal (string capacity "estimator") "O200k_serialized_prepared_request"
         && String.equal (string capacity "profile") "first-party-openai-responses")
        "no qualified prepared count";
      let count name =
        P.Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value (member capacity name)
        |> protocol_ok
      in
      require
        Int64.(
          count "input" > 0L
          && count "total" = 7000L
          && count "output_reserve" = 100L
          && count "hard_input" = 6000L
          && count "soft_input" = 5488L
          && count "warning_input" = 5232L
          && count "estimate_margin" = 100L)
        "qualified assessment did not apply checked limits to the admitted prepared \
         request";
      let other, other_attachment =
        Support.Data_integrity_http.create
          ~mode:Owner_read_write
          client
          "capacity-unselected"
      in
      start client other other_attachment.id "capacity-start-unselected";
      let observed = observe env provider ~index:4 client other other_attachment.id in
      ignore (assert_status observed "disabled" : Jsonaf.t));
    (* Startup now resolves the same model and profile with a different output setting.
       The old durable session has the original resolved target. *)
    Fixture.write
      env
      (Config.prompt_path fixture)
      {|<config model="gpt-4.1" max_tokens="120"/><developer>Check context.</developer>
<tool name="context_remaining"/>|};
    with_stock
      env
      fixture
      ~port
      ~config_path:enabled_config
      (fun client connection server_id ->
         let attachment_id = reattach client session "context-capacity-reattach-again" in
         start client session attachment_id "capacity-start-with-different-qualification";
         let observed = observe env provider ~index:6 client session attachment_id in
         let capacity = assert_status ~window observed "current" in
         assert_manual_fallback capacity;
         require
           (String.equal (string capacity "evidence") "unknown"
            && String.equal (string capacity "reason") "Policy_missing"
            && Option.is_none (Jsonaf.member "input" capacity))
           "changed target inherited the numeric capacity qualification";
         let current, attachment =
           Support.Data_integrity_http.create
             ~mode:Owner_read_write
             client
             "capacity-retargeted"
         in
         let current_window = enable connection ~server_id current attachment.id in
         start client current attachment.id "capacity-start-with-current-qualification";
         let observed =
           observe ~output:120 env provider ~index:8 client current attachment.id
         in
         let capacity = assert_status ~window:current_window observed "current" in
         assert_manual_fallback capacity;
         require
           (String.equal (string capacity "evidence") "estimated"
            && Document_schema.Json.equal
                 (member capacity "output_reserve")
                 (`Number "120"))
           "the currently resolved prompt lost its qualified estimate"))
;;
