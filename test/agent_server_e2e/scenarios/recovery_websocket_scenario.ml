open! Core
module P = Agent_protocol
module Control = P.Recovery_rollover_control
module Scope = P.Recovery_window.Scope
module Client = Agent_client.Recovery_windows
module Fixture = Crash_recovery_fixture
module Stock = Recovery_capacity_scenario
module Rollover = Recovery_operator_rollover_scenario
module Config = Support.Config_fixture
module Data = Support.Data_integrity_http
module Wire = Provider_websocket_fixture
module Auth = Openai.Responses_driver.Auth

let retired_instruction = "Use the original WebSocket scenario instructions."
let retired_first = "The first retired WebSocket example is the red bicycle."
let retired_second = "The second retired WebSocket example is the blue telescope."
let successor_first = "The successor's local question is the green sailboat."
let successor_second = "The next local question is the yellow kite."
let response_id ordinal = sprintf "loopback-response-%d" ordinal
let reply_text ordinal = sprintf "The loopback assistant reply %d." ordinal

let completion ordinal =
  let item =
    Support.Tui_stream_provider.message (response_id ordinal) (reply_text ordinal)
  in
  let response =
    `Object
      [ "type", `String "response.completed"
      ; "sequence_number", `Number "1"
      ; ( "response"
        , `Object
            [ "object", `String "response"
            ; "id", `String (response_id ordinal)
            ; "status", `String "completed"
            ; "output", `Array [ Openai.Responses.Response_stream.Item.jsonaf_of_t item ]
            ] )
      ]
  in
  Wire.server_frame (Jsonaf.to_string response)
;;

let qualify_request body ~previous ~input_count ~present ~absent =
  let field name = Jsonaf.member_exn name body in
  Fixture.require
    (String.equal (Jsonaf.string_exn (field "type")) "response.create")
    "provider request was not the WebSocket application envelope";
  Fixture.require
    (Option.equal
       String.equal
       previous
       (Jsonaf.member "previous_response_id" body |> Option.map ~f:Jsonaf.string_exn))
    "provider request did not use the expected continuation state";
  Fixture.require
    (String.equal (Jsonaf.string_exn (field "model")) "gpt-4.1"
     && Document_schema.Json.equal (field "max_output_tokens") (`Number "100"))
    "provider request lost its selected model or output allowance";
  let input = field "input" in
  Fixture.require
    (Int.equal (List.length (Jsonaf.list_exn input)) input_count)
    "provider submitted the wrong number of current input items";
  let input = Jsonaf.to_string input in
  List.iter present ~f:(fun text ->
    Fixture.require
      (String.is_substring input ~substring:text)
      ("provider input was missing " ^ text));
  List.iter absent ~f:(fun text ->
    Fixture.require
      (not (String.is_substring input ~substring:text))
      ("provider input unexpectedly contained " ^ text))
;;

let run env environment =
  let fixture = Data.fixture env environment "recovery-websocket" in
  Fixture.write
    env
    (Config.prompt_path fixture)
    (sprintf
       {|<config model="gpt-4.1" max_tokens="100"/><developer>%s</developer>|}
       retired_instruction);
  let policy =
    String.substr_replace_first
      Stock.policy
      ~pattern:"first-party-openai-responses"
      ~with_:"generated-loopback-fixture"
  in
  Fixture.write env (Config.config_path fixture) (Stock.configuration fixture policy);
  let connections = ref 0 in
  let requests = Queue.create () in
  let handle flow =
    Int.incr connections;
    let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
    let _, fields = Wire.headers reader in
    Wire.upgrade flow fields;
    while true do
      Queue.enqueue requests (Wire.client_message reader);
      Eio.Flow.copy_string (completion (Queue.length requests)) flow
    done
  in
  Wire.with_server env handle (fun _ endpoint ->
    let token = "recovery-websocket-loopback-fixture" in
    Support.Temporary_environment.register_secret environment token;
    let auth ~sw:_ _ =
      let open Result.Let_syntax in
      let%bind token = Auth.bearer token in
      let%bind token =
        Auth.with_identity
          token
          ~owner:"local-loopback"
          ~generation:1L
          ~check_current:(fun () -> Ok ())
      in
      Auth.with_credential_revision token "local-loopback-revision"
    in
    let options =
      { Agent_server.Daemon.default_options with
        inference_policy =
          Generated_provider_scenario.inference_policy
            ~extra_features:[ Websocket ]
            ~transport_policy:Require_websocket
            env
            ~endpoint
            ~auth
      }
    in
    Support.Daemon_host.with_ env fixture ~options (fun sw _daemon ->
      Rollover.with_client
        ~sw
        env
        fixture
        ~token:(Config.admin_token fixture)
        (fun http connection initialized ->
           let session, attachment =
             Data.create ~mode:Owner_read_write http "websocket-session"
           in
           let reference =
             P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
           in
           let predecessor =
             Stock.enable
               connection
               ~server_id:initialized.server_id
               session
               attachment.id
           in
           Stock.start http session attachment.id "websocket-start";
           let send ~ordinal text =
             Recovery_checkpoint_scenario.send
               http
               session
               attachment.id
               ~key:(sprintf "websocket-turn-%d" ordinal)
               text;
             Recovery_checkpoint_scenario.await_idle env http session;
             Fixture.require
               (Int.equal (Queue.length requests) ordinal)
               "completed foreground did not send exactly one WebSocket request";
             List.last_exn (Queue.to_list requests)
           in
           qualify_request
             (send ~ordinal:1 retired_first)
             ~previous:None
             ~input_count:2
             ~present:[ retired_instruction; retired_first ]
             ~absent:[ retired_second ];
           qualify_request
             (send ~ordinal:2 retired_second)
             ~previous:(Some (response_id 1))
             ~input_count:1
             ~present:[ retired_second ]
             ~absent:[ retired_instruction; retired_first; reply_text 1 ];
           let before = Rollover.basis connection reference attachment.id in
           Fixture.require
             (Scope.equal before.scope predecessor)
             "first window changed before the cut";
           let receipt =
             Client.request_rollover
               connection
               { expected = before
               ; attachment_id = attachment.id
               ; key = Fixture.key "websocket-cut"
               ; force = true
               }
             |> Fixture.protocol_ok
           in
           let successor =
             match receipt.outcome with
             | Committed { successor; quality = Archive_only_missing_checkpoint; _ } ->
               successor
             | _ -> failwith "forced rollover did not commit the archive-only cut"
           in
           Rollover.check_windows connection reference predecessor successor;
           Fixture.require
             (Int.equal (Queue.length requests) 2)
             "the idle cut issued a provider request";
           qualify_request
             (send ~ordinal:3 successor_first)
             ~previous:None
             ~input_count:1
             ~present:[ successor_first ]
             ~absent:
               [ retired_instruction
               ; retired_first
               ; retired_second
               ; reply_text 1
               ; reply_text 2
               ];
           qualify_request
             (send ~ordinal:4 successor_second)
             ~previous:(Some (response_id 3))
             ~input_count:1
             ~present:[ successor_second ]
             ~absent:
               [ retired_instruction
               ; successor_first
               ; retired_first
               ; retired_second
               ; reply_text 3
               ];
           Fixture.require
             (Int.equal !connections 1)
             "the provider WebSocket was replaced during recovery")))
;;
