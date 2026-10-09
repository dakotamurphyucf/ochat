open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module B = Inference_host.Credential_bridge
module Admin = Provider_operator.Profile_admin
module O = Provider_oauth
module Actor = Operator_authorization
module Runtime = Provider_runtime
module Support = Agent_server_test_support

type route =
  | Http
  | Socket
  | Stdio
[@@deriving sexp_of]

type role =
  | Owner
  | Renewed_owner
  | Foreign
  | Viewer

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let model = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : M.Error.t)]
;;

let storage = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Private_storage.Error.t)]
;;

let secret = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Provider_secret_store.Error.t)]
;;

let oauth = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : O.Error.t)]
;;

let template = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Admin.Error.t)]
;;

let id name = M.Id.create name |> model
let key name = P.Idempotency_key.of_string name |> ok
let profile = DTO.Profile_id.of_string "direct" |> ok

module Client = struct
  type t =
    { request : P.Command.t -> (P.Public.Result.t, P.Error.t) result
    ; close : unit -> unit
    }

  let request t command = t.request command
  let close t = t.close ()
end

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; route : route
  ; daemon : Agent_server.Daemon.t
  ; actors : role -> Actor.t
  ; port : int option
  ; socket_path : string
  ; socket_actors : Actor.t Queue.t
  ; entered : unit Eio.Promise.t
  ; release : unit Eio.Promise.u
  ; now : P.Timestamp.t ref
  ; expires_at : P.Timestamp.t
  ; exited : int ref
  ; exchanges : int ref
  ; starts : int ref
  ; expire_flow : unit -> unit
  ; clients : Client.t list ref
  ; runtime : Runtime.t option ref
  ; directory : Private_storage.Directory.t
  }

let wire_command command =
  P.Envelope.request
    ~id:(P.Envelope.Request_id.of_json (`Number "1") |> ok)
    ~method_:(P.Command.method_name command)
    ~params:(P.Command.params command)
    ()
  |> P.Envelope.to_json
  |> Jsonaf.to_string
;;

let decode command json =
  let open Result.Let_syntax in
  let%bind envelope = P.Envelope.of_json (Jsonaf.of_string json) in
  match envelope with
  | Response response ->
    let%bind value = response.outcome in
    P.Public.Result.of_json ~method_:(P.Command.method_name command) value
  | Request _ | Notification _ ->
    Error (P.Error.invalid_request "expected route response")
;;

let initialize client =
  let implementation =
    P.Initialize.Implementation.create ~name:"provider-route" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:[]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:1048576
      ()
    |> ok
  in
  ignore (Client.request client (Protocol_initialize request) |> ok : P.Public.Result.t)
;;

let stream_client ~clock ~input ~output ~close =
  let reader = Eio.Buf_read.of_flow input ~max_size:1048576 in
  let closed = ref false in
  let close () =
    if not !closed
    then (
      closed := true;
      close ())
  in
  let request command =
    Eio.Time.with_timeout_exn clock 10. (fun () ->
      Eio.Flow.copy_string (wire_command command ^ "\n") output;
      decode command (Eio.Buf_read.line reader))
  in
  { Client.request; close }
;;

let http_client t role port =
  let connection_id = ref None in
  let closed = ref false in
  let token =
    match role with
    | Owner -> "owner"
    | Renewed_owner -> "renewed-owner"
    | Foreign -> "foreign"
    | Viewer -> "viewer"
  in
  let request command =
    if !closed
    then Error (P.Error.invalid_request "closed route client")
    else
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 10. (fun () ->
        Eio.Switch.run (fun sw ->
          let flow =
            Eio.Net.connect
              ~sw
              (Eio.Stdenv.net t.env)
              (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
          in
          let body = wire_command command in
          let extra =
            Option.value_map !connection_id ~default:"" ~f:(fun id ->
              "ochat-connection-id: " ^ id ^ "\r\n")
          in
          Eio.Flow.copy_string
            (sprintf
               "POST /v1/rpc HTTP/1.1\r\n\
                Host: localhost\r\n\
                Authorization: Bearer %s\r\n\
                Content-Type: application/json\r\n\
                Ochat-Protocol-Version: 2\r\n\
                Connection: close\r\n\
                %sContent-Length: %d\r\n\
                \r\n\
                %s"
               token
               extra
               (String.length body)
               body)
            flow;
          let reader = Eio.Buf_read.of_flow flow ~max_size:1048576 in
          ignore (Eio.Buf_read.line reader : string);
          let rec headers length =
            match Eio.Buf_read.line reader |> String.strip with
            | "" -> length
            | line ->
              (match String.lsplit2 line ~on:':' with
               | Some (name, value) when String.Caseless.equal name "content-length" ->
                 headers (Int.of_string (String.strip value))
               | Some (name, value) when String.Caseless.equal name "ochat-connection-id"
                 ->
                 connection_id := Some (String.strip value);
                 headers length
               | _ -> headers length)
          in
          decode command (Eio.Buf_read.take (headers 0) reader)))
  in
  (* Dropping the client connection deliberately leaves the host-owned flow alive.
     HTTP uses a logical connection; physical request sockets close each RPC. *)
  { Client.request; close = (fun () -> closed := true) }
;;

let connect t role =
  let client =
    match t.route with
    | Http -> http_client t role (Option.value_exn t.port)
    | Socket ->
      Queue.enqueue t.socket_actors (t.actors role);
      let flow = Eio.Net.connect ~sw:t.sw (Eio.Stdenv.net t.env) (`Unix t.socket_path) in
      stream_client
        ~clock:(Eio.Stdenv.clock t.env)
        ~input:flow
        ~output:flow
        ~close:(fun () -> Eio.Flow.close flow)
    | Stdio ->
      let request_input, request_output = Eio_unix.pipe t.sw in
      let response_input, response_output = Eio_unix.pipe t.sw in
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Switch.run (fun sw ->
          Agent_transport_stdio.Server.run_authenticated
            ~sw
            ~dispatcher:(Agent_server.Daemon.dispatcher t.daemon)
            ~close_connection:(Agent_server.Daemon.close_connection t.daemon)
            ~actor:(t.actors role)
            ~connection_id:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
            ~input:request_input
            ~output:response_output
            ~max_line_length:1048576
            ~outgoing_capacity:16
            ~max_attachments:8
            ~on_error:(fun error -> raise_s [%sexp (error : P.Error.t)])));
      stream_client
        ~clock:(Eio.Stdenv.clock t.env)
        ~input:response_input
        ~output:request_output
        ~close:(fun () ->
          Eio.Flow.close request_output;
          Eio.Flow.close response_input)
  in
  initialize client;
  t.clients := client :: !(t.clients);
  client
;;

let await_poll t =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 10. (fun () ->
    Eio.Promise.await t.entered)
;;

let release_poll t = Eio.Promise.resolve t.release ()

let hold_owner_records t =
  Private_storage.Lock.acquire
    t.directory
    (Private_storage.Name.create "operator-owner-records.lock" |> storage)
    ~sw:t.sw
    ~mode:Exclusive
  |> storage
;;

let await_worker_finished t flow =
  let records =
    Provider_operator.Owner_records.create
      t.directory
      ~incarnation:(id "unused_probe_incarnation")
      ~maximum_records:128
    |> function
    | Ok records -> records
    | Error error -> raise_s [%sexp (error : Provider_operator.Owner_records.Error.t)]
  in
  let rec wait () =
    match Provider_operator.Owner_records.claim records flow ~sw:t.sw with
    | Error Busy ->
      Eio.Fiber.yield ();
      wait ()
    | Error _ -> failwith "worker lease probe failed"
    | Ok lease -> Private_storage.Lock.release lease
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 5. wait
;;

let expire_owner t = t.now := t.expires_at
let expire_flow t = t.expire_flow ()
let poll_exits t = !(t.exited)
let exchanges t = !(t.exchanges)
let login_starts t = !(t.starts)

let assert_inference_unavailable t =
  let runtime = Option.value_exn !(t.runtime) in
  match
    Inference_host.Backend.capture
      (Runtime.backend runtime)
      ~current:None
      ~model:"synthetic"
      ~settings:[]
  with
  | Error Inference_runtime.Preparation_error.Target_unavailable -> ()
  | Error error -> raise_s [%sexp (error : Inference_runtime.Preparation_error.t)]
  | Ok _ -> failwith "OAuth-only host admitted inference before verified enrollment"
;;

let wait_terminal t client flow =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 10. (fun () ->
    let rec loop () =
      let result =
        let rec read () =
          match Client.request client (Provider_status { profile = None }) with
          | Error { code = Command_queue_full; retryable = true; _ } ->
            Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.01;
            read ()
          | result -> ok result
        in
        read ()
      in
      let status =
        match result with
        | Non_history value ->
          (match P.Public.Result.Non_history.value value with
           | Provider_status value -> value
           | _ -> failwith "wrong status")
        | _ -> failwith "wrong status projection"
      in
      match
        List.find status.flows ~f:(fun value ->
          DTO.Flow_id.equal value.DTO.Flow_result.flow.flow_id flow.DTO.Flow_ref.flow_id)
      with
      | Some { phase = Pending; _ } | None ->
        Eio.Fiber.yield ();
        loop ()
      | Some terminal -> terminal
    in
    loop ())
;;

let synthetic_tokens env =
  let number n = `Number (Int64.to_string n) in
  let now = Eio.Time.now (Eio.Stdenv.clock env) |> Int64.of_float in
  let jwt audience =
    let encode json =
      Base64.encode_exn
        ~pad:false
        ~alphabet:Base64.uri_safe_alphabet
        (Jsonaf.to_string json)
    in
    encode (`Object [ "alg", `String "RS256" ])
    ^ "."
    ^ encode
        (`Object
            [ "iss", `String "https://auth.openai.com"
            ; "sub", `String "route-subject"
            ; "aud", `String audience
            ; "iat", number now
            ; "exp", number Int64.(now + 3600L)
            ; ( "https://api.openai.com/auth"
              , `Object [ "chatgpt_account_id", `String "route-account" ] )
            ])
    ^ ".synthetic"
  in
  Jsonaf.to_string
    (`Object
        [ "token_type", `String "Bearer"
        ; "access_token", `String (jwt "opaque-provider-declaration")
        ; "id_token", `String (jwt "app_EMoamEEZ73f0CkXaXp7hrann")
        ; "scope", `String "openid profile email offline_access"
        ; "expires_in", `Number "3600"
        ; "refresh_token", `String "synthetic-route-refresh"
        ])
;;

let with_fixture ?flow_seconds route f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = Support.temporary_root env in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
            let anchor = Eio.Path.(Eio.Stdenv.fs env / root) in
            let mock_clock = Eio_mock.Clock.make () in
            let wall_clock = (mock_clock :> float Eio.Time.clock_ty Eio.Resource.t) in
            let clock_start = 1_700_000_000. in
            let set_time time =
              let quiet =
                { Eio.Debug.traceln =
                    (fun ?__POS__:_ fmt -> Format.ifprintf Format.err_formatter fmt)
                }
              in
              Eio.Fiber.with_binding (Eio.Stdenv.debug env)#traceln quiet (fun () ->
                Eio_mock.Clock.set_time mock_clock time)
            in
            set_time clock_start;
            let host_env : Eio_unix.Stdenv.base =
              object
                method stdin = env#stdin
                method stdout = env#stdout
                method stderr = env#stderr
                method net = env#net
                method domain_mgr = env#domain_mgr
                method process_mgr = env#process_mgr
                method clock = wall_clock
                method mono_clock = env#mono_clock
                method fs = env#fs
                method cwd = env#cwd
                method secure_random = env#secure_random
                method debug = env#debug
                method backend_id = env#backend_id
              end
            in
            let limits =
              match flow_seconds with
              | None -> DTO.Limits.default
              | Some max_flow_seconds ->
                DTO.Limits.create ~max_flows:8 ~max_profiles:8 ~max_flow_seconds |> ok
            in
            let expire_flow () =
              set_time
                (clock_start +. Float.of_int (DTO.Limits.max_flow_seconds limits) +. 1.)
            in
            let policy =
              O.Policy.direct_codex
                ~expected_account:(Some "route-account")
                ~callback_port:1455
                ()
              |> oauth
            in
            let entered, enter = Eio.Promise.create () in
            let released, release = Eio.Promise.create () in
            let exited = ref 0
            and exchanges = ref 0
            and starts = ref 0 in
            let transport =
              O.For_testing.scripted_transport
                ~clock:(Eio.Stdenv.mono_clock env)
                (fun endpoint ~body:_ ~on_possible_submission ->
                   match endpoint with
                   | User_code ->
                     Ok
                       ( 200
                       , {|{"device_auth_id":"route-device","user_code":"ROUTE-PRIVATE-CODE","interval":"1"}|}
                       )
                   | Device_poll ->
                     if not (Eio.Promise.is_resolved entered)
                     then Eio.Promise.resolve enter ();
                     Exn.protect
                       ~finally:(fun () -> incr exited)
                       ~f:(fun () ->
                         Eio.Promise.await released;
                         Ok
                           ( 200
                           , Jsonaf.to_string
                               (`Object
                                   [ "authorization_code", `String "synthetic-route-code"
                                   ; ( "code_verifier"
                                     , `String
                                         "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" )
                                   ; ( "code_challenge"
                                     , `String
                                         "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM" )
                                   ]) ))
                   | Token ->
                     on_possible_submission ();
                     incr exchanges;
                     Ok (200, synthetic_tokens host_env))
            in
            Exn.protect
              ~finally:(fun () -> O.Transport.close transport)
              ~f:(fun () ->
                let oauth =
                  Provider_oauth_registry.create ~transport ~policy ~wall_clock
                in
                let driver =
                  Openai.Responses_driver.create
                    ~net:(Eio.Stdenv.net env)
                    ~clock:(Eio.Stdenv.clock env)
                    ()
                  |> Or_error.ok_exn
                in
                let host = id "route-host"
                and binding = id "route-binding" in
                let expectation =
                  M.Expectation.oauth_acquisition
                    ~host
                    ~provider:"openai"
                    ~billing:"subscription"
                    ~issuer:(O.Policy.issuer policy)
                    ~client_registration:(O.Policy.client_registration policy)
                    ~resource:(O.Policy.resource policy)
                    ~account:(Some "route-account")
                    ~required_scopes:[ "openid"; "profile"; "email"; "offline_access" ]
                  |> model
                in
                let template =
                  Admin.Template.create
                    ~profile
                    ~binding
                    ~revision:(DTO.Revision.of_string "route-config" |> ok)
                    ~authentication:Direct_codex
                    ~expectation
                    ~expected_account:(Some "route-account")
                    ~mapping:(fun identity ->
                      let route =
                        Openai.Responses_driver.Profile.create
                          ~id:"direct"
                          ~account:(Some "route-account")
                          ~endpoint:"https://chatgpt.com/backend-api/codex/responses"
                          ~capabilities:
                            (Openai.Responses_driver.Capability.create
                               ~baseline:[ Text_input, Supported ]
                               ~models:[]
                             |> Or_error.ok_exn)
                          ~defaults:[]
                        |> Or_error.ok_exn
                      in
                      B.Mapping.create route ~revision:"route-config" ~binding ~identity)
                  |> template
                in
                let principal role =
                  P.Principal.create
                    ~id:
                      (P.Id.Principal.of_string
                         (match role with
                          | Owner | Renewed_owner -> "pri_route_owner"
                          | Foreign -> "pri_route_foreign"
                          | Viewer -> "pri_route_viewer")
                       |> ok)
                    ~authentication_kind:"synthetic.route"
                    ~scopes:
                      (P.Scope.Set.of_list
                         (match role with
                          | Viewer -> [ Provider_view ]
                          | Owner | Renewed_owner | Foreign ->
                            [ Provider_view; Provider_manage; Provider_select ]))
                    ~attributes:[]
                  |> ok
                in
                let now = ref (P.Timestamp.of_string "2026-10-08T12:00:00Z" |> ok) in
                let expires_at = P.Timestamp.of_string "2026-10-08T12:01:00Z" |> ok in
                let owner_actor =
                  Actor.bounded
                    ~principal:(principal Owner)
                    ~now:(fun () -> !now)
                    ~expires_at
                  |> ok
                in
                let renewed_actor =
                  Actor.bounded
                    ~principal:(principal Renewed_owner)
                    ~now:(fun () -> !now)
                    ~expires_at:(P.Timestamp.add_ms expires_at 60000 |> ok)
                  |> ok
                in
                let actors = function
                  | Owner -> owner_actor
                  | Renewed_owner -> renewed_actor
                  | role -> Actor.trusted_local (principal role)
                in
                let serial = ref 0 in
                let next () =
                  incr serial;
                  sprintf "route-operation-%d" !serial
                in
                let runtime = ref None in
                let factory ~sw ~server_id =
                  Provider_runtime_host.create
                    ~compatible_profiles:[]
                    ~sw
                    ~env:host_env
                    ~server_id
                    ~anchor
                    ~components:[ Private_storage.Name.create "authority" |> storage ]
                    ~host
                    ~secret_namespace:
                      (Provider_secret_store.Namespace.create "routes" |> secret)
                    ~driver
                    ~templates:[ template ]
                    ~mappings:[]
                    ~default_profile:profile
                    ~environment:None
                    ~environment_sources:[]
                    ~oauth
                    ~oauth_lease:
                      (Some
                         (B.OAuth.create
                            ~lease:(Provider_oauth_registry.lease oauth)
                            ~renewal:(Provider_oauth_registry.renewal oauth)))
                    ~start_login:(fun ~sw ~template:_ ~mode ->
                      incr starts;
                      match mode with
                      | Browser ->
                        failwith "route fixture only qualifies selected device flow"
                      | Device ->
                        O.Login.start_device
                          ~transport
                          ~policy
                          ~sw
                          ~clock:(Eio.Stdenv.mono_clock env)
                          ~wall_clock
                          ~maximum_wait:(Time_ns.Span.of_sec 20.))
                    ~inference_principal:"route-inference"
                    ~authorize_bridge:(fun ~principal:_ ~profile:_ ~operation:_ -> true)
                    ~authorize:(fun actor ~operation ~profile:_ ->
                      let actual = (Actor.principal actor).id in
                      P.Id.Principal.equal actual (principal Owner).id
                      || P.Id.Principal.equal actual (principal Foreign).id
                      || (DTO.Operation.equal operation Status
                          && P.Id.Principal.equal actual (principal Viewer).id))
                    ~authorize_setup:(fun actor ->
                      P.Id.Principal.equal (Actor.principal actor).id (principal Owner).id)
                    ~authorize_status:(fun _ -> true)
                    ~new_operation:(fun () -> id (next ()))
                    ~new_revision:(fun () -> DTO.Revision.of_string (next ()) |> ok)
                    ~maximum_wait:(Time_ns.Span.of_sec 1.)
                    ~limits
                    ~inference_limits:Inference_runtime.Limits.default
                    ~transport_policy:Http_sse
                  |> Result.map ~f:(fun opened ->
                    runtime := Some opened;
                    Runtime.operator_port opened)
                  |> Result.map_error
                       ~f:Agent_server.Provider_operator_port.protocol_error
                in
                let prompt = Filename.concat root "route.chatmd" in
                Eio.Path.save
                  ~create:(`Exclusive 0o600)
                  Eio.Path.(anchor / "route.chatmd")
                  "<developer>Operator routes.</developer>";
                let options =
                  { Agent_server.Daemon.default_options with
                    provider_operator_factory = Some factory
                  ; inference_policy =
                      Support.inference_policy
                        ~default_model:"synthetic"
                        ~post_stream:(fun ~sw:_ ~inputs:_ ->
                          failwith "unexpected inference")
                  }
                in
                let daemon =
                  Agent_server.Daemon.start
                    ~options
                    ~sw
                    ~env
                    ~config:(Support.config root root prompt)
                    ~tool_dir:root
                    ~home:root
                    ~process_start_identity:None
                    ()
                  |> ok
                in
                Exn.protect
                  ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> ok)
                  ~f:(fun () ->
                    let socket_path = Filename.concat root "route.sock" in
                    let socket_actors = Queue.create () in
                    let port =
                      match route with
                      | Stdio -> None
                      | Socket ->
                        let listener =
                          Eio.Net.listen
                            ~sw
                            ~backlog:8
                            (Eio.Stdenv.net env)
                            (`Unix socket_path)
                        in
                        Eio.Fiber.fork_daemon ~sw (fun () ->
                          Eio.Net.run_server
                            listener
                            (Agent_transport_socket.Server.serve
                               ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
                               ~close_connection:
                                 (Agent_server.Daemon.close_connection daemon)
                               ~authenticate:(fun _ _ ->
                                 Ok (Queue.dequeue_exn socket_actors))
                               ~max_line_length:1048576
                               ~outgoing_capacity:16
                               ~max_attachments:8
                               ~on_protocol_error:(fun error ->
                                 raise_s [%sexp (error : P.Error.t)]))
                            ~on_error:raise);
                        None
                      | Http ->
                        let address =
                          Eio.Switch.run (fun reserve_sw ->
                            Eio.Net.listen
                              ~sw:reserve_sw
                              ~backlog:1
                              (Eio.Stdenv.net env)
                              (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
                            |> Eio.Net.listening_addr)
                        in
                        Eio.Fiber.fork_daemon ~sw (fun () ->
                          Eio.Switch.run (fun server_sw ->
                            Agent_transport_http.Server.run
                              ~sw:server_sw
                              ~env
                              ~address
                              ~dispatcher:(Agent_server.Daemon.dispatcher daemon)
                              ~registry:(Agent_server.Daemon.registry daemon)
                              ~blob_store:(Agent_server.Daemon.blob_store daemon)
                              ~health:(Agent_server.Daemon.health daemon)
                              ~close_connection:
                                (Agent_server.Daemon.close_connection daemon)
                              ~authenticate:(fun _ token ->
                                match token with
                                | Some "owner" -> Ok (actors Owner)
                                | Some "renewed-owner" -> Ok (actors Renewed_owner)
                                | Some "foreign" -> Ok (actors Foreign)
                                | Some "viewer" -> Ok (actors Viewer)
                                | _ ->
                                  Error
                                    (P.Error.invalid_request
                                       "unknown synthetic route actor"))
                              ~max_body_bytes:1048576
                              ~max_batch_size:16
                              ~batch_concurrency:4
                              ~outgoing_capacity:16
                              ~max_connections:16
                              ~max_attachments:8
                              ~idle_connection_timeout:60.
                              ~on_error:raise);
                          `Stop_daemon);
                        let rec ready () =
                          match
                            Eio.Switch.run (fun probe_sw ->
                              Eio.Net.connect ~sw:probe_sw (Eio.Stdenv.net env) address
                              |> Eio.Flow.close)
                          with
                          | () -> ()
                          | exception
                              Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
                            Eio.Fiber.yield ();
                            ready ()
                        in
                        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. ready;
                        (match address with
                         | `Tcp (_, port) -> Some port
                         | `Unix _ -> assert false)
                    in
                    let directory =
                      Private_storage.Directory.open_or_create
                        ~sw
                        ~anchor
                        ~components:[ Private_storage.Name.create "authority" |> storage ]
                      |> storage
                    in
                    let fixture =
                      { env
                      ; sw
                      ; route
                      ; daemon
                      ; actors
                      ; port
                      ; socket_path
                      ; socket_actors
                      ; entered
                      ; release
                      ; now
                      ; expires_at
                      ; exited
                      ; exchanges
                      ; starts
                      ; expire_flow
                      ; clients = ref []
                      ; runtime
                      ; directory
                      }
                    in
                    Exn.protect
                      ~finally:(fun () -> List.iter !(fixture.clients) ~f:Client.close)
                      ~f:(fun () -> f fixture)))))))
;;
