open! Core
open Command.Let_syntax
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator

let checked result =
  Result.map_error result ~f:(fun _ -> "Invalid provider operator argument")
  |> Result.ok_or_failwith
;;

let principal =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_local_provider_operator" |> checked)
    ~authentication_kind:"local.provider_operator"
    ~scopes:(P.Scope.Set.of_list [ Provider_view; Provider_manage; Provider_select ])
    ~attributes:[]
  |> checked
;;

let write env text = Eio.Flow.copy_string (text ^ "\n") (Eio.Stdenv.stdout env)

let require = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (DTO.Error.sexp_of_t error))
;;

let profile = DTO.Profile_id.of_string
let key = P.Idempotency_key.of_string

type target =
  | Local of Provider_runtime.t
  | Remote of Agent_client.Connection.t

let remote_error (error : P.Error.t) =
  let finite =
    match error.data with
    | `Object fields ->
      (match
         List.filter fields ~f:(fun (name, _) -> String.equal name "provider_error")
       with
       | [ (_, value) ] -> DTO.Error.of_json value |> Result.ok
       | _ -> None)
    | _ -> None
  in
  match finite with
  | Some Busy when not error.retryable -> DTO.Error.Network
  | Some finite -> finite
  | None ->
    (match error.code with
     | Permission_denied | Unauthenticated -> DTO.Error.Denied
     | Interrupted -> Submission_uncertain
     | Method_not_found -> Unsupported
     | Command_queue_full when error.retryable -> Busy
     | _ -> Network)
;;

let read_status ~clock ~request =
  Eio.Time.with_timeout_exn clock 5. (fun () ->
    let rec read () =
      match request () with
      | Error DTO.Error.Busy ->
        Eio.Time.sleep clock 0.05;
        read ()
      | result -> require result
    in
    read ())
;;

let request target command =
  match target with
  | Local runtime ->
    Provider_runtime.dispatch runtime ~actor:(Actor.trusted_local principal) command
  | Remote connection ->
    Agent_client.Connection.request connection command
    |> Result.map_error ~f:remote_error
    |> Result.bind ~f:(function
      | P.Public.Result.Non_history value -> Ok (P.Public.Result.Non_history.value value)
      | Private_provider_challenge value ->
        Ok (P.Method_result.Provider_login_challenge value)
      | _ -> Error DTO.Error.Invalid_request)
;;

let run ?connection_profile ?provider_home callback =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      match connection_profile with
      | Some path ->
        if Option.is_some provider_home
        then failwith "Provider home applies only to the local operator";
        let profile =
          Agent_transport_client.Connection_profile.load
            ~home:(Sys.getenv "HOME")
            ~env
            ~path
          |> checked
        in
        if
          Option.is_none
            (Agent_transport_client.Connection_profile.expected_server profile)
        then
          failwith
            "Provider operator connection profile requires a pinned server identity";
        let connection =
          Agent_transport_client.Connection_profile.connect
            profile
            ~sw
            ~env
            ~notification_capacity:128
          |> checked
        in
        Exn.protect
          ~finally:(fun () -> Agent_client.Connection.close connection)
          ~f:(fun () -> callback env sw (Remote connection))
      | None ->
        let platform =
          Provider_platform.create
            ~sw
            ~env
            ~home:
              (Option.value
                 provider_home
                 ~default:(Sys.getenv "HOME" |> Option.value ~default:"/"))
            ~api_url:(Sys.getenv "API_URL")
            ~lookup:Sys.getenv
            ~default_model:"gpt-5.4"
            ~namespace:(Provider_platform.new_namespace env)
            ~callback_port:1455
            ()
          |> require
        in
        let runtime =
          Provider_platform.open_operator
            platform
            ~server_id:(P.Id.Server.of_string "srv_local_provider_operator" |> checked)
          |> require
        in
        Exn.protect
          ~finally:(fun () -> Provider_runtime.close runtime)
          ~f:(fun () -> callback env sw (Local runtime))))
;;

let dispatch env runtime command =
  let result = request runtime command |> require in
  write env (P.Method_result.to_json result |> Jsonaf.to_string)
;;

let status =
  Command.basic
    ~summary:"Show bounded provider account and login state"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and selected = flag "-profile" (optional string) ~doc:"ID approved profile" in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_status
               { profile = Option.map selected ~f:(fun value -> profile value |> checked)
               }))]
;;

let setup =
  Command.basic
    ~summary:"Explicitly provision this private provider host"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key =
        flag "-key" (required string) ~doc:"ID stable idempotency key for setup/retry"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_setup { idempotency_key = key operation_key |> checked }))]
;;

let configure_environment =
  Command.basic
    ~summary:"Explicitly enroll the declared OPENAI_API_KEY reference"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key = flag "-key" (required string) ~doc:"ID stable idempotency key"
      and selected =
        flag
          "-profile"
          (optional_with_default "first-party-openai-responses" string)
          ~doc:"ID API-key profile"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_configure_environment
               { profile = profile selected |> checked
               ; source = DTO.Source_id.of_string "openai-api-key" |> checked
               ; idempotency_key = key operation_key |> checked
               }))]
;;

let configure_private =
  Command.basic
    ~summary:"Enroll an API key from an owned private file"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key = flag "-key" (required string) ~doc:"ID stable idempotency key"
      and selected =
        flag
          "-profile"
          (optional_with_default "first-party-openai-responses" string)
          ~doc:"ID API-key profile"
      and filename =
        flag
          "-key-file"
          (required string)
          ~doc:"PATH 0600 file in an owned 0700 directory"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env sw runtime ->
          let runtime =
            match runtime with
            | Local runtime -> runtime
            | Remote _ ->
              failwith "Protected key input is local only; keys are never sent over RPC"
          in
          let result =
            Provider_runtime.enroll_private_key
              runtime
              ~actor:(Actor.trusted_local principal)
              ~profile:(profile selected |> checked)
              ~key:(key operation_key |> checked)
              ~source_reference:filename
              ~sw
              ~read:(fun ~sw ->
                let open Result.Let_syntax in
                let parent = Filename.dirname filename in
                let component = Filename.basename parent in
                let anchor = Filename.dirname parent in
                let invalid _ =
                  Inference_host.Credential_bridge.Error.Invalid_credential
                in
                let%bind component =
                  Private_storage.Name.create component |> Result.map_error ~f:invalid
                in
                let%bind name =
                  Private_storage.Name.create (Filename.basename filename)
                  |> Result.map_error ~f:invalid
                in
                let%bind directory =
                  Private_storage.Directory.open_or_create
                    ~sw
                    ~anchor:Eio.Path.(Eio.Stdenv.fs env / anchor)
                    ~components:[ component ]
                  |> Result.map_error ~f:invalid
                in
                Exn.protect
                  ~finally:(fun () -> Private_storage.Directory.close directory)
                  ~f:(fun () ->
                    let%bind bytes =
                      Private_storage.Directory.read_bounded
                        directory
                        name
                        ~max_bytes:Provider_secret_store.Secret.maximum_bytes
                      |> Result.map_error ~f:invalid
                    in
                    Provider_secret_store.Secret.of_bytes
                      (Bytes.of_string (String.strip (Bytes.to_string bytes)))
                    |> Result.map_error ~f:invalid))
            |> require
          in
          write env (DTO.Configuration_result.to_json result |> Jsonaf.to_string))]
;;

let logout =
  Command.basic
    ~summary:"Disable the selected binding and drain admitted requests"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key = flag "-key" (required string) ~doc:"ID stable idempotency key"
      and selected = flag "-profile" (required string) ~doc:"ID approved profile" in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_logout
               { profile = profile selected |> checked
               ; idempotency_key = key operation_key |> checked
               }))]
;;

let select =
  Command.basic
    ~summary:"Select the profile used by future fresh inference captures"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key = flag "-key" (required string) ~doc:"ID stable idempotency key"
      and selected = flag "-profile" (required string) ~doc:"ID approved profile"
      and revision =
        flag
          "-expected-revision"
          (required string)
          ~doc:"ID current selection revision from status"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_select
               { profile = profile selected |> checked
               ; expected_revision = DTO.Revision.of_string revision |> checked
               ; idempotency_key = key operation_key |> checked
               }))]
;;

let login =
  Command.basic
    ~summary:"Acquire the approved direct-Codex subscription account"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key =
        flag "-key" (required string) ~doc:"ID stable original login/retry key"
      and selected =
        flag
          "-profile"
          (optional_with_default "direct-codex" string)
          ~doc:"ID approved OAuth profile"
      and mode =
        flag "-mode" (optional_with_default "device" string) ~doc:"MODE device or browser"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          let mode =
            match mode with
            | "device" -> DTO.Login_mode.Device
            | "browser" -> Browser
            | _ -> failwith "Mode must be device or browser"
          in
          let flow =
            match
              request
                runtime
                (P.Command.Provider_login_begin
                   { profile = profile selected |> checked
                   ; mode
                   ; idempotency_key = key operation_key |> checked
                   })
              |> require
            with
            | P.Method_result.Provider_login_begin flow -> flow
            | _ -> failwith "Unexpected login response"
          in
          write env (DTO.Flow_ref.to_json flow |> Jsonaf.to_string);
          (match request runtime (P.Command.Provider_login_challenge { flow }) with
           | Ok (P.Method_result.Provider_login_challenge challenge) ->
             ignore
               (DTO.Private_challenge.with_browser_uri challenge ~f:(fun uri ->
                  write env (Uri.to_string uri))
                : unit option);
             ignore
               (DTO.Private_challenge.with_device_prompt
                  challenge
                  ~f:(fun ~verification_uri ~user_code ->
                    write env (Uri.to_string verification_uri);
                    write env user_code)
                : unit option)
           | Error Challenge_unavailable -> ()
           | Error error -> ignore (require (Error error) : unit)
           | _ -> failwith "Unexpected challenge response");
          let rec await () =
            let status_result =
              read_status ~clock:(Eio.Stdenv.clock env) ~request:(fun () ->
                request
                  runtime
                  (P.Command.Provider_status { profile = Some flow.profile }))
            in
            match status_result with
            | P.Method_result.Provider_status status ->
              let current =
                List.find status.flows ~f:(fun result ->
                  DTO.Flow_id.equal result.flow.flow_id flow.flow_id)
              in
              (match current with
               | Some { phase = Completed; _ } -> write env "Login completed"
               | Some { phase = Pending; _ } ->
                 Eio.Time.sleep (Eio.Stdenv.clock env) 0.25;
                 await ()
               | Some result ->
                 failwith (Sexp.to_string_hum (DTO.Flow_result.sexp_of_t result))
               | None -> failwith "Login state unavailable")
            | _ -> failwith "Unexpected status response"
          in
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 900. await)]
;;

let cancel =
  Command.basic
    ~summary:"Cancel an owned original login flow"
    [%map_open
      let connection_profile =
        flag
          "-connection-profile"
          (optional string)
          ~doc:"PATH named daemon connection profile with pinned server identity"
      and provider_home =
        flag
          "-provider-home"
          (optional string)
          ~doc:"PATH local trusted home anchor for the provider store"
      and operation_key =
        flag "-key" (required string) ~doc:"ID stable cancellation idempotency key"
      and selected = flag "-profile" (required string) ~doc:"ID approved profile"
      and flow_id = flag "-flow" (required string) ~doc:"ID original flow identifier"
      and expires_at =
        flag
          "-expires-at"
          (required string)
          ~doc:"UTC original flow expiry from login/status"
      in
      fun () ->
        run ?connection_profile ?provider_home (fun env _ runtime ->
          dispatch
            env
            runtime
            (P.Command.Provider_login_cancel
               { flow =
                   { DTO.Flow_ref.server_id =
                       (match runtime with
                        | Local _ ->
                          P.Id.Server.of_string "srv_local_provider_operator" |> checked
                        | Remote connection ->
                          (Agent_client.Connection.initialization connection
                           |> Option.value_exn)
                            .server_id)
                   ; profile = profile selected |> checked
                   ; flow_id = DTO.Flow_id.of_string flow_id |> checked
                   ; expires_at = P.Timestamp.of_string expires_at |> checked
                   }
               ; idempotency_key = key operation_key |> checked
               }))]
;;

let command =
  Command.group
    ~summary:"Set up and administer the runtime-owned provider accounts"
    [ "setup", setup
    ; "status", status
    ; "login", login
    ; "cancel", cancel
    ; "configure-environment", configure_environment
    ; "configure-key-file", configure_private
    ; "logout", logout
    ; "select", select
    ]
;;

let%expect_test
    "remote provider errors preserve finite recovery and retry only status reads"
  =
  let wrapped finite ~retryable =
    P.Error.create
      Command_queue_full
      ~message:"synthetic provider error"
      ~retryable
      ~data:(`Object [ "provider_error", DTO.Error.to_json finite ])
      ()
  in
  assert (
    DTO.Error.equal (remote_error (wrapped Flow_expired ~retryable:false)) Flow_expired);
  assert (DTO.Error.equal (remote_error (wrapped Busy ~retryable:false)) Network);
  let queue ~retryable =
    P.Error.create Command_queue_full ~message:"synthetic queue" ~retryable ()
  in
  assert (DTO.Error.equal (remote_error (queue ~retryable:true)) Busy);
  assert (DTO.Error.equal (remote_error (queue ~retryable:false)) Network);
  Eio_main.run (fun env ->
    let reads = ref 0 in
    let value =
      read_status ~clock:(Eio.Stdenv.clock env) ~request:(fun () ->
        incr reads;
        if Int.equal !reads 1
        then Error (remote_error (wrapped Busy ~retryable:true))
        else Ok "terminal")
    in
    assert (Int.equal !reads 2 && String.equal value "terminal");
    let refused = ref 0 in
    match
      read_status ~clock:(Eio.Stdenv.clock env) ~request:(fun () ->
        incr refused;
        Error (remote_error (queue ~retryable:false)))
    with
    | exception Failure _ -> assert (Int.equal !refused 1)
    | _ -> failwith "nonretryable queue error was retried");
  print_endline
    "wrapped finite expiry retained; retryable Busy read retried once; nonretryable \
     queue refused";
  [%expect
    {| wrapped finite expiry retained; retryable Busy read retried once; nonretryable queue refused |}]
;;

let%expect_test "remote Busy status polling has a finite deadline" =
  Eio_main.run (fun env ->
    let clock = Eio_mock.Clock.make () in
    let quiet =
      { Eio.Debug.traceln =
          (fun ?__POS__:_ fmt -> Format.ifprintf Format.err_formatter fmt)
      }
    in
    Eio.Fiber.with_binding (Eio.Stdenv.debug env)#traceln quiet (fun () ->
      let started = ref false in
      match
        read_status ~clock ~request:(fun () ->
          started := true;
          Eio_mock.Clock.set_time clock 6.;
          Error DTO.Error.Busy)
      with
      | exception Eio.Time.Timeout -> assert !started
      | _ -> failwith "Busy polling escaped its deadline"));
  print_endline "read-only Busy polling stops at deadline";
  [%expect {| read-only Busy polling stops at deadline |}]
;;
