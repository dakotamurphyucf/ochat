open! Core
module P = Agent_protocol
module Connection = Agent_client.Connection
module Repl = Agent_client.Developer_repl_connection
module Child = Agent_transport_stdio.Client
module Profile = Agent_transport_client.Connection_profile

module Local = struct
  type storage =
    | Durable of string
    | Transient

  type t =
    { executable : string
    ; policy_file : string
    ; prompt_file : string
    ; workspace : string
    ; storage : storage
    }

  let create ~executable ~policy_file ~prompt_file ~workspace ~storage =
    if
      String.is_empty executable
      || (not (Filename.is_absolute policy_file))
      || (not (Filename.is_absolute prompt_file))
      || (not (Filename.is_absolute workspace))
      ||
      match storage with
      | Transient -> false
      | Durable path -> not (Filename.is_absolute path)
    then Error (P.Error.invalid_request "local REPL paths must be absolute")
    else Ok { executable; policy_file; prompt_file; workspace; storage }
  ;;

  let argv t =
    [ t.executable
    ; "--local"
    ; "--developer-only"
    ; "--prompt"
    ; t.prompt_file
    ; "--workspace"
    ; t.workspace
    ; "--developer-policy"
    ; t.policy_file
    ]
    @
    match t.storage with
    | Transient -> [ "--transient" ]
    | Durable path -> [ "--data-root"; path ]
  ;;
end

type target =
  | Local of Local.t
  | Remote of Profile.t

type t =
  { client : Repl.t
  ; connection : Connection.t
  ; description : string
  }

let client t = t.client
let connection t = t.connection
let description t = t.description

let initialize_local connection =
  let open Result.Let_syntax in
  let%bind implementation =
    P.Initialize.Implementation.create ~name:"chatml-repl" ~version:"1"
  in
  let%bind request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        [ P.Developer_common.Features.run
        ; P.Developer_repl_wire.Features.repl
        ; P.Developer_repl_wire.Features.events
        ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
  in
  match
    Connection.request_without_history connection (P.Command.Protocol_initialize request)
  with
  | Ok (Protocol_initialize _) -> Ok ()
  | Ok _ -> Error (P.Error.invalid_request "unexpected local Initialize response")
  | Error error -> Error error
;;

let bind connection ~description f =
  let open Result.Let_syntax in
  let%bind client = Repl.create connection in
  f { client; connection; description }
;;

let with_connection ~env ~target ~notification_capacity ~f =
  if notification_capacity < 1 || notification_capacity > 65_536
  then Error (P.Error.invalid_request "invalid terminal notification capacity")
  else (
    match target with
    | Local local ->
      let open Result.Let_syntax in
      let%bind limits =
        Child.Limits.create
          ~max_line_length:(16 * 1024 * 1024)
          ~notification_capacity
          ~max_stderr_bytes:64_000
          ~shutdown_grace_seconds:2.
      in
      Child.with_spawned ~env ~argv:(Local.argv local) ~limits ~f:(fun child ->
        let connection = Child.connection child in
        let%bind () = initialize_local connection in
        bind connection ~description:"local stdio" f)
    | Remote profile ->
      Eio.Switch.run (fun sw ->
        let open Result.Let_syntax in
        let%bind connection =
          Profile.connect_with_features
            profile
            ~sw
            ~env
            ~notification_capacity
            ~features:
              [ P.Developer_common.Features.run
              ; P.Developer_repl_wire.Features.repl
              ; P.Developer_repl_wire.Features.events
              ]
        in
        let outcome =
          match bind connection ~description:(Profile.description profile) f with
          | value -> Ok value
          | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        let close_outcome =
          match Connection.close connection with
          | () -> Ok ()
          | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        match outcome, close_outcome with
        | Error (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
        | Ok (Error _ as failure), _ -> failure
        | Ok (Ok _ as success), Ok () -> success
        | Ok (Ok _), Error (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace))
;;
