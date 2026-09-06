open! Core

let request_kind = function
  | Some "script_file" -> Session.Shell_state.Request_kind.Script_file
  | Some "raw_shell" -> Raw_shell
  | None | Some _ -> Structured
;;

let ns timestamp =
  Time_ns.of_span_since_epoch (Time_ns.Span.of_sec timestamp)
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
;;

let interrupted (request : Audit_replay.request) =
  let last = List.last_exn request.events in
  Session.Shell_state.Interrupted_request.
    { request_id = request.request_id
    ; runtime_id = request.runtime_id
    ; manifest_sha256 = request.manifest_sha256
    ; request_kind = request_kind request.request_kind
    ; command_sha256 =
        Option.value request.command_sha256 ~default:(Digestif.SHA256.(to_hex (digest_string request.request_id)))
    ; redacted_command = Option.value request.command ~default:"<metadata only>"
    ; cwd_sha256 = Option.value request.cwd_sha256 ~default:"unknown"
    ; effects = request.effects
    ; interrupted_at_ns = ns last.timestamp
    ; reason = "runtime stopped before the request reached a terminal audit event"
    ; audit_sequence = Some last.sequence
    ; retryable = false
    }
;;

let refresh ~env ~(session : Session.t) =
  let path =
    Filename.concat (Session_store.rel_path session.id) ".chatmd/shell-audit.jsonl"
  in
  let file = Eio.Path.(Eio.Stdenv.fs env / path) in
  if not (Eio.Path.is_file file)
  then Ok session
  else
    match Audit_replay.load_rotated ~fs:(Eio.Stdenv.fs env) ~path with
    | Error errors ->
      Error
        (errors
         |> List.map ~f:(fun (error : Audit_replay.error) -> error.message)
         |> String.concat ~sep:"; ")
    | Ok events ->
      (match Audit_replay.validate events with
       | Error errors ->
         Error
           (errors
            |> List.map ~f:(fun (error : Audit_replay.error) -> error.message)
            |> String.concat ~sep:"; ")
       | Ok () ->
         let interrupted_requests =
           Audit_replay.requests events
           |> List.filter ~f:(fun request -> not request.completed)
           |> List.map ~f:interrupted
         in
         let last_audit_sequence =
           List.last events |> Option.map ~f:(fun event -> event.Audit_replay.sequence)
         in
         Ok
           { session with
             shell_state =
               { session.shell_state with
                 interrupted_requests
               ; last_audit_sequence
               }
           })
;;
