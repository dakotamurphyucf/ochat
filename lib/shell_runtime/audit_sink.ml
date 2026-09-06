open! Core
open Jsonaf.Export
module A = Shell_access.Audit
module C = Shell_access.Context
module S = Chatmd_shell_spec.Shell_spec

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

let schema_version = "ochat-shell-audit-v1"

let event_context = function
  | A.Resolved context
  | Policy_decided (context, _)
  | Intercepted (_, context)
  | Plan_created (_, _, context)
  | Started (_, context, _)
  | Output (_, context, _, _)
  | Finished (_, context, _)
  | Terminated (_, context, _)
  | Rejected (context, _) -> context
  | Approval_requested request
  | Approval_answered (request, _)
  | Reviewer_completed (request, _, _) -> request.context
;;

let event_name = function
  | A.Resolved _ -> "resolved"
  | Policy_decided _ -> "policy_decided"
  | Approval_requested _ -> "approval_requested"
  | Approval_answered _ -> "approval_answered"
  | Reviewer_completed _ -> "reviewer_completed"
  | Intercepted _ -> "intercepted"
  | Plan_created _ -> "plan_created"
  | Started _ -> "started"
  | Output _ -> "output"
  | Finished _ -> "finished"
  | Terminated _ -> "terminated"
  | Rejected _ -> "rejected"
;;

let status_fields = function
  | `Exited code -> [ "exit_kind", `String "exited"; "exit_code", jsonaf_of_int code ]
  | `Signaled signal ->
    [ "exit_kind", `String "signaled"; "signal", jsonaf_of_int signal ]
;;

let termination_fields = function
  | A.Timed_out seconds ->
    [ "exit_kind", `String "timed_out"; "timeout_seconds", jsonaf_of_float seconds ]
  | Idle_timed_out seconds ->
    [ "exit_kind", `String "idle_timed_out"
    ; "timeout_seconds", jsonaf_of_float seconds
    ]
  | Output_limit_exceeded bytes ->
    [ "exit_kind", `String "output_limit_exceeded"; "limit_bytes", jsonaf_of_int bytes ]
  | Cancelled -> [ "exit_kind", `String "cancelled" ]
;;

let event_fields secret_filter event =
  let redact = Shell_access.Secret_filter.redact secret_filter in
  let redact_json = function
    | `String value -> `String (redact value)
    | value -> value
  in
  let fields =
    match event with
    | A.Policy_decided (_, decision) ->
      [ "policy_action", `String (Shell_access.Policy.string_of_action decision.action)
      ; "policy_reason", `String decision.reason
      ]
    | Approval_answered (_, answer) -> [ "approval_answer", `String answer ]
    | Reviewer_completed (_, metadata, action) ->
      [ "reviewer_id", `String metadata.reviewer_id
      ; "reviewer_kind", `String metadata.reviewer_kind
      ; "model", jsonaf_of_option jsonaf_of_string metadata.model
      ; "response_action", `String action
      ; "input_tokens", jsonaf_of_option jsonaf_of_int metadata.input_tokens
      ; "output_tokens", jsonaf_of_option jsonaf_of_int metadata.output_tokens
      ; "latency_ms", jsonaf_of_option jsonaf_of_int metadata.latency_ms
      ]
    | Intercepted (name, _) -> [ "interceptor", `String name ]
    | Plan_created (backend, _, _) | Started (backend, _, _) ->
      [ "backend", `String backend ]
    | Output (_, _, channel, bytes) ->
      [ ( "channel"
        , `String
            (match channel with
             | `Stdout -> "stdout"
             | `Stderr -> "stderr") )
      ; "bytes", jsonaf_of_int bytes
      ]
    | Finished (_, _, status) -> status_fields status
    | Terminated (_, _, termination) -> termination_fields termination
    | Rejected (_, reason) -> [ "reason", `String reason ]
    | Resolved _ | Approval_requested _ -> []
  in
  List.map fields ~f:(fun (name, value) -> name, redact_json value)
;;

let command_field content secret_filter context =
  match content with
  | S.Metadata -> []
  | Redacted ->
    [ ( "command"
      , `String
          (Shell_access.Secret_filter.redact_command secret_filter context.C.command) )
    ]
  | Full -> [ "command", `String (Shell_access.Command.to_string context.command) ]
;;

let request_kind = function
  | C.Structured -> "structured"
  | Script_file -> "script_file"
  | Raw_shell -> "raw_shell"
;;

let effect_kind = function
  | Shell_access.Effect.Read_path _ -> "read_path"
  | Write_path _ -> "write_path"
  | Network -> "network"
  | Child_processes -> "child_processes"
  | Arbitrary_code -> "arbitrary_code"
  | Privilege_change -> "privilege_change"
  | Unknown _ -> "unknown"
;;

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let context_metadata context =
  [ ("request_kind", `String (request_kind context.C.request_kind))
  ; ( "command_sha256"
    , `String
        (Shell_access.Command.to_argv context.command
         |> String.concat ~sep:"\000"
         |> sha256) )
  ; "cwd_sha256", `String (sha256 context.cwd)
  ; ( "effects"
    , `Array
        (context.effects
         |> List.map ~f:effect_kind
         |> List.dedup_and_sort ~compare:String.compare
         |> List.map ~f:(fun effect_name -> `String effect_name)) )
  ]
;;

let envelope_fields content secret_filter envelope =
  let context = event_context envelope.A.event in
  let fields =
    [ "schema_version", `String schema_version
    ; "sequence", jsonaf_of_int64 envelope.sequence
    ; "timestamp", jsonaf_of_float envelope.timestamp
    ; "session_id", jsonaf_of_option jsonaf_of_string envelope.session_id
    ; "runtime_id", `String envelope.runtime_id
    ; "manifest_sha256", `String envelope.manifest_sha256
    ; "request_id", `String envelope.request_id
    ; "plan_id", jsonaf_of_option jsonaf_of_string envelope.plan_id
    ; "event", `String (event_name envelope.event)
    ]
    @ context_metadata context
    @ command_field content secret_filter context
    @ event_fields secret_filter envelope.event
  in
  let fields =
    List.filter fields ~f:(fun (name, _) -> not (Set.mem envelope.dropped_fields name))
    |> List.map ~f:(fun (name, value) ->
      match Map.find envelope.replacement_fields name with
      | None -> name, value
      | Some replacement ->
        name, `String (Shell_access.Secret_filter.redact secret_filter replacement))
  in
  let existing = String.Set.of_list (List.map fields ~f:fst) in
  let additions =
    Map.to_alist envelope.replacement_fields
    |> List.filter ~f:(fun (name, _) ->
      not (Set.mem existing name) && not (Set.mem envelope.dropped_fields name))
    |> List.map ~f:(fun (name, value) ->
      name, `String (Shell_access.Secret_filter.redact secret_filter value))
  in
  fields @ additions
;;

type integrity_state = { mutable previous_event_sha256 : string option }

let last_event_sha256 path =
  if not (Eio.Path.is_file path)
  then Ok None
  else
    try
      let lines =
        Eio.Path.load path
        |> String.split_lines
        |> List.filter ~f:(Fn.non String.is_empty)
      in
      match List.last lines with
      | None -> Ok None
      | Some line ->
        (match Jsonaf.of_string line with
         | `Object fields ->
           (match List.Assoc.find fields "event_sha256" ~equal:String.equal with
            | Some (`String digest) -> Ok (Some digest)
            | None | Some _ ->
              Error
                { code = "shell.audit_chain_missing_digest"
                ; message =
                    "existing audit data does not end with a valid event_sha256"
                })
         | _ ->
           Error
             { code = "shell.audit_chain_invalid_tail"
             ; message = "existing audit data does not end with a JSON object"
             })
    with
    | exn ->
      Error
        { code = "shell.audit_chain_read_failed"
        ; message = "failed to recover audit integrity state: " ^ Exn.to_string exn
        }
;;

let last_sequence path =
  if not (Eio.Path.is_file path)
  then Ok None
  else
    try
      let lines =
        Eio.Path.load path
        |> String.split_lines
        |> List.filter ~f:(Fn.non String.is_empty)
      in
      match List.last lines with
      | None -> Ok None
      | Some line ->
        (match Jsonaf.of_string line with
         | `Object fields ->
           (match List.Assoc.find fields "sequence" ~equal:String.equal with
            | Some (`Number value) -> Ok (Some (Int64.of_string value))
            | None | Some _ ->
              Error
                { code = "shell.audit_sequence_missing"
                ; message = "existing audit data does not end with a valid sequence"
                })
         | _ ->
           Error
             { code = "shell.audit_sequence_invalid_tail"
             ; message = "existing audit data does not end with a JSON object"
             })
    with
    | exn ->
      Error
        { code = "shell.audit_sequence_read_failed"
        ; message = "failed to recover audit sequence: " ^ Exn.to_string exn
        }
;;

let serialize ?integrity content secret_filter envelope =
  let fields = envelope_fields content secret_filter envelope in
  match integrity with
  | None -> Jsonaf.to_string (`Object fields) ^ "\n"
  | Some state ->
    let fields =
      fields
      @ [ "previous_event_sha256", jsonaf_of_option jsonaf_of_string state.previous_event_sha256 ]
    in
    let unsigned = Jsonaf.to_string (`Object fields) in
    let digest = Digestif.SHA256.(to_hex (digest_string unsigned)) in
    state.previous_event_sha256 <- Some digest;
    Jsonaf.to_string (`Object (fields @ [ "event_sha256", `String digest ])) ^ "\n"
;;

let strongest_failure_policy sinks =
  if
    List.exists sinks ~f:(fun sink ->
      match Shell_access.Audit.failure_policy sink with
      | Shell_access.Audit.Terminate_runtime -> true
      | Ignore_failure | Deny_start -> false)
  then Shell_access.Audit.Terminate_runtime
  else if
    List.exists sinks ~f:(fun sink ->
      match Shell_access.Audit.failure_policy sink with
      | Shell_access.Audit.Deny_start -> true
      | Ignore_failure | Terminate_runtime -> false)
  then Shell_access.Audit.Deny_start
  else Shell_access.Audit.Ignore_failure
;;

let fan_out sinks =
  let failure_policy = strongest_failure_policy sinks in
  A.create ~failure_policy (fun envelope ->
    let errors =
      List.filter_map sinks ~f:(fun sink ->
        match A.write sink envelope with
        | Ok () -> None
        | Error message -> Some message)
    in
    if List.is_empty errors then Ok () else Error (String.concat ~sep:"; " errors))
;;

let acquire_management_lock ~env path =
  let clock = Eio.Stdenv.clock env in
  let rec loop remaining =
    match Or_error.try_with (fun () -> Eio.Path.save ~create:(`Exclusive 0o600) path "") with
    | Ok () -> Ok ()
    | Error error when remaining > 0 ->
      Eio.Time.sleep clock 0.01;
      loop (remaining - 1)
    | Error error ->
      Error
        { code = "shell.audit_management_locked"
        ; message = Error.to_string_hum error
        }
  in
  loop 200
;;

let append_management_event
      ~env
      ~path
      ~session_id
      ~runtime_id
      ~manifest_sha256
      ~request_id
      ~event
      ~fields
  =
  let fs = Eio.Stdenv.fs env in
  let destination = Eio.Path.(fs / path) in
  Io.mkdir ~exists_ok:true ~dir:fs (Filename.dirname path);
  let lock = Eio.Path.(fs / (path ^ ".lock")) in
  Result.bind (acquire_management_lock ~env lock) ~f:(fun () ->
    Fun.protect
      ~finally:(fun () ->
        try Eio.Path.unlink lock with
        | _ -> ())
      (fun () ->
         Result.bind (last_event_sha256 destination) ~f:(fun previous_event_sha256 ->
           Result.bind (last_sequence destination) ~f:(fun previous_sequence ->
             let sequence =
               Option.value_map previous_sequence ~default:0L ~f:Int64.succ
             in
             let base_fields =
               [ "schema_version", `String schema_version
               ; "sequence", jsonaf_of_int64 sequence
               ; "timestamp", jsonaf_of_float (Eio.Time.now (Eio.Stdenv.clock env))
               ; "session_id", jsonaf_of_option jsonaf_of_string session_id
               ; "runtime_id", `String runtime_id
               ; "manifest_sha256", `String manifest_sha256
               ; "request_id", `String request_id
               ; "plan_id", `Null
               ; "event", `String event
               ]
               @ fields
               @ [ ( "previous_event_sha256"
                   , jsonaf_of_option jsonaf_of_string previous_event_sha256 ) ]
             in
             let unsigned = Jsonaf.to_string (`Object base_fields) in
             let event_sha256 = Digestif.SHA256.(to_hex (digest_string unsigned)) in
             let line =
               Jsonaf.to_string
                 (`Object (base_fields @ [ "event_sha256", `String event_sha256 ]))
               ^ "\n"
             in
             try
               Eio.Path.with_open_out
                 ~append:true
                 ~create:(`If_missing 0o600)
                 destination
                 (fun flow ->
                    Eio.Flow.copy_string line flow;
                    Eio.File.sync flow);
               Ok sequence
             with
             | exn ->
               Error
                 { code = "shell.audit_management_write_failed"
                 ; message = Exn.to_string exn
                 }))))
;;

let writer
      ~flow
      ~sync
      ~content
      ~failure_policy
      ~secret_filter
      ~integrity_chaining
      ~initial_previous
  =
  let mutex = Eio.Mutex.create () in
  let integrity =
    Option.some_if integrity_chaining { previous_event_sha256 = initial_previous }
  in
  let write envelope =
    try
      Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        Eio.Flow.copy_string (serialize ?integrity content secret_filter envelope) flow;
        sync ());
      Ok ()
    with
    | exn -> Error (Exn.to_string exn)
  in
  A.create ~failure_policy write
;;

let create_flow ~flow ~content ~failure_policy ~secret_filter =
  writer
    ~flow
    ~sync:ignore
    ~content
    ~failure_policy
    ~secret_filter
    ~integrity_chaining:false
    ~initial_previous:None
;;

let create_jsonl_internal
      ~sw
      ~path
      ~content
      ~failure_policy
      ~secret_filter
      ~integrity_chaining
  =
  let initial_previous =
    if integrity_chaining then last_event_sha256 path else Ok None
  in
  Result.bind initial_previous ~f:(fun initial_previous ->
    try
      let flow = Eio.Path.open_out ~sw ~append:true ~create:(`If_missing 0o600) path in
      Ok
        (writer
           ~flow
           ~sync:(fun () -> Eio.File.sync flow)
           ~content
           ~failure_policy
           ~secret_filter
           ~integrity_chaining
           ~initial_previous)
    with
    | exn ->
      Error
        { code = "shell.audit_open_failed"
        ; message = "failed to open shell audit sink: " ^ Exn.to_string exn
        })
;;

let create_jsonl ~sw ~path ~content ~failure_policy ~secret_filter =
  create_jsonl_internal
    ~sw
    ~path
    ~content
    ~failure_policy
    ~secret_filter
    ~integrity_chaining:false
;;

let validate_next_sequence previous current =
  let expected = Option.value_map previous ~default:0L ~f:Int64.succ in
  if Int64.equal expected current
  then Ok ()
  else
    Error
      (sprintf
         "audit sequence conflict: expected %Ld but received %Ld"
         expected
         current)
;;

let create_chained_jsonl ~env ~sw ~path ~content ~failure_policy ~secret_filter =
  try
    let flow = Eio.Path.open_out ~sw ~append:true ~create:(`If_missing 0o600) path in
    let mutex = Eio.Mutex.create () in
    let native_path = Eio.Path.native_exn path in
    let lock = Eio.Path.(Eio.Stdenv.fs env / (native_path ^ ".lock")) in
    let write envelope =
      Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        Result.bind (acquire_management_lock ~env lock) ~f:(fun () ->
          Fun.protect
            ~finally:(fun () ->
              try Eio.Path.unlink lock with
              | _ -> ())
            (fun () ->
               Result.bind (last_event_sha256 path) ~f:(fun previous ->
                 Result.bind (last_sequence path) ~f:(fun previous_sequence ->
                   Result.bind
                     (validate_next_sequence previous_sequence envelope.A.sequence
                      |> Result.map_error ~f:(fun message ->
                        { code = "shell.audit_sequence_conflict"; message }))
                     ~f:(fun () ->
                       try
                         let integrity = { previous_event_sha256 = previous } in
                         Eio.Flow.copy_string
                           (serialize ~integrity content secret_filter envelope)
                           flow;
                         Eio.File.sync flow;
                         Ok ()
                       with
                       | exn ->
                         Error
                           { code = "shell.audit_write_failed"
                           ; message = Exn.to_string exn
                           })))))
        |> Result.map_error ~f:(fun error -> error.message))
    in
    Ok (A.create ~failure_policy write)
  with
  | exn ->
    Error
      { code = "shell.audit_open_failed"
      ; message = "failed to open shell audit sink: " ^ Exn.to_string exn
      }
;;

let unlink_if_file path =
  if Eio.Path.is_file path then Eio.Path.unlink path
;;

let rotate ~fs ~path ~max_files =
  for index = max_files - 1 downto 1 do
    let source = Eio.Path.(fs / sprintf "%s.%d" path index) in
    let destination = Eio.Path.(fs / sprintf "%s.%d" path (index + 1)) in
    if Eio.Path.is_file source
    then (
      unlink_if_file destination;
      Eio.Path.rename source destination)
  done;
  let current = Eio.Path.(fs / path) in
  if Eio.Path.is_file current
  then (
    let first = Eio.Path.(fs / (path ^ ".1")) in
    unlink_if_file first;
    Eio.Path.rename current first)
;;

let needs_rotation path max_bytes incoming_bytes =
  if not (Eio.Path.is_file path)
  then false
  else
    let size = Eio.Path.stat ~follow:true path |> fun stat -> stat.Eio.File.Stat.size in
    Int64.(Optint.Int63.to_int64 size + of_int incoming_bytes > max_bytes)
;;

let create_rotating_jsonl
      ~env
      ~path
      ~max_bytes
      ~max_files
      ~content
      ~failure_policy
      ~secret_filter
      ~integrity_chaining
  =
  if Int64.(max_bytes <= 0L) || max_files < 1
  then
    Error
      { code = "shell.audit_rotation_invalid"
      ; message = "max_bytes and max_files must be positive"
      }
  else (
    let fs = Eio.Stdenv.fs env in
    let destination = Eio.Path.(fs / path) in
    let mutex = Eio.Mutex.create () in
    let initial_previous =
      if integrity_chaining then last_event_sha256 destination else Ok None
    in
    Result.map initial_previous ~f:(fun initial_previous ->
      let integrity =
        Option.some_if integrity_chaining { previous_event_sha256 = initial_previous }
      in
      let value_or_fail = function
        | Ok value -> value
        | Error error -> failwith error.message
      in
      let write envelope =
        try
          Eio.Mutex.use_rw ~protect:true mutex (fun () ->
            let lock_path = Eio.Path.(fs / (path ^ ".lock")) in
            match acquire_management_lock ~env lock_path with
            | Error error -> failwith error.message
            | Ok () ->
              Fun.protect
                ~finally:(fun () ->
                  try Eio.Path.unlink lock_path with
                  | _ -> ())
                (fun () ->
                   let previous_sequence = last_sequence destination |> value_or_fail in
                   validate_next_sequence previous_sequence envelope.A.sequence
                   |> Result.ok_or_failwith;
                   Option.iter integrity ~f:(fun state ->
                     state.previous_event_sha256
                     <- last_event_sha256 destination |> value_or_fail);
                   let line = serialize ?integrity content secret_filter envelope in
                   if needs_rotation destination max_bytes (String.length line)
                   then rotate ~fs ~path ~max_files;
                   Eio.Path.with_open_out
                     ~append:true
                     ~create:(`If_missing 0o600)
                     destination
                     (fun flow ->
                        Eio.Flow.copy_string line flow;
                        Eio.File.sync flow)));
          Ok ()
        with
        | exn -> Error (Exn.to_string exn)
      in
      A.create ~failure_policy write))
;;

let create_session
      ~env
      ~session_dir
      ~content
      ~failure_policy
      ~secret_filter
  =
  let root = Eio.Path.native_exn session_dir in
  let path = Filename.concat root ".chatmd/shell-audit.jsonl" in
  Io.mkdir ~exists_ok:true ~dir:(Eio.Stdenv.fs env) (Filename.dirname path);
  create_rotating_jsonl
    ~env
    ~path
    ~max_bytes:10_485_760L
    ~max_files:5
    ~content
    ~failure_policy
    ~secret_filter
    ~integrity_chaining:true
;;

let session_sequence_counter ~session_dir =
  let path = Eio.Path.(session_dir / ".chatmd/shell-audit.jsonl") in
  Result.bind (last_sequence path) ~f:(fun sequence ->
    let next = Option.value_map sequence ~default:0L ~f:Int64.succ in
    match Int64.to_int next with
    | Some next -> Ok (Atomic.make next)
    | None ->
      Error
        { code = "shell.audit_sequence_overflow"
        ; message = "persisted audit sequence cannot be represented by this runtime"
        })
;;

let create_organization_collector ~failure_policy ~send =
  A.create ~failure_policy (fun envelope -> send (serialize S.Metadata Shell_access.Secret_filter.empty envelope))
;;
