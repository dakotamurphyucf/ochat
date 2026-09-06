open Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_client_test" |> protocol_ok
;;

let principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_client_test" |> protocol_ok
;;

let operation_id =
  Agent_protocol.Id.Operation.of_string "op_agent_client_test" |> protocol_ok
;;

let timestamp = Agent_protocol.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok

let operation state =
  Agent_protocol.Operation.
    { id = operation_id
    ; generation = 0
    ; kind = Turn User_submit
    ; state
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let history_id =
  History_entry.Id.create ~namespace:"client" ~sequence:0
  |> function
  | Ok value -> value
  | Error error -> failwith error
;;

let protocol_spec =
  Agent_protocol.Session.Spec.create
    ~execution_host:Embedded
    ~prompt:(Local_path "/prompt.chatmd")
    ~workspace:Current
    ~liveness:Process_bound
    ~persistence:Transient
    ~start_immediately:false
    ~labels:[]
    ()
  |> protocol_ok
;;

let session =
  Agent_protocol.Session.
    { id = session_id
    ; creator = Some principal_id
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec = protocol_spec
    ; desired_state = Stopped
    ; observed_state = Stopped
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 0L
    ; latest_event_sequence = 0L
    }
;;

let window entries =
  Agent_protocol.History.Window.
    { entries
    ; previous_cursor = None
    ; next_cursor = None
    ; reached_start = true
    ; reached_end = true
    ; structurally_complete = true
    }
;;

let snapshot =
  Agent_protocol.Snapshot.
    { session
    ; canonical_history = window []
    ; archived_revisions = []
    ; effective_history = None
    ; deferred_entries = []
    ; permissions = []
    ; grants = []
    ; jobs = []
    ; schedules = []
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = false
    ; halt_reason = None
    ; failure = None
    ; revision = 0L
    ; latest_event_sequence = 0L
    }
;;

let entry =
  Agent_protocol.History.
    { id = history_id
    ; role = User
    ; kind = Message
    ; payload = `Object [ "text", `String "hello" ]
    ; provenance = Canonical
    ; redacted = false
    }
;;

let%expect_test
    "administrative replacement clears cached history and rejects a wrong cursor"
  =
  let previous =
    { snapshot with canonical_history = window [ entry ]; deferred_entries = [ entry ] }
  in
  let projection = Agent_client.Projection.install_snapshot previous in
  let event =
    Agent_protocol.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (Session_updated session)
  in
  let event = Agent_protocol.Event.Durable.with_replacement_snapshot event snapshot in
  let restored =
    Agent_protocol.Event.Durable.to_json event
    |> Agent_protocol.Event.Durable.of_json
    |> protocol_ok
  in
  let projection =
    Agent_client.Projection.apply_event projection restored |> protocol_ok
  in
  let current = Agent_client.Projection.snapshot projection in
  assert (
    List.is_empty current.canonical_history.entries
    && List.is_empty current.deferred_entries);
  assert (
    Int64.equal current.latest_event_sequence 1L
    && Int64.equal current.session.revision 1L);
  let invalid = { restored with sequence = 2L } in
  assert (Result.is_error (Agent_client.Projection.apply_event projection invalid));
  print_endline
    "replacement survives codec; history/deferred cleared; mismatched nested cursor \
     rejected";
  [%expect
    {| replacement survives codec; history/deferred cleared; mismatched nested cursor rejected |}]
;;

let%expect_test "projection applies contiguous durable events and rejects gaps" =
  let projection = Agent_client.Projection.install_snapshot snapshot in
  let event =
    Agent_protocol.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (History_appended [ entry ])
  in
  let projection = Agent_client.Projection.apply_event projection event |> protocol_ok in
  let gap = { event with sequence = 3L; revision = 2L } in
  let gap_code =
    match Agent_client.Projection.apply_event projection gap with
    | Ok _ -> "accepted"
    | Error error -> Agent_protocol.Error.code_to_string error.code
  in
  let current = Agent_client.Projection.snapshot projection in
  print_s
    [%sexp
      { history = (List.length current.canonical_history.entries : int)
      ; sequence = (current.latest_event_sequence : int64)
      ; gap_code : string
      }];
  [%expect {| ((history 1) (sequence 1) (gap_code snapshot_required)) |}]
;;

let%expect_test "projection validates monotonic live operation sequences" =
  let projection = Agent_client.Projection.install_snapshot snapshot in
  let event sequence =
    Agent_protocol.Event.Recoverable.
      { session_id
      ; operation_id
      ; operation_sequence = sequence
      ; anchor_sequence = 0L
      ; timestamp
      ; kind = Activity
      ; payload = `Object []
      }
  in
  let projection =
    Agent_client.Projection.apply_live_event projection (event 1L) |> protocol_ok
  in
  let duplicate_rejected =
    Agent_client.Projection.apply_live_event projection (event 1L) |> Result.is_error
  in
  print_s
    [%sexp
      { live_count = (List.length (Agent_client.Projection.live_events projection) : int)
      ; duplicate_rejected : bool
      }];
  [%expect {| ((live_count 1) (duplicate_rejected true)) |}]
;;

let%expect_test "projection clears terminal foreground operations" =
  let projection = Agent_client.Projection.install_snapshot snapshot in
  let durable sequence revision payload =
    Agent_protocol.Event.Durable.of_payload
      ~session_id
      ~sequence
      ~revision
      ~timestamp
      payload
  in
  let projection =
    durable 1L 1L (Operation_started (operation Running))
    |> Agent_client.Projection.apply_event projection
    |> protocol_ok
  in
  let active_after_start =
    Agent_client.Projection.snapshot projection
    |> fun (snapshot : Agent_protocol.Snapshot.t) ->
    Option.is_some snapshot.session.active_operation
  in
  let projection =
    durable 2L 2L (Operation_completed (operation Completed))
    |> Agent_client.Projection.apply_event projection
    |> protocol_ok
  in
  let active_after_completion =
    Agent_client.Projection.snapshot projection
    |> fun (snapshot : Agent_protocol.Snapshot.t) ->
    Option.is_some snapshot.session.active_operation
  in
  print_s [%sexp { active_after_start : bool; active_after_completion : bool }];
  [%expect {| ((active_after_start true) (active_after_completion false)) |}]
;;

let%expect_test "terminal operation events discard recoverable deltas" =
  let durable sequence revision payload =
    Agent_protocol.Event.Durable.of_payload
      ~session_id
      ~sequence
      ~revision
      ~timestamp
      payload
  in
  let projection =
    Agent_client.Projection.install_snapshot snapshot
    |> fun projection ->
    Agent_client.Projection.apply_event
      projection
      (durable 1L 1L (Operation_started (operation Running)))
    |> protocol_ok
  in
  let live =
    Agent_protocol.Event.Recoverable.
      { session_id
      ; operation_id
      ; operation_sequence = 1L
      ; anchor_sequence = 1L
      ; timestamp
      ; kind = Activity
      ; payload = `Object []
      }
  in
  let projection =
    Agent_client.Projection.apply_live_event projection live |> protocol_ok
  in
  let projection =
    Agent_client.Projection.apply_event
      projection
      (durable 2L 2L (Operation_completed (operation Completed)))
    |> protocol_ok
  in
  print_s [%sexp (List.length (Agent_client.Projection.live_events projection) : int)];
  [%expect {| 0 |}]
;;

let blob_connection content blob =
  let request = function
    | Agent_protocol.Command.Blob_read request ->
      let offset = Int64.to_int_exn request.offset in
      let count = Int.min 3 (String.length content - offset) in
      let data = String.sub content ~pos:offset ~len:count in
      let next_offset = Int64.of_int (offset + count) in
      Ok
        (Agent_protocol.Method_result.Blob_read
           { blob
           ; offset = request.offset
           ; next_offset
           ; data_base64 = Base64.encode_exn data
           ; eof = Int64.equal next_offset blob.Agent_protocol.Blob.Metadata.byte_length
           })
    | _ -> Error (Agent_protocol.Error.invalid_request "unexpected command")
  in
  Agent_client.Transport.create ~request ~next_notification:(fun () -> None) ~close:Fn.id
  |> Agent_client.Connection.create
;;

let%expect_test "blob download streams short chunks and verifies the final digest" =
  let content = "streamed-export" in
  let generator =
    Agent_protocol.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
  in
  let blob digest =
    Agent_protocol.Blob.Metadata.create
      ~id:(Agent_protocol.Id.Blob.create_with generator)
      ~kind:File
      ~media_type:"text/plain"
      ~byte_length:(Int64.of_int (String.length content))
      ~digest
      ()
    |> protocol_ok
  in
  let digest = Digestif.SHA256.digest_string content |> Digestif.SHA256.to_hex in
  let downloaded, contents_match, digest_rejected =
    Eio_main.run (fun _env ->
      let valid_blob = blob digest in
      let output = Buffer.create 32 in
      let downloaded =
        Agent_client.Blob_download.download
          ~connection:(blob_connection content valid_blob)
          ~session_id
          ~attachment_id:(Agent_protocol.Id.Attachment.create_with generator)
          ~blob:valid_blob
          ~output:(Eio.Flow.buffer_sink output)
        |> Result.is_ok
      in
      let invalid_blob = blob (String.make 64 '0') in
      let digest_rejected =
        Agent_client.Blob_download.download
          ~connection:(blob_connection content invalid_blob)
          ~session_id
          ~attachment_id:(Agent_protocol.Id.Attachment.create_with generator)
          ~blob:invalid_blob
          ~output:(Eio.Flow.buffer_sink (Buffer.create 32))
        |> Result.is_error
      in
      downloaded, String.equal content (Buffer.contents output), digest_rejected)
  in
  print_s [%sexp { downloaded : bool; contents_match : bool; digest_rejected : bool }];
  [%expect {| ((downloaded true) (contents_match true) (digest_rejected true)) |}]
;;
