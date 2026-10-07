open Core
module P = Agent_protocol
module Public = P.Public
module Payload = History_entry.Payload

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let neutral_ok = Result.ok_or_failwith
let session_id = P.Id.Session.of_string "ses_agent_client_test" |> protocol_ok
let principal_id = P.Id.Principal.of_string "pri_agent_client_test" |> protocol_ok
let operation_id = P.Id.Operation.of_string "op_agent_client_test" |> protocol_ok
let timestamp = P.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok

let operation state =
  P.Operation.
    { id = operation_id
    ; generation = 0
    ; kind = Turn User_submit
    ; state
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let history_id = History_entry.Id.create ~namespace:"client" ~sequence:0 |> neutral_ok

let protocol_spec =
  P.Session.Spec.create
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
  P.Session.
    { id = session_id
    ; creator = Some principal_id
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec = protocol_spec
    ; desired_state = Running
    ; observed_state = Idle
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 0L
    ; latest_event_sequence = 0L
    }
;;

let window entries =
  Public.History.Window.
    { entries
    ; previous_cursor = None
    ; next_cursor = None
    ; reached_start = true
    ; reached_end = true
    ; structurally_complete = true
    }
;;

let snapshot_fields =
  Public.Snapshot.Fields.
    { session
    ; canonical_history = window []
    ; archived_revisions = []
    ; effective_history = None
    ; deferred_entries = []
    ; permissions = []
    ; grants = []
    ; jobs = []
    ; extension_status = []
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

let snapshot = Public.Snapshot.create snapshot_fields |> protocol_ok

let semantic =
  Payload.Semantic.create
    (Message
       { form = Input
       ; role = Developer
       ; content = [ Text { text = "hello"; annotations = []; logprobs = Absent } ]
       ; phase = Absent
       })
    ~metadata:Payload.Metadata.empty
  |> neutral_ok
;;

let native_entry = History_entry.create_with_id ~id:history_id (Payload.authored semantic)

let entry =
  Public.History.full native_entry ~provenance:P.History.Canonical |> protocol_ok
;;

let shared sequence payload =
  let internal =
    P.Event.Durable.of_payload ~session_id ~sequence ~revision:sequence ~timestamp payload
  in
  Public.Durable.of_internal_envelope
    internal
    ~body:
      (Full (Shared (Public.Durable.Shared_payload.of_internal payload |> protocol_ok)))
    ~extension_status:None
    ~replacement_snapshot:None
  |> protocol_ok
;;

let history_event sequence entries =
  let internal =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence
      ~revision:sequence
      ~timestamp
      (History_appended [])
  in
  Public.Durable.of_internal_envelope
    internal
    ~body:(Full (History_appended entries))
    ~extension_status:None
    ~replacement_snapshot:None
  |> protocol_ok
;;

let active_projection () =
  Agent_client.Projection.install_snapshot snapshot
  |> fun t ->
  Agent_client.Projection.apply_event
    t
    (shared 1L (Operation_started (operation Running)))
  |> protocol_ok
;;

let fields t = Agent_client.Projection.snapshot t |> Public.Snapshot.fields

let scope name relation =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string name |> neutral_ok)
    ~attempt:(Transcript.Attempt_id.of_string "attempt" |> neutral_ok)
    ~relation
  |> neutral_ok
;;

let root_scope = scope "source" Root

let item ?(scope = root_scope) ?(entry_id = None) () =
  Transcript.Item.create
    ~scope
    ~id:(Transcript.Item_id.of_string "item" |> neutral_ok)
    ~entry_id
    ~header:(Some (Message Developer))
    ~call_name:None
  |> neutral_ok
;;

let part item =
  Transcript.Part.create
    ~item
    ~id:(Transcript.Part_id.of_string "part" |> neutral_ok)
    ~index:None
    ~kind:Text
  |> neutral_ok
;;

let stream view =
  Transcript.Stream.create view ~limits:Document_schema.Limits.default |> neutral_ok
;;

let live ?(anchor = 1L) sequence view =
  P.Event.Recoverable.create
    ~session_id
    ~operation_id
    ~operation_sequence:sequence
    ~anchor_sequence:anchor
    ~timestamp
    ~invocation_id:None
    ~parent_invocation_id:None
    (Transcript (stream view))
  |> protocol_ok
;;

let apply_live t event = Agent_client.Projection.apply_live_event t event |> protocol_ok

let operation_view t =
  Agent_client.Projection.live t |> Agent_client.Live_projection.operations |> List.hd_exn
;;

let draft_items t = Transcript.Draft.items (operation_view t).drafts

let%expect_test
    "public history retains Developer and immutable captured raw without provider decode"
  =
  let payload =
    Payload.captured
      semantic
      ~origin:Payload.Origin.unavailable
      ~raw:(`Object [ "future", `Number "1.00"; "kind", `String "provider-future" ])
    |> neutral_ok
  in
  let native = History_entry.create_with_id ~id:history_id payload in
  let public = Public.History.full native ~provenance:Canonical |> protocol_ok in
  let restored = Public.History.of_json (Public.History.to_json public) |> protocol_ok in
  assert (Public.History.equal public restored);
  assert (
    Option.equal
      Transcript.Header.equal
      (Public.History.header restored)
      (Some (Message Developer)));
  let redacted =
    Public.History.redacted
      history_id
      ~provenance:Canonical
      (Public.History.Redaction.create ~disclosed_header:None)
    |> protocol_ok
  in
  assert (Option.is_none (Public.History.full_payload redacted));
  assert (Option.is_none (Public.History.header redacted));
  print_endline
    "Developer retained; raw literal retained; redaction has no invented payload/header";
  [%expect
    {| Developer retained; raw literal retained; redaction has no invented payload/header |}]
;;

let%expect_test
    "administrative replacement clears history and rejects a wrong durable cursor"
  =
  let deferred =
    let id = History_entry.Id.create ~namespace:"client" ~sequence:1 |> neutral_ok in
    Public.History.full
      (History_entry.create_with_id ~id (Payload.authored semantic))
      ~provenance:Canonical
    |> protocol_ok
  in
  let previous =
    Public.Snapshot.create
      { snapshot_fields with
        canonical_history = window [ entry ]
      ; deferred_entries = [ deferred ]
      }
    |> protocol_ok
  in
  let internal =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (Session_updated session)
  in
  let replacement =
    Public.Snapshot.create
      { snapshot_fields with
        session = { session with revision = 1L; latest_event_sequence = 1L }
      ; revision = 1L
      ; latest_event_sequence = 1L
      }
    |> protocol_ok
  in
  let event =
    Public.Durable.of_internal_envelope
      internal
      ~body:
        (Full
           (Shared
              (Public.Durable.Shared_payload.of_internal (Session_updated session)
               |> protocol_ok)))
      ~extension_status:None
      ~replacement_snapshot:(Some replacement)
    |> protocol_ok
  in
  let event = Public.Durable.of_json (Public.Durable.to_json event) |> protocol_ok in
  let t =
    Agent_client.Projection.apply_event
      (Agent_client.Projection.install_snapshot previous)
      event
    |> protocol_ok
  in
  assert (
    List.is_empty (fields t).canonical_history.entries
    && List.is_empty (fields t).deferred_entries);
  assert (Result.is_error (Agent_client.Projection.apply_event t event));
  print_endline
    "replacement survives codec; history/deferred cleared; duplicate durable cursor \
     rejected";
  [%expect
    {| replacement survives codec; history/deferred cleared; duplicate durable cursor rejected |}]
;;

let%expect_test "projection applies contiguous durable events and rejects gaps" =
  let t =
    Agent_client.Projection.apply_event
      (Agent_client.Projection.install_snapshot snapshot)
      (history_event 1L [ entry ])
    |> protocol_ok
  in
  let gap_code =
    match Agent_client.Projection.apply_event t (history_event 3L []) with
    | Ok _ -> "accepted"
    | Error error -> P.Error.code_to_string error.code
  in
  print_s
    [%sexp
      { history = (List.length (fields t).canonical_history.entries : int)
      ; sequence = ((fields t).latest_event_sequence : int64)
      ; gap_code : string
      }];
  [%expect {| ((history 1) (sequence 1) (gap_code snapshot_required)) |}]
;;

let%expect_test "live duplicate conflicts, truthful gaps and replacement completeness" =
  let announced = live 1L (Item_announced (item ())) in
  let t = apply_live (active_projection ()) announced in
  let duplicate = apply_live t announced in
  assert (
    Int.equal
      (Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live t))
      (Agent_client.Live_projection.retained_bytes
         (Agent_client.Projection.live duplicate)));
  assert (
    Result.is_error
      (Agent_client.Projection.apply_live_event
         t
         (live 1L (Part_announced (part (item ()))))));
  let t =
    apply_live
      t
      (live 3L (Changed { target = Content (part (item ())); change = Append "tail" }))
  in
  assert (
    Agent_client.Live_projection.equal_continuity
      (operation_view t).continuity
      (Incomplete { first_missing_sequence = 2L }));
  let missing =
    match (List.hd_exn (draft_items t)).state with
    | Partial p ->
      (match p.completeness with
       | Missing_prefix -> true
       | Prefix_observed -> false)
    | Finalized _ -> false
  in
  assert missing;
  let t =
    apply_live
      t
      (live 4L (Changed { target = Content (part (item ())); change = Replace "whole" }))
  in
  assert (
    Agent_client.Live_projection.equal_continuity
      (operation_view t).continuity
      (Incomplete { first_missing_sequence = 2L }));
  print_endline
    "exact duplicate idempotent; conflict rejected; gap remains truthful after text \
     replacement";
  [%expect
    {| exact duplicate idempotent; conflict rejected; gap remains truthful after text replacement |}]
;;

let%expect_test
    "future anchors wait for durable history and terminals fence late live events"
  =
  let t =
    apply_live (active_projection ()) (live ~anchor:2L 1L (Item_announced (item ())))
  in
  assert (List.is_empty (draft_items t));
  let t =
    Agent_client.Projection.apply_event
      t
      (shared
         2L
         (Session_updated { session with active_operation = Some (operation Running) }))
    |> protocol_ok
  in
  assert (Int.equal (List.length (draft_items t)) 1);
  let t =
    Agent_client.Projection.apply_event
      t
      (shared 3L (Operation_completed (operation Completed)))
    |> protocol_ok
  in
  assert (
    List.is_empty
      (Agent_client.Live_projection.operations (Agent_client.Projection.live t)));
  let t = apply_live t (live ~anchor:3L 2L (Part_announced (part (item ())))) in
  assert (
    List.is_empty
      (Agent_client.Live_projection.operations (Agent_client.Projection.live t)));
  assert (Option.is_none (fields t).session.active_operation);
  assert (Option.is_some (Agent_client.Projection.terminal_operation t));
  print_endline
    "future draft held then drained; terminal clears and prevents resurrection";
  [%expect
    {| future draft held then drained; terminal clears and prevents resurrection |}]
;;

let%expect_test "finalized live root reconciles exactly and nested items remain isolated" =
  let root_item = item ~entry_id:(Some history_id) () in
  let nested =
    scope
      "nested"
      (Nested
         { scope = Transcript.Scope.key root_scope
         ; call_entry_id = None
         ; call_alias = Some "alias"
         })
  in
  let nested_item = item ~scope:nested ~entry_id:(Some history_id) () in
  let t =
    apply_live
      (active_projection ())
      (live 1L (Item_finalized { item = root_item; entry = native_entry }))
  in
  let t =
    apply_live t (live 2L (Item_finalized { item = nested_item; entry = native_entry }))
  in
  assert (List.is_empty (fields t).canonical_history.entries);
  let t =
    Agent_client.Projection.apply_event t (history_event 2L [ entry ]) |> protocol_ok
  in
  let remaining = List.hd_exn (draft_items t) in
  assert (Int.equal (List.length (draft_items t)) 1);
  assert (Transcript.Scope.Key.equal remaining.descriptor.scope.key nested.key);
  assert (Public.History.equal (List.hd_exn (fields t).canonical_history.entries) entry);
  print_endline
    "live cannot append canonical; exact durable root wins; nested stays separate";
  [%expect
    {| live cannot append canonical; exact durable root wins; nested stays separate |}]
;;

let%expect_test
    "receipt eviction preserves high water and bounded future admission is atomic"
  =
  let draft_limits =
    Transcript.Draft.Limits.create
      ~max_scopes:8
      ~max_items:8
      ~max_parts:8
      ~max_unknown_events:8
      ~max_retained_bytes:4096
      ~document_limits:Document_schema.Limits.default
    |> neutral_ok
  in
  let limits =
    Agent_client.Live_projection.Limits.create
      ~max_operations:2
      ~max_receipts:1
      ~max_future_events:1
      ~max_event_bytes:4096
      ~max_total_bytes:16384
      ~draft_limits
    |> protocol_ok
  in
  let t = Agent_client.Projection.install_snapshot ~live_limits:limits snapshot in
  let t =
    Agent_client.Projection.apply_event
      t
      (shared 1L (Operation_started (operation Running)))
    |> protocol_ok
  in
  let t = apply_live t (live 1L (Item_announced (item ()))) in
  let t = apply_live t (live 2L (Part_announced (part (item ())))) in
  let obsolete =
    apply_live t (live 1L (Source_finished { scope = root_scope; completion = Failed }))
  in
  assert (Int.equal (List.length (draft_items obsolete)) 1);
  let queued =
    apply_live
      t
      (live
         ~anchor:3L
         3L
         (Changed { target = Content (part (item ())); change = Append "future" }))
  in
  assert (
    Result.is_error
      (Agent_client.Projection.apply_live_event
         queued
         (live
            ~anchor:3L
            4L
            (Source_finished { scope = root_scope; completion = Complete }))));
  assert (
    Result.is_error
      (Agent_client.Projection.apply_live_event
         queued
         (live
            ~anchor:3L
            3L
            (Source_finished { scope = root_scope; completion = Failed }))));
  assert (Int.equal (List.length (draft_items queued)) 1);
  print_endline
    "evicted obsolete ignored; future queue bound and retained conflict reject atomically";
  [%expect
    {| evicted obsolete ignored; future queue bound and retained conflict reject atomically |}]
;;

let blob_connection content blob =
  let request = function
    | Agent_protocol.Command.Blob_read request ->
      let offset = Int64.to_int_exn request.offset in
      let count = Int.min 3 (String.length content - offset) in
      let data = String.sub content ~pos:offset ~len:count in
      let next_offset = Int64.of_int (offset + count) in
      Agent_protocol.Public.Result.Non_history.of_internal
        (Agent_protocol.Method_result.Blob_read
           { blob
           ; offset = request.offset
           ; next_offset
           ; data_base64 = Base64.encode_exn data
           ; eof = Int64.equal next_offset blob.Agent_protocol.Blob.Metadata.byte_length
           })
      |> Result.map ~f:(fun result -> Agent_protocol.Public.Result.Non_history result)
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

let%expect_test
    "decoded unknown receipt fields participate in exact equality and byte charge"
  =
  let original = live 1L (Item_announced (item ())) in
  let decoded value =
    match P.Event.Recoverable.to_json original with
    | `Object fields ->
      P.Event.Recoverable.of_json (`Object (fields @ [ "future_receipt", `Number value ]))
      |> protocol_ok
    | _ -> assert false
  in
  let event = decoded "1.00" in
  let t = apply_live (active_projection ()) event in
  let same = apply_live t (decoded "1.00") in
  assert (
    Int.equal
      (Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live t))
      (Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live same)));
  assert (Result.is_error (Agent_client.Projection.apply_live_event t (decoded "1.0")));
  assert (
    Jsonaf.exactly_equal
      (P.Event.Recoverable.to_json event)
      (match P.Event.Recoverable.to_json original with
       | `Object fields -> `Object (fields @ [ "future_receipt", `Number "1.00" ])
       | _ -> assert false));
  print_endline "unknown envelope evidence retained; numeric spelling conflict rejected";
  [%expect {| unknown envelope evidence retained; numeric spelling conflict rejected |}]
;;

let%expect_test "combined receipts and drafts enforce aggregate bytes before append" =
  let draft_limits =
    Transcript.Draft.Limits.create
      ~max_scopes:8
      ~max_items:8
      ~max_parts:8
      ~max_unknown_events:8
      ~max_retained_bytes:16384
      ~document_limits:Document_schema.Limits.default
    |> neutral_ok
  in
  let limits =
    Agent_client.Live_projection.Limits.create
      ~max_operations:2
      ~max_receipts:2
      ~max_future_events:2
      ~max_event_bytes:8192
      ~max_total_bytes:2500
      ~draft_limits
    |> protocol_ok
  in
  let t =
    Agent_client.Projection.install_snapshot ~live_limits:limits snapshot
    |> fun t ->
    Agent_client.Projection.apply_event
      t
      (shared 1L (Operation_started (operation Running)))
    |> protocol_ok
  in
  let event =
    live
      1L
      (Changed
         { target = Content (part (item ())); change = Append (String.make 1800 'x') })
  in
  assert (String.length (Jsonaf.to_string (P.Event.Recoverable.to_json event)) < 2500);
  let before =
    Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live t)
  in
  assert (Result.is_error (Agent_client.Projection.apply_live_event t event));
  assert (
    Int.equal
      before
      (Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live t)));
  assert (
    List.is_empty
      (Agent_client.Live_projection.operations (Agent_client.Projection.live t)));
  print_endline
    "individually legal event rejected for receipt plus draft budget; previous state \
     unchanged";
  [%expect
    {| individually legal event rejected for receipt plus draft budget; previous state unchanged |}]
;;

let%expect_test
    "attachment stream failure marks stale and close still detaches exactly once"
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let notifications = Eio.Stream.create 8 in
      let attachment_id = P.Id.Attachment.of_string "att_client_test" |> protocol_ok in
      let detached = ref 0 in
      let non_history result =
        Public.Result.Non_history.of_internal result
        |> protocol_ok
        |> fun value -> Ok (Public.Result.Non_history value)
      in
      let request = function
        | P.Command.Session_attach _ ->
          Ok
            (Public.Result.Session_attach
               Public.Result.Attach.
                 { attachment =
                     P.Session.Attachment.
                       { id = attachment_id
                       ; session_id
                       ; mode = Read_write
                       ; owner_lease = None
                       }
                 ; replay = Snapshot snapshot
                 ; latest_event_sequence = 0L
                 ; reclaim_token = None
                 })
        | Session_detach _ ->
          incr detached;
          non_history
            (Session_detach
               P.Mutation_result.{ revision = 0L; latest_event_sequence = 0L })
        | _ -> Error (P.Error.invalid_request "unexpected request")
      in
      let connection =
        Agent_client.Transport.create
          ~request
          ~next_notification:(fun () -> Eio.Stream.take notifications)
          ~close:(fun () -> ())
        |> Agent_client.Connection.create
      in
      let handle =
        Agent_client.Session_handle.attach
          ~sw
          ~clock:env#clock
          ~connection
          ~session_id
          ~mode:Read_write
          ()
        |> protocol_ok
      in
      let failure =
        P.Error.create
          Snapshot_required
          ~message:"subscriber capacity exceeded"
          ~retryable:true
          ()
      in
      let other = P.Id.Attachment.of_string "att_other" |> protocol_ok in
      Eio.Stream.add
        notifications
        (Some
           (P.Stream_error.create ~session_id ~attachment_id:other failure
            |> P.Stream_error.to_notification));
      Eio.Fiber.yield ();
      assert (not (Agent_client.Session_handle.is_closed handle));
      Eio.Stream.add
        notifications
        (Some
           (P.Stream_error.create ~session_id ~attachment_id failure
            |> P.Stream_error.to_notification));
      Eio.Time.with_timeout_exn env#clock 1. (fun () ->
        Agent_client.Session_handle.await_closed handle);
      assert (
        Option.exists (Agent_client.Session_handle.last_error handle) ~f:(fun error ->
          P.Error.equal_code error.code Snapshot_required));
      assert (
        match
          Agent_client.Projection.synchronization
            (Agent_client.Session_handle.projection handle)
        with
        | Snapshot_required _ -> true
        | Current -> false);
      assert (Int.equal !detached 0);
      Agent_client.Session_handle.close handle;
      Agent_client.Session_handle.close handle;
      assert (Int.equal !detached 1);
      Agent_client.Connection.close connection));
  print_endline
    "unrelated attachment ignored; matching failure stale/closed; explicit close \
     detaches once";
  [%expect
    {| unrelated attachment ignored; matching failure stale/closed; explicit close detaches once |}]
;;

let%expect_test "snapshot all-tools plus classified subset seeds one active scoped call" =
  let key =
    P.Activity.Key.create ~scope:root_scope.key ~call_alias:"actual-call" ~parent:None
    |> protocol_ok
  in
  let descriptor =
    P.Activity.Tool.descriptor
      key
      ~call_entry_id:None
      ~name:"shell"
      ~kind:Function
      ~input:"command"
      ~classification:(Some Shell_script)
    |> protocol_ok
  in
  let summary =
    P.Activity.Tool.summary
      key
      ~descriptor:(Some descriptor)
      ~channels:[ { channel = Stdout; text = "before"; complete = true } ]
      ~state:Running
    |> protocol_ok
  in
  let snapshot =
    Public.Snapshot.create
      { snapshot_fields with
        session = { session with active_operation = Some (operation Running) }
      ; active_tool_calls = [ summary ]
      ; active_agent_calls = [ summary ]
      }
    |> protocol_ok
  in
  let t = Agent_client.Projection.install_snapshot snapshot in
  assert (
    match Agent_client.Projection.synchronization t with
    | Current -> true
    | Snapshot_required _ -> false);
  assert (
    Int.equal
      (List.length
         (Agent_client.Live_projection.activities (Agent_client.Projection.live t)))
      1);
  let event =
    P.Event.Recoverable.create
      ~session_id
      ~operation_id
      ~operation_sequence:1L
      ~anchor_sequence:0L
      ~timestamp
      ~invocation_id:None
      ~parent_invocation_id:None
      (Tool_activity
         (Progress { key; progress = { channel = Stdout; update = Append " after" } }))
    |> protocol_ok
  in
  let t = apply_live t event in
  let summary = List.hd_exn (fields t).active_tool_calls in
  let channel = List.hd_exn summary.channels in
  assert (String.equal channel.text "before after" && channel.complete);
  assert (Int.equal (List.length (fields t).active_agent_calls) 1);
  print_endline
    "classified subset deduplicated by admission; one seeded call preserves prefix and \
     classification";
  [%expect
    {| classified subset deduplicated by admission; one seeded call preserves prefix and classification |}]
;;

let%expect_test "replacement snapshot resets transient text but retains receipt ordering" =
  let key =
    P.Activity.Key.create
      ~scope:root_scope.key
      ~call_alias:"replacement-call"
      ~parent:None
    |> protocol_ok
  in
  let descriptor =
    P.Activity.Tool.descriptor
      key
      ~call_entry_id:None
      ~name:"tool"
      ~kind:Function
      ~input:"input"
      ~classification:None
    |> protocol_ok
  in
  let summary channels =
    P.Activity.Tool.summary key ~descriptor:(Some descriptor) ~channels ~state:Running
    |> protocol_ok
  in
  let active = { session with active_operation = Some (operation Running) } in
  let initial =
    Public.Snapshot.create
      { snapshot_fields with
        session = active
      ; active_tool_calls =
          [ summary [ { channel = Stdout; text = "old channel"; complete = true } ] ]
      }
    |> protocol_ok
  in
  let original =
    live
      ~anchor:0L
      1L
      (Changed { target = Content (part (item ())); change = Append "old draft" })
  in
  let t =
    Agent_client.Projection.install_snapshot initial |> fun t -> apply_live t original
  in
  let replacement =
    Public.Snapshot.create
      { snapshot_fields with
        session = { active with revision = 1L; latest_event_sequence = 1L }
      ; revision = 1L
      ; latest_event_sequence = 1L
      ; active_tool_calls = [ summary [] ]
      }
    |> protocol_ok
  in
  let internal =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (Session_updated active)
  in
  let event =
    Public.Durable.of_internal_envelope
      internal
      ~body:
        (Full
           (Shared
              (Public.Durable.Shared_payload.of_internal (Session_updated active)
               |> protocol_ok)))
      ~extension_status:None
      ~replacement_snapshot:(Some replacement)
    |> protocol_ok
  in
  let t = Agent_client.Projection.apply_event t event |> protocol_ok in
  assert (List.is_empty (draft_items t));
  assert (List.is_empty (List.hd_exn (fields t).active_tool_calls).channels);
  let t = apply_live t original in
  assert (List.is_empty (draft_items t));
  let t =
    apply_live
      t
      (live
         2L
         (Changed { target = Content (part (item ())); change = Append "new draft" }))
  in
  assert (
    match (List.hd_exn (draft_items t)).state with
    | Partial value ->
      (match value.completeness with
       | Missing_prefix -> true
       | Prefix_observed -> false)
    | Finalized _ -> false);
  let progress =
    P.Event.Recoverable.create
      ~session_id
      ~operation_id
      ~operation_sequence:3L
      ~anchor_sequence:1L
      ~timestamp
      ~invocation_id:None
      ~parent_invocation_id:None
      (Tool_activity
         (Progress { key; progress = { channel = Stdout; update = Append "new channel" } }))
    |> protocol_ok
  in
  let t = apply_live t progress in
  let channel = List.hd_exn (List.hd_exn (fields t).active_tool_calls).channels in
  assert (String.equal channel.text "new channel" && not channel.complete);
  print_endline
    "authoritative replacement clears omitted text; duplicates stay idempotent; new \
     append prefix is missing";
  [%expect
    {| authoritative replacement clears omitted text; duplicates stay idempotent; new append prefix is missing |}]
;;

let%expect_test
    "terminal read view retains observed partial activity without an invented outcome"
  =
  let key =
    P.Activity.Key.create ~scope:root_scope.key ~call_alias:"unfinished" ~parent:None
    |> protocol_ok
  in
  let descriptor =
    P.Activity.Tool.descriptor
      key
      ~call_entry_id:None
      ~name:"tool"
      ~kind:Function
      ~input:"input"
      ~classification:None
    |> protocol_ok
  in
  let activity sequence payload =
    P.Event.Recoverable.create
      ~session_id
      ~operation_id
      ~operation_sequence:sequence
      ~anchor_sequence:1L
      ~timestamp
      ~invocation_id:None
      ~parent_invocation_id:None
      (Tool_activity payload)
    |> protocol_ok
  in
  let partial =
    live
      ~anchor:1L
      1L
      (Changed { target = Content (part (item ())); change = Append "partial draft" })
  in
  let progress =
    activity
      3L
      (Progress { key; progress = { channel = Stdout; update = Append "partial output" } })
  in
  let t =
    active_projection ()
    |> fun t ->
    apply_live t partial
    |> fun t ->
    apply_live t (activity 2L (Started descriptor)) |> fun t -> apply_live t progress
  in
  let before =
    Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live t)
  in
  let terminal =
    Agent_client.Projection.apply_event
      t
      (shared 2L (Operation_cancelled (operation Cancelled)))
    |> protocol_ok
  in
  let view =
    Agent_client.Live_projection.terminal_view (Agent_client.Projection.live terminal)
    |> Option.value_exn
  in
  assert (P.Id.Operation.equal view.operation_id operation_id);
  assert (List.length (Transcript.Draft.items view.drafts) = 1);
  let summary = List.hd_exn view.activities in
  assert (
    match summary.state with
    | Running -> true
    | Finished _ -> false);
  assert (String.equal (List.hd_exn summary.channels).text "partial output");
  assert (List.is_empty (fields terminal).active_tool_calls);
  assert (Option.is_none (fields terminal).session.active_operation);
  assert (
    List.is_empty
      (Agent_client.Live_projection.operations (Agent_client.Projection.live terminal)));
  assert (
    Agent_client.Live_projection.retained_bytes (Agent_client.Projection.live terminal)
    = before);
  let contradictory =
    Agent_client.Live_projection.replace_snapshot
      (Agent_client.Projection.live terminal)
      ~active_operation:(Some (operation Running))
      []
  in
  assert (
    match contradictory with
    | Error error -> P.Error.equal_code error.code Snapshot_required
    | Ok _ -> false);
  assert (
    Option.is_some
      (Agent_client.Live_projection.terminal_view (Agent_client.Projection.live terminal)));
  let late =
    activity
      4L
      (Progress { key; progress = { channel = Stdout; update = Append " stale" } })
  in
  let terminal = apply_live terminal late in
  let frozen =
    Agent_client.Live_projection.terminal_view (Agent_client.Projection.live terminal)
    |> Option.value_exn
  in
  assert (
    String.equal
      (List.hd_exn (List.hd_exn frozen.activities).channels).text
      "partial output");
  let restarted =
    let next =
      { (operation Running) with
        id = P.Id.Operation.of_string "op_next_client_test" |> protocol_ok
      }
    in
    Agent_client.Projection.apply_event terminal (shared 3L (Operation_started next))
    |> protocol_ok
  in
  assert (
    Option.is_none
      (Agent_client.Live_projection.terminal_view
         (Agent_client.Projection.live restarted)));
  let replacement =
    Public.Snapshot.create
      { (fields terminal) with
        session =
          { (fields terminal).session with revision = 3L; latest_event_sequence = 3L }
      ; revision = 3L
      ; latest_event_sequence = 3L
      }
    |> protocol_ok
  in
  let replacement_event =
    let native =
      P.Event.Durable.of_payload
        ~session_id
        ~sequence:3L
        ~revision:3L
        ~timestamp
        (Session_updated session)
    in
    Public.Durable.of_internal_envelope
      native
      ~body:
        (Full
           (Shared
              (Public.Durable.Shared_payload.of_internal (Session_updated session)
               |> protocol_ok)))
      ~extension_status:None
      ~replacement_snapshot:(Some replacement)
    |> protocol_ok
  in
  let replaced =
    Agent_client.Projection.apply_event terminal replacement_event |> protocol_ok
  in
  assert (
    Option.is_none
      (Agent_client.Live_projection.terminal_view (Agent_client.Projection.live replaced)));
  assert (
    Option.is_none
      (Agent_client.Live_projection.terminal_view
         (Agent_client.Live_projection.clear (Agent_client.Projection.live terminal))));
  print_endline
    "terminal progress stays bounded and fenced; no tool outcome fabricated; new \
     operation and replacement retire it";
  [%expect
    {| terminal progress stays bounded and fenced; no tool outcome fabricated; new operation and replacement retire it |}]
;;

let%expect_test "gap admission at the public tool byte boundary is atomic" =
  let module Live = Agent_client.Live_projection in
  let module Tool = P.Activity.Tool in
  let key =
    P.Activity.Key.create ~scope:root_scope.key ~call_alias:"boundary" ~parent:None
    |> protocol_ok
  in
  let make text =
    Tool.summary
      key
      ~descriptor:None
      ~channels:[ { channel = Stdout; text; complete = true } ]
      ~state:Running
  in
  let measure summary =
    Document_schema.Json.validate_and_measure
      ~limits:Transcript.Admission.default
      (Tool.summary_to_json summary)
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
    |> neutral_ok
  in
  let bound = 16 * 1024 * 1024 in
  let overhead = make "" |> protocol_ok |> measure in
  let summary = make (String.make (bound - overhead) 'x') |> protocol_ok in
  assert (Int.equal (measure summary) bound);
  let t = Live.seed_activity (Live.empty ()) ~operation_id [ summary ] |> protocol_ok in
  let bytes = Live.retained_bytes t in
  let apply sequence =
    Live.apply
      t
      ~durable_sequence:1L
      ~active_operation:(Some (operation Running))
      ~canonical_history:[]
      (live sequence (Source_finished { scope = root_scope; completion = Complete }))
  in
  assert (
    match apply 2L with
    | Error error -> P.Error.equal_code error.code Snapshot_required
    | Ok _ -> false);
  assert (Int.equal (Live.retained_bytes t) bytes);
  assert (List.hd_exn (List.hd_exn (Live.activities t)).channels).complete;
  (* A failed gap must not consume its sequence or change the original prefix. *)
  let next = apply 1L |> protocol_ok in
  assert (List.hd_exn (List.hd_exn (Live.activities next)).channels).complete;
  print_endline
    "oversized gap returns Snapshot_required; original prefix and sequence survive";
  [%expect
    {| oversized gap returns Snapshot_required; original prefix and sequence survive |}]
;;
