open! Core

let inference_options () =
  { Agent_server.Daemon.default_options with
    inference_policy =
      Agent_server_test_support.inference_policy
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected fixture model dispatch")
  }
;;

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let key text = Agent_protocol.Idempotency_key.of_string text |> ok
let request = Agent_client.Connection.request

let snapshot connection session_id =
  match request connection (Session_get { session_id; history = None }) |> ok with
  | Agent_protocol.Public.Result.Session_get value ->
    Agent_protocol.Public.Snapshot.fields value
  | _ -> failwith "expected snapshot"
;;

(* Private fixtures are constructed from known authored content, never by
   converting a public Visible/Redacted body into canonical history. *)
let private_fixture (fields : Agent_protocol.Public.Snapshot.Fields.t) =
  let semantic =
    History_entry.Payload.Semantic.create
      (Message
         { form = Input
         ; role = Developer
         ; content =
             [ Text { text = "private fixture"; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:History_entry.Payload.Metadata.empty
    |> Result.ok_or_failwith
  in
  let id =
    History_entry.Id.create ~namespace:"private-fixture" ~sequence:0
    |> Result.ok_or_failwith
  in
  let entry =
    Agent_protocol.History.
      { id
      ; role = System
      ; kind = Message
      ; payload = History_entry.Payload.to_json (History_entry.Payload.authored semantic)
      ; provenance = Canonical
      ; content_revision = Agent_protocol.History.Content_revision.zero
      ; redacted = false
      }
  in
  let history =
    Agent_protocol.History.Window.
      { entries = [ entry ]
      ; previous_cursor = None
      ; next_cursor = None
      ; reached_start = true
      ; reached_end = true
      ; structurally_complete = true
      }
  in
  Agent_protocol.Snapshot.
    { session = fields.session
    ; canonical_history = history
    ; archived_revisions = fields.archived_revisions
    ; effective_history = Some history
    ; deferred_entries = []
    ; permissions = fields.permissions
    ; grants = fields.grants
    ; jobs = fields.jobs
    ; extension_status = fields.extension_status
    ; schedules = fields.schedules
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = fields.halted
    ; halt_reason = fields.halt_reason
    ; failure = fields.failure
    ; revision = fields.revision
    ; latest_event_sequence = fields.latest_event_sequence
    }
;;

let install fields =
  Agent_protocol.Public.Snapshot.create fields
  |> ok
  |> Agent_client.Projection.install_snapshot
;;

let projection_fields projection =
  Agent_client.Projection.snapshot projection |> Agent_protocol.Public.Snapshot.fields
;;

let request_without_history = Agent_client.Connection.request_without_history

let non_history = function
  | Agent_protocol.Public.Result.Non_history value ->
    Agent_protocol.Public.Result.Non_history.value value
  | Session_get _ | Session_attach _ | Session_create _ | Private_provider_challenge _ ->
    failwith "expected non-history result"
;;

let with_host ?(prompt = "<developer>Offline gap regression.</developer>") f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      "/tmp/ochat-gap-test-" ^ Agent_protocol.Id.Transaction.(to_string (create ()))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:false path)
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "prompt.chatmd") prompt;
        Eio.Switch.run (fun sw ->
          let host =
            Agent_server.Embedded.start
              ~daemon_options:(inference_options ())
              ~sw
              ~env
              { prompt_file
              ; workspace = root
              ; tool_dir = root
              ; home = root
              ; data_root = Some (Filename.concat root "store")
              ; start_immediately = false
              ; permission_profile = Agent_server.Embedded.default_permission_profile
              ; attachment_mode = Read_write
              ; event_capacity = 128
              }
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Embedded.close host)
            ~f:(fun () -> f env root host))))
;;

let%test_unit "new runtime rejects a changed tree even when the revision is cached" =
  with_host (fun env root host ->
    let connection = Agent_server.Embedded.connection host in
    let initial = snapshot connection (Agent_server.Embedded.session_id host) in
    let revision = Option.value_exn initial.session.prompt_revision in
    let tree_file =
      Filename.concat
        root
        ("store/prompt-artifacts/"
         ^ Agent_protocol.Id.Prompt_revision.to_string revision
         ^ "/tree/prompt.chatmd")
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / tree_file) in
    Eio.Path.unlink path;
    Eio.Path.save ~create:(`Exclusive 0o600) path "<developer>altered</developer>";
    let result =
      request
        connection
        (Session_create
           { spec = initial.session.spec
           ; requested_mode = None
           ; subscribe = false
           ; idempotency_key = key "cached-tree-corruption"
           })
    in
    match result with
    | Error error ->
      assert (
        String.is_substring error.message ~substring:"prompt tree verification failed")
    | Ok _ -> failwith "cached revision accepted an altered materialized tree")
;;

let%expect_test
    "ordinary session create cannot turn a known revision into a generated child"
  =
  with_host (fun env root host ->
    let connection = Agent_server.Embedded.connection host in
    let initial = snapshot connection (Agent_server.Embedded.session_id host) in
    let revision = Option.value_exn initial.session.prompt_revision in
    let sessions = Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "store/sessions") in
    let before = Eio.Path.read_dir sessions |> List.sort ~compare:String.compare in
    let result =
      request
        connection
        (Session_create
           { spec = { initial.session.spec with prompt = Generated revision }
           ; requested_mode = None
           ; subscribe = false
           ; idempotency_key = key "reject-generated-bypass"
           })
    in
    (match result with
     | Error { code = Permission_denied; _ } -> ()
     | Error error ->
       raise_s [%sexp "unexpected creation rejection", (error : Agent_protocol.Error.t)]
     | Ok _ -> failwith "ordinary create bypassed delegation admission");
    [%test_eq: string list]
      before
      (Eio.Path.read_dir sessions |> List.sort ~compare:String.compare);
    print_endline "known revision rejected; no child directory created");
  [%expect {| known revision rejected; no child directory created |}]
;;

let attach_reader host session_id =
  let reader = Agent_server.Embedded.connect host in
  ignore
    (Agent_client.Session_handle.initialize
       reader
       ~implementation_name:"gap-test"
       ~implementation_version:"test"
     |> ok
     : Agent_protocol.Initialize.Response.t);
  let attached =
    request
      reader
      (Session_attach
         { session_id
         ; requested_mode = Read_only
         ; subscribe = false
         ; after_sequence = None
         ; reclaim_token = None
         ; idempotency_key = key "attach-reader"
         })
    |> ok
  in
  match attached with
  | Agent_protocol.Public.Result.Session_attach value -> reader, value.attachment.id
  | _ -> failwith "expected attachment"
;;

let mutations session_id attachment_id expected_revision target_revision =
  let open Agent_protocol.Command in
  let idempotency_key = key "rejected" in
  [ Session_start { session_id; attachment_id; queue_if_limited = true; idempotency_key }
  ; Session_stop { session_id; attachment_id; mode = Cancel; idempotency_key }
  ; Session_send_message
      { session_id
      ; attachment_id
      ; content = { kind = Plain_text; text = "denied"; attachments = [] }
      ; idempotency_key
      }
  ; Session_cancel_operation
      { session_id
      ; attachment_id
      ; operation_id = Agent_protocol.Id.Operation.create ()
      ; idempotency_key
      }
  ; Session_compact
      { session_id
      ; attachment_id
      ; expected_revision = Some expected_revision
      ; idempotency_key
      }
  ; Session_reset
      { session_id
      ; attachment_id
      ; expected_revision
      ; keep_history = false
      ; keep_tasks = false
      ; keep_cache = false
      ; keep_workspace = false
      ; keep_grants = false
      ; keep_labels = false
      ; idempotency_key
      }
  ; Session_rebuild
      { session_id
      ; attachment_id
      ; expected_revision
      ; prompt_choice = Pinned
      ; idempotency_key
      }
  ; Session_upgrade_prompt
      { session_id
      ; attachment_id
      ; expected_revision
      ; target_revision
      ; allow_migration = true
      ; idempotency_key
      }
  ; Session_delete
      { session_id
      ; attachment_id
      ; expected_revision
      ; policy = Remove
      ; confirmation = Agent_protocol.Id.Session.to_string session_id
      ; idempotency_key
      }
  ; Permission_respond
      { session_id
      ; attachment_id
      ; permission_id = Agent_protocol.Id.Permission.create ()
      ; permission_generation = 0
      ; choice = Approve_once
      ; reason = None
      ; idempotency_key
      }
  ; Grant_revoke
      { session_id
      ; attachment_id
      ; grant_id = Agent_protocol.Id.Grant.create ()
      ; reason = "denied"
      ; idempotency_key
      }
  ; Job_cancel
      { session_id
      ; attachment_id
      ; job_id = Agent_protocol.Id.Job.create ()
      ; expected_generation = None
      ; expected_attempt = None
      ; idempotency_key
      }
  ; Schedule_create
      { session_id
      ; attachment_id
      ; payload = `String "denied"
      ; due = After_ms 60000
      ; misfire = Deliver_once_immediately
      ; idempotency_key
      }
  ; Schedule_cancel
      { session_id
      ; attachment_id
      ; schedule_id = Agent_protocol.Id.Schedule.create ()
      ; expected_generation = None
      ; idempotency_key
      }
  ]
;;

let%expect_test "every read-only mutation is rejected before stopped runtime preparation" =
  with_host (fun env root host ->
    let writer = Agent_server.Embedded.connection host in
    let session_id = Agent_server.Embedded.session_id host in
    let reader, attachment_id = attach_reader host session_id in
    let before = snapshot writer session_id in
    let marker = Eio.Path.(Eio.Stdenv.fs env / root / "workspace-marker") in
    Eio.Path.save ~create:(`Exclusive 0o600) marker "preserve";
    List.iter
      (mutations
         session_id
         attachment_id
         before.revision
         (Option.value_exn before.session.prompt_revision))
      ~f:(fun command ->
        (match request reader command with
         | Error error when Agent_protocol.Error.equal_code error.code Permission_denied
           -> ()
         | result ->
           raise_s
             [%sexp
               (command : Agent_protocol.Command.t)
             , (result : (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)]);
        let after = snapshot writer session_id in
        assert (Int64.equal before.revision after.revision);
        assert (Int64.equal before.latest_event_sequence after.latest_event_sequence);
        assert (String.equal (Eio.Path.load marker) "preserve"));
    Agent_client.Connection.close reader;
    print_endline
      "14 mutations denied; session state, event position and workspace unchanged");
  [%expect
    {| 14 mutations denied; session state, event position and workspace unchanged |}]
;;

let principal scopes =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.of_string "pri_gap_test" |> ok)
    ~authentication_kind:"test"
    ~scopes:(Agent_protocol.Scope.Set.of_list scopes)
    ~attributes:[]
  |> ok
;;

let%expect_test "creation cannot attach without transcript scope" =
  with_host (fun _ _ host ->
    let initial =
      snapshot
        (Agent_server.Embedded.connection host)
        (Agent_server.Embedded.session_id host)
    in
    let principal =
      { (Agent_server.Embedded.principal host) with
        scopes = Agent_protocol.Scope.Set.singleton Create_sessions
      }
    in
    let context =
      Agent_server.Connection_context.create
        ~connection_id:"create-without-transcript"
        ~principal
        ~transport:In_memory
        ~max_attachments:8
        ~publish_notification:(fun (_ : Agent_protocol.Envelope.t) -> ())
    in
    Agent_server.Connection_context.mark_initialized context;
    let result =
      Agent_server.Dispatcher.dispatch_command
        (Agent_server.Embedded.dispatcher host)
        ~context
        (Session_create
           { spec = initial.session.spec
           ; requested_mode = Some Read_only
           ; subscribe = true
           ; idempotency_key = key "no-transcript"
           })
    in
    (match result with
     | Error error ->
       assert (Agent_protocol.Error.equal_code error.code Permission_denied)
     | Ok _ -> failwith "creation attached without transcript scope");
    let private_snapshot = private_fixture initial in
    let projected =
      Agent_server.Principal_projection.snapshot
        principal
        { private_snapshot with
          deferred_entries = private_snapshot.canonical_history.entries
        }
      |> ok
      |> Agent_protocol.Public.Snapshot.fields
    in
    assert (List.is_empty projected.canonical_history.entries);
    assert (List.is_empty projected.deferred_entries);
    Agent_server.Embedded.close_connection host context;
    print_endline
      "creation attachment denied; cached transcript and deferred entries omitted");
  [%expect
    {| creation attachment denied; cached transcript and deferred entries omitted |}]
;;

let%expect_test "attachment mode checks cover original requests and cached retries" =
  List.iter [ true; false ] ~f:(fun attached_create ->
    with_host (fun env root host ->
      let module P = Agent_protocol in
      let module E = Agent_server.Embedded in
      let module C = Agent_server.Connection_context in
      let full = E.principal host in
      let contexts = ref [] in
      let context principal name =
        let context =
          C.create
            ~connection_id:name
            ~principal
            ~transport:In_memory
            ~max_attachments:8
            ~publish_notification:(fun (_ : P.Envelope.t) -> ())
        in
        C.mark_initialized context;
        contexts := context :: !contexts;
        context
      in
      Exn.protect
        ~finally:(fun () -> List.iter !contexts ~f:(E.close_connection host))
        ~f:(fun () ->
          let owner = context full "attachment-owner" in
          let dispatch context command =
            Agent_server.Dispatcher.dispatch_command (E.dispatcher host) ~context command
          in
          let initial = snapshot (E.connection host) (E.session_id host) in
          let spec =
            { initial.session.spec with
              execution_host = Daemon
            ; liveness = Owner_bound { disconnect_grace_ms = 60000; stop_mode = Cancel }
            }
          in
          let create_request =
            P.Session.Create_request.
              { spec
              ; requested_mode = (if attached_create then Some Owner_read_write else None)
              ; subscribe = false
              ; idempotency_key = key "scope-retry-create"
              }
          in
          let created =
            match dispatch owner (Session_create create_request) |> ok with
            | P.Public.Result.Session_create value -> value
            | _ -> failwith "expected created session"
          in
          let session_id = created.session.id in
          let command, accepted, with_key =
            if attached_create
            then (
              let attached = Option.value_exn created.attachment in
              assert (Option.is_some attached.reclaim_token);
              assert (Option.is_some attached.attachment.owner_lease);
              ( P.Command.Session_create create_request
              , P.Public.Result.Session_create created
              , fun idempotency_key ->
                  P.Command.Session_create { create_request with idempotency_key } ))
            else (
              let attach_request =
                P.Session.Attach_request.
                  { session_id
                  ; requested_mode = Owner_read_write
                  ; subscribe = false
                  ; after_sequence = None
                  ; reclaim_token = None
                  ; idempotency_key = key "scope-retry-attach"
                  }
              in
              let command = P.Command.Session_attach attach_request in
              let accepted = dispatch owner command |> ok in
              (match accepted with
               | P.Public.Result.Session_attach attached ->
                 assert (Option.is_some attached.reclaim_token);
                 assert (Option.is_some attached.attachment.owner_lease)
               | _ -> failwith "expected owner attachment");
              ( command
              , accepted
              , fun idempotency_key ->
                  P.Command.Session_attach { attach_request with idempotency_key } ))
          in
          let snapshot_json () =
            dispatch owner (Session_get { session_id; history = None })
            |> ok
            |> P.Public.Result.to_json
          in
          let before = snapshot_json () in
          let replay = dispatch owner command |> ok in
          assert (
            Jsonaf.exactly_equal
              (P.Public.Result.to_json accepted)
              (P.Public.Result.to_json replay));
          assert (Jsonaf.exactly_equal before (snapshot_json ()));
          let sessions = Eio.Path.(Eio.Stdenv.fs env / root / "store/sessions") in
          let before_sessions =
            Eio.Path.read_dir sessions |> List.sort ~compare:String.compare
          in
          List.iteri
            [ [ P.Scope.Create_sessions ]
            ; [ Create_sessions; View_session_transcript; Own_sessions ]
            ; [ Create_sessions; View_session_transcript; Send_messages ]
            ]
            ~f:(fun index scopes ->
              let narrowed = { full with scopes = P.Scope.Set.of_list scopes } in
              let context = context narrowed ("narrowed-" ^ Int.to_string index) in
              let denied command =
                match dispatch context command with
                | Error error when P.Error.equal_code error.code Permission_denied -> ()
                | _ -> failwith "narrowed principal received an attachment result"
              in
              denied command;
              denied (with_key (key ("fresh-denied-" ^ Int.to_string index)));
              assert (List.is_empty (C.attachments context));
              assert (Jsonaf.exactly_equal before (snapshot_json ()));
              [%test_eq: string list]
                before_sessions
                (Eio.Path.read_dir sessions |> List.sort ~compare:String.compare));
          print_endline
            (if attached_create
             then
               "create: authorized retry exact; narrowed original/retry denied without \
                changes"
             else
               "attach: authorized retry exact; narrowed original/retry denied without \
                changes"))));
  [%expect
    {|
    create: authorized retry exact; narrowed original/retry denied without changes
    attach: authorized retry exact; narrowed original/retry denied without changes
    |}]
;;

let%expect_test
    "scope projection filters durable events without gaps or client decode errors"
  =
  with_host (fun _ _ host ->
    let session_id = Agent_server.Embedded.session_id host in
    let initial = snapshot (Agent_server.Embedded.connection host) session_id in
    let reader = principal [ View_session_transcript ] in
    let full =
      principal
        [ View_session_transcript; View_security_state; Manage_grants; Send_messages ]
    in
    let kinds =
      Agent_protocol.Event.Durable.
        [ Permission_requested
        ; Permission_resolved
        ; Grant_created
        ; Grant_revoked
        ; Job_state_changed
        ; Schedule_created
        ; Schedule_state_changed
        ; Schedule_cancelled
        ; Moderator_notification
        ]
    in
    let projection = ref (install initial) in
    List.iteri kinds ~f:(fun index kind ->
      let event =
        Agent_protocol.Event.Durable.
          { session_id
          ; sequence = Int64.(initial.latest_event_sequence + of_int index + 1L)
          ; revision = initial.revision
          ; timestamp = initial.session.updated_at
          ; kind
          ; visibility = Full
          ; payload = `Object [ "secret", `String "must-not-leak" ]
          }
      in
      let hidden = Agent_server.Principal_projection.durable reader event |> ok in
      assert (
        match hidden.body with
        | Hidden -> true
        | Full _ | Filtered _ -> false);
      (* Protected malformed data is hidden from an unauthorized reader, while
         an authorized projection must reject malformed typed payloads. *)
      if not (Agent_protocol.Event.Durable.equal_kind kind Moderator_notification)
      then assert (Result.is_error (Agent_server.Principal_projection.durable full event));
      projection := Agent_client.Projection.apply_event !projection hidden |> ok);
    assert (
      Int64.equal
        (projection_fields !projection).latest_event_sequence
        Int64.(initial.latest_event_sequence + of_int (List.length kinds)));
    print_endline "9 protected event kinds hidden; contiguous client projection");
  [%expect {| 9 protected event kinds hidden; contiguous client projection |}]
;;

let schedules snapshot count =
  List.init count ~f:(fun index ->
    Agent_protocol.Schedule.
      { id = Agent_protocol.Id.Schedule.of_string (sprintf "sch_gap_%03d" index) |> ok
      ; session_id = snapshot.Agent_protocol.Public.Snapshot.Fields.session.id
      ; generation = 0
      ; payload = `String (Int.to_string index)
      ; created_at = snapshot.session.created_at
      ; next_due_at = snapshot.session.created_at
      ; misfire = Deliver_once_immediately
      ; status = Scheduled
      ; delivery_count = 0
      ; last_delivery_at = None
      ; delivery_cancellation = None
      ; ownership = None
      })
;;

let%expect_test
    "restricted dispatcher snapshot replay live updates and exports share filtering"
  =
  with_host (fun env _ host ->
    let writer = Agent_server.Embedded.connection host in
    let session_id = Agent_server.Embedded.session_id host in
    let writer_id = (Agent_server.Embedded.attachment host).id in
    let reader =
      { (Agent_server.Embedded.principal host) with
        scopes = Agent_protocol.Scope.Set.singleton View_session_transcript
      }
    in
    let events = Queue.create () in
    let context =
      Agent_server.Connection_context.create
        ~connection_id:"restricted"
        ~principal:reader
        ~transport:In_memory
        ~max_attachments:8
        ~publish_notification:(Queue.enqueue events)
    in
    Agent_server.Connection_context.mark_initialized context;
    let dispatch =
      Agent_server.Dispatcher.dispatch_command
        (Agent_server.Embedded.dispatcher host)
        ~context
    in
    let initial = snapshot writer session_id in
    let attach after_sequence idempotency_key =
      dispatch
        (Session_attach
           { session_id
           ; requested_mode = Read_only
           ; subscribe = true
           ; after_sequence
           ; reclaim_token = None
           ; idempotency_key
           })
      |> ok
      |> function
      | Agent_protocol.Public.Result.Session_attach value -> value
      | _ -> failwith "attach"
    in
    let attached = attach None (key "restricted-attach") in
    let marker = "private-schedule-must-not-leak" in
    ignore
      (request
         writer
         (Schedule_create
            { session_id
            ; attachment_id = writer_id
            ; payload = `String marker
            ; due = After_ms 3600000
            ; misfire = Deliver_once_immediately
            ; idempotency_key = key "private-schedule"
            })
       |> ok
       : Agent_protocol.Public.Result.t);
    let projected = dispatch (Session_get { session_id; history = None }) |> ok in
    assert (
      not
        (String.is_substring
           (Agent_protocol.Public.Result.to_json projected |> Jsonaf.to_string)
           ~substring:marker));
    let replay = attach (Some initial.latest_event_sequence) (key "restricted-replay") in
    (match replay.replay with
     | Events values ->
       assert (
         List.exists values ~f:(fun event ->
           Agent_protocol.Event.Durable.equal_kind event.kind Schedule_created
           &&
           match event.body with
           | Hidden -> true
           | Full _ | Filtered _ -> false))
     | _ -> failwith "expected replay");
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      while Queue.is_empty events do
        Eio.Fiber.yield ()
      done);
    Queue.iter events ~f:(fun event ->
      assert (
        not
          (String.is_substring
             (Agent_protocol.Envelope.to_json event |> Jsonaf.to_string)
             ~substring:marker)));
    let export connection attachment_id =
      request
        connection
        (Session_export
           { session_id; attachment_id; format = Json; revision = None; history = None })
      |> ok
      |> non_history
      |> function
      | Agent_protocol.Method_result.Session_export value -> value.blob
      | _ -> failwith "export"
    in
    let full_blob = export writer writer_id in
    let denied =
      dispatch
        (Blob_read
           { session_id
           ; attachment_id = attached.attachment.id
           ; blob_id = full_blob.id
           ; offset = 0L
           ; max_bytes = 65536
           })
    in
    (match denied with
     | Error error ->
       assert (Agent_protocol.Error.equal_code error.code Permission_denied)
     | Ok _ -> failwith "privileged export leaked");
    let filtered =
      dispatch
        (Session_export
           { session_id
           ; attachment_id = attached.attachment.id
           ; format = Json
           ; revision = None
           ; history = None
           })
      |> ok
      |> non_history
      |> function
      | Agent_protocol.Method_result.Session_export value -> value.blob
      | _ -> failwith "export"
    in
    let chunk =
      dispatch
        (Blob_read
           { session_id
           ; attachment_id = attached.attachment.id
           ; blob_id = filtered.id
           ; offset = 0L
           ; max_bytes = 65536
           })
      |> ok
      |> non_history
      |> function
      | Agent_protocol.Method_result.Blob_read value -> value
      | _ -> failwith "chunk"
    in
    assert (
      not (String.is_substring (Base64.decode_exn chunk.data_base64) ~substring:marker));
    Agent_server.Embedded.close_connection host context;
    print_endline "snapshot/replay/live/export filtered; privileged export blob denied");
  [%expect {| snapshot/replay/live/export filtered; privileged export blob denied |}]
;;

let%expect_test
    "pagination reaches every item and binds cursors to principal query data and host"
  =
  with_host (fun _ _ host ->
    let session_id = Agent_server.Embedded.session_id host in
    let initial = snapshot (Agent_server.Embedded.connection host) session_id in
    let items = schedules initial 5 in
    let service = Agent_server.Pagination.create () in
    let owner = principal [ View_session_transcript; Send_messages ] in
    let response =
      Agent_protocol.Method_result.Schedule_list { items; next_cursor = None }
    in
    let command cursor status =
      Agent_protocol.Command.Schedule_list
        { session_id; page = { limit = 2; cursor }; status }
    in
    let page service principal command =
      Agent_server.Pagination.lists service principal command response
    in
    let get = function
      | Agent_protocol.Method_result.Schedule_list page -> page
      | _ -> failwith "page"
    in
    let first = page service owner (command None None) |> ok |> get in
    let second = page service owner (command first.next_cursor None) |> ok |> get in
    let third = page service owner (command second.next_cursor None) |> ok |> get in
    assert (List.length (first.items @ second.items @ third.items) = 5);
    assert (Option.is_none third.next_cursor);
    assert (
      Result.is_error
        (page
           service
           (principal [ View_session_transcript ])
           (command first.next_cursor None)));
    assert (
      Result.is_error (page service owner (command first.next_cursor (Some "scheduled"))));
    assert (
      Result.is_error
        (page (Agent_server.Pagination.create ()) owner (command first.next_cursor None)));
    assert (
      Result.is_error
        (Agent_server.Pagination.lists
           service
           owner
           (command first.next_cursor None)
           (Schedule_list { items = List.take items 4; next_cursor = None })));
    print_endline
      "pages 2/2/1; foreign-scope, changed-query, changed-data and restart cursors \
       rejected");
  [%expect
    {| pages 2/2/1; foreign-scope, changed-query, changed-data and restart cursors rejected |}]
;;

let%expect_test
    "history windows bound entries and expose navigation and structural incompleteness"
  =
  with_host (fun _ _ host ->
    let session_id = Agent_server.Embedded.session_id host in
    let initial = snapshot (Agent_server.Embedded.connection host) session_id in
    let entries =
      List.init 5 ~f:(fun sequence ->
        Agent_protocol.History.
          { id =
              History_entry.Id.create ~namespace:"gap-history" ~sequence
              |> Result.ok_or_failwith
          ; role = Assistant
          ; kind =
              (if sequence = 2
               then Tool_call
               else if sequence = 3
               then Tool_output
               else Message)
          ; payload = `Object [ "text", `String (Int.to_string sequence) ]
          ; provenance = Canonical
          ; content_revision = Agent_protocol.History.Content_revision.zero
          ; redacted = false
          })
    in
    let initial = private_fixture initial in
    let initial =
      { initial with
        canonical_history = { initial.canonical_history with entries }
      ; effective_history = None
      }
    in
    let service = Agent_server.Pagination.create () in
    let owner = principal [ View_session_transcript; View_security_state ] in
    let get position effective =
      Agent_server.Pagination.history
        service
        owner
        { session_id; history = Some { position; limit = 2; effective } }
        initial
      |> ok
    in
    let first = (get (Before (List.nth_exn entries 2).id) false).canonical_history in
    assert (List.length first.entries = 2 && first.reached_start && not first.reached_end);
    assert (not first.structurally_complete);
    let second =
      (get (Cursor (Option.value_exn first.next_cursor)) false).canonical_history
    in
    let third =
      (get (Cursor (Option.value_exn second.next_cursor)) false).canonical_history
    in
    assert (
      List.length second.entries = 2 && List.length third.entries = 1 && third.reached_end);
    let tail = (get (Tail 20) false).canonical_history in
    assert (List.length tail.entries = 2 && tail.reached_end && not tail.reached_start);
    let after = (get (After (List.hd_exn entries).id) false).canonical_history in
    assert (List.length after.entries = 2 && not after.structurally_complete);
    let effective = get (Tail 2) true in
    assert (List.is_empty effective.canonical_history.entries);
    assert (List.length (Option.value_exn effective.effective_history).entries = 2);
    print_endline
      "before/after/tail/cursor bounded; 2/2/1 navigation; partial tool history flagged");
  [%expect
    {| before/after/tail/cursor bounded; 2/2/1 navigation; partial tool history flagged |}]
;;

let%expect_test "RPC history windows select canonical or effective history explicitly" =
  let prompt =
    "<developer>Offline window regression.</developer>\n\
     <user>first</user>\n\
     <assistant>second</assistant>\n\
     <user>third</user>"
  in
  with_host ~prompt (fun _ _ host ->
    let connection = Agent_server.Embedded.connection host in
    let session_id = Agent_server.Embedded.session_id host in
    let full = snapshot connection session_id in
    assert (List.length full.canonical_history.entries = 4);
    let get effective =
      request
        connection
        (Session_get
           { session_id; history = Some { position = Tail 2; limit = 2; effective } })
      |> ok
      |> function
      | Agent_protocol.Public.Result.Session_get value ->
        Agent_protocol.Public.Snapshot.fields value
      | _ -> failwith "expected snapshot"
    in
    let canonical = (get false).canonical_history in
    assert (List.length canonical.entries = 2);
    assert (canonical.reached_end && not canonical.reached_start);
    assert (not canonical.structurally_complete);
    assert (Option.is_some canonical.previous_cursor);
    let effective = get true in
    assert (List.is_empty effective.canonical_history.entries);
    assert (not effective.canonical_history.structurally_complete);
    assert (List.length (Option.value_exn effective.effective_history).entries = 2);
    print_endline "RPC canonical/effective tails bounded; full snapshot remains complete");
  [%expect {| RPC canonical/effective tails bounded; full snapshot remains complete |}]
;;

let%expect_test
    "extension status filtering matches snapshots, replay and replacement snapshots"
  =
  with_host (fun _ _ host ->
    let initial =
      snapshot
        (Agent_server.Embedded.connection host)
        (Agent_server.Embedded.session_id host)
    in
    let status =
      Agent_protocol.Extension_status.of_json
        (`Object
            [ "version", `Number "1"
            ; "kind", `String "delivery"
            ; "id", `String "dlv_filtered"
            ; "generation", `Number "0"
            ; "state", `String "pending"
            ])
      |> ok
    in
    let initial = { (private_fixture initial) with extension_status = [ status ] } in
    List.iter
      Agent_protocol.Scope.
        [ []; [ Send_messages ]; [ View_session_transcript ]; [ View_security_state ] ]
      ~f:(fun scopes ->
        let reader = principal scopes in
        let allowed = Agent_protocol.Principal.has_scope reader View_security_state in
        let projected = Agent_server.Principal_projection.snapshot reader initial |> ok in
        let fields = Agent_protocol.Public.Snapshot.fields projected in
        assert (Bool.equal (not (List.is_empty fields.extension_status)) allowed);
        let revision = Int64.(initial.revision + 1L) in
        let latest_event_sequence = Int64.(initial.latest_event_sequence + 1L) in
        let updated =
          { initial with
            revision
          ; latest_event_sequence
          ; session = { initial.session with revision; latest_event_sequence }
          }
        in
        let event =
          Agent_protocol.Event.Durable.of_payload
            ~session_id:initial.session.id
            ~sequence:latest_event_sequence
            ~revision
            ~timestamp:initial.session.updated_at
            (Session_updated updated.session)
          |> fun event ->
          Agent_protocol.Event.Durable.with_extension_status event [ status ]
        in
        List.iter
          [ event; Agent_protocol.Event.Durable.with_replacement_snapshot event updated ]
          ~f:(fun event ->
            let event = Agent_server.Principal_projection.durable reader event |> ok in
            let projection = Agent_client.Projection.install_snapshot projected in
            let result =
              Agent_client.Projection.apply_event projection event
              |> ok
              |> projection_fields
            in
            assert (Bool.equal (not (List.is_empty result.extension_status)) allowed);
            if not allowed
            then
              assert (
                not
                  (String.is_substring
                     (Agent_protocol.Public.Durable.to_json event |> Jsonaf.to_string)
                     ~substring:"dlv_filtered")))));
  print_endline
    "four scope combinations agree across snapshot, live/replay and replacement paths";
  [%expect
    {| four scope combinations agree across snapshot, live/replay and replacement paths |}]
;;

let%expect_test
    "initialization reports actual host and flush mode without enabling extension tools"
  =
  with_host (fun _ _ host ->
    let reader = Agent_server.Embedded.connect host in
    let implementation =
      Agent_protocol.Initialize.Implementation.create ~name:"extensions" ~version:"test"
      |> ok
    in
    let query =
      Agent_protocol.Initialize.Request.create
        ~implementation
        ~protocol_min:Agent_protocol.Version.current
        ~protocol_max:Agent_protocol.Version.current
        ~features:Agent_protocol.Extension_capabilities.known_features
        ~event_encodings:[ Json ]
        ~max_inbound_event_bytes:65536
        ()
      |> ok
    in
    let result =
      match request_without_history reader (Protocol_initialize query) |> ok with
      | Agent_protocol.Method_result.Protocol_initialize result -> result
      | _ -> failwith "expected initialization"
    in
    let metadata = Option.value_exn result.extensions in
    assert (
      Agent_protocol.Extension_capabilities.equal_host metadata.host Embedded_durable);
    assert (
      Agent_protocol.Extension_capabilities.equal_journal_flush
        metadata.journal_flush
        Synced);
    let expected =
      List.sort
        Agent_protocol.Extension_capabilities.known_features
        ~compare:String.compare
    in
    [%test_eq: string list] expected metadata.available_features;
    [%test_eq: string list]
      expected
      (List.sort result.enabled_features ~compare:String.compare);
    let json = Agent_protocol.Initialize.Response.to_json result in
    ignore
      (Agent_protocol.Initialize.Response.of_json json |> ok
       : Agent_protocol.Initialize.Response.t);
    let legacy =
      match json with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) -> not (String.equal name "extensions")))
      | _ -> assert false
    in
    assert (
      Option.is_none (Agent_protocol.Initialize.Response.of_json legacy |> ok).extensions);
    let forged =
      { result with
        enabled_features = [ "chatml.invocations.v1" ]
      ; extensions =
          Some
            (Agent_protocol.Extension_capabilities.create
               ~host:metadata.host
               ~journal_flush:metadata.journal_flush
               ~available_features:[]
             |> ok)
      }
    in
    assert (
      Result.is_error
        (Agent_protocol.Initialize.Response.of_json
           (Agent_protocol.Initialize.Response.to_json forged)));
    Agent_client.Connection.close reader);
  print_endline
    "embedded durable/synced reported; installed features negotiated; legacy response \
     accepted";
  [%expect
    {| embedded durable/synced reported; installed features negotiated; legacy response accepted |}]
;;
