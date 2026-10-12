open! Core
module P = Agent_protocol
module Public = P.Public

let ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let session_id = P.Id.Session.of_string "ses_completion_view" |> ok
let operation_id = P.Id.Operation.of_string "op_completion_view" |> ok
let principal_id = P.Id.Principal.of_string "pri_completion_view" |> ok
let attachment_id = P.Id.Attachment.of_string "att_completion_view" |> ok
let timestamp = P.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok
let after = P.Mutation_result.{ revision = 1L; latest_event_sequence = 1L }

let operation state =
  P.Operation.
    { id = operation_id
    ; generation = 0
    ; kind = Turn Administrative
    ; state
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let spec =
  P.Session.Spec.create
    ~execution_host:Embedded
    ~prompt:(Local_path "/prompt.chatmd")
    ~workspace:Current
    ~liveness:Process_bound
    ~persistence:Transient
    ~start_immediately:false
    ~labels:[]
    ()
  |> ok
;;

let snapshot ?(id = session_id) sequence =
  let session =
    P.Session.
      { id
      ; creator = Some principal_id
      ; created_at = timestamp
      ; updated_at = timestamp
      ; generation = 0
      ; spec
      ; desired_state = Running
      ; observed_state = Idle
      ; prompt_revision = None
      ; workspace_instance = None
      ; active_operation = None
      ; revision = sequence
      ; metadata_revision = 0L
      ; organization = P.Session_organization.Values.empty
      ; latest_event_sequence = sequence
      ; inference_summary = History_entry.Payload.Presence.Absent
      }
  in
  Public.Snapshot.create
    { session
    ; lifecycle = None
    ; canonical_history =
        { entries = []
        ; previous_cursor = None
        ; next_cursor = None
        ; reached_start = true
        ; reached_end = true
        ; structurally_complete = true
        }
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
    ; revision = sequence
    ; latest_event_sequence = sequence
    }
  |> ok
;;

let foreign_projection id = snapshot ~id 0L |> Agent_client.Projection.install_snapshot

type t =
  { handle : Agent_client.Session_handle.t
  ; notifications : P.Envelope.t option Eio.Stream.t
  ; detached : int ref
  }

let handle t = t.handle
let detach_count t = !(t.detached)

let with_handle ~on_update f =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let notifications = Eio.Stream.create 8 in
      let detached = ref 0 in
      let initial = snapshot 0L in
      let request = function
        | P.Command.Session_attach _ ->
          Ok
            (Public.Result.Session_attach
               { attachment =
                   { id = attachment_id
                   ; session_id
                   ; mode = Read_write
                   ; owner_lease = None
                   }
               ; replay = Snapshot initial
               ; latest_event_sequence = 0L
               ; reclaim_token = None
               })
        | Session_detach _ ->
          incr detached;
          P.Method_result.Session_detach { revision = 0L; latest_event_sequence = 0L }
          |> Public.Result.Non_history.of_internal
          |> ok
          |> fun result -> Ok (Public.Result.Non_history result)
        | _ -> Error (P.Error.invalid_request "unexpected completion fixture command")
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
          ~on_update
          ()
        |> ok
      in
      Exn.protect
        ~finally:(fun () ->
          Agent_client.Session_handle.close handle;
          Agent_client.Connection.close connection)
        ~f:(fun () -> f { handle; notifications; detached })))
;;

let notify t event =
  Public.Durable.to_json event
  |> fun params ->
  Eio.Stream.add
    t.notifications
    (Some (P.Envelope.Notification { method_ = "session.event"; params }))
;;

let event ~sequence ~replacement_snapshot payload =
  let internal =
    P.Event.Durable.of_payload ~session_id ~sequence ~revision:sequence ~timestamp payload
  in
  Public.Durable.of_internal_envelope
    internal
    ~body:(Full (Shared (Public.Durable.Shared_payload.of_internal payload |> ok)))
    ~extension_status:None
    ~replacement_snapshot
  |> ok
;;

let emit t ~sequence payload =
  event ~sequence ~replacement_snapshot:None payload |> notify t
;;

let replace_idle t ~sequence =
  let replacement = snapshot sequence in
  event
    ~sequence
    ~replacement_snapshot:(Some replacement)
    (Session_updated (Public.Snapshot.fields replacement).session)
  |> notify t
;;

let fail t failure =
  Eio.Stream.add
    t.notifications
    (Some
       (P.Stream_error.create ~session_id ~attachment_id failure
        |> P.Stream_error.to_notification))
;;

let end_transport t = Eio.Stream.add t.notifications None
