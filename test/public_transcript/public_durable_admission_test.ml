open! Core
module P = Agent_protocol
module D = P.Public.Durable

let ok = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let session_id = P.Id.Session.of_string "ses_public_admission" |> ok
let foreign_id = P.Id.Session.of_string "ses_foreign_admission" |> ok
let timestamp = P.Timestamp.of_time_ns Time_ns.epoch

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

let session =
  P.Session.
    { id = session_id
    ; creator = None
    ; created_at = timestamp
    ; updated_at = timestamp
    ; generation = 0
    ; spec
    ; desired_state = Running
    ; observed_state = Idle
    ; prompt_revision = None
    ; workspace_instance = None
    ; active_operation = None
    ; revision = 1L
    ; metadata_revision = 0L
    ; organization = Agent_protocol.Session_organization.Values.empty
    ; latest_event_sequence = 1L
    ; inference_summary = History_entry.Payload.Presence.Absent
    }
;;

let event payload =
  P.Event.Durable.of_payload ~session_id ~sequence:1L ~revision:1L ~timestamp payload
;;

let public payload statuses =
  let shared = D.Shared_payload.of_internal payload |> ok in
  D.of_internal_envelope
    (event payload)
    ~body:(Full (Shared shared))
    ~extension_status:statuses
    ~replacement_snapshot:None
;;

let status generation =
  P.Extension_status.of_json
    (`Object
        [ "version", `Number "1"
        ; "kind", `String "invocation"
        ; "id", `String "inv_public_admission"
        ; "generation", `Number (Int.to_string generation)
        ; "state", `String "admitted"
        ])
  |> ok
;;

let with_status event statuses =
  P.Event.Durable.with_extension_status event statuses |> P.Event.Durable.to_json
;;

let%expect_test "durable payload ownership is checked on native and wire admission" =
  let foreign =
    P.Event.Durable.Payload.Session_updated { session with id = foreign_id }
  in
  print_s
    [%sexp
      (Result.is_error (public foreign None) : bool)
    , (Result.is_error (D.of_json (P.Event.Durable.to_json (event foreign))) : bool)];
  let accepted = public (Session_updated session) None |> ok in
  print_s
    [%sexp
      (P.Id.Session.equal accepted.session_id session_id : bool)
    , (Result.is_ok (D.of_json (D.to_json accepted)) : bool)];
  [%expect
    {|
    (true true)
    (true true)
    |}]
;;

let%expect_test "extension summaries reject future generations and duplicate identities" =
  let payload = P.Event.Durable.Payload.Session_updated session in
  let future = status 1 in
  let current = status 0 in
  print_s
    [%sexp
      (Result.is_error (public payload (Some [ future ])) : bool)
    , (Result.is_error (D.of_json (with_status (event payload) [ future ])) : bool)];
  print_s
    [%sexp
      (Result.is_error (public payload (Some [ current; current ])) : bool)
    , (Result.is_error (D.of_json (with_status (event payload) [ current; current ]))
       : bool)];
  let accepted = public payload (Some [ current ]) |> ok in
  print_s [%sexp (Result.is_ok (D.of_json (D.to_json accepted)) : bool)];
  [%expect
    {|
    (true true)
    (true true)
    true
    |}]
;;

let snapshot =
  let window =
    P.Public.History.Window.
      { entries = []
      ; previous_cursor = None
      ; next_cursor = None
      ; reached_start = true
      ; reached_end = true
      ; structurally_complete = true
      }
  in
  P.Public.Snapshot.create
    { session
    ; lifecycle = None
    ; canonical_history = window
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
    ; revision = 1L
    ; latest_event_sequence = 1L
    }
  |> ok
;;

let%expect_test "whole public results validate native children anchors and outer bounds" =
  let bad_session = { session with generation = -1 } in
  let mutation = P.Mutation_result.{ revision = 1L; latest_event_sequence = 1L } in
  let malformed_create =
    P.Public.Result.Session_create { session = bad_session; mutation; attachment = None }
  in
  let malformed_shared =
    P.Public.Result.Non_history.of_internal
      (P.Method_result.Session_start { session = bad_session; mutation })
    |> ok
    |> fun value -> P.Public.Result.Non_history value
  in
  let mismatched_attach =
    P.Public.Result.Session_attach
      { attachment =
          { id = P.Id.Attachment.of_string "att_public_admission" |> ok
          ; session_id
          ; mode = Read_only
          ; owner_lease = None
          }
      ; replay = Snapshot snapshot
      ; latest_event_sequence = 2L
      ; reclaim_token = None
      }
  in
  print_s
    [%sexp
      (Result.is_error (P.Public.Result.validate malformed_create) : bool)
    , (Result.is_error (P.Public.Result.validate malformed_shared) : bool)
    , (Result.is_error (P.Public.Result.validate mismatched_attach) : bool)];
  let valid = P.Public.Result.Session_create { session; mutation; attachment = None } in
  let oversized =
    match P.Public.Result.to_json valid with
    | `Object fields ->
      `Object (("future", `String (String.make (16 * 1024 * 1024) 'x')) :: fields)
    | `Array _ | `String _ | `Number _ | `True | `False | `Null -> assert false
  in
  print_s
    [%sexp
      (Result.is_ok (P.Public.Result.validate valid) : bool)
    , (Result.is_error (P.Public.Result.of_json ~method_:"session.create" oversized)
       : bool)];
  [%expect
    {|
    (true true true)
    (true true)
    |}]
;;

let%expect_test "snapshot decode preserves constructor checks and normalized bounds" =
  let module S = P.Public.Snapshot in
  let fields = S.fields snapshot in
  let rejected = function
    | Error { P.Error.code = Invalid_request; _ } -> true
    | Error _ | Ok _ -> false
  in
  let bad_session = { fields with session = { session with generation = -1 } } in
  let wire_with name value =
    match S.to_json snapshot with
    | `Object values ->
      `Object ((name, value) :: List.Assoc.remove values name ~equal:String.equal)
    | _ -> assert false
  in
  let invalid_session_wire =
    wire_with "session" (P.Session.to_json bad_session.session)
  in
  let future = { fields with extension_status = [ status 1 ] } in
  let future_wire =
    wire_with "extension_status" (`Array [ P.Extension_status.to_json (status 1) ])
  in
  let mismatched = { fields with revision = 2L } in
  let missing_defaults =
    match S.to_json snapshot with
    | `Object values ->
      `Object
        (List.filter values ~f:(fun (name, _) ->
           not
             (List.mem
                [ "archived_revisions"; "extension_status" ]
                name
                ~equal:String.equal)))
    | _ -> assert false
  in
  let normalized = S.of_json missing_defaults |> ok in
  let oversized_normalization =
    match missing_defaults with
    | `Object values ->
      let empty = `Object (("halt_reason", `String "") :: values) in
      let padding = (16 * 1024 * 1024) - String.length (Jsonaf.to_string empty) in
      `Object (("halt_reason", `String (String.make padding 'x')) :: values)
    | _ -> assert false
  in
  print_s
    [%sexp
      (rejected (S.create bad_session) && rejected (S.of_json invalid_session_wire)
       : bool)
    , (rejected (S.create future) && rejected (S.of_json future_wire) : bool)
    , (rejected (S.create mismatched)
       && rejected (S.of_json (wire_with "revision" (`Number "2")))
       : bool)
    , (Jsonaf.exactly_equal (S.to_json normalized) (S.to_json snapshot) : bool)
    , (Result.is_ok
         (Document_schema.Json.validate
            ~limits:Transcript.Admission.default
            oversized_normalization)
       : bool)
    , (rejected (S.of_json oversized_normalization) : bool)];
  [%expect {| (true true true true true true) |}]
;;
