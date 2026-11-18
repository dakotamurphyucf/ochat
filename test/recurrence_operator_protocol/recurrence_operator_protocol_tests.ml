open! Core
module P = Agent_protocol
module Q = P.Recurrence_query
module C = P.Recurrence_control

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let id bytes = P.Id.Generator.create ~bytes:(fun length -> String.make length bytes)
let original = id 'a'
let alternate = id 'b'
let server_id = P.Id.Server.create_with original

let session ids =
  P.Session_ref.create ~server_id ~session_id:(P.Id.Session.create_with ids)
;;

let controller = session original
let recurrence = P.Id.Recurrence.create_with original
let principal = P.Id.Principal.create_with original
let key = P.Idempotency_key.of_string "ordinary-recurrence-edit" |> get
let at = P.Timestamp.of_string "2026-11-17T19:00:00Z" |> get
let lifecycle = P.Session_lifecycle.Revision.one

let source =
  P.Run_source.create
    ~observer:{ script_id = "main"; source_sha256 = String.make 64 'a' }
    ~generation:2
    ~installation_epoch:4L
  |> get
;;

let schedule =
  let module Rule = Recurrence_rules.Rule in
  let period =
    Rule.Period.create_ms 60_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let rule = Rule.Interval.create ~mode:Fixed_rate ~period in
  P.Recurrence_schedule.Interval { rule; first_due = P.Timestamp.to_time_ns at }
;;

let edit ~input ~expected_control_revision =
  let input : P.Session.Message_content.t =
    { kind = Plain_text; text = input; attachments = [] }
  in
  let capture =
    C.Capture.create
      ~schedule
      ~policy:P.Recurrence_policy.default
      ~selector:(Existing { session = session alternate; input })
      ()
    |> get
  in
  C.Intent.create
    ~generation:2
    ~expected_catalog_revision:9L
    ~key
    (Edit
       { id = recurrence
       ; expected_control_revision
       ; basis = { source; expected_lifecycle_revision = lifecycle }
       ; capture
       })
  |> get
;;

let%expect_test "exact recurrence replay retains the selector and old control fence" =
  let intent = edit ~input:"Run the check" ~expected_control_revision:5L in
  let digest intent = C.Intent.digest intent ~session:controller |> get in
  let round_trip ids =
    let attachment_id = P.Id.Attachment.create_with ids in
    let request = C.Request.create ~session:controller ~attachment_id intent |> get in
    let method_ = C.Request.method_name request in
    let params = C.Request.params request in
    match P.Command.of_method_and_params ~method_ ~params |> get with
    | Recurrence_control decoded ->
      P.Id.Attachment.equal decoded.attachment_id attachment_id
      && String.equal (digest decoded.intent) (digest intent)
    | _ -> false
  in
  let same_replay = round_trip original && round_trip alternate in
  let different_input =
    edit ~input:"Run a different check" ~expected_control_revision:5L
  in
  let different_fence = edit ~input:"Run the check" ~expected_control_revision:6L in
  let distinct_input = not (String.equal (digest different_input) (digest intent)) in
  let distinct_fence = not (String.equal (digest different_fence) (digest intent)) in
  print_s [%sexp (same_replay : bool), (distinct_input : bool), (distinct_fence : bool)];
  let accepted intent ~control_revision ~catalog_revision =
    let receipt =
      C.Receipt.create
        ~session:controller
        ~generation:intent.C.Intent.generation
        ~principal_id:principal
        ~key
        ~intent_sha256:(digest intent)
        ~recurrence_id:recurrence
        ~verb:(C.Action.verb intent.action)
        ~definition_revision:1L
        ~control_revision
        ~catalog_revision
        ~accepted_at:at
      |> get
    in
    let method_ = C.Result.method_name receipt in
    let result = C.Result.payload receipt in
    let receipt = C.Result.of_method_and_result ~method_ ~result |> get in
    C.Receipt.check_intent receipt ~session:controller ~principal_id:principal ~intent
    |> Result.is_ok
  in
  let pause ~expected_catalog_revision =
    C.Intent.create
      ~generation:2
      ~expected_catalog_revision
      ~key
      (Pause { id = recurrence; expected_control_revision = 5L })
    |> get
  in
  let exact = accepted intent ~control_revision:6L ~catalog_revision:10L in
  let wrong_control = accepted intent ~control_revision:5L ~catalog_revision:10L in
  let wrong_catalog = accepted intent ~control_revision:6L ~catalog_revision:11L in
  let no_op =
    accepted
      (pause ~expected_catalog_revision:9L)
      ~control_revision:5L
      ~catalog_revision:10L
  in
  let at_limit =
    accepted
      (pause ~expected_catalog_revision:Int64.max_value)
      ~control_revision:5L
      ~catalog_revision:Int64.max_value
  in
  print_s
    [%sexp
      (exact : bool)
    , (wrong_control : bool)
    , (wrong_catalog : bool)
    , (no_op : bool)
    , (at_limit : bool)];
  [%expect
    {|
    (true true true)
    (true false false true false)
    |}]
;;

let basis : Q.Basis.t =
  { session = controller
  ; generation = 2
  ; state_revision = 10L
  ; catalog_revision = 9L
  ; lifecycle_revision = lifecycle
  ; automatic = true
  ; source = Some source
  ; availability = Active
  }
;;

let definition : Q.Definition.t =
  { id = recurrence
  ; controller
  ; definition_revision = 1L
  ; control_revision = 5L
  ; control = Enabled
  ; source
  ; birth_lifecycle_revision = lifecycle
  ; creator = principal
  ; created_at = at
  ; schedule
  ; target_kind = Existing_session
  ; policy = P.Recurrence_policy.default
  ; actions = [ Pause ]
  }
;;

let nominal revision ~slot : Q.Nominal.t =
  { occurrence =
      { definition = { controller; recurrence_id = recurrence; revision }; slot }
  ; at
  }
;;

let denied_attempt : Q.Attempt.t =
  { number = 3
  ; rejection = None
  ; uncertainty = None
  ; retry_resolution_recorded = false
  ; admission_recorded = true
  ; terminal_recorded = true
  ; target = Denied
  }
;;

let status : Q.Status.t =
  { entry = definition
  ; planning = Available
  ; planning_revision = 12L
  ; pending_count = 1
  ; unresolved_count = 0
  ; issue = None
  ; totals = { consumed = 2L; omitted = 0L; withdrawn = 0L; settled = 1L }
  ; ready = [ nominal 1L ~slot:1L ]
  ; lookahead = None
  ; alarm_due_at = None
  ; protected_creations = []
  ; occurrences = [ { nominal = nominal 0L ~slot:0L; attempts = [ denied_attempt ] } ]
  }
;;

let print_code = function
  | Ok _ -> print_endline "ok"
  | Error (error : P.Error.t) -> print_endline (P.Error.code_to_string error.code)
;;

let%expect_test "degraded recurrence can retain history after current authority changes" =
  let retired = { definition with control = Retired Lifecycle_changed; actions = [] } in
  let status =
    { status with entry = retired; planning = Unavailable Controller_not_loaded }
  in
  let basis =
    { basis with
      generation = 3
    ; lifecycle_revision = P.Session_lifecycle.Revision.of_int64 2L |> get
    ; source = None
    ; automatic = false
    ; availability = Stopped
    }
  in
  let decode status ~can_create =
    let result = Q.Result.Status (Available { basis; can_create; value = status }) in
    Q.Result.of_method_and_result
      ~method_:(Q.Result.method_name result)
      ~result:(Q.Result.payload result)
  in
  decode status ~can_create:false |> print_code;
  decode status ~can_create:true |> print_code;
  decode { status with pending_count = 0 } ~can_create:false |> print_code;
  let no_admission = { denied_attempt with admission_recorded = false } in
  let occurrences =
    [ { Q.Occurrence.nominal = nominal 0L ~slot:0L; attempts = [ no_admission ] } ]
  in
  decode { status with occurrences } ~can_create:false |> print_code;
  [%expect
    {|
    ok
    invalid_request
    invalid_request
    invalid_request
    |}]
;;
