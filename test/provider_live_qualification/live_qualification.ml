open! Core
module O = Inference.Observation
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module S = Private_storage
module Secret = Provider_secret_store
module Actor = Operator_authorization
module Driver = Openai.Responses_driver
module B = Inference_host.Credential_bridge
module Admin = Provider_operator.Profile_admin
module Runtime = Provider_runtime
module D = Agent_server.Daemon
module A = Agent_session.Session_actor
module Ledger = Agent_session.Inference_ledger
module Profile_policy = Provider_runtime_host.Profile_policy

exception Qualification_failure of string

let checked_named ~stage result =
  match result with
  | Ok value -> value
  | Error _ -> raise (Qualification_failure (stage ^ "_rejected"))
;;

let checked_protocol ~stage = function
  | Ok value -> value
  | Error (error : P.Error.t) ->
    raise (Qualification_failure (stage ^ "_" ^ P.Error.code_to_string error.code))
;;

let checked_operator ~stage = function
  | Ok value -> value
  | Error error ->
    let code =
      Jsonaf.to_string (DTO.Error.to_json error) |> String.strip ~drop:(Char.equal '"')
    in
    raise (Qualification_failure (stage ^ "_" ^ code))
;;

let id value = M.Id.create value |> checked_named ~stage:"boundary_0040"
let revision value = DTO.Revision.of_string value |> checked_named ~stage:"boundary_0041"
let key value = P.Idempotency_key.of_string value |> checked_named ~stage:"boundary_0042"

let profile_id =
  DTO.Profile_id.of_string "live-qualified-route" |> checked_named ~stage:"boundary_0043"
;;

module Key_input = struct
  type t =
    | Private_file of string
    | Environment of string

  let private_file value =
    if
      Filename.is_absolute value
      && (not (String.equal (Filename.dirname value) "/"))
      && String.length value <= 4096
      && not (String.mem value '\000')
    then Ok (Private_file value)
    else Error "invalid private input path"
  ;;

  let environment value =
    if
      String.length value > 0
      && String.length value <= 64
      && (Char.is_alpha value.[0] || Char.equal value.[0] '_')
      && String.for_all value ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')
    then Ok (Environment value)
    else Error "invalid explicit environment reference"
  ;;

  let reference = function
    | Private_file _ -> "explicit-private-local-file"
    | Environment name -> name
  ;;

  let secret bytes =
    Secret.Secret.of_bytes (Bytes.of_string (String.strip (Bytes.to_string bytes)))
    |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential)
  ;;

  let read t ~env ~sw =
    match t with
    | Environment name ->
      (* This is the only environment lookup; caller invokes it only inside the
         actual authorized enrollment callback. No fallback or temp artifact. *)
      (match Sys.getenv name with
       | Some value when String.length value <= 16384 -> secret (Bytes.of_string value)
       | Some _ | None -> Error B.Error.Invalid_credential)
    | Private_file file ->
      let parent = Filename.dirname file in
      S.Directory.open_or_create
        ~sw
        ~anchor:Eio.Path.(Eio.Stdenv.fs env / Filename.dirname parent)
        ~components:
          [ S.Name.create (Filename.basename parent)
            |> checked_named ~stage:"boundary_0093"
          ]
      |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential)
      |> Result.bind ~f:(fun directory ->
        S.Directory.read_bounded
          directory
          (S.Name.create (Filename.basename file) |> checked_named ~stage:"boundary_0098")
          ~max_bytes:16384
        |> Result.map_error ~f:(fun _ -> B.Error.Invalid_credential)
        |> Result.bind ~f:secret)
  ;;
end

module Browser_presentation = struct
  type t =
    | Private_terminal
    | Launch_local
  [@@deriving equal]
end

module Plan = struct
  type auth =
    | Api
    | Browser
    | Device
  [@@deriving equal]

  type transport =
    | Sse
    | Require_websocket
  [@@deriving equal]

  type phase =
    | Journey
    | Renew
    | Logout
    | Feature
  [@@deriving equal]

  type t =
    { auth : auth
    ; transport : transport
    ; model : string
    ; account_alias : string
    ; max_attempts : int
    ; maximum_phase : Time_ns.Span.t
    ; settings : Driver.Setting.t list
    ; feature : Feature_case.Case.t option
    }

  (* Only bounded numeric controls are declaration inputs. Arbitrary provider
     strings/objects cannot enter the persisted plan or evidence document. *)
  let valid_setting setting =
    let name = Driver.Setting.name setting in
    List.mem [ "max_output_tokens"; "temperature"; "top_p" ] name ~equal:String.equal
    &&
    match Driver.Setting.value setting with
    | Openai.Responses_codec.Request.Field.Absent | Null -> true
    | Value (`Number number) ->
      (match name with
       | "max_output_tokens" ->
         (match Int.of_string_opt number with
          | Some n -> n >= 1 && n <= 4096
          | None -> false)
       | "temperature" | "top_p" ->
         (match Float.of_string_opt number with
          | Some value ->
            Float.is_finite value
            && Float.(
                 value >= 0.
                 && value <= if String.equal name "temperature" then 2. else 1.)
          | None -> false)
       | _ -> false)
    | Value (`Object _ | `Array _ | `String _ | `True | `False | `Null) -> false
  ;;

  let create
        ?feature
        ~auth
        ~transport
        ~model
        ~account_alias
        ~max_attempts
        ~maximum_phase
        ~settings
        ()
    =
    if
      String.is_empty model
      || String.length model > 128
      || (not
            (String.for_all model ~f:(fun c ->
               Char.is_alphanum c || List.mem [ '-'; '_'; '.' ] c ~equal:Char.equal)))
      || String.is_empty account_alias
      || String.length account_alias > 64
      || (not
            (String.for_all account_alias ~f:(fun c ->
               Char.is_alphanum c || Char.equal c '-')))
      || max_attempts < 1
      || max_attempts > 16
      || (Option.is_some feature && max_attempts <> 1)
      || Time_ns.Span.(maximum_phase <= zero || maximum_phase > of_sec 900.)
      || List.length settings > 16
      || (not (List.for_all settings ~f:valid_setting))
      || Option.is_some
           (List.find_a_dup
              (List.map settings ~f:Driver.Setting.name)
              ~compare:String.compare)
    then Error "invalid bounded qualification plan"
    else
      let open Result.Let_syntax in
      let%bind feature_settings =
        match feature with
        | None -> Ok []
        | Some case ->
          Feature_case.input case
          |> Result.map_error ~f:(fun _ -> "invalid fixed feature fixture")
          |> Result.bind ~f:(fun input ->
            Feature_case.Input.settings input
            |> List.map ~f:(fun setting ->
              let value =
                match Inference.Request.Setting.value setting with
                | Absent -> Openai.Responses_codec.Request.Field.Absent
                | Null -> Null
                | Value value -> Value value
              in
              Driver.Setting.create
                ~name:(Inference.Request.Setting.name setting)
                ~value
                ~provenance:Profile_default
              |> Result.map_error ~f:(fun _ -> "invalid fixed feature setting"))
            |> Result.all)
      in
      Ok
        { auth
        ; transport
        ; model
        ; account_alias
        ; max_attempts
        ; maximum_phase
        ; settings = settings @ feature_settings
        ; feature
        }
  ;;

  let auth_name = function
    | Api -> "api"
    | Browser -> "codex-browser"
    | Device -> "codex-device"
  ;;

  let transport_name = function
    | Sse -> "sse"
    | Require_websocket -> "require-websocket"
  ;;

  let document t =
    `Object
      [ "version", `Number "1"
      ; "auth", `String (auth_name t.auth)
      ; "transport", `String (transport_name t.transport)
      ; "model", `String t.model
      ; "account_alias", `String t.account_alias
      ; "max_attempts", `Number (Int.to_string t.max_attempts)
      ; ( "settings"
        , `Array
            (List.map t.settings ~f:(fun setting ->
               let value =
                 match Driver.Setting.value setting with
                 | Openai.Responses_codec.Request.Field.Absent -> `Object []
                 | Null -> `Null
                 | Value value -> value
               in
               `Object [ "name", `String (Driver.Setting.name setting); "value", value ]))
        )
      ]
    |> fun document ->
    match t.feature, document with
    | None, _ -> document
    | Some case, `Object fields ->
      `Object
        (fields
         @ [ "feature_fixture_version", `Number "1"
           ; "feature", `String (Feature_case.Case.name case)
           ])
    | Some _, _ -> assert false
  ;;
end

let browser_selection_valid ~auth ~phase presentation =
  Browser_presentation.equal presentation Private_terminal
  || (Plan.equal_auth auth Browser
      && (Plan.equal_phase phase Journey || Plan.equal_phase phase Feature))
;;

module Failure = struct
  module T = Inference.Event.Terminal

  type t =
    | Preparation of Inference_runtime.Preparation_error.t
    | Terminal of
        { outcome : T.outcome
        ; delivery : T.delivery
        }

  let is_unsupported = function
    | Preparation (Unsupported_input | Unsupported_setting)
    | Terminal { outcome = Failed (Transport Unsupported_transport); _ } -> true
    | Preparation
        ( Invalid_request _
        | Target_mismatch
        | Target_unavailable
        | Target_denied
        | Reauthorization_required
        | Incompatible_replay
        | Asset_unavailable
        | Transport_unavailable
        | Session_closed
        | Invalid_preparation
        | Request_limit _ )
    | Terminal _ -> false
  ;;

  let preparation_code = function
    | Inference_runtime.Preparation_error.Invalid_request _ -> "invalid_request"
    | Target_mismatch -> "target_mismatch"
    | Target_unavailable -> "target_unavailable"
    | Target_denied -> "target_denied"
    | Reauthorization_required -> "reauthorization_required"
    | Unsupported_input -> "unsupported_input"
    | Unsupported_setting -> "unsupported_setting"
    | Incompatible_replay -> "incompatible_replay"
    | Asset_unavailable -> "asset_unavailable"
    | Transport_unavailable -> "transport_unavailable"
    | Session_closed -> "session_closed"
    | Invalid_preparation -> "invalid_preparation"
    | Request_limit _ -> "request_limit"
  ;;

  let auth_code = function
    | T.Missing -> "missing"
    | Denied -> "denied"
    | Profile_changed -> "profile_changed"
    | Reauthorization_required -> "reauthorization_required"
    | Invalid_credential -> "invalid_credential"
    | Timed_out -> "timed_out"
  ;;

  let transport_code = function
    | T.Connection -> "connection"
    | Timeout -> "timeout"
    | Invalid_http -> "invalid_http"
    | Invalid_content_type -> "invalid_content_type"
    | Body_limit -> "body_limit"
    | Framing_limit -> "framing_limit"
    | Protocol -> "protocol"
    | Unsupported_transport -> "unsupported_transport"
    | Session_closed -> "session_closed"
    | Session_busy -> "session_busy"
    | Http_status _ -> "http_status"
  ;;

  let provider_code = function
    | T.Provider_failure.Invalid_request -> "invalid_request"
    | Denied -> "denied"
    | Rate_limited -> "rate_limited"
    | Unavailable -> "unavailable"
    | Unknown -> "unknown"
  ;;

  let to_json = function
    | Preparation error ->
      `Object
        [ "category", `String "preparation"; "reason", `String (preparation_code error) ]
    | Terminal { outcome; delivery } ->
      let category, reason, http_status =
        match outcome with
        | Completed -> "completed", "completed", None
        | Refused -> "refused", "refused", None
        | Incomplete why ->
          ( "incomplete"
          , (match why with
             | Output_limit -> "output_limit"
             | Filtered -> "filtered"
             | Other -> "other"
             | Unavailable -> "unavailable")
          , None )
        | Failed (Authentication why) -> "authentication", auth_code why, None
        | Failed (Transport why) ->
          ( "transport"
          , transport_code why
          , (match why with
             | Http_status status -> Some status
             | Connection
             | Timeout
             | Invalid_http
             | Invalid_content_type
             | Body_limit
             | Framing_limit
             | Protocol
             | Unsupported_transport
             | Session_closed
             | Session_busy -> None) )
        | Failed (Provider why) -> "provider", provider_code why, None
      in
      `Object
        [ "category", `String category
        ; "reason", `String reason
        ; ( "delivery"
          , `String
              (match delivery with
               | Definitely_not_submitted -> "definitely_not_submitted"
               | Possibly_submitted -> "possibly_submitted"
               | Response_started -> "response_started") )
        ; ( "http_status"
          , Option.value_map http_status ~default:`Null ~f:(fun status ->
              `Number (Int.to_string status)) )
        ; "denial_scope", `String "unspecified"
        ]
  ;;
end

let oauth_failure_json error =
  let stage =
    match Provider_oauth.Error.stage error with
    | Configure -> "configure"
    | Listen -> "listen"
    | Callback -> "callback"
    | Device_challenge -> "device_challenge"
    | Device_poll -> "device_poll"
    | Exchange -> "exchange"
    | Identity -> "identity"
  in
  let code =
    match Provider_oauth.Error.code error with
    | Invalid_configuration -> "invalid_configuration"
    | Unsupported_route -> "unsupported_route"
    | Closed -> "closed"
    | Timed_out -> "timed_out"
    | Invalid_callback -> "invalid_callback"
    | State_mismatch -> "state_mismatch"
    | Authorization_denied -> "authorization_denied"
    | Protocol_error -> "protocol_error"
    | Response_too_large -> "response_too_large"
    | Transport_unavailable -> "transport_unavailable"
    | Identity_mismatch -> "identity_mismatch"
    | Identity_unverifiable -> "identity_unverifiable"
    | Submission_uncertain -> "submission_uncertain"
  in
  let identity_failure =
    Option.value_map
      (Provider_oauth.Error.identity_failure error)
      ~default:`Null
      ~f:(fun cause ->
        `String
          (match cause with
           | Invalid_token -> "invalid_token"
           | Issuer -> "issuer"
           | Audience -> "audience"
           | Account -> "account"
           | Subject -> "subject"
           | Nonce -> "nonce"
           | Expiry -> "expiry"
           | Scopes -> "scopes"))
  in
  `Object
    [ "stage", `String stage; "code", `String code; "identity_failure", identity_failure ]
;;

module Evidence = struct
  type status =
    | Incomplete
    | Live_attempted
    | Live_pass
    | Unsupported

  type t =
    { status : status
    ; plan : Plan.t
    ; attempts : int
    ; turns : int
    ; tool_effects : int
    ; restored : bool
    ; identity_preserved : bool
    ; renewal : bool
    ; logout : bool
    ; phase : Plan.phase
    ; recorded_at_ms : int64
    ; configuration_proven : bool
    ; transport_proven : bool
    ; renewal_expires_at_ms : int64 option
    ; renewal_not_before_ms : int64 option
    ; renewal_ws_continuity_proven : bool
    ; cancelled_owner_flow_proven : bool
    ; failure : Failure.t option
    ; oauth_failure : Provider_oauth.Error.t option
    ; protocol_violation : O.Diagnostic.Protocol_violation.t option
    ; reason : string
    ; feature_evidence : Feature_case.Evidence.t option
    ; feature_failure : Feature_case.Error.t option
    }

  let to_json t =
    `Object
      [ "schema_version", `Number "1"
      ; ( "status"
        , `String
            (match t.status with
             | Incomplete -> "incomplete"
             | Live_attempted -> "live_attempted"
             | Live_pass -> "live_pass"
             | Unsupported -> "unsupported") )
      ; "profile_alias", `String t.plan.account_alias
      ; "auth", `String (Plan.auth_name t.plan.auth)
      ; "model", `String t.plan.model
      ; "transport", `String (Plan.transport_name t.plan.transport)
      ; ( "protocol_violation"
        , Option.value_map
            t.protocol_violation
            ~default:`Null
            ~f:O.Diagnostic.Protocol_violation.to_json )
      ; "actual_attempts", `Number (Int.to_string t.attempts)
      ; "completed_turns", `Number (Int.to_string t.turns)
      ; "tool_effects", `Number (Int.to_string t.tool_effects)
      ; ("history_restored", if t.restored then `True else `False)
      ; ("identity_preserved", if t.identity_preserved then `True else `False)
      ; ("renewal_proven", if t.renewal then `True else `False)
      ; ("logout_proven", if t.logout then `True else `False)
      ; ( "phase"
        , `String
            (match t.phase with
             | Journey -> "journey"
             | Renew -> "renew"
             | Logout -> "logout"
             | Feature -> "feature") )
      ; "recorded_at_utc_ms", `Number (Int64.to_string t.recorded_at_ms)
      ; ("configuration_proven", if t.configuration_proven then `True else `False)
      ; ("actual_transport_proven", if t.transport_proven then `True else `False)
      ; ( "renewal_expires_at_utc_ms"
        , Option.value_map t.renewal_expires_at_ms ~default:`Null ~f:(fun n ->
            `Number (Int64.to_string n)) )
      ; ( "renewal_not_before_utc_ms"
        , Option.value_map t.renewal_not_before_ms ~default:`Null ~f:(fun n ->
            `Number (Int64.to_string n)) )
      ; ( "renewal_ws_continuity_proven"
        , if t.renewal_ws_continuity_proven then `True else `False )
      ; ( "cancelled_owner_flow_proven"
        , if t.cancelled_owner_flow_proven then `True else `False )
      ; ( "oauth_failure"
        , Option.value_map t.oauth_failure ~default:`Null ~f:oauth_failure_json )
      ; "observed_failure", Option.value_map t.failure ~default:`Null ~f:Failure.to_json
      ; ( "feature"
        , Option.value_map t.plan.feature ~default:`Null ~f:(fun case ->
            `String (Feature_case.Case.name case)) )
      ; ( "feature_evidence"
        , Option.value_map
            t.feature_evidence
            ~default:`Null
            ~f:Feature_case.Evidence.to_json )
      ; ( "feature_failure"
        , Option.value_map t.feature_failure ~default:`Null ~f:(fun error ->
            `String (Sexp.to_string (Feature_case.Error.sexp_of_t error))) )
      ; "reason", `String t.reason
      ]
  ;;

  let write t path =
    Eio.Path.save ~create:(`Or_truncate 0o600) path (Jsonaf.to_string (to_json t) ^ "\n")
  ;;
end

let budget_check ~maximum ledger ~accounting_id =
  let summary = Ledger.summary ledger in
  let coverage = P.Inference_query.Summary.coverage summary in
  if
    coverage.before_tracking_unknown
    || Int64.(coverage.untracked_attempts <> 0L)
    || Int64.(coverage.retired_attempts <> 0L)
  then raise (Qualification_failure "budget_tracking_unavailable");
  let row =
    List.find (Ledger.rows ledger) ~f:(fun row ->
      Inference.Observation.Observation_id.equal
        (Ledger.Handle.accounting_id (Ledger.Row.handle row))
        accounting_id)
  in
  match row with
  | None -> raise (Qualification_failure "budget_tracking_unavailable")
  | Some row ->
    if
      Int64.(Ledger.Handle.ordinal (Ledger.Row.handle row) > of_int maximum)
      || List.length (Ledger.rows ledger) > maximum
    then raise (Qualification_failure "attempt_budget_exhausted")
;;

let budget_self_check () =
  let module O = Inference.Observation in
  let target =
    Inference.Request.Target.create
      ~adapter:"offline-budget"
      ~profile:"offline"
      ~profile_revision:None
      ~account:None
      ~endpoint:"https://offline.invalid"
      ~model:"offline"
      ~settings:[]
      ~limits:Transcript.Admission.default
    |> checked_named ~stage:"boundary_0461"
  in
  let configuration =
    O.Configuration.of_target
      target
      ~preparation_id:"offline"
      ~transport:Http_sse
      ~capabilities:[]
      ~limits:O.Admission.observation
    |> checked_named ~stage:"boundary_0470"
  in
  let document_limits =
    Transcript.Admission.limits ~max_bytes:(1024 * 1024)
    |> checked_named ~stage:"boundary_0472"
  in
  let limits =
    Ledger.Limits.create
      ~max_attempts:32
      ~max_turns:32
      ~max_retained_bytes:(512 * 1024)
      ~document_limits
    |> checked_named ~stage:"boundary_0479"
  in
  let create unknown =
    Ledger.create
      ~session_id:
        (P.Id.Session.of_string "ses_offline_budget"
         |> checked_named ~stage:"boundary_0483")
      ~generation:0
      ~before_tracking_unknown:unknown
      ~limits
    |> checked_named ~stage:"boundary_0487"
  in
  let admit ledger =
    Ledger.admit
      ledger
      ~source:
        (Transcript.Source_id.of_string "offline" |> checked_named ~stage:"boundary_0492")
      ~relation:Root
      ~operation_id:None
      ~invocation_id:None
      ~configuration
    |> checked_named ~stage:"boundary_0497"
  in
  let first, handle, _ = admit (create false) in
  let first =
    Ledger.to_document first
    |> checked_named ~stage:"boundary_0502"
    |> fun document ->
    Ledger.of_document document ~limits |> checked_named ~stage:"boundary_0503"
  in
  budget_check ~maximum:1 first ~accounting_id:(Ledger.Handle.accounting_id handle);
  let first =
    Ledger.set_state
      first
      handle
      (Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted })
    |> checked_named ~stage:"boundary_0511"
  in
  let restored =
    Ledger.with_generation first ~generation:1 |> checked_named ~stage:"boundary_0513"
  in
  let second, next, _ = admit restored in
  let denied ledger handle =
    try
      budget_check ~maximum:1 ledger ~accounting_id:(Ledger.Handle.accounting_id handle);
      false
    with
    | Qualification_failure _ -> true
  in
  assert (denied second next);
  let unknown, unknown_handle, _ = admit (create true) in
  assert (denied unknown unknown_handle);
  assert (Int64.equal (Ledger.Handle.ordinal next) 2L)
;;

let assistant_repeated_marker ~before after =
  List.exists after ~f:(fun (entry : P.History.entry) ->
    (not
       (List.exists before ~f:(fun (prior : P.History.entry) ->
          P.History.Id.equal prior.id entry.id)))
    &&
    match Agent_session.History_codec.of_canonical entry with
    | Error _ -> false
    | Ok entry ->
      let module Payload = History_entry.Payload in
      (match Payload.Semantic.view (Payload.semantic (History_entry.payload entry)) with
       | Message { role = Assistant; form = Output; content; _ } ->
         List.exists content ~f:(function
           | Text { text; _ } ->
             String.is_substring text ~substring:"QUALIFICATION-LOCAL-HISTORY-ONE"
           | Refusal _ | Image _ | Unknown _ -> false)
       | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> false))
;;

let continuity_self_check () =
  let module Payload = History_entry.Payload in
  let entry role sequence =
    let semantic =
      Payload.Semantic.create
        (Message
           { form = (if Payload.Role.equal role Assistant then Output else Input)
           ; role
           ; content =
               [ Text
                   { text = "QUALIFICATION-LOCAL-HISTORY-ONE"
                   ; annotations = []
                   ; logprobs = Absent
                   }
               ]
           ; phase = Absent
           })
        ~metadata:Payload.Metadata.empty
      |> checked_named ~stage:"offline_continuity_semantic"
    in
    History_entry.create_with_id
      ~id:
        (History_entry.Id.create ~namespace:"continuity" ~sequence
         |> checked_named ~stage:"offline_continuity_id")
      (Payload.authored semantic)
    |> Agent_session.History_codec.to_canonical
  in
  let assistant = entry Assistant 0 in
  let user = entry User 1 in
  assert (assistant_repeated_marker ~before:[] [ assistant ]);
  assert (not (assistant_repeated_marker ~before:[ assistant ] [ assistant ]));
  assert (not (assistant_repeated_marker ~before:[] [ user ]))
;;

let resume_selection_valid ~auth ~phase =
  (Plan.equal_auth auth Browser || Plan.equal_auth auth Device)
  && Plan.equal_phase phase Journey
;;

let resume_session_admissible ~checkpoint ~stored_sessions ~registered_sessions =
  (not checkpoint) && stored_sessions = 0 && registered_sessions = 0
;;

let resume_registration_matches
      ~owner
      ~record_owner
      ~phase
      ~operation_result
      ~original_operation
      ~active_revision
  =
  P.Id.Principal.equal owner record_owner
  && DTO.Flow_result.equal_phase phase Completed
  && (match operation_result with
      | M.Operation.Committed -> true
      | Pending | Rejected | Unavailable -> false)
  && Option.equal M.Id.equal active_revision (Some original_operation)
;;

let resume_self_check () =
  assert (resume_selection_valid ~auth:Browser ~phase:Journey);
  assert (resume_selection_valid ~auth:Device ~phase:Journey);
  assert (not (resume_selection_valid ~auth:Api ~phase:Journey));
  assert (not (resume_selection_valid ~auth:Browser ~phase:Feature));
  assert (
    resume_session_admissible ~checkpoint:false ~stored_sessions:0 ~registered_sessions:0);
  assert (
    not
      (resume_session_admissible
         ~checkpoint:true
         ~stored_sessions:0
         ~registered_sessions:0));
  assert (
    not
      (resume_session_admissible
         ~checkpoint:false
         ~stored_sessions:1
         ~registered_sessions:0));
  assert (
    not
      (resume_session_admissible
         ~checkpoint:false
         ~stored_sessions:0
         ~registered_sessions:1));
  let owner =
    P.Id.Principal.of_string "pri_resume_owner"
    |> checked_named ~stage:"offline_resume_owner"
  in
  let foreign =
    P.Id.Principal.of_string "pri_resume_foreign"
    |> checked_named ~stage:"offline_resume_foreign"
  in
  let operation = id "offline-original-enrollment" in
  let accepts ~record_owner ~phase ~operation_result ~active_revision =
    resume_registration_matches
      ~owner
      ~record_owner
      ~phase
      ~operation_result
      ~original_operation:operation
      ~active_revision
  in
  assert (
    accepts
      ~record_owner:owner
      ~phase:Completed
      ~operation_result:Committed
      ~active_revision:(Some operation));
  assert (
    not
      (accepts
         ~record_owner:foreign
         ~phase:Completed
         ~operation_result:Committed
         ~active_revision:(Some operation)));
  List.iter
    [ DTO.Flow_result.Pending; Failed DTO.Error.Network; Cancelled; Interrupted; Expired ]
    ~f:(fun phase ->
      assert (
        not
          (accepts
             ~record_owner:owner
             ~phase
             ~operation_result:Committed
             ~active_revision:(Some operation))));
  List.iter [ M.Operation.Pending; Rejected; Unavailable ] ~f:(fun operation_result ->
    assert (
      not
        (accepts
           ~record_owner:owner
           ~phase:Completed
           ~operation_result
           ~active_revision:(Some operation))));
  List.iter
    [ None; Some (id "offline-replacement") ]
    ~f:(fun active_revision ->
      assert (
        not
          (accepts
             ~record_owner:owner
             ~phase:Completed
             ~operation_result:Committed
             ~active_revision)))
;;

let feature_self_check () =
  List.iter
    (List.filter Feature_case.Case.all ~f:(fun case ->
       not (Feature_case.Case.equal case Function_call)))
    ~f:(fun case ->
      let input =
        Feature_case.input case |> checked_named ~stage:"offline_feature_input"
      in
      let target =
        Inference.Request.Target.create
          ~adapter:"offline-feature"
          ~profile:"offline"
          ~profile_revision:None
          ~account:None
          ~endpoint:"https://offline.invalid"
          ~model:"offline"
          ~settings:(Feature_case.Input.settings input)
          ~limits:Transcript.Admission.default
        |> checked_named ~stage:"offline_feature_target"
      in
      let configuration =
        O.Configuration.of_target
          target
          ~preparation_id:"offline-feature"
          ~transport:Http_sse
          ~capabilities:[]
          ~limits:Transcript.Admission.default
        |> checked_named ~stage:"offline_feature_configuration"
      in
      let answer =
        match case with
        | Feature_case.Case.Json_schema -> {|{"marker":"SCHEMA_OK"}|}
        | Image -> "RED"
        | Reasoning -> "42"
        | Document -> "DOC_OK"
        | Function_call -> raise (Qualification_failure "offline_feature_case")
      in
      let validate answer tool_candidates =
        Feature_case.validate
          case
          ~target
          ~outcome:Completed
          ~assistant_text:[ answer ]
          ~configuration
          ~selected_transport:Http_sse
          ~expected_transport:Http_sse
          ~tool_candidates
      in
      assert (Result.is_ok (validate answer 0));
      assert (Result.is_error (validate "WRONG" 0));
      assert (Result.is_error (validate answer 1));
      match Feature_case.Input.session_content input with
      | None -> assert (Feature_case.Case.equal case Document)
      | Some content ->
        if Feature_case.Case.equal case Image
        then assert (not (String.is_substring content.text ~substring:"RED"));
        if Feature_case.Case.equal case Reasoning
        then assert (not (String.is_substring content.text ~substring:"42")));
  let input =
    Feature_case.input Function_call |> checked_named ~stage:"offline_function_input"
  in
  let target =
    Inference.Request.Target.create
      ~adapter:"offline-feature"
      ~profile:"offline"
      ~profile_revision:None
      ~account:None
      ~endpoint:"https://offline.invalid"
      ~model:"offline"
      ~settings:(Feature_case.Input.settings input)
      ~limits:Transcript.Admission.default
    |> checked_named ~stage:"offline_function_target"
  in
  let configuration =
    O.Configuration.of_target
      target
      ~preparation_id:"offline-function"
      ~transport:Http_sse
      ~capabilities:[]
      ~limits:Transcript.Admission.default
    |> checked_named ~stage:"offline_function_configuration"
  in
  let payload ~name ~call_id ~arguments =
    let module Payload = History_entry.Payload in
    Payload.Semantic.create
      (Call
         { kind = Function
         ; name
         ; namespace = Absent
         ; input_bytes = arguments
         ; async = Absent
         })
      ~metadata:{ Payload.Metadata.empty with call_id = Value call_id }
    |> checked_named ~stage:"offline_function_payload"
    |> Payload.authored
  in
  let valid =
    payload
      ~name:"qualification_echo"
      ~call_id:"offline-call"
      ~arguments:{|{"marker":"FUNCTION_OK"}|}
  in
  let validate candidates tool_candidates =
    Feature_case.validate_function
      ~target
      ~outcome:Completed
      ~configuration
      ~selected_transport:Http_sse
      ~expected_transport:Http_sse
      ~tool_candidates
      ~candidates
  in
  let unknown =
    History_entry.Payload.Semantic.create
      (Unknown { provider_kind = "offline_unknown" })
      ~metadata:History_entry.Payload.Metadata.empty
    |> checked_named ~stage:"offline_unknown_payload"
    |> History_entry.Payload.authored
  in
  assert (Result.is_error (validate [ valid; unknown ] 2));
  let request =
    Feature_case.Input.request
      input
      ~target
      ~history_id:
        (History_entry.Id.create ~namespace:"offline-private-function" ~sequence:0
         |> checked_named ~stage:"offline_function_id")
      ~limits:Document_schema.Limits.default
    |> checked_named ~stage:"offline_function_request"
  in
  assert (Inference.Request.Target.equal target (Inference.Request.target request));
  (match Inference.Request.tools request with
   | [ tool ] ->
     assert (String.equal (Inference.Request.Tool_spec.name tool) "qualification_echo");
     (match Inference.Request.Tool_spec.view tool with
      | Function { parameters = Value parameters; strict = Value true } ->
        assert (
          Jsonaf.exactly_equal
            parameters
            (Jsonaf.of_string
               {|{"type":"object","properties":{"marker":{"type":"string","enum":["FUNCTION_OK"]}},"required":["marker"],"additionalProperties":false}|}))
      | _ -> raise (Qualification_failure "offline_function_schema"))
   | _ -> raise (Qualification_failure "offline_function_tools"));
  let choice =
    List.find_exn (Inference.Request.Target.settings target) ~f:(fun setting ->
      String.equal (Inference.Request.Setting.name setting) "tool_choice")
  in
  (match Inference.Request.Setting.value choice with
   | Value value ->
     assert (
       Jsonaf.exactly_equal
         value
         (Jsonaf.of_string {|{"type":"function","name":"qualification_echo"}|}))
   | Absent | Null -> raise (Qualification_failure "offline_function_choice"));
  assert (Result.is_ok (validate [ valid ] 1));
  assert (Result.is_error (validate [] 0));
  assert (Result.is_error (validate [ valid ] 0));
  assert (Result.is_error (validate [ valid; valid ] 2));
  List.iter
    [ payload
        ~name:"wrong"
        ~call_id:"offline-call"
        ~arguments:{|{"marker":"FUNCTION_OK"}|}
    ; payload
        ~name:"qualification_echo"
        ~call_id:""
        ~arguments:{|{"marker":"FUNCTION_OK"}|}
    ; payload
        ~name:"qualification_echo"
        ~call_id:"offline-call"
        ~arguments:{|{"marker":"WRONG"}|}
    ; payload
        ~name:"qualification_echo"
        ~call_id:"offline-call"
        ~arguments:{|{"marker":"FUNCTION_OK","marker":"FUNCTION_OK"}|}
    ]
    ~f:(fun invalid -> assert (Result.is_error (validate [ invalid ] 1)));
  assert (
    Result.is_error
      (Plan.create
         ~feature:Image
         ~auth:Api
         ~transport:Sse
         ~model:"offline"
         ~account_alias:"synthetic"
         ~max_attempts:2
         ~maximum_phase:(Time_ns.Span.of_sec 60.)
         ~settings:[]
         ()))
;;

let self_check () =
  assert (browser_selection_valid ~auth:Browser ~phase:Journey Launch_local);
  assert (not (browser_selection_valid ~auth:Api ~phase:Journey Launch_local));
  assert (not (browser_selection_valid ~auth:Device ~phase:Feature Launch_local));
  assert (not (browser_selection_valid ~auth:Browser ~phase:Renew Launch_local));
  budget_self_check ();
  continuity_self_check ();
  resume_self_check ();
  feature_self_check ();
  assert (Result.is_ok (Key_input.environment "OPENAI_KEY"));
  assert (Result.is_error (Key_input.environment "OPENAI_KEY\000"));
  assert (Result.is_error (Key_input.environment "HOME/other"));
  let plan =
    Plan.create
      ~auth:Api
      ~transport:Sse
      ~model:"selected-model"
      ~account_alias:"synthetic"
      ~max_attempts:12
      ~maximum_phase:(Time_ns.Span.of_sec 60.)
      ~settings:[]
      ()
    |> checked_named ~stage:"boundary_0542"
  in
  assert (
    Result.is_error
      (Plan.create
         ~auth:Api
         ~transport:Sse
         ~model:"bad\nmodel"
         ~account_alias:"synthetic"
         ~max_attempts:17
         ~maximum_phase:(Time_ns.Span.of_sec 60.)
         ~settings:[]
         ()));
  let evidence =
    { Evidence.status = Incomplete
    ; plan
    ; attempts = 0
    ; turns = 0
    ; tool_effects = 0
    ; restored = false
    ; identity_preserved = false
    ; renewal = false
    ; logout = false
    ; phase = Journey
    ; recorded_at_ms = 0L
    ; configuration_proven = false
    ; transport_proven = false
    ; renewal_expires_at_ms = None
    ; renewal_not_before_ms = None
    ; renewal_ws_continuity_proven = false
    ; cancelled_owner_flow_proven = false
    ; failure = None
    ; oauth_failure = None
    ; protocol_violation = None
    ; feature_evidence = None
    ; feature_failure = None
    ; reason = "not_attempted"
    }
  in
  let encoded = Jsonaf.to_string (Evidence.to_json evidence) in
  List.iter
    [ "token"; "challenge"; "history_body"; "authorization_uri"; "account_id" ]
    ~f:(fun forbidden -> assert (not (String.is_substring encoded ~substring:forbidden)));
  print_endline "pure bounded plan and allowlisted manifest checks passed"
;;

let principal =
  P.Principal.create
    ~id:
      (P.Id.Principal.of_string "pri_live_qualification"
       |> checked_named ~stage:"boundary_0585")
    ~authentication_kind:"trusted-local-qualification"
    ~scopes:
      (P.Scope.Set.of_list
         [ List_prompts
         ; List_workspaces
         ; Create_sessions
         ; View_session_transcript
         ; Send_messages
         ; Own_sessions
         ; Stop_sessions
         ; Answer_approvals
         ; Provider_view
         ; Provider_manage
         ; Provider_select
         ; Diagnostics
         ])
    ~attributes:[]
  |> checked_named ~stage:"boundary_0603"
;;

let actor = Actor.trusted_local principal

let permission actor scope =
  Actor.is_current actor && P.Principal.has_scope (Actor.principal actor) scope
;;

let random_id () = id P.Id.Operation.(create () |> to_string)

let required = function
  | DTO.Operation.Status -> P.Scope.Provider_view
  | Select -> Provider_select
  | Setup | Login | Challenge | Cancel | Logout | Configure_environment -> Provider_manage
;;

let path env root child = Eio.Path.(Eio.Stdenv.fs env / root / child)
let metadata_name value = S.Name.create value |> checked_named ~stage:"boundary_0621"

let config env root =
  let atom value = Sexp.to_string (Sexp.Atom value) in
  let source =
    sprintf
      {|(version 1)
(server ((data_dir %s) (unix_socket %s) (shutdown_grace_ms 1000)
(http ((enabled false) (address 127.0.0.1) (port 19876) (require_auth true)))
(durability ((journal_flush each) (journal_flush_ms 10) (snapshot_every_events 100) (snapshot_every_ms 5000)))))
(workspaces (((id qualification) (source (physical %s)) (access exclusive)
(prompt_limits (((prompt qualification) (max_root_agents 1) (overflow reject)))))))
(prompts (((id qualification) (path %s) (allowed_workspaces (qualification)) (permission_profile qualification) (enabled true))))
(permission_profiles (((id qualification) (tool_default ask) (approval_timeout none) (approval_fallback deny) (manifest_authorization deny))))
(manifest_grants ())|}
      (atom (Filename.concat root "sessions"))
      (atom (Filename.concat root "daemon.sock"))
      (atom (Filename.concat root "workspace"))
      (atom (Filename.concat root "journey.chatmd"))
  in
  Agent_server.Config_parser.parse_string ~source_file:"qualification-config" source
  |> checked_named ~stage:"boundary_0642"
  |> Agent_server.Config_validator.validate ~env
  |> checked_named ~stage:"boundary_0644"
;;

type host =
  { plan : Plan.t
  ; effect_target : Eio.Fs.dir_ty Eio.Path.t
  ; daemon : D.t
  ; runtime : Runtime.t
  ; connection : Agent_client.Connection.t
  ; upstream : A.t -> Agent_server.Graph_tracking.Upstream.t
  ; configuration_proven : bool ref
  ; transport_proven : bool ref
  ; budget_denial : string option ref
  ; failure : Failure.t option ref
  ; oauth_failure : Provider_oauth.Error.t option ref
  ; expectation : M.Expectation.t
  ; login : Provider_oauth.Login.t option ref
  ; protocol_violation : O.Diagnostic.Protocol_violation.t option ref
  ; close : unit -> unit
  }

let open_host
      env
      ~sw
      root
      plan
      counters
      ~configuration_proven
      ~transport_proven
      ~budget_denial
      ~failure
      ~oauth_failure
      ~protocol_violation
  =
  let driver =
    Driver.create
      ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
      ~max_request_bytes:(1024 * 1024)
      ~max_body_bytes:(256 * 1024)
      ~max_frame_bytes:(256 * 1024)
      ~timeout_seconds:(Float.min 60. (Time_ns.Span.to_sec plan.Plan.maximum_phase))
      ()
    |> checked_named ~stage:"boundary_0680"
  in
  let transport =
    Provider_oauth.Transport.create
      ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.mono_clock env)
    |> checked_named ~stage:"boundary_0686"
  in
  Eio.Switch.on_release sw (fun () -> Provider_oauth.Transport.close transport);
  let policy =
    Provider_oauth.Policy.direct_codex ~expected_account:None ~callback_port:1455 ()
    |> checked_named ~stage:"boundary_0691"
  in
  let oauth =
    Provider_oauth_registry.create ~transport ~policy ~wall_clock:(Eio.Stdenv.clock env)
  in
  let features =
    [ Driver.Capability.Text_input; Function_tools; Custom_tools; Opaque_replay ]
    @ (match plan.feature with
       | Some Feature_case.Case.Image -> [ Driver.Capability.Image_input ]
       | Some Document -> [ Document_input ]
       | Some (Json_schema | Reasoning | Function_call) | None -> [])
    @ (if Plan.equal_transport plan.transport Require_websocket
       then [ Driver.Capability.Websocket ]
       else [])
    @ List.map plan.settings ~f:(fun s ->
      Driver.Capability.Setting (Driver.Setting.name s))
  in
  let route =
    if Plan.equal_auth plan.auth Api then Profile_policy.Public_api else Direct_codex
  in
  let baseline = Profile_policy.baseline route in
  let capabilities =
    Driver.Capability.create
      ~baseline
      ~models:
        [ ( plan.model
          , List.map features ~f:(fun feature ->
              let support =
                match
                  List.Assoc.find baseline feature ~equal:Driver.Capability.equal_feature
                with
                | Some Driver.Capability.Unsupported -> Driver.Capability.Unsupported
                | Some Supported | Some Unknown | None -> Supported
              in
              feature, support) )
        ]
    |> checked_named ~stage:"boundary_0723"
  in
  let host_id = id "live-qualification-host"
  and binding = id "live-route" in
  let endpoint = Profile_policy.endpoint route in
  let mapping identity =
    Driver.Profile.create
      ~id:(DTO.Profile_id.to_string profile_id)
      ~account:(M.Identity.account identity)
      ~endpoint
      ~capabilities
      ~defaults:plan.settings
    |> Result.map_error ~f:(fun _ -> B.Error.Invalid_mapping)
    |> Result.bind ~f:(fun profile ->
      B.Mapping.create profile ~revision:"live-plan-v1" ~binding ~identity)
  in
  let expectation, initial_mappings, authentication =
    if Plan.equal_auth plan.auth Api
    then (
      let identity =
        M.Identity.api_key
          ~host:host_id
          ~provider:"openai"
          ~billing:"api"
          ~account:None
          ~key_reference:binding
        |> checked_named ~stage:"boundary_0749"
      in
      ( M.Expectation.exact identity
      , [ mapping identity |> checked_named ~stage:"boundary_0752" ]
      , Admin.Template.Api_key ))
    else
      ( M.Expectation.oauth_acquisition
          ~host:host_id
          ~provider:"openai"
          ~billing:"subscription"
          ~issuer:(Provider_oauth.Policy.issuer policy)
          ~client_registration:(Provider_oauth.Policy.client_registration policy)
          ~resource:(Provider_oauth.Policy.resource policy)
          ~account:None
          ~required_scopes:[]
        |> checked_named ~stage:"boundary_0764"
      , []
      , Admin.Template.Direct_codex )
  in
  let template =
    Admin.Template.create
      ~profile:profile_id
      ~binding
      ~revision:(revision "live-plan-v1")
      ~authentication
      ~expectation
      ~expected_account:None
      ~mapping
    |> checked_named ~stage:"boundary_0777"
  in
  let runtime = ref None in
  let login = ref None in
  let get_runtime () = Option.value_exn !runtime in
  let observe_preparation result =
    Result.map_error result ~f:(fun error ->
      failure := Some (Failure.Preparation error);
      error)
  in
  let rec backend bound =
    Inference_host.Backend.create
      ~capture:(fun ~current ~model ~settings ->
        Inference_host.Backend.capture
          (Runtime.backend (get_runtime ()))
          ~current
          ~model
          ~settings
        |> observe_preparation)
      ~resolve:(fun target ->
        let source = Runtime.backend (get_runtime ()) in
        let source =
          match bound with
          | None -> Ok source
          | Some n -> Inference_host.Backend.with_response_limit source ~max_body_bytes:n
        in
        Result.bind source ~f:(fun source -> Inference_host.Backend.resolve source target)
        |> observe_preparation)
      ~with_response_limit:(fun ~max_body_bytes -> Ok (backend (Some max_body_bytes)))
  in
  let inference_host =
    Inference_host.create_with_backend
      (backend None)
      ~default_model:plan.model
      ~namespace:"live-qualification"
    |> checked_named ~stage:"boundary_0811"
  in
  let options = Inference_composition.daemon_options inference_host in
  let upstream session_actor =
    { Agent_server.Graph_tracking.Upstream.new_preparation_id =
        (Inference_host.identity inference_host).new_preparation_id
    ; on_admitted =
        (fun ~scope:_ ~accounting_id ->
          let state = A.state session_actor |> checked_named ~stage:"boundary_0823" in
          counters := List.length (Ledger.rows state.inference_ledger);
          try
            budget_check ~maximum:plan.max_attempts state.inference_ledger ~accounting_id
          with
          | Qualification_failure code as exn ->
            budget_denial := Some code;
            raise exn)
    ; on_attempt = ignore
    ; on_observation = ignore
    ; on_completion = ignore
    }
  in
  let policy_options =
    { options.inference_policy with
      runtime_inference_ports = (fun session_actor -> Ok (upstream session_actor))
    }
  in
  let factory ~sw ~server_id =
    let value =
      Provider_runtime_host.create
        ~sw
        ~env
        ~server_id
        ~anchor:Eio.Path.(Eio.Stdenv.fs env / root)
        ~components:[ metadata_name "credentials" ]
        ~host:host_id
        ~secret_namespace:
          (Secret.Namespace.create "qualification" |> checked_named ~stage:"boundary_0849")
        ~driver
        ~templates:[ template ]
        ~mappings:initial_mappings
        ~default_profile:profile_id
        ~environment:None
        ~environment_sources:[]
        ~oauth
        ~oauth_lease:
          (Some
             (B.OAuth.create
                ~lease:(Provider_oauth_registry.lease oauth)
                ~renewal:(Provider_oauth_registry.renewal oauth)))
        ~start_login:(fun ~sw ~template:_ ~mode ->
          let result =
            match mode with
            | DTO.Login_mode.Browser ->
              Provider_oauth.Login.start_browser
                ~transport
                ~policy
                ~sw
                ~net:(Eio.Stdenv.net env)
                ~secure_random:(Eio.Stdenv.secure_random env)
                ~clock:(Eio.Stdenv.mono_clock env)
                ~wall_clock:(Eio.Stdenv.clock env)
                ~maximum_wait:plan.maximum_phase
            | Device ->
              Provider_oauth.Login.start_device
                ~transport
                ~policy
                ~sw
                ~clock:(Eio.Stdenv.mono_clock env)
                ~wall_clock:(Eio.Stdenv.clock env)
                ~maximum_wait:plan.maximum_phase
          in
          (match result with
           | Ok (value, _) -> if Option.is_none !login then login := Some value
           | Error error -> oauth_failure := Some error);
          result)
        ~inference_principal:(P.Id.Principal.to_string principal.id)
        ~authorize_bridge:(fun ~principal:who ~profile:_ ~operation:_ ->
          String.equal who (P.Id.Principal.to_string principal.id))
        ~authorize:(fun actor ~operation ~profile:_ ->
          permission actor (required operation))
        ~authorize_setup:(fun a -> permission a Provider_manage)
        ~authorize_status:(fun a -> permission a Provider_view)
        ~new_operation:random_id
        ~new_revision:(fun () -> revision (M.Id.to_string (random_id ())))
        ~maximum_wait:(Time_ns.Span.of_sec 5.)
        ~limits:DTO.Limits.default
        ~inference_limits:Inference_runtime.Limits.default
        ~transport_policy:
          (match plan.transport with
           | Sse -> Http_sse
           | Require_websocket -> Require_websocket)
      |> checked_named ~stage:"boundary_0898"
    in
    runtime := Some value;
    Ok (Runtime.operator_port value)
  in
  let options =
    { options with
      inference_policy = policy_options
    ; provider_operator_factory = Some factory
    }
  in
  let daemon =
    D.start
      ~sw
      ~env
      ~config:(config env root)
      ~tool_dir:(Filename.concat root "workspace")
      ~home:root
      ~process_start_identity:None
      ~options
      ()
    |> checked_named ~stage:"boundary_0919"
  in
  let socket_path = Filename.concat root "daemon.sock" in
  Agent_transport_socket.Server.prepare_path ~env ~socket_path
  |> checked_named ~stage:"boundary_0922";
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Agent_transport_socket.Server.run
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~socket_path
      ~backlog:4
      ~dispatcher:(D.dispatcher daemon)
      ~close_connection:(D.close_connection daemon)
      ~authenticate:(fun _ _ -> Ok actor)
      ~max_line_length:(1024 * 1024)
      ~outgoing_capacity:64
      ~max_attachments:4
      ~on_error:raise
      ~on_protocol_error:(fun _ -> raise (Qualification_failure "protocol_failure"));
    `Stop_daemon);
  let rec await_socket () =
    if
      match Eio.Path.kind ~follow:false (path env root "daemon.sock") with
      | `Socket -> true
      | _ -> false
    then ()
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      await_socket ())
  in
  await_socket ();
  let connection =
    Agent_transport_socket.Client.connect
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~socket_path
      ~max_line_length:(1024 * 1024)
      ~notification_capacity:128
  in
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"explicit-live-qualification"
    ~implementation_version:"1"
  |> checked_named ~stage:"boundary_0961"
  |> ignore;
  let runtime = get_runtime () in
  { plan
  ; effect_target = path env root "workspace/qualification-effect.txt"
  ; configuration_proven
  ; transport_proven
  ; budget_denial
  ; failure
  ; oauth_failure
  ; expectation
  ; login
  ; protocol_violation
  ; daemon
  ; runtime
  ; connection
  ; upstream
  ; close =
      (fun () ->
        Exn.protect
          ~finally:(fun () -> Runtime.close runtime)
          ~f:(fun () ->
            Agent_client.Connection.close connection;
            D.shutdown daemon |> checked_named ~stage:"boundary_0979"))
  }
;;

let wait env f =
  let rec loop () =
    if f ()
    then ()
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
      loop ())
  in
  loop ()
;;

let request host command =
  let stage =
    match command with
    | P.Command.Prompt_list _ -> "prompt_list"
    | Workspace_list _ -> "workspace_list"
    | _ -> "protocol_request"
  in
  Agent_client.Connection.request_without_history host.connection command
  |> checked_protocol ~stage
;;

let session_state host session_id =
  Agent_server.Session_registry.find (D.registry host.daemon) session_id
  |> Option.value_exn
  |> fun (entry : Agent_server.Session_registry.entry) ->
  A.state entry.actor |> checked_named ~stage:"boundary_1008"
;;

let require_ok condition reason =
  if not condition then raise (Qualification_failure reason)
;;

let current_registry_model directory =
  Eio.Switch.run (fun sw ->
    let lease =
      S.Lock.acquire directory (metadata_name "provider-registry-M.lock") ~sw ~mode:Shared
      |> checked_named ~stage:"metadata_lock"
    in
    Exn.protect
      ~finally:(fun () -> S.Lock.release lease)
      ~f:(fun () ->
        let bytes =
          S.Directory.read_bounded
            directory
            (metadata_name "provider-registry.json")
            ~max_bytes:(1024 * 1024)
          |> checked_named ~stage:"metadata_read"
        in
        let document =
          Document_schema.Document.decode
            ~limits:Document_schema.Limits.default
            (Bytes.to_string bytes)
          |> checked_named ~stage:"metadata_document_decode"
        in
        let model =
          M.of_document document |> checked_named ~stage:"metadata_model_decode"
        in
        model))
;;

let current_identity directory =
  M.find (current_registry_model directory) ~binding:(id "live-route")
  |> checked_named ~stage:"metadata_binding_lookup"
;;

let source_revision snapshot =
  Option.bind (M.Snapshot.active snapshot) ~f:(fun active ->
    match M.Active.source active with
    | Protected_revision revision -> Some revision
    | Environment_reference _ -> None)
;;

let launch_browser env uri =
  try
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Path.with_open_out
        ~create:`Never
        Eio.Path.(Eio.Stdenv.fs env / "/dev/null")
        (fun sink ->
           Eio.Process.run
             (Eio.Stdenv.process_mgr env)
             ~stdout:sink
             ~stderr:sink
             [ "/usr/bin/open"; Uri.to_string uri ]))
  with
  | Eio.Io (Eio.Process.E (Child_error _), _) ->
    raise (Qualification_failure "browser_child_failed")
  | Eio.Io (Eio.Process.E (Executable_not_found _), _) ->
    raise (Qualification_failure "browser_executable_unavailable")
  | Eio.Io _ -> raise (Qualification_failure "browser_launch_io")
  | Eio.Time.Timeout -> raise (Qualification_failure "browser_launch_timeout")
;;

let flow_reference_equal (a : DTO.Flow_ref.t) (b : DTO.Flow_ref.t) =
  P.Id.Server.equal a.server_id b.server_id
  && DTO.Profile_id.equal a.profile b.profile
  && DTO.Flow_id.equal a.flow_id b.flow_id
  && P.Timestamp.equal a.expires_at b.expires_at
;;

let reconcile_enrolled host credential_directory mode =
  let original =
    P.Command.Provider_login_begin
      { profile = profile_id; mode; idempotency_key = key "live-oauth-enrollment" }
  in
  let flow =
    match
      Runtime.receipt host.runtime ~actor original
      |> checked_operator ~stage:"resume_original_receipt"
    with
    | P.Command_receipt.Committed (Provider_login flow) -> flow
    | Missing | Unavailable | Pending _ | Failed _ | Committed _ ->
      raise (Qualification_failure "resume_original_enrollment_unproven")
  in
  require_ok
    (P.Id.Server.equal
       flow.server_id
       (Agent_store.Session_store.server_id (D.store host.daemon))
     && DTO.Profile_id.equal flow.profile profile_id)
    "resume_flow_host_mismatch";
  let model = current_registry_model credential_directory in
  let owners =
    Provider_operator.Owner_records.create
      credential_directory
      ~incarnation:(M.incarnation model)
      ~maximum_records:128
    |> checked_named ~stage:"resume_owner_store"
  in
  let record =
    Provider_operator.Owner_records.find owners flow
    |> checked_named ~stage:"resume_owner_record"
  in
  let binding = Provider_operator.Owner_records.Record.binding record in
  require_ok
    (flow_reference_equal (Provider_operator.Owner_records.Record.result record).flow flow
     && M.Id.equal binding (id "live-route")
     && P.Idempotency_key.equal
          (Provider_operator.Owner_records.Record.key record)
          (key "live-oauth-enrollment")
     && DTO.Login_mode.equal (Provider_operator.Owner_records.Record.mode record) mode)
    "resume_owner_parameters_mismatch";
  let operation = Provider_operator.Owner_records.Record.operation record in
  let operation_proof =
    M.operation model ~binding ~operation
    |> checked_named ~stage:"resume_registry_operation"
  in
  let snapshot =
    M.find model ~binding |> checked_named ~stage:"resume_registry_binding"
  in
  let active =
    match M.Snapshot.active snapshot with
    | Some active -> active
    | None -> raise (Qualification_failure "resume_active_registration_missing")
  in
  let candidate_pending =
    M.candidate_pending model ~binding |> checked_named ~stage:"resume_candidate_state"
  in
  let active_revision =
    match M.Active.source active with
    | Protected_revision revision -> Some revision
    | Environment_reference _ -> None
  in
  require_ok
    (resume_registration_matches
       ~owner:(Actor.principal actor).id
       ~record_owner:(Provider_operator.Owner_records.Record.owner record)
       ~phase:(Provider_operator.Owner_records.Record.result record).phase
       ~operation_result:(M.Operation.result operation_proof)
       ~original_operation:operation
       ~active_revision
     && (not candidate_pending)
     && Option.is_none (M.Snapshot.disabled snapshot)
     && (match M.Snapshot.refresh snapshot with
         | Idle -> true
         | Possibly_sent _ | Renewal_uncertain _ -> false)
     && M.Expectation.accepts host.expectation (M.Active.identity active))
    "resume_original_active_registration_unproven"
;;

let authorize
      env
      ~sw
      root
      plan
      host
      key_input
      ~browser_presentation
      ~cancelled_owner_flow_proven
      ~resume_enrolled
      ~credential_directory
  =
  ignore
    (Runtime.dispatch
       host.runtime
       ~actor
       (P.Command.Provider_setup { idempotency_key = key "live-setup" })
     |> checked_operator ~stage:"provider_setup"
     : P.Method_result.t);
  if Plan.equal_auth plan.Plan.auth Api
  then (
    let input = Option.value_exn key_input in
    ignore
      (Runtime.enroll_private_key
         host.runtime
         ~actor
         ~profile:profile_id
         ~key:(key "live-api-enrollment")
         ~source_reference:(Key_input.reference input)
         ~sw
         ~read:(fun ~sw -> Key_input.read input ~env ~sw)
       |> checked_operator ~stage:"private_enrollment"
       : DTO.Configuration_result.t))
  else (
    let mode =
      if Plan.equal_auth plan.auth Browser then DTO.Login_mode.Browser else Device
    in
    if resume_enrolled
    then reconcile_enrolled host credential_directory mode
    else (
      let flow =
        match
          Runtime.dispatch
            host.runtime
            ~actor
            (P.Command.Provider_login_begin
               { profile = profile_id
               ; mode
               ; idempotency_key = key "live-oauth-enrollment"
               })
          |> checked_named ~stage:"boundary_1081"
        with
        | P.Method_result.Provider_login_begin flow -> flow
        | _ -> raise (Qualification_failure "login_result_invalid")
      in
      let challenge =
        match
          Runtime.dispatch
            host.runtime
            ~actor
            (P.Command.Provider_login_challenge { flow })
          |> checked_named ~stage:"boundary_1089"
        with
        | P.Method_result.Provider_login_challenge c -> c
        | _ -> raise (Qualification_failure "challenge_unavailable")
      in
      (match browser_presentation with
       | Browser_presentation.Launch_local ->
         (match
            DTO.Private_challenge.with_browser_uri challenge ~f:(fun uri ->
              launch_browser env uri)
          with
          | Some () -> ()
          | None -> raise (Qualification_failure "browser_challenge_invalid"))
       | Private_terminal ->
         (* The terminal descriptor is separate from stdout/stderr/artifact capture. *)
         Eio.Path.with_open_out
           ~create:`Never
           Eio.Path.(Eio.Stdenv.fs env / "/dev/tty")
           (fun sink ->
              let browser =
                DTO.Private_challenge.with_browser_uri challenge ~f:(fun uri ->
                  Eio.Flow.copy_string
                    ("Authorize this qualification at " ^ Uri.to_string uri ^ "\n")
                    sink)
              in
              let device =
                DTO.Private_challenge.with_device_prompt
                  challenge
                  ~f:(fun ~verification_uri ~user_code ->
                    Eio.Flow.copy_string
                      ("Authorize at "
                       ^ Uri.to_string verification_uri
                       ^ " using code "
                       ^ user_code
                       ^ "\n")
                      sink)
              in
              if Option.is_none browser && Option.is_none device
              then raise (Qualification_failure "challenge_invalid")));
      wait env (fun () ->
        match
          Runtime.dispatch
            host.runtime
            ~actor
            (P.Command.Provider_status { profile = Some profile_id })
          |> checked_named ~stage:"boundary_1125"
        with
        | P.Method_result.Provider_status status ->
          let flow =
            List.find status.flows ~f:(fun f ->
              DTO.Flow_id.equal f.flow.flow_id flow.flow_id)
            |> Option.value_exn
          in
          (match flow.phase with
           | Pending -> false
           | Completed -> true
           | _ ->
             Option.iter !(host.login) ~f:(fun value ->
               Option.iter (Provider_oauth.Login.error value) ~f:(fun error ->
                 host.oauth_failure := Some error));
             raise (Qualification_failure "login_not_completed"))
        | _ -> raise (Qualification_failure "status_invalid"))));
  if not (Plan.equal_auth plan.auth Api)
  then (
    let mode =
      match plan.auth with
      | Api -> assert false
      | Browser -> DTO.Login_mode.Browser
      | Device -> Device
    in
    let flow =
      match
        Runtime.dispatch
          host.runtime
          ~actor
          (P.Command.Provider_login_begin
             { profile = profile_id
             ; mode
             ; idempotency_key =
                 key
                   (if resume_enrolled
                    then "live-resume-cancel-enrollment"
                    else "live-cancel-enrollment")
             })
        |> checked_named ~stage:"boundary_1156"
      with
      | P.Method_result.Provider_login_begin flow -> flow
      | _ -> raise (Qualification_failure "cancel_flow_begin_invalid")
    in
    (* Obtain only the actual authorized opaque challenge; never serialize or
       print this deliberately cancelled flow's private value. *)
    (match
       Runtime.dispatch host.runtime ~actor (P.Command.Provider_login_challenge { flow })
       |> checked_named ~stage:"boundary_1165"
     with
     | P.Method_result.Provider_login_challenge _ -> ()
     | _ -> raise (Qualification_failure "cancel_challenge_missing"));
    ignore
      (Runtime.dispatch
         host.runtime
         ~actor
         (P.Command.Provider_login_cancel
            { flow
            ; idempotency_key =
                key
                  (if resume_enrolled
                   then "live-resume-cancel-owned-flow"
                   else "live-cancel-owned-flow")
            })
       |> checked_named ~stage:"boundary_1175"
       : P.Method_result.t);
    match
      Runtime.dispatch
        host.runtime
        ~actor
        (P.Command.Provider_status { profile = Some profile_id })
      |> checked_named ~stage:"boundary_1182"
    with
    | P.Method_result.Provider_status status ->
      let cancelled =
        List.find status.flows ~f:(fun candidate ->
          DTO.Flow_id.equal candidate.flow.flow_id flow.flow_id)
        |> Option.value_exn
      in
      (match cancelled.phase with
       | Cancelled -> cancelled_owner_flow_proven := true
       | Pending | Completed | Failed _ | Interrupted | Expired ->
         raise (Qualification_failure "owner_cancel_unproven"))
    | _ -> raise (Qualification_failure "cancel_status_invalid"));
  let selection =
    match
      Runtime.dispatch host.runtime ~actor (P.Command.Provider_status { profile = None })
      |> checked_operator ~stage:"provider_status_for_selection"
    with
    | P.Method_result.Provider_status status -> status.selection |> Option.value_exn
    | _ -> raise (Qualification_failure "status_invalid")
  in
  if not (DTO.Profile_id.equal selection.profile profile_id)
  then
    ignore
      (Runtime.dispatch
         host.runtime
         ~actor
         (P.Command.Provider_select
            { profile = profile_id
            ; expected_revision = selection.revision
            ; idempotency_key = key "live-select"
            })
       |> checked_operator ~stage:"provider_select"
       : P.Method_result.t)
;;

let create_session env ~sw host =
  let page = P.Page.Request.create ~limit:16 () |> checked_named ~stage:"boundary_1217" in
  let prompt =
    match
      request
        host
        (P.Command.Prompt_list { page; enabled = Some true; available = Some true })
    with
    | P.Method_result.Prompt_list prompts -> (List.hd_exn prompts.items).id
    | _ -> raise (Qualification_failure "prompt_missing")
  in
  let workspace =
    match
      request
        host
        (P.Command.Workspace_list
           { page; kind = None; access = None; available = Some true })
    with
    | P.Method_result.Workspace_list workspaces -> (List.hd_exn workspaces.items).id
    | _ -> raise (Qualification_failure "workspace_missing")
  in
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt)
      ~workspace:(Configured workspace)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"qualification"
      ~start_immediately:false
      ~labels:[ "qualification", "live" ]
      ()
    |> checked_named ~stage:"boundary_1248"
  in
  Agent_client.Session_handle.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection:host.connection
    ~spec
    ~mode:Read_write
    ~subscribe:false
    ()
  |> checked_protocol ~stage:"session_attach_or_create"
;;

let attach env ~sw host session_id =
  Agent_client.Session_handle.attach
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection:host.connection
    ~session_id
    ~mode:Read_write
    ~subscribe:false
    ()
  |> checked_protocol ~stage:"session_attach_or_create"
;;

let assert_dispatch_configuration host record =
  let plan = host.plan in
  let module O = Inference.Observation in
  let configuration = O.Attempt_record.configuration record in
  if
    (not (String.equal (O.Configuration.model configuration) plan.Plan.model))
    || not
         (String.equal
            (O.Configuration.profile configuration)
            (DTO.Profile_id.to_string profile_id))
  then raise (Qualification_failure "selected_configuration_mismatch");
  List.iter
    (List.filter plan.settings ~f:(fun setting ->
       List.mem
         [ "max_output_tokens"; "temperature"; "top_p" ]
         (Driver.Setting.name setting)
         ~equal:String.equal))
    ~f:(fun declared ->
      let name =
        match Driver.Setting.name declared with
        | "max_output_tokens" -> O.Configuration.Name.Max_output_tokens
        | "temperature" -> Temperature
        | "top_p" -> Top_p
        | _ -> raise (Qualification_failure "setting_declaration_invalid")
      in
      let actual =
        List.find (O.Configuration.settings configuration) ~f:(fun setting ->
          O.Configuration.Name.equal setting.name name)
      in
      let matches =
        match
          ( Driver.Setting.value declared
          , Option.map actual ~f:(fun setting -> setting.O.Configuration.selection) )
        with
        | ( Openai.Responses_codec.Request.Field.Absent
          , (None | Some O.Configuration.Omitted) ) -> true
        | Null, Some Explicit_null -> true
        | Value (`Number number), Some (Value (Tokens n)) ->
          Option.value_map (Int64.of_string_opt number) ~default:false ~f:(Int64.equal n)
        | Value (`Number number), Some (Value (Temperature n | Probability n)) ->
          Option.value_map (Float.of_string_opt number) ~default:false ~f:(Float.equal n)
        | _ -> false
      in
      if not matches then raise (Qualification_failure "effective_setting_mismatch"));
  host.configuration_proven := true;
  let selections =
    List.filter_map (O.Attempt_record.observations record) ~f:(fun observation ->
      match O.payload observation with
      | Transport_selection selection -> Some selection
      | Usage _ | Context_estimate _ | Configuration _ | Diagnostic _ -> None)
  in
  match List.last selections with
  | None -> raise (Qualification_failure "actual_transport_unproven")
  | Some selection ->
    let expected =
      match plan.transport with
      | Sse -> O.Transport_selection.Http_sse
      | Require_websocket -> Websocket
    in
    if
      (not
         (O.Transport_selection.equal_transport
            (O.Transport_selection.selected selection)
            expected))
      || Option.is_some (O.Transport_selection.fallback selection)
    then raise (Qualification_failure "actual_transport_mismatch");
    host.transport_proven := true
;;

let expected_patch =
  "*** Begin Patch\n\
   *** Add File: qualification-effect.txt\n\
   +QUALIFICATION-ONE-EFFECT\n\
   *** End Patch"
;;

let expected_file = String.concat_lines [ "QUALIFICATION-ONE-EFFECT" ]

let exact_patch input =
  String.equal input expected_patch || String.equal input (expected_patch ^ "\n")
;;

let target_denied = function
  | Error Inference_runtime.Preparation_error.Target_unavailable -> true
  | Ok _ | Error _ -> false
;;

let safe_permission (state : Agent_session.Session_state.t) (permission : P.Permission.t) =
  P.Id.Session.equal permission.session_id state.identity.session_id
  && Int.equal permission.generation state.identity.generation
  && String.equal permission.tool_name "apply_patch"
  && List.mem
       permission.choices
       P.Permission.Approve_once
       ~equal:P.Permission.equal_choice
  &&
  match permission.owner with
  | Operation _ -> false
  | Invocation id ->
    List.exists state.invocations ~f:(fun invocation ->
      let context = invocation.context in
      P.Id.Invocation.equal context.id id
      && P.Id.Session.equal context.session_id permission.session_id
      && Int.equal context.generation permission.generation
      && P.Invocation.equal_origin context.origin Model
      && Option.equal String.equal context.provider_call_id (Some permission.call_id)
      && String.equal context.tool_name "apply_patch"
      &&
      match context.input with
      | `String input -> exact_patch input
      | _ -> false)
;;

let answer_permissions host handle state =
  List.iter state.Agent_session.Session_state.permissions ~f:(fun permission ->
    if P.Permission.equal_state permission.state Pending
    then (
      if Option.is_some host.plan.feature
      then (
        ignore
          (Agent_client.Session_handle.respond_permission
             handle
             ~permission_id:permission.id
             ~permission_generation:permission.generation
             ~choice:Deny
             ~reason:(Some "no-tool feature qualification")
           |> checked_named ~stage:"feature_permission_deny"
           : P.Permission.t);
        raise (Qualification_failure "feature_tool_effect_refused"));
      let already_approved =
        List.exists state.permissions ~f:(fun prior ->
          Option.exists prior.resolution ~f:(fun resolution ->
            P.Permission.equal_choice resolution.choice Approve_once))
      in
      let approved =
        (not already_approved)
        && List.length state.invocations = 1
        && safe_permission state permission
        &&
        match Eio.Path.kind ~follow:false host.effect_target with
        | `Not_found -> true
        | _ -> false
      in
      ignore
        (Agent_client.Session_handle.respond_permission
           handle
           ~permission_id:permission.id
           ~permission_generation:permission.generation
           ~choice:(if approved then Approve_once else Deny)
           ~reason:(Some "exact bounded qualification patch")
         |> checked_named ~stage:"boundary_1405"
         : P.Permission.t);
      if not approved then raise (Qualification_failure "unsafe_tool_input")))
;;

let ensure_effect_absent env root =
  match
    Eio.Path.kind ~follow:false (path env root "workspace/qualification-effect.txt")
  with
  | `Not_found -> ()
  | _ -> raise (Qualification_failure "effect_target_preexists")
;;

let verify_effect_file env root =
  let target = path env root "workspace/qualification-effect.txt" in
  (match Eio.Path.kind ~follow:false target with
   | `Regular_file -> ()
   | _ -> raise (Qualification_failure "effect_target_not_regular"));
  if not (String.equal (Eio.Path.load target) expected_file)
  then raise (Qualification_failure "effect_content_mismatch")
;;

let check_budget_denial host =
  (match !(host.budget_denial) with
   | None -> ()
   | Some code -> raise (Qualification_failure code));
  match !(host.failure) with
  | Some (Failure.Preparation _) ->
    raise (Qualification_failure "observed_preparation_failure")
  | Some (Terminal _) | None -> ()
;;

let await_turn env host handle prior =
  let session_id = Agent_client.Session_handle.session_id handle in
  wait env (fun () ->
    check_budget_denial host;
    let state = session_state host session_id in
    answer_permissions host handle state;
    if Option.is_some host.plan.feature
    then
      require_ok
        (List.is_empty state.permissions && List.is_empty state.invocations)
        "feature_tool_effect_refused";
    let rows =
      Ledger.rows state.inference_ledger
      |> List.filter ~f:(fun row ->
        Int64.(Ledger.Handle.ordinal (Ledger.Row.handle row) > prior))
    in
    let completed = ref false
    and pending = ref false in
    List.iter rows ~f:(fun row ->
      List.iter
        (O.Attempt_record.observations (Ledger.Row.record row))
        ~f:(fun observation ->
          match O.payload observation with
          | Diagnostic diagnostic ->
            (match O.Diagnostic.reason diagnostic with
             | Protocol_violation detail -> host.protocol_violation := Some detail
             | _ -> ())
          | _ -> ());
      match Inference.Observation.Attempt_record.state (Ledger.Row.record row) with
      | Terminal terminal ->
        (match Inference.Event.Terminal.outcome terminal with
         | Completed ->
           assert_dispatch_configuration host (Ledger.Row.record row);
           completed := true
         | Refused | Incomplete _ | Failed _ ->
           host.failure
           := Some
                (Failure.Terminal
                   { outcome = Inference.Event.Terminal.outcome terminal
                   ; delivery = Inference.Event.Terminal.delivery terminal
                   });
           raise (Qualification_failure "observed_inference_failure"))
      | Prepared | Running -> pending := true
      | Interrupted _ -> raise (Qualification_failure "submission_uncertain"));
    !completed
    && (not !pending)
    && Option.is_none state.active_operation
    && List.is_empty state.conversation.deferred_user_entries)
;;

let last_ordinal state =
  List.fold
    (Ledger.rows state.Agent_session.Session_state.inference_ledger)
    ~init:0L
    ~f:(fun n row -> Int64.max n (Ledger.Handle.ordinal (Ledger.Row.handle row)))
;;

let send env host handle text =
  check_budget_denial host;
  let session_id = Agent_client.Session_handle.session_id handle in
  let previous = last_ordinal (session_state host session_id) in
  ignore
    (Agent_client.Session_handle.send_message
       handle
       { P.Session.Message_content.kind = Plain_text; text; attachments = [] }
     |> checked_protocol ~stage:"session_send"
     : P.Method_result.Send_message.t);
  await_turn env host handle previous
;;

let stop_for_restore env host handle =
  ignore
    (Agent_client.Session_handle.stop handle ~mode:Graceful
     |> checked_named ~stage:"boundary_1494"
     : P.Session.t);
  let session_id = Agent_client.Session_handle.session_id handle in
  wait env (fun () ->
    let state = session_state host session_id in
    match state.lifecycle.observed with
    | Stopped -> Option.is_none state.active_operation
    | Failed _ -> raise (Qualification_failure "session_stop_failed")
    | Starting
    | Recovering
    | Queued_for_slot
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping -> false)
;;

let one_effect state =
  match state.Agent_session.Session_state.invocations with
  | [ invocation ] when String.equal invocation.context.tool_name "apply_patch" ->
    (match invocation.context.input, invocation.status with
     | `String input, Published (Complete _) when exact_patch input -> 1
     | _ -> raise (Qualification_failure "exact_tool_completion_unproven"))
  | [] | _ :: _ -> raise (Qualification_failure "tool_invocation_count_mismatch")
;;

(* Auxiliary probes borrow the actual session's actor and durable admission ports.
   They do not start a foreground worker or commit an assistant turn. *)
let run_auxiliary_feature ~sw host session_id input =
  let entry : Agent_server.Session_registry.entry =
    Agent_server.Session_registry.find (D.registry host.daemon) session_id
    |> Option.value_exn
  in
  let before = A.state entry.actor |> checked_protocol ~stage:"auxiliary_state" in
  require_ok
    (P.Id.Session.equal before.identity.session_id session_id
     && List.is_empty (Ledger.rows before.inference_ledger)
     && Option.is_none before.active_operation
     && List.is_empty before.invocations
     && List.is_empty before.permissions)
    "auxiliary_identity_or_prior_attempt";
  let target =
    match Inference.Selection.view before.spec.inference_target with
    | Captured target -> target
    | Unresolved -> raise (Qualification_failure "feature_target_unresolved")
  in
  let context =
    match Inference_host.Backend.resolve (Runtime.backend host.runtime) target with
    | Ok context -> context
    | Error error ->
      host.failure := Some (Failure.Preparation error);
      raise (Qualification_failure "auxiliary_resolution_failed")
  in
  let history_id =
    History_entry.Id.create
      ~namespace:("qualification-input-" ^ M.Id.to_string (random_id ()))
      ~sequence:0
    |> checked_named ~stage:"auxiliary_input_identity"
  in
  let request =
    Feature_case.Input.request
      input
      ~target
      ~history_id
      ~limits:Document_schema.Limits.default
    |> checked_named ~stage:"auxiliary_request"
  in
  let tracking =
    Agent_server.Graph_tracking.create
      entry.actor
      ~source:
        (Transcript.Source_id.of_string (M.Id.to_string (random_id ()))
         |> checked_named ~stage:"auxiliary_source")
      ~upstream:(host.upstream entry.actor)
    |> checked_protocol ~stage:"auxiliary_tracking"
  in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      Agent_server.Graph_tracking.seal tracking
      |> checked_protocol ~stage:"auxiliary_seal";
      Agent_server.Graph_tracking.finish tracking
      |> checked_protocol ~stage:"auxiliary_finish")
  in
  let execute () =
    let execution =
      Inference_client.Execution.create
        ~context
        ~identity:(Agent_server.Graph_tracking.identity tracking)
        ~relation:Root
        ~before_dispatch:(fun prepared ->
          require_ok
            (Inference.Request.Target.equal
               target
               (Inference_runtime.Prepared.target prepared))
            "auxiliary_target_changed")
        ~on_attempt:(Agent_server.Graph_tracking.on_attempt tracking)
        ~on_completion:(Agent_server.Graph_tracking.on_completion tracking)
        ~on_observation:(Agent_server.Graph_tracking.on_observation tracking)
    in
    let receipt =
      match Inference_client.Execution.run execution ~sw ~request ~on_event:ignore with
      | Ok receipt -> receipt
      | Error (Preparation error) ->
        host.failure := Some (Failure.Preparation error);
        raise (Qualification_failure "auxiliary_preparation_failed")
      | Error (Attempt _) -> raise (Qualification_failure "auxiliary_attempt_failed")
    in
    let after =
      A.state entry.actor |> checked_protocol ~stage:"auxiliary_completed_state"
    in
    require_ok
      (Int.equal before.identity.generation after.identity.generation
       && List.equal
            P.History.equal_entry
            before.conversation.canonical_history
            after.conversation.canonical_history
       && List.is_empty after.invocations
       && List.is_empty after.permissions)
      "auxiliary_session_mutation";
    let record =
      match Ledger.rows after.inference_ledger with
      | [ row ] -> Ledger.Row.record row
      | [] | _ :: _ -> raise (Qualification_failure "feature_attempt_count")
    in
    require_ok
      (Inference_runtime.Receipt.equal_output_coverage
         (Inference_runtime.Receipt.output_coverage receipt)
         Response_output)
      "auxiliary_output_coverage";
    let terminal = Inference_runtime.Receipt.terminal receipt in
    (match O.Attempt_record.state record with
     | Terminal recorded ->
       require_ok
         (Inference.Event.Terminal.equal terminal recorded)
         "auxiliary_terminal_mismatch"
     | Prepared | Running | Interrupted _ ->
       raise (Qualification_failure "feature_terminal_missing"));
    require_ok
      (Transcript.Scope.equal
         (Inference.Event.Terminal.scope terminal)
         (O.Attempt_record.scope record))
      "feature_scope_mismatch";
    let selection =
      List.filter_map (O.Attempt_record.observations record) ~f:(fun observation ->
        match O.payload observation with
        | Transport_selection s -> Some s
        | _ -> None)
      |> function
      | [ selection ] -> selection
      | [] | _ :: _ -> raise (Qualification_failure "feature_transport_count")
    in
    require_ok
      (O.Observation_id.equal
         (O.Transport_selection.accounting_id selection)
         (O.Attempt_record.accounting_id record)
       && Option.is_none (O.Transport_selection.fallback selection))
      "feature_transport_binding";
    target, receipt, record, selection
  in
  match execute () with
  | result ->
    cleanup ();
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try cleanup () with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let run
      ~env
      ~(plan : Plan.t)
      ~phase
      ~root
      ~key_input
      ~hold_until_expiry
      ~browser_presentation
      ~resume_enrolled
  =
  let attempts = ref 0
  and turns = ref 0
  and tool_effects = ref 0
  and restored = ref false
  and identity_preserved = ref false
  and renewal = ref false
  and logout = ref false
  and configuration_proven = ref false
  and transport_proven = ref false
  and renewal_expires_at_ms = ref None
  and renewal_not_before_ms = ref None
  and renewal_ws_continuity_proven = ref false
  and budget_denial = ref None
  and cancelled_owner_flow_proven = ref false
  and failure = ref None
  and oauth_failure = ref None
  and protocol_violation = ref None
  and feature_evidence = ref None
  and feature_failure = ref None in
  let status = ref Evidence.Incomplete
  and reason = ref "not_attempted" in
  let execute () =
    require_ok
      ((not resume_enrolled) || resume_selection_valid ~auth:plan.auth ~phase)
      "resume_selection_invalid";
    require_ok
      (browser_selection_valid ~auth:plan.auth ~phase browser_presentation)
      "browser_launch_selection_invalid";
    require_ok
      (Bool.equal (Plan.equal_phase phase Feature) (Option.is_some plan.feature))
      "feature_phase_plan_mismatch";
    require_ok
      (not (Plan.equal_phase phase Feature && hold_until_expiry))
      "feature_expiry_hold_refused";
    require_ok (Filename.is_absolute root) "invalid_root";
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
    Eio.Switch.run (fun sw ->
      let anchor = Eio.Path.(Eio.Stdenv.fs env / root) in
      let metadata =
        S.Directory.open_or_create
          ~sw
          ~anchor
          ~components:[ metadata_name "qualification" ]
        |> checked_named ~stage:"boundary_1572"
      in
      let owner =
        S.Lock.acquire metadata (metadata_name "runner.lock") ~sw ~mode:Exclusive
        |> checked_named ~stage:"boundary_1576"
      in
      Exn.protect
        ~finally:(fun () -> S.Lock.release owner)
        ~f:(fun () ->
          let plan_name = metadata_name "plan.json" in
          let plan_bytes = Bytes.of_string (Jsonaf.to_string (Plan.document plan)) in
          (if resume_enrolled
           then (
             let original =
               S.Directory.read_bounded metadata plan_name ~max_bytes:16384
               |> checked_named ~stage:"resume_existing_plan"
             in
             require_ok (Bytes.equal original plan_bytes) "plan_mismatch");
           match S.Directory.create_immutable metadata plan_name plan_bytes with
           | Ok () -> ()
           | Error error when S.Error.equal_code (S.Error.code error) Exists ->
             let old =
               S.Directory.read_bounded metadata plan_name ~max_bytes:16384
               |> checked_named ~stage:"boundary_1587"
             in
             require_ok (Bytes.equal old plan_bytes) "plan_mismatch"
           | Error _ -> raise (Qualification_failure "plan_publication_uncertain"));
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (path env root "workspace");
          let prompt =
            if Option.is_some plan.feature
            then
              sprintf
                {|<config model="%s"/><authoring_context policy="manual"/><developer>This is a bounded synthetic feature qualification. Follow the user output format. No tools, file access, or credential access.</developer>|}
                plan.model
            else
              sprintf
                {|<config model="%s"/><authoring_context policy="manual"/>
<developer>This is an explicitly bounded synthetic qualification. Keep answers short. Use apply_patch only when asked, exactly once; preserve all local conversation context. Do not access any credentials or other files.</developer>
<tool name="apply_patch"/>|}
                plan.model
          in
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            (path env root "journey.chatmd")
            prompt;
          let checkpoint = metadata_name "session.json" in
          let previous =
            match S.Directory.read_bounded metadata checkpoint ~max_bytes:1024 with
            | Ok bytes ->
              P.Id.Session.of_json (Jsonaf.of_string (Bytes.to_string bytes))
              |> checked_named ~stage:"boundary_1608"
              |> Option.some
            | Error error when S.Error.equal_code (S.Error.code error) Missing -> None
            | Error _ -> raise (Qualification_failure "checkpoint_unavailable")
          in
          let credential_directory =
            S.Directory.open_or_create
              ~sw
              ~anchor
              ~components:[ metadata_name "credentials" ]
            |> checked_named ~stage:"boundary_1618"
          in
          let with_host f =
            Eio.Switch.run (fun host_sw ->
              let host =
                open_host
                  env
                  ~sw:host_sw
                  root
                  plan
                  attempts
                  ~configuration_proven
                  ~transport_proven
                  ~budget_denial
                  ~failure
                  ~oauth_failure
                  ~protocol_violation
              in
              Exn.protect ~finally:host.close ~f:(fun () -> f host_sw host))
          in
          let selected = ref previous in
          if Plan.equal_phase phase Feature
          then (
            require_ok (Option.is_none previous) "feature_already_admitted";
            let case = Option.value_exn plan.feature in
            if
              Feature_case.Case.equal case Document
              || Feature_case.Case.equal case Function_call
            then
              if not (Plan.equal_auth plan.auth Api)
              then reason := "auxiliary_route_unqualified"
              else
                with_host (fun host_sw host ->
                  authorize
                    env
                    ~sw:host_sw
                    root
                    plan
                    host
                    key_input
                    ~browser_presentation
                    ~cancelled_owner_flow_proven
                    ~resume_enrolled
                    ~credential_directory;
                  let input =
                    Feature_case.input case |> checked_named ~stage:"feature_fixture"
                  in
                  let handle = create_session env ~sw:host_sw host in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Session_handle.close handle)
                    ~f:(fun () ->
                      let session_id = Agent_client.Session_handle.session_id handle in
                      S.Directory.create_immutable
                        metadata
                        checkpoint
                        (Bytes.of_string
                           (Jsonaf.to_string (P.Id.Session.to_json session_id)))
                      |> checked_named ~stage:"feature_checkpoint";
                      status := Live_attempted;
                      let target, receipt, record, selection =
                        run_auxiliary_feature ~sw:host_sw host session_id input
                      in
                      let payloads =
                        List.filter_map
                          (Inference_runtime.Receipt.output receipt)
                          ~f:(fun event ->
                            match Inference.Event.view event with
                            | Candidate_ready { payload; _ } -> Some payload
                            | Live _ | Terminal _ -> None)
                      in
                      let candidates =
                        List.count payloads ~f:(fun payload ->
                          match
                            History_entry.Payload.Semantic.view
                              (History_entry.Payload.semantic payload)
                          with
                          | Call _ | Unknown _ -> true
                          | Message _ | Result _ | Reasoning _ -> false)
                      in
                      if Feature_case.Case.equal case Document
                      then (
                        require_ok
                          (List.is_empty
                             (Inference_client.Text.refusals
                                (Inference_client.Text.of_receipt receipt)))
                          "feature_refusal";
                        List.iter payloads ~f:(fun payload ->
                          match
                            History_entry.Payload.Semantic.view
                              (History_entry.Payload.semantic payload)
                          with
                          | Message { role = Assistant; form = Output; content; _ } ->
                            List.iter content ~f:(function
                              | Text _ -> ()
                              | Refusal _ | Image _ | Unknown _ ->
                                raise (Qualification_failure "feature_output_shape"))
                          | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> ()));
                      let expected_transport =
                        match plan.transport with
                        | Sse -> O.Transport_selection.Http_sse
                        | Require_websocket -> Websocket
                      in
                      let outcome =
                        Inference.Event.Terminal.outcome
                          (Inference_runtime.Receipt.terminal receipt)
                      in
                      let configuration = O.Attempt_record.configuration record in
                      let selected_transport = O.Transport_selection.selected selection in
                      let validation =
                        if Feature_case.Case.equal case Function_call
                        then
                          Feature_case.validate_function
                            ~target
                            ~outcome
                            ~configuration
                            ~selected_transport
                            ~expected_transport
                            ~tool_candidates:candidates
                            ~candidates:payloads
                        else
                          Feature_case.validate
                            case
                            ~target
                            ~outcome
                            ~assistant_text:
                              (Inference_client.Text.messages
                                 (Inference_client.Text.of_receipt receipt))
                            ~configuration
                            ~selected_transport
                            ~expected_transport
                            ~tool_candidates:candidates
                      in
                      feature_evidence
                      := Some
                           (match validation with
                            | Ok evidence -> evidence
                            | Error error ->
                              feature_failure := Some error;
                              raise (Qualification_failure "feature_validation_failed"));
                      assert_dispatch_configuration host record;
                      require_ok
                        (!attempts = 1 && !turns = 0 && !tool_effects = 0)
                        "auxiliary_feature_budget_mismatch";
                      status := Live_pass;
                      reason := "auxiliary_feature_completed"))
            else
              with_host (fun host_sw host ->
                authorize
                  env
                  ~sw:host_sw
                  root
                  plan
                  host
                  key_input
                  ~browser_presentation
                  ~cancelled_owner_flow_proven
                  ~resume_enrolled
                  ~credential_directory;
                let input =
                  Feature_case.input case |> checked_named ~stage:"feature_fixture"
                in
                let handle = create_session env ~sw:host_sw host in
                let session_id = Agent_client.Session_handle.session_id handle in
                S.Directory.create_immutable
                  metadata
                  checkpoint
                  (Bytes.of_string (Jsonaf.to_string (P.Id.Session.to_json session_id)))
                |> checked_named ~stage:"feature_checkpoint";
                ignore
                  (Agent_client.Session_handle.start handle ~queue_if_limited:false
                   |> checked_protocol ~stage:"feature_start"
                   : P.Session.t);
                let before = session_state host session_id in
                require_ok
                  (List.is_empty (Ledger.rows before.inference_ledger))
                  "feature_prior_attempt";
                let prior_history = before.conversation.canonical_history in
                status := Live_attempted;
                ignore
                  (Agent_client.Session_handle.send_message
                     handle
                     (Feature_case.Input.session_content input |> Option.value_exn)
                   |> checked_protocol ~stage:"feature_send"
                   : P.Method_result.Send_message.t);
                await_turn env host handle 0L;
                let state = session_state host session_id in
                let row =
                  match Ledger.rows state.inference_ledger with
                  | [ row ] -> row
                  | [] | _ :: _ -> raise (Qualification_failure "feature_attempt_count")
                in
                let record = Ledger.Row.record row in
                let terminal =
                  match O.Attempt_record.state record with
                  | Terminal terminal -> terminal
                  | Prepared | Running | Interrupted _ ->
                    raise (Qualification_failure "feature_terminal_missing")
                in
                require_ok
                  (Transcript.Scope.equal
                     (Inference.Event.Terminal.scope terminal)
                     (O.Attempt_record.scope record))
                  "feature_scope_mismatch";
                let target =
                  match Inference.Selection.view state.spec.inference_target with
                  | Captured target -> target
                  | Unresolved ->
                    raise (Qualification_failure "feature_target_unresolved")
                in
                let fresh =
                  List.filter state.conversation.canonical_history ~f:(fun entry ->
                    not
                      (List.exists prior_history ~f:(fun old ->
                         P.History.Id.equal old.id entry.id)))
                in
                let entries =
                  Agent_session.History_codec.all_of_protocol fresh
                  |> checked_protocol ~stage:"feature_canonical_output"
                in
                let text = ref []
                and candidates = ref 0 in
                List.iter entries ~f:(fun entry ->
                  match
                    History_entry.Payload.Semantic.view
                      (History_entry.Payload.semantic (History_entry.payload entry))
                  with
                  | Message { role = Assistant; form = Output; content; _ } ->
                    List.iter content ~f:(function
                      | Text { text = value; _ } -> text := value :: !text
                      | Refusal _ | Image _ | Unknown _ ->
                        raise (Qualification_failure "feature_output_shape"))
                  | Call _ | Unknown _ -> incr candidates
                  | Message _ | Result _ | Reasoning _ -> ());
                require_ok
                  (List.is_empty state.invocations && List.is_empty state.permissions)
                  "feature_tool_effect_refused";
                let selections =
                  List.filter_map
                    (O.Attempt_record.observations record)
                    ~f:(fun observation ->
                      match O.payload observation with
                      | Transport_selection s -> Some s
                      | _ -> None)
                in
                let selection =
                  match selections with
                  | [ s ] -> s
                  | [] | _ :: _ -> raise (Qualification_failure "feature_transport_count")
                in
                require_ok
                  (O.Observation_id.equal
                     (O.Transport_selection.accounting_id selection)
                     (O.Attempt_record.accounting_id record))
                  "feature_accounting_mismatch";
                require_ok
                  (Option.is_none (O.Transport_selection.fallback selection))
                  "feature_fallback_refused";
                let expected_transport =
                  match plan.transport with
                  | Sse -> O.Transport_selection.Http_sse
                  | Require_websocket -> Websocket
                in
                feature_evidence
                := Some
                     (Feature_case.validate
                        case
                        ~target
                        ~outcome:(Inference.Event.Terminal.outcome terminal)
                        ~assistant_text:(List.rev !text)
                        ~configuration:(O.Attempt_record.configuration record)
                        ~selected_transport:(O.Transport_selection.selected selection)
                        ~expected_transport
                        ~tool_candidates:!candidates
                      |> function
                      | Ok evidence -> evidence
                      | Error error ->
                        feature_failure := Some error;
                        raise (Qualification_failure "feature_validation_failed"));
                incr turns;
                stop_for_restore env host handle;
                Agent_client.Session_handle.close handle;
                require_ok (!attempts = 1 && !tool_effects = 0) "feature_budget_mismatch";
                status := Live_pass;
                reason := "feature_completed"))
          else if Plan.equal_phase phase Journey
          then (
            require_ok (Option.is_none previous) "journey_already_admitted";
            with_host (fun host_sw host ->
              if resume_enrolled
              then
                require_ok
                  (resume_session_admissible
                     ~checkpoint:(Option.is_some previous)
                     ~stored_sessions:
                       (List.length
                          (Agent_store.Session_store.list_sessions (D.store host.daemon)))
                     ~registered_sessions:
                       (List.length
                          (Agent_server.Session_registry.entries (D.registry host.daemon))))
                  "resume_prior_session_unproven";
              authorize
                env
                ~sw:host_sw
                root
                plan
                host
                key_input
                ~browser_presentation
                ~cancelled_owner_flow_proven
                ~resume_enrolled
                ~credential_directory;
              let before = current_identity credential_directory in
              let handle = create_session env ~sw:host_sw host in
              let session_id = Agent_client.Session_handle.session_id handle in
              selected := Some session_id;
              S.Directory.create_immutable
                metadata
                checkpoint
                (Bytes.of_string (Jsonaf.to_string (P.Id.Session.to_json session_id)))
              |> checked_named ~stage:"boundary_1657";
              ignore
                (Agent_client.Session_handle.start handle ~queue_if_limited:false
                 |> checked_protocol ~stage:"session_start"
                 : P.Session.t);
              status := Live_attempted;
              send
                env
                host
                handle
                "Remember synthetic marker QUALIFICATION-LOCAL-HISTORY-ONE. Reply \
                 briefly.";
              incr turns;
              ensure_effect_absent env root;
              send
                env
                host
                handle
                ("Call apply_patch exactly once with this exact single-file Add patch, \
                  no other changes:\n"
                 ^ expected_patch
                 ^ "\nThen reply briefly.");
              incr turns;
              let state = session_state host session_id in
              tool_effects := one_effect state;
              require_ok (!tool_effects = 1) "tool_effect_count_invalid";
              verify_effect_file env root;
              stop_for_restore env host handle;
              let old_history =
                (session_state host session_id).conversation.canonical_history
              in
              Agent_client.Session_handle.close handle;
              let after = current_identity credential_directory in
              identity_preserved
              := Option.equal
                   M.Identity.equal
                   (Option.map (M.Snapshot.active before) ~f:M.Active.identity)
                   (Option.map (M.Snapshot.active after) ~f:M.Active.identity);
              require_ok !identity_preserved "identity_changed";
              (* Private checkpoint only; no transcript goes into evidence. *)
              S.Directory.create_immutable
                metadata
                (metadata_name "history.json")
                (Bytes.of_string
                   (Jsonaf.to_string
                      (`Array (List.map old_history ~f:P.History.entry_to_json))))
              |> checked_named ~stage:"boundary_1703");
            with_host (fun host_sw host ->
              let session_id = Option.value_exn !selected in
              let handle = attach env ~sw:host_sw host session_id in
              let stored =
                S.Directory.read_bounded
                  metadata
                  (metadata_name "history.json")
                  ~max_bytes:(1024 * 1024)
                |> checked_named ~stage:"boundary_1712"
                |> Bytes.to_string
                |> Jsonaf.of_string
              in
              let current = session_state host session_id in
              let actual =
                `Array
                  (List.map
                     current.conversation.canonical_history
                     ~f:P.History.entry_to_json)
              in
              require_ok (Jsonaf.exactly_equal stored actual) "history_restore_mismatch";
              restored := true;
              ignore
                (Agent_client.Session_handle.start handle ~queue_if_limited:false
                 |> checked_protocol ~stage:"session_start"
                 : P.Session.t);
              let before_continuation =
                (session_state host session_id).conversation.canonical_history
              in
              send
                env
                host
                handle
                "Using the fully retained local history, repeat the synthetic marker. Do \
                 not call any tool.";
              let after_continuation =
                (session_state host session_id).conversation.canonical_history
              in
              require_ok
                (List.equal
                   P.History.equal_entry
                   before_continuation
                   (List.take after_continuation (List.length before_continuation)))
                "local_history_changed";
              require_ok
                (assistant_repeated_marker ~before:before_continuation after_continuation)
                "continuity_output_unproven";
              incr turns;
              tool_effects := one_effect (session_state host session_id);
              require_ok (!tool_effects = 1) "tool_replayed";
              verify_effect_file env root;
              stop_for_restore env host handle;
              Agent_client.Session_handle.close handle);
            status := Live_pass;
            reason := "journey_completed")
          else (
            let session_id = Option.value_exn previous in
            let before = current_identity credential_directory in
            let logged_out_target = ref None in
            with_host (fun host_sw host ->
              let handle = attach env ~sw:host_sw host session_id in
              attempts
              := List.length
                   (Ledger.rows (session_state host session_id).inference_ledger);
              if Plan.equal_phase phase Logout
              then (
                let target =
                  Inference_host.Backend.capture
                    (Runtime.backend host.runtime)
                    ~current:None
                    ~model:plan.model
                    ~settings:[]
                  |> checked_named ~stage:"boundary_1775"
                in
                logged_out_target := Some target;
                ignore
                  (Runtime.dispatch
                     host.runtime
                     ~actor
                     (P.Command.Provider_logout
                        { profile = profile_id; idempotency_key = key "live-logout" })
                   |> checked_named ~stage:"boundary_1784"
                   : P.Method_result.t);
                let after = current_identity credential_directory in
                require_ok
                  (Option.is_some (M.Snapshot.disabled after)
                   && not
                        (M.Epoch.equal (M.Snapshot.epoch before) (M.Snapshot.epoch after))
                  )
                  "logout_not_proven";
                require_ok
                  (target_denied
                     (Inference_host.Backend.resolve
                        (Runtime.backend host.runtime)
                        target))
                  "old_target_still_authorized";
                logout := true;
                tool_effects := one_effect (session_state host session_id);
                require_ok (!tool_effects = 1) "tool_replayed";
                reason := "logout_restart_pending")
              else (
                let active = Option.value_exn (M.Snapshot.active before) in
                let grant = Option.value_exn (M.Active.grant active) in
                let now_ms () =
                  Int64.of_float (Eio.Time.now (Eio.Stdenv.clock env) *. 1000.)
                in
                let prepared =
                  match (M.Grant.effective grant).expiry with
                  | Unknown ->
                    reason := "renewal_expiry_unknown";
                    false
                  | Known { at_ms; _ } ->
                    renewal_expires_at_ms := Some at_ms;
                    let now = now_ms () in
                    if hold_until_expiry
                    then (
                      let available_ms =
                        Int64.of_float (Time_ns.Span.to_sec plan.maximum_phase *. 1000.)
                      in
                      if Int64.(now + available_ms - 60_000L < at_ms)
                      then (
                        renewal_not_before_ms
                        := Some Int64.(at_ms - available_ms + 60_000L);
                        reason := "renewal_outside_hold_bound";
                        false)
                      else if Int64.(now + 60_000L >= at_ms)
                      then (
                        reason := "renewal_warm_window_missed";
                        false)
                      else (
                        ignore
                          (Agent_client.Session_handle.start
                             handle
                             ~queue_if_limited:false
                           |> checked_named ~stage:"boundary_1837"
                           : P.Session.t);
                        send
                          env
                          host
                          handle
                          "Acknowledge this synthetic pre-expiry channel check briefly, \
                           without tools.";
                        incr turns;
                        let warm = current_identity credential_directory in
                        require_ok
                          (Option.equal
                             M.Id.equal
                             (source_revision before)
                             (source_revision warm))
                          "renewal_before_real_expiry";
                        wait env (fun () -> Int64.(now_ms () >= at_ms));
                        true))
                    else if Int64.(now < at_ms)
                    then (
                      renewal_not_before_ms := Some at_ms;
                      reason := "renewal_not_due";
                      false)
                    else if Plan.equal_transport plan.transport Require_websocket
                    then (
                      reason := "renewal_cold_channel_unproven";
                      false)
                    else true
                in
                if prepared
                then (
                  ignore
                    (Agent_client.Session_handle.start handle ~queue_if_limited:false
                     |> checked_named ~stage:"boundary_1870"
                     : P.Session.t);
                  send
                    env
                    host
                    handle
                    "Continue the synthetic restored conversation briefly, without tools.";
                  incr turns;
                  let after = current_identity credential_directory in
                  identity_preserved
                  := Option.equal
                       M.Identity.equal
                       (Option.map (M.Snapshot.active before) ~f:M.Active.identity)
                       (Option.map (M.Snapshot.active after) ~f:M.Active.identity);
                  renewal
                  := !identity_preserved
                     && not
                          (Option.equal
                             M.Id.equal
                             (source_revision before)
                             (source_revision after));
                  require_ok !renewal "renewal_not_proven";
                  renewal_ws_continuity_proven
                  := hold_until_expiry
                     && Plan.equal_transport plan.transport Require_websocket;
                  tool_effects := one_effect (session_state host session_id);
                  require_ok (!tool_effects = 1) "tool_replayed";
                  status := Live_pass;
                  reason := "renewal_completed"));
              stop_for_restore env host handle;
              Agent_client.Session_handle.close handle);
            if Plan.equal_phase phase Logout
            then (
              with_host (fun host_sw host ->
                let handle = attach env ~sw:host_sw host session_id in
                let after = current_identity credential_directory in
                require_ok
                  (Option.is_some (M.Snapshot.disabled after))
                  "logout_restart_resurrected";
                let target = Option.value_exn !logged_out_target in
                require_ok
                  (target_denied
                     (Inference_host.Backend.resolve
                        (Runtime.backend host.runtime)
                        target))
                  "logout_restart_old_target_authorized";
                require_ok
                  (target_denied
                     (Inference_host.Backend.capture
                        (Runtime.backend host.runtime)
                        ~current:None
                        ~model:plan.model
                        ~settings:[]))
                  "logout_restart_capture_authorized";
                tool_effects := one_effect (session_state host session_id);
                require_ok (!tool_effects = 1) "tool_replayed";
                stop_for_restore env host handle;
                Agent_client.Session_handle.close handle);
              status := Live_pass;
              reason := "logout_completed"))))
  in
  (try
     Eio.Time.with_timeout_exn
       (Eio.Stdenv.clock env)
       (Time_ns.Span.to_sec plan.Plan.maximum_phase)
       execute
   with
   | Qualification_failure code ->
     reason := code;
     status
     := if Option.value_map !failure ~default:false ~f:Failure.is_unsupported
        then Unsupported
        else Incomplete
   | Eio.Time.Timeout ->
     reason := "phase_timed_out";
     status := Incomplete
   | Eio.Cancel.Cancelled _ as exn -> raise exn);
  { Evidence.status = !status
  ; plan
  ; attempts = !attempts
  ; turns = !turns
  ; tool_effects = !tool_effects
  ; restored = !restored
  ; identity_preserved = !identity_preserved
  ; renewal = !renewal
  ; logout = !logout
  ; phase
  ; recorded_at_ms = Int64.of_float (Eio.Time.now (Eio.Stdenv.clock env) *. 1000.)
  ; configuration_proven = !configuration_proven
  ; transport_proven = !transport_proven
  ; renewal_expires_at_ms = !renewal_expires_at_ms
  ; renewal_not_before_ms = !renewal_not_before_ms
  ; renewal_ws_continuity_proven = !renewal_ws_continuity_proven
  ; cancelled_owner_flow_proven = !cancelled_owner_flow_proven
  ; failure = !failure
  ; oauth_failure = !oauth_failure
  ; protocol_violation = !protocol_violation
  ; feature_evidence = !feature_evidence
  ; feature_failure = !feature_failure
  ; reason = !reason
  }
;;
