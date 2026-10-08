open! Core
module O = Inference.Observation
module D = Document_schema

let ( let* ) result f = Result.bind result ~f
let invalid message = Error (Protocol_error.invalid_request message)

let json_error result =
  Result.map_error result ~f:(fun _ ->
    Protocol_error.invalid_request "invalid bounded inference read JSON")
;;

let inference_error result =
  Result.map_error result ~f:(fun _ ->
    Protocol_error.invalid_request "invalid neutral inference observation")
;;

let external_error result =
  Result.map_error result ~f:(fun _ ->
    Protocol_error.invalid_request "invalid neutral inference identity/state")
;;

let str value = `String value
let dec value = str (Int64.to_string value)
let boolean value = if value then `True else `False
let obj values = `Object values

let nullable f = function
  | None -> `Null
  | Some value -> f value
;;

let array f values = `Array (List.map values ~f)
let get = Json_codec.required_as

let nonnegative value =
  if Int64.(value < zero) then invalid "negative inference count" else Ok ()
;;

let positive value =
  if Int64.(value <= zero) then invalid "required positive inference ordinal" else Ok ()
;;

let decimal json =
  let* value = Json_codec.string json in
  match Int64.of_string_opt value with
  | Some value64 when String.equal value (Int64.to_string value64) ->
    let* () = nonnegative value64 in
    Ok value64
  | Some _ | None -> invalid "expected canonical nonnegative decimal int64"
;;

let host_integer json = Json_codec.bounded_int ~min:0 ~max:Int.max_value json

let optional f = function
  | `Null -> Ok None
  | value -> Result.map (f value) ~f:Option.some
;;

let checked_sum values =
  List.fold values ~init:(Ok 0L) ~f:(fun acc value ->
    let* acc = acc in
    let* () = nonnegative value in
    if Int64.(value > max_value - acc)
    then invalid "inference count overflow"
    else Ok Int64.(acc + value))
;;

let measure ~limits json = json_error (D.Json.validate_and_measure ~limits json)

let validated_of_sexp decode sexp =
  match decode (Jsonaf.t_of_sexp sexp) with
  | Ok value -> value
  | Error (error : Protocol_error.t) -> Sexplib.Conv.of_sexp_error error.message sexp
;;

module Features = struct
  let observations = "inference.observations.v1"
  let configuration = "inference.configuration.v1"
  let diagnostics = "inference.diagnostics.v1"
  let all = [ observations; configuration; diagnostics ]
end

module Summary_request = struct
  type t = { session_id : Id.Session.t }

  let to_json t = obj [ "session_id", Id.Session.to_json t.session_id ]

  let of_json json =
    let* _ = measure ~limits:O.Admission.observation json in
    let* fields = Json_codec.fields json in
    let* session_id = get fields "session_id" Id.Session.of_json in
    Ok { session_id }
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)
  let t_of_sexp = validated_of_sexp of_json
end

module Coverage = struct
  type limit_kind =
    | Attempt_count
    | Turn_count
    | Retained_bytes
    | Protected_future_data
  [@@deriving equal, sexp_of]

  type tracking_status =
    | Available
    | Limited of limit_kind
  [@@deriving equal, sexp_of]

  type t =
    { before_tracking_unknown : bool
    ; retired_attempts : int64
    ; untracked_attempts : int64
    ; retired_turns : int64
    ; untracked_turns : int64
    ; tracking_status : tracking_status
    }

  let create
        ~before_tracking_unknown
        ~retired_attempts
        ~untracked_attempts
        ~retired_turns
        ~untracked_turns
        ~tracking_status
    =
    let* () =
      Result.all_unit
        (List.map
           [ retired_attempts; untracked_attempts; retired_turns; untracked_turns ]
           ~f:nonnegative)
    in
    Ok
      { before_tracking_unknown
      ; retired_attempts
      ; untracked_attempts
      ; retired_turns
      ; untracked_turns
      ; tracking_status
      }
  ;;

  let limit_to_string = function
    | Attempt_count -> "attempt_count"
    | Turn_count -> "turn_count"
    | Retained_bytes -> "retained_bytes"
    | Protected_future_data -> "protected_future_data"
  ;;

  let status_to_json = function
    | Available -> `Null
    | Limited limit -> str (limit_to_string limit)
  ;;

  let status_of_json = function
    | `Null -> Ok Available
    | json ->
      Result.map
        (Json_codec.enum
           ~name:"tracking limit"
           [ "attempt_count", Attempt_count
           ; "turn_count", Turn_count
           ; "retained_bytes", Retained_bytes
           ; "protected_future_data", Protected_future_data
           ]
           json)
        ~f:(fun limit -> Limited limit)
  ;;

  let to_json t =
    obj
      [ "before_tracking_unknown", boolean t.before_tracking_unknown
      ; "retired_attempts", dec t.retired_attempts
      ; "untracked_attempts", dec t.untracked_attempts
      ; "retired_turns", dec t.retired_turns
      ; "untracked_turns", dec t.untracked_turns
      ; "tracking_limit", status_to_json t.tracking_status
      ]
  ;;

  let of_json json =
    let* _ = measure ~limits:O.Admission.observation json in
    let* fields = Json_codec.fields json in
    let* before_tracking_unknown = get fields "before_tracking_unknown" Json_codec.bool in
    let* retired_attempts = get fields "retired_attempts" decimal in
    let* untracked_attempts = get fields "untracked_attempts" decimal in
    let* retired_turns = get fields "retired_turns" decimal in
    let* untracked_turns = get fields "untracked_turns" decimal in
    let* tracking_status = get fields "tracking_limit" status_of_json in
    create
      ~before_tracking_unknown
      ~retired_attempts
      ~untracked_attempts
      ~retired_turns
      ~untracked_turns
      ~tracking_status
  ;;
end

module Metric = struct
  type sum =
    | Tokens of int64
    | Overflow
  [@@deriving equal, sexp_of]

  type unknown_counts =
    { not_reported : int64
    ; explicit_null : int64
    ; interrupted : int64
    ; not_submitted : int64
    ; before_tracking : int64
    }

  type t =
    { actual : sum
    ; actual_attempts : int64
    ; estimated : sum
    ; estimated_attempts : int64
    ; mixed_estimators : bool
    ; unknown : unknown_counts
    }

  let unknown_values t =
    [ t.not_reported; t.explicit_null; t.interrupted; t.not_submitted; t.before_tracking ]
  ;;

  let count t =
    checked_sum (t.actual_attempts :: t.estimated_attempts :: unknown_values t.unknown)
  ;;

  let validate_sum sum attempts =
    let* () = nonnegative attempts in
    match sum with
    | Tokens value ->
      let* () = nonnegative value in
      if Int64.equal attempts 0L && not (Int64.equal value 0L)
      then invalid "tokens without observed attempts"
      else Ok ()
    | Overflow ->
      if Int64.(attempts < 2L)
      then invalid "overflow requires multiple contributions"
      else Ok ()
  ;;

  let create
        ~actual
        ~actual_attempts
        ~estimated
        ~estimated_attempts
        ~mixed_estimators
        ~unknown
    =
    let* () = validate_sum actual actual_attempts in
    let* () = validate_sum estimated estimated_attempts in
    let* () = Result.all_unit (List.map (unknown_values unknown) ~f:nonnegative) in
    if mixed_estimators && Int64.(estimated_attempts < 2L)
    then invalid "mixed estimator marker requires multiple estimates"
    else (
      let t =
        { actual
        ; actual_attempts
        ; estimated
        ; estimated_attempts
        ; mixed_estimators
        ; unknown
        }
      in
      let* _ = count t in
      Ok t)
  ;;

  let sum_to_json = function
    | Tokens tokens -> obj [ "kind", str "tokens"; "tokens", dec tokens ]
    | Overflow -> obj [ "kind", str "overflow" ]
  ;;

  let sum_of_json json =
    let* fields = Json_codec.fields json in
    let* kind = get fields "kind" Json_codec.string in
    match kind with
    | "tokens" -> Result.map (get fields "tokens" decimal) ~f:(fun value -> Tokens value)
    | "overflow" -> Ok Overflow
    | _ -> invalid "unknown inference sum kind"
  ;;

  let to_json t =
    obj
      [ "actual", sum_to_json t.actual
      ; "actual_attempts", dec t.actual_attempts
      ; "estimated", sum_to_json t.estimated
      ; "estimated_attempts", dec t.estimated_attempts
      ; "mixed_estimators", boolean t.mixed_estimators
      ; ( "unknown"
        , obj
            [ "not_reported", dec t.unknown.not_reported
            ; "explicit_null", dec t.unknown.explicit_null
            ; "interrupted", dec t.unknown.interrupted
            ; "not_submitted", dec t.unknown.not_submitted
            ; "before_tracking", dec t.unknown.before_tracking
            ] )
      ]
  ;;

  let of_json json =
    let* fields = Json_codec.fields json in
    let* actual = get fields "actual" sum_of_json in
    let* actual_attempts = get fields "actual_attempts" decimal in
    let* estimated = get fields "estimated" sum_of_json in
    let* estimated_attempts = get fields "estimated_attempts" decimal in
    let* mixed_estimators = get fields "mixed_estimators" Json_codec.bool in
    let* raw_unknown = Json_codec.required fields "unknown" in
    let* unknown = Json_codec.fields raw_unknown in
    let* not_reported = get unknown "not_reported" decimal in
    let* explicit_null = get unknown "explicit_null" decimal in
    let* interrupted = get unknown "interrupted" decimal in
    let* not_submitted = get unknown "not_submitted" decimal in
    let* before_tracking = get unknown "before_tracking" decimal in
    create
      ~actual
      ~actual_attempts
      ~estimated
      ~estimated_attempts
      ~mixed_estimators
      ~unknown:
        { not_reported; explicit_null; interrupted; not_submitted; before_tracking }
  ;;
end

module Summary = struct
  type turns =
    { pending : int64
    ; completed : int64
    ; failed : int64
    ; cancelled : int64
    ; interrupted : int64
    }

  type components =
    { input : Metric.t
    ; output : Metric.t
    ; reported_total : Metric.t
    ; cached_input : Metric.t
    ; cache_write_input : Metric.t
    ; reasoning_output : Metric.t
    }

  type t =
    { retained_attempts : int64
    ; turns : turns
    ; components : components
    ; coverage : Coverage.t
    ; accounting_revision : int64
    }

  let retained_attempts t = t.retained_attempts
  let turns t = t.turns
  let components t = t.components
  let coverage t = t.coverage
  let accounting_revision t = t.accounting_revision

  let all components =
    [ "input", components.input
    ; "output", components.output
    ; "reported_total", components.reported_total
    ; "cached_input", components.cached_input
    ; "cache_write_input", components.cache_write_input
    ; "reasoning_output", components.reasoning_output
    ]
  ;;

  let to_json t =
    obj
      [ "retained_attempts", dec t.retained_attempts
      ; ( "turns"
        , obj
            [ "pending", dec t.turns.pending
            ; "completed", dec t.turns.completed
            ; "failed", dec t.turns.failed
            ; "cancelled", dec t.turns.cancelled
            ; "interrupted", dec t.turns.interrupted
            ] )
      ; ( "components"
        , obj
            (List.map (all t.components) ~f:(fun (name, value) ->
               name, Metric.to_json value)) )
      ; "coverage", Coverage.to_json t.coverage
      ; "accounting_revision", dec t.accounting_revision
      ]
  ;;

  let create ~retained_attempts ~turns ~components ~coverage ~accounting_revision =
    let* () = nonnegative retained_attempts in
    let* () = nonnegative accounting_revision in
    let* _ =
      checked_sum
        [ turns.pending
        ; turns.completed
        ; turns.failed
        ; turns.cancelled
        ; turns.interrupted
        ]
    in
    let* () =
      Result.all_unit
        (List.map (all components) ~f:(fun (_, metric) ->
           let* count = Metric.count metric in
           if Int64.equal count retained_attempts
           then Ok ()
           else invalid "component observation count differs from retained attempts"))
    in
    let t = { retained_attempts; turns; components; coverage; accounting_revision } in
    let* _ = measure ~limits:O.Admission.observation (to_json t) in
    Ok t
  ;;

  let of_json json =
    let* _ = measure ~limits:O.Admission.observation json in
    let* fields = Json_codec.fields json in
    let* retained_attempts = get fields "retained_attempts" decimal in
    let* accounting_revision = get fields "accounting_revision" decimal in
    let* coverage = get fields "coverage" Coverage.of_json in
    let* raw_turns = Json_codec.required fields "turns" in
    let* turns = Json_codec.fields raw_turns in
    let* pending = get turns "pending" decimal in
    let* completed = get turns "completed" decimal in
    let* failed = get turns "failed" decimal in
    let* cancelled = get turns "cancelled" decimal in
    let* interrupted = get turns "interrupted" decimal in
    let* raw_components = Json_codec.required fields "components" in
    let* components = Json_codec.fields raw_components in
    let* input = get components "input" Metric.of_json in
    let* output = get components "output" Metric.of_json in
    let* reported_total = get components "reported_total" Metric.of_json in
    let* cached_input = get components "cached_input" Metric.of_json in
    let* cache_write_input = get components "cache_write_input" Metric.of_json in
    let* reasoning_output = get components "reasoning_output" Metric.of_json in
    create
      ~retained_attempts
      ~turns:{ pending; completed; failed; cancelled; interrupted }
      ~components:
        { input
        ; output
        ; reported_total
        ; cached_input
        ; cache_write_input
        ; reasoning_output
        }
      ~coverage
      ~accounting_revision
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)
  let t_of_sexp = validated_of_sexp of_json
end

module Attempt = struct
  type t =
    { ordinal : int64
    ; generation : int
    ; scope : Transcript.Scope.t
    ; operation_id : Id.Operation.t option
    ; invocation_id : Id.Invocation.t option
    ; accounting_id : O.Observation_id.t
    ; state : O.Attempt_record.state
    ; usage : O.t option
    ; context : O.t option
    ; configuration : O.Configuration.t option
    ; transport_selection : O.t option
    ; diagnostics : O.t list option
    ; omitted_diagnostics : int64 option
    }

  let ordinal t = t.ordinal
  let generation t = t.generation
  let scope t = t.scope
  let operation_id t = t.operation_id
  let invocation_id t = t.invocation_id
  let accounting_id t = t.accounting_id
  let state t = t.state
  let usage t = t.usage
  let context t = t.context
  let configuration t = t.configuration
  let transport_selection t = t.transport_selection
  let diagnostics t = t.diagnostics
  let omitted_diagnostics t = t.omitted_diagnostics

  let delivery_to_json = function
    | Inference.Event.Terminal.Definitely_not_submitted -> str "definitely_not_submitted"
    | Possibly_submitted -> str "possibly_submitted"
    | Response_started -> str "response_started"
  ;;

  let delivery_of_json json =
    Json_codec.enum
      ~name:"delivery"
      [ "definitely_not_submitted", Inference.Event.Terminal.Definitely_not_submitted
      ; "possibly_submitted", Possibly_submitted
      ; "response_started", Response_started
      ]
      json
  ;;

  let state_to_json = function
    | O.Attempt_record.Prepared -> obj [ "kind", str "prepared" ]
    | Running -> obj [ "kind", str "running" ]
    | Terminal terminal ->
      obj
        [ "kind", str "terminal"; "terminal", Inference.Event.Terminal.to_json terminal ]
    | Interrupted { reason; delivery } ->
      obj
        [ "kind", str "interrupted"
        ; ( "reason"
          , str
              (match reason with
               | Cancelled -> "cancelled"
               | Host_interrupted -> "host_interrupted") )
        ; "delivery", delivery_to_json delivery
        ]
  ;;

  let state_of_json json =
    let* fields = Json_codec.fields json in
    let* kind = get fields "kind" Json_codec.string in
    match kind with
    | "prepared" -> Ok O.Attempt_record.Prepared
    | "running" -> Ok Running
    | "terminal" ->
      Result.map
        (get fields "terminal" (fun json ->
           external_error
             (Inference.Event.Terminal.of_json json ~limits:O.Admission.attempt)))
        ~f:(fun terminal -> O.Attempt_record.Terminal terminal)
    | "interrupted" ->
      let* reason =
        get
          fields
          "reason"
          (Json_codec.enum
             ~name:"interruption"
             [ "cancelled", O.Attempt_record.Cancelled
             ; "host_interrupted", Host_interrupted
             ])
      in
      let* delivery = get fields "delivery" delivery_of_json in
      Ok (O.Attempt_record.Interrupted { reason; delivery })
    | _ -> invalid "unknown attempt state"
  ;;

  let to_json t =
    obj
      ([ "ordinal", dec t.ordinal
       ; "generation", `Number (Int.to_string t.generation)
       ; "scope", Transcript.Scope.to_json t.scope
       ; "operation_id", nullable Id.Operation.to_json t.operation_id
       ; "invocation_id", nullable Id.Invocation.to_json t.invocation_id
       ; "accounting_id", str (O.Observation_id.to_string t.accounting_id)
       ; "state", state_to_json t.state
       ; "usage", nullable O.to_json t.usage
       ; "context", nullable O.to_json t.context
       ; "configuration", nullable O.Configuration.to_json t.configuration
       ; "diagnostics", nullable (array O.to_json) t.diagnostics
       ; "omitted_diagnostics", nullable dec t.omitted_diagnostics
       ]
       @ Option.to_list
           (Option.map t.transport_selection ~f:(fun observation ->
              "transport_selection", O.to_json observation)))
  ;;

  let create
        ~ordinal
        ~generation
        ~scope
        ~operation_id
        ~invocation_id
        ~accounting_id
        ~state
        ~usage
        ~context
        ~configuration
        ~diagnostics
        ~omitted_diagnostics
    =
    let* () = positive ordinal in
    if generation < 0
    then invalid "negative inference generation"
    else
      let* () =
        match state with
        | O.Attempt_record.Terminal terminal
          when not
                 (Transcript.Scope.equal scope (Inference.Event.Terminal.scope terminal))
          -> invalid "terminal scope differs from attempt"
        | Prepared | Running | Terminal _ | Interrupted _ -> Ok ()
      in
      let validate_observation expected observation =
        if not (Transcript.Scope.equal scope (O.scope observation))
        then invalid "observation scope differs from attempt"
        else
          let* () =
            inference_error (O.validate observation ~limits:O.Admission.observation)
          in
          match expected, O.payload observation with
          | `Usage, Usage _ when O.Observation_id.equal accounting_id (O.id observation)
            -> Ok ()
          | `Context, Context_estimate context ->
            (match configuration with
             | Some configuration
               when not
                      (String.equal
                         (O.Configuration.preparation_id configuration)
                         (O.Context_estimate.preparation_id context)) ->
               invalid "context preparation differs"
             | Some _ | None -> Ok ())
          | `Diagnostic, Diagnostic _ ->
            inference_error (O.validate observation ~limits:O.Admission.diagnostic)
          | (`Usage | `Context | `Diagnostic), _ ->
            invalid "wrong observation family/accounting identity"
      in
      let* () =
        Result.all_unit
          (List.filter_map
             [ Option.map usage ~f:(validate_observation `Usage)
             ; Option.map context ~f:(validate_observation `Context)
             ]
             ~f:Fn.id)
      in
      let* () =
        match diagnostics, omitted_diagnostics with
        | None, None -> Ok ()
        | Some values, Some omitted ->
          let* () = nonnegative omitted in
          if
            List.length values > 16
            || List.contains_dup (List.map values ~f:O.key) ~compare:O.Key.compare
          then invalid "diagnostic ring invalid"
          else Result.all_unit (List.map values ~f:(validate_observation `Diagnostic))
        | None, Some _ | Some _, None ->
          invalid "diagnostic disclosure metadata must be paired"
      in
      let* () =
        match configuration with
        | None -> Ok ()
        | Some value ->
          let* _ =
            measure ~limits:O.Admission.observation (O.Configuration.to_json value)
          in
          Ok ()
      in
      let t =
        { ordinal
        ; generation
        ; scope
        ; operation_id
        ; invocation_id
        ; accounting_id
        ; state
        ; usage
        ; context
        ; configuration
        ; transport_selection = None
        ; diagnostics
        ; omitted_diagnostics
        }
      in
      let* _ = measure ~limits:O.Admission.attempt (to_json t) in
      Ok t
  ;;

  let with_transport_selection t value =
    let* () =
      match value with
      | None -> Ok ()
      | Some observation ->
        if not (Transcript.Scope.equal t.scope (O.scope observation))
        then invalid "transport scope differs"
        else (
          match O.payload observation, t.configuration with
          | O.Transport_selection selection, Some configuration
            when O.Observation_id.equal
                   t.accounting_id
                   (O.Transport_selection.accounting_id selection) ->
            (match O.Configuration.transport_policy configuration with
             | Some policy
               when O.Transport_policy.equal
                      policy
                      (O.Transport_selection.requested selection) ->
               inference_error (O.validate observation ~limits:O.Admission.observation)
             | Some _ | None -> invalid "transport policy differs")
          | _ ->
            invalid
              "transport selection requires matching configuration/accounting identity")
    in
    let t = { t with transport_selection = value } in
    let* _ = measure ~limits:O.Admission.attempt (to_json t) in
    Ok t
  ;;

  let of_json json =
    let* _ = measure ~limits:O.Admission.attempt json in
    let* fields = Json_codec.fields json in
    let* ordinal = get fields "ordinal" decimal in
    let* generation = get fields "generation" host_integer in
    let* scope =
      get fields "scope" (fun json ->
        external_error (Transcript.Scope.of_json json ~limits:O.Admission.attempt))
    in
    let* operation_id = get fields "operation_id" (optional Id.Operation.of_json) in
    let* invocation_id = get fields "invocation_id" (optional Id.Invocation.of_json) in
    let* accounting_id =
      get fields "accounting_id" (fun json ->
        let* value = Json_codec.string json in
        inference_error (O.Observation_id.of_string value))
    in
    let* state = get fields "state" state_of_json in
    let decode_observation json =
      inference_error (O.of_json json ~limits:O.Admission.observation)
    in
    let* usage = get fields "usage" (optional decode_observation) in
    let* context = get fields "context" (optional decode_observation) in
    let* configuration =
      get
        fields
        "configuration"
        (optional (fun json ->
           inference_error (O.Configuration.of_json json ~limits:O.Admission.observation)))
    in
    let* diagnostics =
      get
        fields
        "diagnostics"
        (optional (fun json ->
           match json with
           | `Array values when List.length values <= 16 ->
             Result.all (List.map values ~f:decode_observation)
           | `Array _ -> invalid "diagnostic ring exceeds sixteen entries"
           | _ -> invalid "diagnostics must be an array"))
    in
    let* omitted_diagnostics = get fields "omitted_diagnostics" (optional decimal) in
    create
      ~ordinal
      ~generation
      ~scope
      ~operation_id
      ~invocation_id
      ~accounting_id
      ~state
      ~usage
      ~context
      ~configuration
      ~diagnostics
      ~omitted_diagnostics
    |> Result.bind ~f:(fun t ->
      match Json_codec.optional fields "transport_selection" with
      | None -> Ok t
      | Some json ->
        let* observation = decode_observation json in
        with_transport_selection t (Some observation))
  ;;
end

module Request = struct
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; include_configuration : bool
    ; include_diagnostics : bool
    }

  let create ~session_id ~page ~include_configuration ~include_diagnostics =
    if page.Page.Request.limit > 1000
    then invalid "inference page maximum is 1000"
    else
      let* _ = Page.Request.create ~limit:page.limit ?cursor:page.cursor () in
      Ok { session_id; page; include_configuration; include_diagnostics }
  ;;

  let to_json t =
    obj
      ([ "session_id", Id.Session.to_json t.session_id
       ; "include_configuration", boolean t.include_configuration
       ; "include_diagnostics", boolean t.include_diagnostics
       ]
       @ Page.Request.to_fields t.page)
  ;;

  let of_json json =
    let* _ = measure ~limits:O.Admission.observation json in
    let* fields = Json_codec.fields json in
    let* session_id = get fields "session_id" Id.Session.of_json in
    let* page = Page.Request.of_fields fields in
    let* include_configuration = get fields "include_configuration" Json_codec.bool in
    let* include_diagnostics = get fields "include_diagnostics" Json_codec.bool in
    create ~session_id ~page ~include_configuration ~include_diagnostics
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)
  let t_of_sexp = validated_of_sexp of_json
end

module Response = struct
  type t =
    { summary : Summary.t
    ; attempts : Attempt.t Page.t
    }

  let summary t = t.summary
  let attempts t = t.attempts

  let to_json t =
    obj
      [ "summary", Summary.to_json t.summary
      ; "attempts", Page.to_json Attempt.to_json t.attempts
      ]
  ;;

  let bounds max_bytes =
    if max_bytes <= 0 || max_bytes > 16 * 1024 * 1024
    then invalid "invalid inference response byte ceiling"
    else json_error (Transcript.Admission.limits ~max_bytes)
  ;;

  let create ~summary ~attempts ~max_bytes =
    let* limits = bounds max_bytes in
    if List.length attempts.Page.items > 1000
    then invalid "inference page exceeds 1000 rows"
    else if
      List.contains_dup
        (List.map attempts.items ~f:Attempt.ordinal)
        ~compare:Int64.compare
    then invalid "duplicate inference attempt ordinal"
    else (
      let t = { summary; attempts } in
      let* _ = measure ~limits (to_json t) in
      Ok t)
  ;;

  let of_json json ~max_bytes =
    let* limits = bounds max_bytes in
    let* _ = measure ~limits json in
    let* fields = Json_codec.fields json in
    let* summary = get fields "summary" Summary.of_json in
    let* raw_attempts = Json_codec.required fields "attempts" in
    let* page_fields = Json_codec.fields raw_attempts in
    let* raw_rows =
      get page_fields "items" (fun json ->
        match json with
        | `Array values -> Ok values
        | _ -> invalid "expected attempt array")
    in
    if List.length raw_rows > 1000
    then invalid "inference page exceeds 1000 rows"
    else
      let* attempts = Page.of_json Attempt.of_json raw_attempts in
      create ~summary ~attempts ~max_bytes
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp =
    validated_of_sexp (fun json -> of_json json ~max_bytes:(16 * 1024 * 1024))
  ;;

  let create_response = create

  module Builder = struct
    type response = t

    type t =
      { summary : Summary.t
      ; reversed : Attempt.t list
      ; ordinals : Int64.Set.t
      ; count : int
      ; row_bytes : int
      ; base_bytes : int
      ; max_bytes : int
      ; next_cursor : Page.Cursor.t option
      }

    let create ~summary ~max_bytes =
      let* limits = bounds max_bytes in
      let* base_bytes =
        D.Json.validate_and_measure
          ~limits
          (to_json { summary; attempts = { items = []; next_cursor = None } })
        |> Result.map_error ~f:(function
          | D.Error.Limit_exceeded "bytes" ->
            Protocol_error.create
              Resource_limit
              ~message:"inference summary cannot fit response allowance"
              ~retryable:false
              ()
          | _ -> Protocol_error.invalid_request "invalid bounded inference read JSON")
      in
      Ok
        { summary
        ; reversed = []
        ; ordinals = Int64.Set.empty
        ; count = 0
        ; row_bytes = 0
        ; base_bytes
        ; max_bytes
        ; next_cursor = None
        }
    ;;

    let add t row ~next_cursor =
      if Set.mem t.ordinals (Attempt.ordinal row)
      then invalid "duplicate inference attempt ordinal"
      else
        let* row_bytes = measure ~limits:O.Admission.attempt (Attempt.to_json row) in
        let* cursor_bytes =
          match next_cursor with
          | None -> Ok 0
          | Some cursor ->
            (* Cursor validity is independent of the remaining page allowance.
               Measure escaping under the protocol ceiling; the fit check below
               classifies a valid cursor that cannot fit as a capacity failure. *)
            let* _ = Page.Cursor.of_json (Page.Cursor.to_json cursor) in
            let* limits = bounds (16 * 1024 * 1024) in
            let* bytes = measure ~limits (Page.Cursor.to_json cursor) in
            Ok (String.length ",\"next_cursor\":" + bytes)
        in
        let count = t.count in
        let available = t.max_bytes - t.base_bytes in
        let comma = if count = 0 then 0 else 1 in
        let fits =
          count < 1000
          && cursor_bytes <= available
          && t.row_bytes <= available - cursor_bytes
          && comma <= available - cursor_bytes - t.row_bytes
          && row_bytes <= available - cursor_bytes - t.row_bytes - comma
        in
        if not fits
        then
          if count = 0
          then
            Error
              (Protocol_error.create
                 Resource_limit
                 ~message:"first inference row cannot fit response allowance"
                 ~retryable:false
                 ())
          else Ok None
        else
          Ok
            (Some
               { t with
                 reversed = row :: t.reversed
               ; ordinals = Set.add t.ordinals (Attempt.ordinal row)
               ; count = t.count + 1
               ; row_bytes = t.row_bytes + comma + row_bytes
               ; next_cursor
               })
    ;;

    let finish t =
      create_response
        ~summary:t.summary
        ~attempts:{ items = List.rev t.reversed; next_cursor = t.next_cursor }
        ~max_bytes:t.max_bytes
    ;;
  end
end
