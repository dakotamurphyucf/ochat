open! Core
module D = Document_schema

let json_error error = Sexp.to_string_hum (D.Error.sexp_of_t error)
let member json name = D.Json.field json ~name

let string json name =
  match member json name with
  | Value (`String value) -> Ok value
  | Absent | Null | Value _ -> Error (name ^ " must be a string")
;;

module Terminal = struct
  type delivery =
    | Definitely_not_submitted
    | Possibly_submitted
    | Response_started
  [@@deriving equal, sexp_of]

  type incomplete_reason =
    | Output_limit
    | Filtered
    | Other
    | Unavailable
  [@@deriving equal, sexp_of]

  type auth_failure =
    | Missing
    | Denied
    | Profile_changed
    | Reauthorization_required
    | Invalid_credential
    | Timed_out
  [@@deriving equal, sexp_of]

  type transport_failure =
    | Connection
    | Timeout
    | Invalid_http
    | Invalid_content_type
    | Body_limit
    | Framing_limit
    | Protocol
    | Unsupported_transport
    | Session_closed
    | Session_busy
    | Http_status of int
  [@@deriving equal, sexp_of]

  module Provider_failure = struct
    type t =
      | Invalid_request
      | Denied
      | Rate_limited
      | Unavailable
      | Unknown
    [@@deriving equal, sexp_of]
  end

  type failure =
    | Authentication of auth_failure
    | Transport of transport_failure
    | Provider of Provider_failure.t
  [@@deriving equal, sexp_of]

  type outcome =
    | Completed
    | Refused
    | Incomplete of incomplete_reason
    | Failed of failure
  [@@deriving equal, sexp_of]

  type t =
    { scope : Transcript.Scope.t
    ; delivery : delivery
    ; outcome : outcome
    }

  let sexp_of_t t =
    [%sexp
      { scope = (Transcript.Scope.to_json t.scope : Jsonaf.t)
      ; delivery = (t.delivery : delivery)
      ; outcome = (t.outcome : outcome)
      }]
  ;;

  let create ~scope ~delivery ~outcome =
    let open Result.Let_syntax in
    let%bind () =
      match outcome, delivery with
      | Failed (Authentication _), Definitely_not_submitted -> Ok ()
      | Failed (Authentication _), (Possibly_submitted | Response_started) ->
        Error "authentication failure cannot follow submission"
      | (Completed | Refused | Incomplete _ | Failed (Provider _)), Response_started ->
        Ok ()
      | ( (Completed | Refused | Incomplete _ | Failed (Provider _))
        , (Definitely_not_submitted | Possibly_submitted) ) ->
        Error "provider outcome requires response evidence"
      | Failed (Transport _), _ -> Ok ()
    in
    let%map () =
      match outcome with
      | Failed (Transport (Http_status status)) when status < 100 || status > 599 ->
        Error "HTTP status must be in 100..599"
      | Completed | Refused | Incomplete _ | Failed _ -> Ok ()
    in
    { scope; delivery; outcome }
  ;;

  let scope t = t.scope
  let delivery t = t.delivery
  let outcome t = t.outcome

  let equal a b =
    Transcript.Scope.equal a.scope b.scope
    && equal_delivery a.delivery b.delivery
    && equal_outcome a.outcome b.outcome
  ;;

  let delivery_name = function
    | Definitely_not_submitted -> "definitely_not_submitted"
    | Possibly_submitted -> "possibly_submitted"
    | Response_started -> "response_started"
  ;;

  let incomplete_name = function
    | Output_limit -> "output_limit"
    | Filtered -> "filtered"
    | Other -> "other"
    | Unavailable -> "unavailable"
  ;;

  let auth_name = function
    | Missing -> "missing"
    | Denied -> "denied"
    | Profile_changed -> "profile_changed"
    | Reauthorization_required -> "reauthorization_required"
    | Invalid_credential -> "invalid_credential"
    | Timed_out -> "timed_out"
  ;;

  let provider_name : Provider_failure.t -> string = function
    | Invalid_request -> "invalid_request"
    | Denied -> "denied"
    | Rate_limited -> "rate_limited"
    | Unavailable -> "unavailable"
    | Unknown -> "unknown"
  ;;

  let failure_json = function
    | Authentication reason ->
      `Object [ "type", `String "authentication"; "reason", `String (auth_name reason) ]
    | Provider reason ->
      `Object [ "type", `String "provider"; "reason", `String (provider_name reason) ]
    | Transport reason ->
      let name, extra =
        match reason with
        | Connection -> "connection", []
        | Timeout -> "timeout", []
        | Invalid_http -> "invalid_http", []
        | Invalid_content_type -> "invalid_content_type", []
        | Body_limit -> "body_limit", []
        | Framing_limit -> "framing_limit", []
        | Protocol -> "protocol", []
        | Unsupported_transport -> "unsupported_transport", []
        | Session_closed -> "session_closed", []
        | Session_busy -> "session_busy", []
        | Http_status status ->
          "http_status", [ "status", `Number (Int.to_string status) ]
      in
      `Object ([ "type", `String "transport"; "reason", `String name ] @ extra)
  ;;

  let outcome_json = function
    | Completed -> `Object [ "type", `String "completed" ]
    | Refused -> `Object [ "type", `String "refused" ]
    | Incomplete reason ->
      `Object [ "type", `String "incomplete"; "reason", `String (incomplete_name reason) ]
    | Failed failure ->
      `Object [ "type", `String "failed"; "failure", failure_json failure ]
  ;;

  let to_json t =
    `Object
      [ "scope", Transcript.Scope.to_json t.scope
      ; "delivery", `String (delivery_name t.delivery)
      ; "outcome", outcome_json t.outcome
      ]
  ;;

  let decode_failure json =
    let open Result.Let_syntax in
    let%bind kind = string json "type" in
    let%bind reason = string json "reason" in
    match kind with
    | "authentication" ->
      (match reason with
       | "missing" -> Ok (Authentication Missing)
       | "denied" -> Ok (Authentication Denied)
       | "profile_changed" -> Ok (Authentication Profile_changed)
       | "reauthorization_required" -> Ok (Authentication Reauthorization_required)
       | "invalid_credential" -> Ok (Authentication Invalid_credential)
       | "timed_out" -> Ok (Authentication Timed_out)
       | _ -> Error "unknown authentication failure")
    | "provider" ->
      (match reason with
       | "invalid_request" -> Ok (Provider Invalid_request)
       | "denied" -> Ok (Provider Denied)
       | "rate_limited" -> Ok (Provider Rate_limited)
       | "unavailable" -> Ok (Provider Unavailable)
       | "unknown" -> Ok (Provider Unknown)
       | _ -> Error "unknown provider failure")
    | "transport" ->
      (match reason with
       | "connection" -> Ok (Transport Connection)
       | "timeout" -> Ok (Transport Timeout)
       | "invalid_http" -> Ok (Transport Invalid_http)
       | "invalid_content_type" -> Ok (Transport Invalid_content_type)
       | "body_limit" -> Ok (Transport Body_limit)
       | "framing_limit" -> Ok (Transport Framing_limit)
       | "protocol" -> Ok (Transport Protocol)
       | "unsupported_transport" -> Ok (Transport Unsupported_transport)
       | "session_closed" -> Ok (Transport Session_closed)
       | "session_busy" -> Ok (Transport Session_busy)
       | "http_status" ->
         (match member json "status" with
          | Value (`Number value) ->
            (match Int.of_string_opt value with
             | Some status when String.equal value (Int.to_string status) ->
               Ok (Transport (Http_status status))
             | Some _ | None -> Error "HTTP status must be an integer")
          | Absent | Null | Value _ -> Error "HTTP status must be an integer")
       | _ -> Error "unknown transport failure")
    | _ -> Error "unknown inference failure family"
  ;;

  let decode_outcome json =
    let open Result.Let_syntax in
    let%bind kind = string json "type" in
    match kind with
    | "completed" -> Ok Completed
    | "refused" -> Ok Refused
    | "incomplete" ->
      let%bind reason = string json "reason" in
      (match reason with
       | "output_limit" -> Ok (Incomplete Output_limit)
       | "filtered" -> Ok (Incomplete Filtered)
       | "other" -> Ok (Incomplete Other)
       | "unavailable" -> Ok (Incomplete Unavailable)
       | _ -> Error "unknown incomplete reason")
    | "failed" ->
      (match member json "failure" with
       | Value json -> Result.map (decode_failure json) ~f:(fun failure -> Failed failure)
       | Absent | Null -> Error "inference failure is required")
    | _ -> Error "unknown inference outcome"
  ;;

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate ~limits json |> Result.map_error ~f:json_error in
    let%bind scope =
      match member json "scope" with
      | Value scope -> Transcript.Scope.of_json scope ~limits
      | Absent | Null -> Error "inference scope is required"
    in
    let%bind delivery =
      let%bind value = string json "delivery" in
      match value with
      | "definitely_not_submitted" -> Ok Definitely_not_submitted
      | "possibly_submitted" -> Ok Possibly_submitted
      | "response_started" -> Ok Response_started
      | _ -> Error "unknown inference delivery"
    in
    let%bind outcome =
      match member json "outcome" with
      | Value value -> decode_outcome value
      | Absent | Null -> Error "inference outcome is required"
    in
    create ~scope ~delivery ~outcome
  ;;
end

type local_execution =
  | Not_eligible
  | Tool_candidate
[@@deriving equal, sexp_of]

type view =
  | Live of Transcript.Stream.t
  | Candidate_ready of
      { item : Transcript.Item.t
      ; payload : History_entry.Payload.t
      ; local_execution : local_execution
      }
  | Terminal of Terminal.t

type t =
  { view : view
  ; encoded_bytes : int
  }

let create view ~limits =
  let open Result.Let_syntax in
  let%bind json =
    match view with
    | Live event ->
      (match Transcript.Stream.view event with
       | Item_finalized _ -> Error "provider evidence cannot commit history"
       | Source_finished _ -> Error "host owns final local output admission"
       | Source_started _
       | Item_announced _
       | Part_announced _
       | Changed _
       | Unknown_event _ ->
         Ok (`Object [ "type", `String "live"; "event", Transcript.Stream.to_json event ]))
    | Candidate_ready { item; payload; local_execution } ->
      let%bind () = History_entry.Payload.validate payload in
      let semantic = History_entry.Payload.semantic payload in
      let header = Transcript.Header.of_semantic semantic in
      let%bind () =
        if Option.exists item.header ~f:(Transcript.Header.equal header)
        then Ok ()
        else Error "candidate requires its exact semantic header"
      in
      let%bind () =
        let wanted =
          match History_entry.Payload.Semantic.view semantic with
          | Call { name; _ } -> Some name
          | Message _ | Result _ | Reasoning _ | Unknown _ -> None
        in
        if Option.equal String.equal item.call_name wanted
        then Ok ()
        else Error "candidate call name conflicts with semantics"
      in
      let%bind () =
        match local_execution with
        | Not_eligible -> Ok ()
        | Tool_candidate ->
          let selected =
            match History_entry.Payload.Semantic.view semantic with
            | Call { namespace; async; _ } ->
              (match namespace with
               | Absent -> true
               | Null | Value _ -> false)
              && (match async with
                  | Absent | Value false -> true
                  | Null | Value true -> false)
              &&
                (match (History_entry.Payload.Semantic.metadata semantic).status with
                | Absent | Value "completed" -> true
                | Null | Value _ -> false)
            | Message _ | Result _ | Reasoning _ | Unknown _ -> false
          in
          if selected then Ok () else Error "candidate is not a selected local call"
      in
      let descriptor =
        (* Existing item encoding is exercised through the same admitted stream
           representation; no independent item/identity codec is invented. *)
        let%map event = Transcript.Stream.create (Item_announced item) ~limits in
        Transcript.Stream.to_json event
      in
      let%map descriptor = descriptor in
      `Object
        [ "type", `String "candidate_ready"
        ; "item", descriptor
        ; "payload", History_entry.Payload.to_json payload
        ; ( "local_execution"
          , `String
              (match local_execution with
               | Not_eligible -> "not_eligible"
               | Tool_candidate -> "tool_candidate") )
        ]
    | Terminal terminal ->
      Ok (`Object [ "type", `String "terminal"; "terminal", Terminal.to_json terminal ])
  in
  let%map encoded_bytes =
    D.Json.validate_and_measure ~limits json |> Result.map_error ~f:json_error
  in
  { view; encoded_bytes }
;;

let view t = t.view
let encoded_bytes t = t.encoded_bytes

let scope t =
  match t.view with
  | Live event -> Transcript.Stream.scope event
  | Candidate_ready { item; _ } -> item.scope
  | Terminal terminal -> Terminal.scope terminal
;;
