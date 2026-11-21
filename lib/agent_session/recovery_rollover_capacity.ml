open! Core
module P = Agent_protocol
module J = P.Json_codec
module Configuration = Inference.Observation.Configuration
module Trigger = Chat_response.Capacity_rollover.Trigger

type boundary =
  | Unsent
  | Rejected
[@@deriving equal, sexp_of]

type t =
  { operation : P.Id.Operation.t
  ; trigger : Trigger.t
  ; boundary : boundary
  ; attempt_high_water : int64
  ; configuration_revision : int64
  ; configuration : (Configuration.t[@sexp.opaque])
  ; history : Chatml_source.Digest.t
  }
[@@deriving equal, sexp_of]

let invalid () = Error (P.Error.invalid_request "invalid automatic rollover cut")

let create
      ~operation
      ~(trigger : Trigger.t)
      ~boundary
      ~attempt_high_water
      ~configuration_revision
      ~configuration
      ~history
  =
  let matches_trigger =
    match boundary, trigger with
    | Unsent, (Soft_input_limit | Hard_input_limit) -> true
    | Rejected, Provider_context_limit -> Int64.(attempt_high_water > 0L)
    | Unsent, _ | Rejected, _ -> false
  in
  if Int64.(attempt_high_water < 0L || configuration_revision < 0L) || not matches_trigger
  then invalid ()
  else
    Ok
      { operation
      ; trigger
      ; boundary
      ; attempt_high_water
      ; configuration_revision
      ; configuration
      ; history
      }
;;

let trigger_name = function
  | Trigger.Soft_input_limit -> "soft_input_limit"
  | Hard_input_limit -> "hard_input_limit"
  | Provider_context_limit -> "provider_context_limit"
;;

let trigger_of_json = function
  | `String "soft_input_limit" -> Ok Trigger.Soft_input_limit
  | `String "hard_input_limit" -> Ok Hard_input_limit
  | `String "provider_context_limit" -> Ok Provider_context_limit
  | _ -> invalid ()
;;

let boundary_of_json = function
  | `String "unsent" -> Ok Unsent
  | `String "rejected" -> Ok Rejected
  | _ -> invalid ()
;;

let to_json t =
  let integer value = `Number (Int64.to_string value) in
  `Object
    [ "operation", P.Id.Operation.to_json t.operation
    ; "trigger", `String (trigger_name t.trigger)
    ; ( "boundary"
      , `String
          (match t.boundary with
           | Unsent -> "unsent"
           | Rejected -> "rejected") )
    ; "attempt_high_water", integer t.attempt_high_water
    ; "configuration_revision", integer t.configuration_revision
    ; "configuration", Configuration.to_json t.configuration
    ; "history", `String (Chatml_source.Digest.to_hex t.history)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let integer = J.bounded_int64 ~min:0L ~max:Int64.max_value in
  let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
  let%bind trigger = J.required_as fields "trigger" trigger_of_json in
  let%bind boundary = J.required_as fields "boundary" boundary_of_json in
  let%bind attempt_high_water = J.required_as fields "attempt_high_water" integer in
  let%bind configuration_revision =
    J.required_as fields "configuration_revision" integer
  in
  let%bind configuration =
    J.required_as fields "configuration" (fun json ->
      match Configuration.of_json json ~limits:Document_schema.Limits.default with
      | Ok configuration -> Ok configuration
      | Error _ -> invalid ())
  in
  let%bind history = J.required_as fields "history" J.string in
  let%bind history =
    match Chatml_source.Digest.of_hex history with
    | Ok history -> Ok history
    | Error _ -> invalid ()
  in
  create
    ~operation
    ~trigger
    ~boundary
    ~attempt_high_water
    ~configuration_revision
    ~configuration
    ~history
;;
