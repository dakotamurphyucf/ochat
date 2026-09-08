open Core
open Extension_codec

type t =
  | Succeeded of Jsonaf.t
  | Failed of Invocation.tool_error
  | Cancelled of string
  | Expired
[@@deriving sexp]

type wake =
  | Request_turn
  | Next_turn
  | No_wake
[@@deriving compare, equal, sexp]

let validate = function
  | Succeeded value -> Invocation.validate_outcome (Complete value)
  | Failed error -> Invocation.validate_outcome (Fail error)
  | Cancelled reason -> Invocation.validate_outcome (Cancelled reason)
  | Expired -> Ok ()
;;

let rename_type json kind =
  match json with
  | `Object fields ->
    `Object (List.Assoc.add fields ~equal:String.equal "type" (`String kind))
  | _ -> assert false
;;

let to_json = function
  | Succeeded value ->
    rename_type (Invocation.outcome_to_json (Complete value)) "succeeded"
  | Failed error -> rename_type (Invocation.outcome_to_json (Fail error)) "failed"
  | Cancelled reason -> Invocation.outcome_to_json (Cancelled reason)
  | Expired -> `Object [ "type", `String "expired" ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(9 * 1024 * 1024) ~max_depth:132 json in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "expired" ->
    let%map () = closed fields [ "type" ] in
    Expired
  | "succeeded" | "failed" | "cancelled" ->
    let mapped =
      match kind with
      | "succeeded" -> "complete"
      | "failed" -> "fail"
      | _ -> "cancelled"
    in
    let%bind outcome = Invocation.outcome_of_json (rename_type json mapped) in
    (match outcome with
     | Complete value -> Ok (Succeeded value)
     | Fail error -> Ok (Failed error)
     | Cancelled reason -> Ok (Cancelled reason)
     | Pending _ -> invalid "completion cannot be pending")
  | _ -> invalid "unknown completion outcome"
;;

let wake_to_json = function
  | Request_turn -> `String "request_turn"
  | Next_turn -> `String "next_turn"
  | No_wake -> `String "no_wake"
;;

let wake_of_json =
  Json_codec.enum
    ~name:"completion wake policy"
    [ "request_turn", Request_turn; "next_turn", Next_turn; "no_wake", No_wake ]
;;
