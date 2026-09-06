open Core

type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup
  | Recovery_retry
  | Administrative
[@@deriving compare, equal, sexp]

type kind =
  | Turn of turn_start_reason
  | Compaction
[@@deriving compare, equal, sexp]

type state =
  | Starting
  | Running
  | Cancelling
  | Completed
  | Failed of Protocol_error.t
  | Cancelled
  | Interrupted of
      { reason : string
      ; retryable : bool
      }
[@@deriving sexp]

type t =
  { id : Id.Operation.t
  ; generation : int
  ; kind : kind
  ; state : state
  ; started_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving sexp]

let reason_to_string = function
  | User_submit -> "user_submit"
  | Moderator_request -> "moderator_request"
  | Idle_followup -> "idle_followup"
  | Recovery_retry -> "recovery_retry"
  | Administrative -> "administrative"
;;

let reason_of_json =
  Json_codec.enum
    ~name:"turn start reason"
    [ "user_submit", User_submit
    ; "moderator_request", Moderator_request
    ; "idle_followup", Idle_followup
    ; "recovery_retry", Recovery_retry
    ; "administrative", Administrative
    ]
;;

let kind_to_json = function
  | Turn reason ->
    `Object [ "type", `String "turn"; "reason", `String (reason_to_string reason) ]
  | Compaction -> `Object [ "type", `String "compaction" ]
;;

let kind_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "turn" ->
    Result.map (Json_codec.required_as fields "reason" reason_of_json) ~f:(fun reason ->
      Turn reason)
  | "compaction" -> Ok Compaction
  | _ -> Error (Protocol_error.invalid_request "unknown operation kind")
;;

let state_to_json = function
  | Starting -> `Object [ "type", `String "starting" ]
  | Running -> `Object [ "type", `String "running" ]
  | Cancelling -> `Object [ "type", `String "cancelling" ]
  | Completed -> `Object [ "type", `String "completed" ]
  | Failed error ->
    `Object [ "type", `String "failed"; "error", Protocol_error.to_json error ]
  | Cancelled -> `Object [ "type", `String "cancelled" ]
  | Interrupted { reason; retryable } ->
    `Object
      [ "type", `String "interrupted"
      ; "reason", `String reason
      ; ("retryable", if retryable then `True else `False)
      ]
;;

let interrupted_of_fields fields =
  let open Result.Let_syntax in
  let%bind reason = Json_codec.required_as fields "reason" Json_codec.string in
  let%map retryable = Json_codec.required_as fields "retryable" Json_codec.bool in
  Interrupted { reason; retryable }
;;

let state_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "starting" -> Ok Starting
  | "running" -> Ok Running
  | "cancelling" -> Ok Cancelling
  | "completed" -> Ok Completed
  | "failed" ->
    Result.map (Json_codec.required_as fields "error" Protocol_error.of_json) ~f:(fun e ->
      Failed e)
  | "cancelled" -> Ok Cancelled
  | "interrupted" -> interrupted_of_fields fields
  | _ -> Error (Protocol_error.invalid_request "unknown operation state")
;;

let to_json t =
  `Object
    [ "id", Id.Operation.to_json t.id
    ; "generation", `Number (Int.to_string t.generation)
    ; "kind", kind_to_json t.kind
    ; "state", state_to_json t.state
    ; "started_at", Timestamp.to_json t.started_at
    ; "updated_at", Timestamp.to_json t.updated_at
    ]
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Operation.of_json in
  let%map generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  id, generation
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, generation = decode_identity fields in
  let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
  let%bind state = Json_codec.required_as fields "state" state_of_json in
  let%bind started_at = Json_codec.required_as fields "started_at" Timestamp.of_json in
  let%bind updated_at = Json_codec.required_as fields "updated_at" Timestamp.of_json in
  if Timestamp.compare updated_at started_at < 0
  then Error (Protocol_error.invalid_request "operation update precedes its start")
  else Ok { id; generation; kind; state; started_at; updated_at }
;;
