open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module I = Recurrence_identity

type t =
  { key : I.Alarm.t
  ; slot : int64
  ; due_at : Time_ns.Alternate_sexp.t
  }
[@@deriving equal, sexp_of]

let key t = t.key
let slot t = t.slot
let due_at t = t.due_at
let invalid message = Error (P.Error.invalid_request message)

let create ~key ~slot ~due_at =
  if Int64.(slot < 0L)
  then invalid "negative recurrence alarm slot"
  else Ok { key; slot; due_at }
;;

let to_json t =
  `Object
    [ "key", I.Alarm.to_json t.key
    ; "slot", X.int64_json t.slot
    ; "due_at", P.Timestamp.to_json (P.Timestamp.of_time_ns t.due_at)
    ]
;;

let limits =
  D.Limits.create ~max_bytes:8192 ~max_depth:16 ~max_fields:64 ~max_nodes:128
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind key = X.required fields "key" I.Alarm.of_json in
  let%bind slot =
    X.required fields "slot" (function
      | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
      | _ -> invalid "invalid recurrence alarm slot")
  in
  let%bind due_at = X.required fields "due_at" P.Timestamp.of_json in
  create ~key ~slot ~due_at:(P.Timestamp.to_time_ns due_at)
;;

let shape =
  X.shape_exn [ "key", I.Alarm.shape; "slot", D.Shape.value; "due_at", D.Shape.value ]
;;
