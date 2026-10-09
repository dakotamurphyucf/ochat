open! Core
module P = Agent_protocol

module Retirement_reason = struct
  type t =
    | Source_reset
    | Source_replaced
    | Canonical_history_retired
  [@@deriving equal, sexp]

  let to_json = function
    | Source_reset -> `String "source_reset"
    | Source_replaced -> `String "source_replaced"
    | Canonical_history_retired -> `String "canonical_history_retired"
  ;;

  let of_json =
    P.Json_codec.enum
      ~name:"pending retirement reason"
      [ "source_reset", Source_reset
      ; "source_replaced", Source_replaced
      ; "canonical_history_retired", Canonical_history_retired
      ]
  ;;
end

module Outcome = struct
  type t =
    | Adopted of P.History.Content_revision.t
    | Cancelled
    | Retired of Retirement_reason.t
  [@@deriving equal, sexp]

  let to_json = function
    | Adopted revision ->
      `Object
        [ "kind", `String "adopted"
        ; "content_revision", P.History.Content_revision.to_json revision
        ]
    | Cancelled -> `Object [ "kind", `String "cancelled" ]
    | Retired reason ->
      `Object [ "kind", `String "retired"; "reason", Retirement_reason.to_json reason ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = P.Json_codec.fields json in
    let%bind kind = P.Json_codec.required_as fields "kind" P.Json_codec.string in
    match kind with
    | "adopted" ->
      P.Json_codec.required_as
        fields
        "content_revision"
        P.History.Content_revision.of_json
      |> Result.map ~f:(fun revision -> Adopted revision)
    | "cancelled" -> Ok Cancelled
    | "retired" ->
      P.Json_codec.required_as fields "reason" Retirement_reason.of_json
      |> Result.map ~f:(fun reason -> Retired reason)
    | _ -> Error (P.Error.invalid_request "unsupported pending disposition")
  ;;
end

type t =
  { history_id : P.History.Id.t
  ; generation : int
  ; pending_revision : P.Pending_input.Revision.t
  ; outcome : Outcome.t
  }
[@@deriving equal, sexp]

let create ~history_id ~generation ~pending_revision ~outcome =
  if generation < 0
  then
    Error (P.Error.invalid_request "pending disposition generation must be nonnegative")
  else Ok { history_id; generation; pending_revision; outcome }
;;

let unchecked_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = unchecked_of_sexp sexp in
  match
    create
      ~history_id:value.history_id
      ~generation:value.generation
      ~pending_revision:value.pending_revision
      ~outcome:value.outcome
  with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;

let history_id t = t.history_id
let generation t = t.generation
let pending_revision t = t.pending_revision
let outcome t = t.outcome

let to_json t =
  `Object
    [ "history_id", P.History.Id.to_json t.history_id
    ; "generation", `Number (Int.to_string t.generation)
    ; "pending_revision", P.Pending_input.Revision.to_json t.pending_revision
    ; "outcome", Outcome.to_json t.outcome
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind history_id =
    P.Json_codec.required_as fields "history_id" P.History.Id.of_json
  in
  let%bind generation =
    P.Json_codec.required_as
      fields
      "generation"
      (P.Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind pending_revision =
    P.Json_codec.required_as fields "pending_revision" P.Pending_input.Revision.of_json
  in
  let%bind outcome = P.Json_codec.required_as fields "outcome" Outcome.of_json in
  create ~history_id ~generation ~pending_revision ~outcome
;;

module Retention = struct
  type disposition = t
  type t = { max_records : int }

  let create ~max_records =
    if max_records <= 0
    then Error (P.Error.invalid_request "pending disposition retention must be positive")
    else Ok { max_records }
  ;;

  let default = { max_records = 4096 }
  let max_records t = t.max_records
  let retain t values = List.take values t.max_records
end
