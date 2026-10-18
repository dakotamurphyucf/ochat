open! Core
module P = Agent_protocol
module Stored = Agent_store.Developer_command_receipt
module Wire = P.Developer_receipt
module D = Document_schema

let checked_add left right =
  if left < 0 || right < 0 || left > Int.max_value - right
  then None
  else Some (left + right)
;;

let required_limits ~stored =
  (* A protocol source namespace is at most 1,024 bytes. Its base64 length
     is at most 4 * ceil(1,024 / 3) = 1,368; the original JSON string is at
     least 1,024 bytes before escaping. The nested source field names and
     punctuation add 18 more bytes: 1,368 - 1,024 + 18 = 362. There is at
     most one diagnostic location in a command receipt. Document_schema
     counts fields across the whole tree, not per object. *)
  let open Option.Let_syntax in
  let required =
    let%bind max_bytes = checked_add (D.Limits.max_bytes stored) 362 in
    let%bind max_depth = checked_add (D.Limits.max_depth stored) 1 in
    let%bind max_fields = checked_add (D.Limits.max_fields stored) 1 in
    let%map max_nodes = checked_add (D.Limits.max_nodes stored) 1 in
    max_bytes, max_depth, max_fields, max_nodes
  in
  match required with
  | None -> Error (P.Error.invalid_request "developer receipt wire bound overflowed")
  | Some (max_bytes, max_depth, max_fields, max_nodes) ->
    D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "developer receipt wire bound is unrepresentable")
;;

let validate_prepared prepared =
  Chatml_prepared_program.sources prepared
  |> List.fold_result ~init:() ~f:(fun () source ->
    P.Authoring_codec.validate_source_id (Chatml_source.id source))
;;

let diagnostic (value : Stored.Diagnostic.t) =
  let open Result.Let_syntax in
  let%bind location =
    match value.location with
    | None -> Ok None
    | Some location ->
      let%map location =
        Wire.Diagnostic.Location.create
          ~source:location.source
          ~digest:location.digest
          ~source_bytes:location.source_bytes
          ~start_byte:location.start_byte
          ~end_byte:location.end_byte
      in
      Some location
  in
  Wire.Diagnostic.create ~code:value.code ~message:value.message ~location
;;

let check = function
  | Stored.Check.Ready -> Ok Wire.Check.Ready
  | Rejected value ->
    Result.map (diagnostic value) ~f:(fun value -> Wire.Check.Rejected value)
  | Interrupted -> Ok Wire.Check.Interrupted
  | Cancelled -> Ok Wire.Check.Cancelled
;;

let failure = function
  | Stored.Fixture.Failure.Compile value ->
    Result.map (diagnostic value) ~f:(fun value -> Wire.Fixture.Failure.Compile value)
  | Fixture_mismatch { expected_step; actual_tool } ->
    Ok (Wire.Fixture.Failure.Fixture_mismatch { expected_step; actual_tool })
  | Fixture_missing { next_step } ->
    Ok (Wire.Fixture.Failure.Fixture_missing { next_step })
  | Unsupported_effect { operation } ->
    Ok (Wire.Fixture.Failure.Unsupported_effect { operation })
  | Runtime value ->
    Result.map (diagnostic value) ~f:(fun value -> Wire.Fixture.Failure.Runtime value)
  | Invalid_result value ->
    Result.map (diagnostic value) ~f:(fun value ->
      Wire.Fixture.Failure.Invalid_result value)
  | Pending_without_owner -> Ok Wire.Fixture.Failure.Pending_without_owner
  | Admission_revoked -> Ok Wire.Fixture.Failure.Admission_revoked
  | Output_limit -> Ok Wire.Fixture.Failure.Output_limit
;;

let log (value : Stored.Fixture.Log.t) =
  let level =
    match value.level with
    | Stored.Fixture.Log.Level.Debug -> Wire.Fixture.Log.Debug
    | Info -> Info
    | Warn -> Warn
    | Error -> Error
  in
  Wire.Fixture.Log.create ~level ~message:value.message
;;

let fixture = function
  | Stored.Fixture.Interrupted -> Ok Wire.Fixture.Interrupted
  | Cancelled -> Ok Wire.Fixture.Cancelled
  | Completed completed ->
    let open Result.Let_syntax in
    let%bind logs = List.map completed.logs ~f:log |> Result.all in
    let%bind outcome =
      match completed.outcome with
      | Stored.Fixture.Succeeded value -> Ok (Wire.Fixture.Succeeded value)
      | Tool_failed value -> Ok (Wire.Fixture.Tool_failed value)
      | Failed value ->
        Result.map (failure value) ~f:(fun value -> Wire.Fixture.Failed value)
    in
    let%map completed =
      Wire.Fixture.completed
        ~graph_fingerprint:completed.graph_fingerprint
        ~fixture_digest:completed.fixture_digest
        ~tool_selection_fingerprint:completed.tool_selection_fingerprint
        ~matched_steps:completed.matched_steps
        ~logs
        ~outcome
    in
    Wire.Fixture.Completed completed
;;

let live = function
  | Stored.Live.Succeeded value -> Wire.Live.Succeeded value
  | Failed error -> Wire.Live.Failed error
  | Limited error -> Wire.Live.Limited error
  | Interrupted -> Wire.Live.Interrupted
  | Cancelled -> Wire.Live.Cancelled
;;

let project receipt ~limits =
  let open Result.Let_syntax in
  let%bind value =
    match Stored.value receipt with
    | Stored.Check value -> Result.map (check value) ~f:(fun value -> Wire.Check value)
    | Fixture value -> Result.map (fixture value) ~f:(fun value -> Wire.Fixture value)
    | Live value -> Ok (Wire.Live (live value))
    | Task _ ->
      Error
        (P.Error.create
           P.Error.Permission_denied
           ~message:"REPL Task row is not a Developer command"
           ~retryable:false
           ())
  in
  Wire.create ~limits value
;;
