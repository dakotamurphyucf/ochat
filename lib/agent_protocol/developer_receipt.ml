open! Core
module Error = Protocol_error
module C = Developer_codec
module S = Chatml_source
module D = Document_schema

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode
let text = C.string ~max_bytes:16_384
let decimal = C.integer_json

module Kind = struct
  type t =
    | Check
    | Fixture_run
    | Live_run
  [@@deriving compare, equal, sexp_of]
end

module Diagnostic = struct
  module Location = struct
    type t =
      { source : S.Id.t
      ; digest : S.Digest.t
      ; source_bytes : int
      ; start_byte : int
      ; end_byte : int
      }

    let create ~source ~digest ~source_bytes ~start_byte ~end_byte =
      let* () = Authoring_codec.validate_source_id source in
      if
        source_bytes < 0
        || source_bytes > Authoring_codec.maximum_wire_integer
        || start_byte < 0
        || start_byte > Authoring_codec.maximum_wire_integer
        || end_byte < start_byte
        || end_byte > Authoring_codec.maximum_wire_integer
        || end_byte > source_bytes
      then C.invalid "invalid developer diagnostic byte range"
      else Ok { source; digest; source_bytes; start_byte; end_byte }
    ;;

    let to_json t =
      `Object
        [ "source", C.source_id_json t.source
        ; "digest", C.digest_json t.digest
        ; "source_bytes", decimal t.source_bytes
        ; "start_byte", decimal t.start_byte
        ; "end_byte", decimal t.end_byte
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:[ "source"; "digest"; "source_bytes"; "start_byte"; "end_byte" ]
          json
      in
      let* source = get fields "source" C.source_id in
      let* digest = get fields "digest" C.digest in
      let* source_bytes = get fields "source_bytes" C.nonnegative in
      let* start_byte = get fields "start_byte" C.nonnegative in
      let* end_byte = get fields "end_byte" C.nonnegative in
      create ~source ~digest ~source_bytes ~start_byte ~end_byte
    ;;
  end

  type t =
    { code : string
    ; message : string
    ; location : Location.t option
    }

  let create ~code ~message ~location =
    if String.is_empty code || String.length code > 256 || String.length message > 16_384
    then C.invalid "invalid developer diagnostic scalar"
    else Ok { code; message; location }
  ;;

  let to_json t =
    `Object
      ([ "code", `String t.code; "message", `String t.message ]
       @ C.optional_field "location" t.location Location.to_json)
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "code"; "message"; "location" ] json in
    let* code = get fields "code" (C.string ~max_bytes:256) in
    let* message = get fields "message" text in
    let* location = C.optional fields "location" in
    let* location =
      Option.value_map location ~default:(Ok None) ~f:(fun value ->
        Result.map (Location.of_json value) ~f:Option.some)
    in
    create ~code ~message ~location
  ;;
end

module Check = struct
  type t =
    | Ready
    | Rejected of Diagnostic.t
    | Interrupted
    | Cancelled
end

module Fixture = struct
  module Failure = struct
    type t =
      | Compile of Diagnostic.t
      | Fixture_mismatch of
          { expected_step : int
          ; actual_tool : string
          }
      | Fixture_missing of { next_step : int }
      | Unsupported_effect of { operation : string }
      | Runtime of Diagnostic.t
      | Invalid_result of Diagnostic.t
      | Pending_without_owner
      | Admission_revoked
      | Output_limit
  end

  module Log = struct
    type level =
      | Debug
      | Info
      | Warn
      | Error

    type t =
      { level : level
      ; message : string
      }

    let create ~level ~message =
      if String.length message > 16_384
      then C.limit "developer fixture log exceeds bound"
      else Ok { level; message }
    ;;

    let level_string = function
      | Debug -> "debug"
      | Info -> "info"
      | Warn -> "warn"
      | Error -> "error"
    ;;

    let to_json t =
      `Object [ "level", `String (level_string t.level); "message", `String t.message ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "level"; "message" ] json in
      let* level = get fields "level" (C.string ~max_bytes:16) in
      let* level =
        match level with
        | "debug" -> Ok Debug
        | "info" -> Ok Info
        | "warn" -> Ok Warn
        | "error" -> Ok Error
        | _ -> C.invalid "unknown developer fixture log level"
      in
      let* message = get fields "message" text in
      create ~level ~message
    ;;
  end

  type outcome =
    | Succeeded of Jsonaf.t
    | Tool_failed of Invocation.tool_error
    | Failed of Failure.t

  type completed =
    { graph_fingerprint : S.Digest.t
    ; fixture_digest : S.Digest.t
    ; tool_selection_fingerprint : S.Digest.t option
    ; matched_steps : int
    ; logs : Log.t list
    ; outcome : outcome
    }

  let validate_failure =
    let open Failure in
    function
    | Compile _
    | Runtime _
    | Invalid_result _
    | Pending_without_owner
    | Admission_revoked
    | Output_limit -> Ok ()
    | Fixture_mismatch { expected_step; actual_tool } ->
      if
        expected_step < 0
        || expected_step > Authoring_codec.maximum_wire_integer
        || String.is_empty actual_tool
        || String.length actual_tool > 1024
      then C.invalid "invalid fixture mismatch"
      else Ok ()
    | Fixture_missing { next_step } ->
      if next_step < 0 || next_step > Authoring_codec.maximum_wire_integer
      then C.invalid "invalid missing fixture step"
      else Ok ()
    | Unsupported_effect { operation } ->
      if String.is_empty operation || String.length operation > 1024
      then C.invalid "invalid unsupported fixture operation"
      else Ok ()
  ;;

  let completed
        ~graph_fingerprint
        ~fixture_digest
        ~tool_selection_fingerprint
        ~matched_steps
        ~logs
        ~outcome
    =
    if matched_steps < 0 || matched_steps > Authoring_codec.maximum_wire_integer
    then C.invalid "negative developer fixture step count"
    else (
      match outcome with
      | Tool_failed error ->
        let* () = Invocation.validate_outcome (Fail error) in
        if Option.is_none tool_selection_fingerprint
        then C.invalid "Tool failure lacks selected ABI fingerprint"
        else
          Ok
            { graph_fingerprint
            ; fixture_digest
            ; tool_selection_fingerprint
            ; matched_steps
            ; logs
            ; outcome
            }
      | Failed failure ->
        let* () = validate_failure failure in
        if
          match failure with
          | Failure.Pending_without_owner -> Option.is_none tool_selection_fingerprint
          | _ -> false
        then C.invalid "pending Tool failure lacks selected ABI fingerprint"
        else
          Ok
            { graph_fingerprint
            ; fixture_digest
            ; tool_selection_fingerprint
            ; matched_steps
            ; logs
            ; outcome
            }
      | Succeeded _ ->
        Ok
          { graph_fingerprint
          ; fixture_digest
          ; tool_selection_fingerprint
          ; matched_steps
          ; logs
          ; outcome
          })
  ;;

  type t =
    | Completed of completed
    | Interrupted
    | Cancelled

  let failure_json =
    let open Failure in
    function
    | Failure.Compile diagnostic ->
      `Object [ "kind", `String "compile"; "diagnostic", Diagnostic.to_json diagnostic ]
    | Fixture_mismatch { expected_step; actual_tool } ->
      `Object
        [ "kind", `String "fixture_mismatch"
        ; "expected_step", decimal expected_step
        ; "actual_tool", `String actual_tool
        ]
    | Fixture_missing { next_step } ->
      `Object [ "kind", `String "fixture_missing"; "next_step", decimal next_step ]
    | Unsupported_effect { operation } ->
      `Object [ "kind", `String "unsupported_effect"; "operation", `String operation ]
    | Runtime diagnostic ->
      `Object [ "kind", `String "runtime"; "diagnostic", Diagnostic.to_json diagnostic ]
    | Invalid_result diagnostic ->
      `Object
        [ "kind", `String "invalid_result"; "diagnostic", Diagnostic.to_json diagnostic ]
    | Pending_without_owner -> `Object [ "kind", `String "pending_without_owner" ]
    | Admission_revoked -> `Object [ "kind", `String "admission_revoked" ]
    | Output_limit -> `Object [ "kind", `String "output_limit" ]
  ;;

  let failure_of_json json =
    let open Failure in
    let* fields =
      C.fields
        ~allowed:
          [ "kind"
          ; "diagnostic"
          ; "expected_step"
          ; "actual_tool"
          ; "next_step"
          ; "operation"
          ]
        json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:40) in
    let check allowed = C.fields ~allowed json |> Result.map ~f:ignore in
    match kind with
    | "compile" | "runtime" | "invalid_result" ->
      let* () = check [ "kind"; "diagnostic" ] in
      let* diagnostic = get fields "diagnostic" Diagnostic.of_json in
      (match kind with
       | "compile" -> Ok (Failure.Compile diagnostic)
       | "runtime" -> Ok (Runtime diagnostic)
       | "invalid_result" -> Ok (Invalid_result diagnostic)
       | _ -> assert false)
    | "fixture_mismatch" ->
      let* () = check [ "kind"; "expected_step"; "actual_tool" ] in
      let* expected_step = get fields "expected_step" C.nonnegative in
      let* actual_tool = get fields "actual_tool" (C.string ~max_bytes:1024) in
      Ok (Fixture_mismatch { expected_step; actual_tool })
    | "fixture_missing" ->
      let* () = check [ "kind"; "next_step" ] in
      let* next_step = get fields "next_step" C.nonnegative in
      Ok (Fixture_missing { next_step })
    | "unsupported_effect" ->
      let* () = check [ "kind"; "operation" ] in
      let* operation = get fields "operation" (C.string ~max_bytes:1024) in
      Ok (Unsupported_effect { operation })
    | "pending_without_owner" ->
      let* () = check [ "kind" ] in
      Ok Pending_without_owner
    | "admission_revoked" ->
      let* () = check [ "kind" ] in
      Ok Admission_revoked
    | "output_limit" ->
      let* () = check [ "kind" ] in
      Ok Output_limit
    | _ -> C.invalid "unknown developer fixture failure"
  ;;

  let outcome_json = function
    | Succeeded value -> `Object [ "kind", `String "succeeded"; "value", value ]
    | Tool_failed error ->
      `Object
        [ "kind", `String "tool_failed"
        ; "code", `String error.code
        ; "message", `String error.message
        ; ("retryable", if error.retryable then `True else `False)
        ; "details", error.details
        ]
    | Failed failure ->
      `Object [ "kind", `String "failed"; "failure", failure_json failure ]
  ;;

  let outcome_of_json json =
    let* fields =
      C.fields
        ~allowed:[ "kind"; "value"; "failure"; "code"; "message"; "retryable"; "details" ]
        json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    let check allowed = C.fields ~allowed json |> Result.map ~f:ignore in
    match kind with
    | "succeeded" ->
      let* () = check [ "kind"; "value" ] in
      let* value = Json_codec.required fields "value" in
      Ok (Succeeded value)
    | "failed" ->
      let* () = check [ "kind"; "failure" ] in
      let* failure = get fields "failure" failure_of_json in
      Ok (Failed failure)
    | "tool_failed" ->
      let* () = check [ "kind"; "code"; "message"; "retryable"; "details" ] in
      let* code = get fields "code" (C.string ~max_bytes:256) in
      let* message = get fields "message" text in
      let* retryable = get fields "retryable" C.boolean in
      let* details = Json_codec.required fields "details" in
      let error : Invocation.tool_error = { code; message; retryable; details } in
      let* () = Invocation.validate_outcome (Fail error) in
      Ok (Tool_failed error)
    | _ -> C.invalid "unknown developer fixture outcome"
  ;;

  let to_json = function
    | Interrupted ->
      `Object [ "kind", `String "fixture_run"; "state", `String "interrupted" ]
    | Cancelled -> `Object [ "kind", `String "fixture_run"; "state", `String "cancelled" ]
    | Completed completed ->
      `Object
        ([ "kind", `String "fixture_run"
         ; "state", `String "completed"
         ; "graph_fingerprint", C.digest_json completed.graph_fingerprint
         ; "fixture_digest", C.digest_json completed.fixture_digest
         ; "matched_steps", decimal completed.matched_steps
         ; "logs", `Array (List.map completed.logs ~f:Log.to_json)
         ; "outcome", outcome_json completed.outcome
         ]
         @ C.optional_field
             "tool_selection_fingerprint"
             completed.tool_selection_fingerprint
             C.digest_json)
  ;;

  let of_json ~limits json =
    let* fields =
      C.fields
        ~allowed:
          [ "kind"
          ; "state"
          ; "graph_fingerprint"
          ; "fixture_digest"
          ; "tool_selection_fingerprint"
          ; "matched_steps"
          ; "logs"
          ; "outcome"
          ]
        json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    let* state = get fields "state" (C.string ~max_bytes:32) in
    if not (String.equal kind "fixture_run")
    then C.invalid "not a developer fixture receipt"
    else (
      match state with
      | "interrupted" | "cancelled" ->
        let* () = C.fields ~allowed:[ "kind"; "state" ] json |> Result.map ~f:ignore in
        Ok (if String.equal state "interrupted" then Interrupted else Cancelled)
      | "completed" ->
        let* graph_fingerprint = get fields "graph_fingerprint" C.digest in
        let* fixture_digest = get fields "fixture_digest" C.digest in
        let* tool_selection_fingerprint =
          C.optional fields "tool_selection_fingerprint"
        in
        let* tool_selection_fingerprint =
          Option.value_map tool_selection_fingerprint ~default:(Ok None) ~f:(fun json ->
            Result.map (C.digest json) ~f:Option.some)
        in
        let* matched_steps = get fields "matched_steps" C.nonnegative in
        let* logs =
          get fields "logs" (C.array ~max_items:(D.Limits.max_nodes limits) Log.of_json)
        in
        let* outcome = get fields "outcome" outcome_of_json in
        let* completed =
          completed
            ~graph_fingerprint
            ~fixture_digest
            ~tool_selection_fingerprint
            ~matched_steps
            ~logs
            ~outcome
        in
        Ok (Completed completed)
      | _ -> C.invalid "unknown developer fixture state")
  ;;
end

module Live = struct
  type t =
    | Succeeded of Jsonaf.t
    | Failed of Error.t
    | Limited of Error.t
    | Interrupted
    | Cancelled
end

type value =
  | Check of Check.t
  | Fixture of Fixture.t
  | Live of Live.t

type t =
  { value : value
  ; json : Jsonaf.t
  ; encoded_bytes : int
  }

let value t = t.value
let encoded_bytes t = t.encoded_bytes
let to_json t = t.json
let equal a b = D.Json.equal a.json b.json

let kind t =
  match t.value with
  | Check _ -> Kind.Check
  | Fixture _ -> Fixture_run
  | Live _ -> Live_run
;;

let json_of_value = function
  | Check Check.Ready -> `Object [ "kind", `String "check"; "state", `String "ready" ]
  | Check (Rejected diagnostic) ->
    `Object
      [ "kind", `String "check"
      ; "state", `String "rejected"
      ; "diagnostic", Diagnostic.to_json diagnostic
      ]
  | Check Interrupted ->
    `Object [ "kind", `String "check"; "state", `String "interrupted" ]
  | Check Cancelled -> `Object [ "kind", `String "check"; "state", `String "cancelled" ]
  | Fixture fixture -> Fixture.to_json fixture
  | Live (Succeeded json) ->
    `Object [ "kind", `String "live_run"; "state", `String "succeeded"; "value", json ]
  | Live (Failed error) ->
    `Object
      [ "kind", `String "live_run"
      ; "state", `String "failed"
      ; "error", Error.to_json error
      ]
  | Live (Limited error) ->
    `Object
      [ "kind", `String "live_run"
      ; "state", `String "limited"
      ; "error", Error.to_json error
      ]
  | Live Interrupted ->
    `Object [ "kind", `String "live_run"; "state", `String "interrupted" ]
  | Live Cancelled -> `Object [ "kind", `String "live_run"; "state", `String "cancelled" ]
;;

let create ~limits value =
  let* () =
    match value with
    | Fixture (Fixture.Completed completed)
      when List.length completed.logs >= D.Limits.max_nodes limits ->
      C.limit "developer fixture log count exceeds receipt bound"
    | Check _ | Fixture _ | Live _ -> Ok ()
  in
  let json = json_of_value value in
  let* encoded_bytes = C.measure ~limits json in
  Ok { value; json; encoded_bytes }
;;

let of_json ~limits json =
  let* () = C.validate ~limits json in
  let* fields =
    C.fields
      ~allowed:
        [ "kind"
        ; "state"
        ; "diagnostic"
        ; "value"
        ; "error"
        ; "graph_fingerprint"
        ; "fixture_digest"
        ; "tool_selection_fingerprint"
        ; "matched_steps"
        ; "logs"
        ; "outcome"
        ]
      json
  in
  let* kind = get fields "kind" (C.string ~max_bytes:32) in
  let* state = get fields "state" (C.string ~max_bytes:32) in
  let check allowed = C.fields ~allowed json |> Result.map ~f:ignore in
  let* value =
    match kind, state with
    | "check", "ready" ->
      let* () = check [ "kind"; "state" ] in
      Ok (Check Check.Ready)
    | "check", "rejected" ->
      let* () = check [ "kind"; "state"; "diagnostic" ] in
      let* diagnostic = get fields "diagnostic" Diagnostic.of_json in
      Ok (Check (Rejected diagnostic))
    | "check", "interrupted" ->
      let* () = check [ "kind"; "state" ] in
      Ok (Check Interrupted)
    | "check", "cancelled" ->
      let* () = check [ "kind"; "state" ] in
      Ok (Check Cancelled)
    | "fixture_run", _ ->
      Result.map (Fixture.of_json ~limits json) ~f:(fun v -> Fixture v)
    | "live_run", "succeeded" ->
      let* () = check [ "kind"; "state"; "value" ] in
      let* output = Json_codec.required fields "value" in
      Ok (Live (Succeeded output))
    | "live_run", "failed" | "live_run", "limited" ->
      let* () = check [ "kind"; "state"; "error" ] in
      let* error =
        get fields "error" (fun json ->
          let* _ = C.fields ~allowed:[ "code"; "message"; "retryable"; "data" ] json in
          Error.of_json json)
      in
      Ok (Live (if String.equal state "failed" then Failed error else Limited error))
    | "live_run", "interrupted" ->
      let* () = check [ "kind"; "state" ] in
      Ok (Live Interrupted)
    | "live_run", "cancelled" ->
      let* () = check [ "kind"; "state" ] in
      Ok (Live Cancelled)
    | _ -> C.invalid "unknown developer receipt kind or state"
  in
  let* projected = create ~limits value in
  if D.Json.equal json projected.json
  then Ok projected
  else C.invalid "developer receipt changed during canonical projection"
;;
