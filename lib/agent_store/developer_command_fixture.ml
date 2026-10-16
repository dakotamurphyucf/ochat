open! Core
module D = Document_schema
module S = Chatml_source
module Diagnostic = Developer_command_diagnostic

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
  [@@deriving sexp_of]
end

module Log = struct
  module Level = struct
    type t =
      | Debug
      | Info
      | Warn
      | Error
    [@@deriving equal, sexp_of]
  end

  type t =
    { level : Level.t
    ; message : string
    }
  [@@deriving sexp_of]
end

type outcome =
  | Succeeded of Jsonaf.t
  | Tool_failed of Agent_protocol.Invocation.tool_error
  | Failed of Failure.t
[@@deriving sexp_of]

type completed =
  { graph_fingerprint : S.Digest.t
  ; fixture_digest : S.Digest.t
  ; tool_selection_fingerprint : S.Digest.t option
  ; matched_steps : int
  ; logs : Log.t list
  ; outcome : outcome
  }
[@@deriving sexp_of]

type t =
  | Completed of completed
  | Interrupted
  | Cancelled
[@@deriving sexp_of]

let invalid name reason = Error (D.Error.Invalid_field { path = [ name ]; reason })
let ( let* ) result f = Result.bind result ~f

let bounded_text ~name value =
  if String.is_empty value || String.length value > 1024
  then invalid name "field must be 1..1024 bytes"
  else Ok ()
;;

let validate_failure = function
  | Failure.Compile diagnostic | Runtime diagnostic | Invalid_result diagnostic ->
    Diagnostic.validate diagnostic
  | Fixture_mismatch { expected_step; actual_tool } ->
    if expected_step < 0
    then invalid "expected_step" "negative fixture step"
    else bounded_text ~name:"actual_tool" actual_tool
  | Fixture_missing { next_step } ->
    if next_step < 0 then invalid "next_step" "negative fixture step" else Ok ()
  | Unsupported_effect { operation } -> bounded_text ~name:"operation" operation
  | Pending_without_owner | Admission_revoked | Output_limit -> Ok ()
;;

let validate t ~limits ~poll =
  match t with
  | Interrupted | Cancelled -> Ok ()
  | Completed complete ->
    if complete.matched_steps < 0
    then invalid "matched_steps" "negative matched fixture count"
    else if
      List.fold_until
        complete.logs
        ~init:0
        ~f:(fun count _ ->
          poll ();
          if count >= D.Limits.max_nodes limits then Stop true else Continue (count + 1))
        ~finish:(fun _ -> false)
    then Error (D.Error.Limit_exceeded "fixture log count")
    else (
      match complete.outcome with
      | Succeeded _ -> Ok ()
      | Tool_failed error ->
        if Option.is_none complete.tool_selection_fingerprint
        then invalid "tool_selection_fingerprint" "Tool_v1 failure has no selection"
        else
          Agent_protocol.Invocation.validate_outcome (Fail error)
          |> Result.map_error ~f:(fun _ ->
            D.Error.Invalid_field
              { path = [ "outcome" ]; reason = "invalid Tool_v1 failure" })
      | Failed Pending_without_owner
        when Option.is_none complete.tool_selection_fingerprint ->
        invalid "tool_selection_fingerprint" "Tool_v1 Pending has no selection"
      | Failed failure -> validate_failure failure)
;;

let failure_json = function
  | Failure.Compile diagnostic ->
    `Object [ "kind", `String "compile"; "diagnostic", Diagnostic.to_json diagnostic ]
  | Fixture_mismatch { expected_step; actual_tool } ->
    `Object
      [ "kind", `String "fixture_mismatch"
      ; "expected_step", `Number (Int.to_string expected_step)
      ; "actual_tool", `String actual_tool
      ]
  | Fixture_missing { next_step } ->
    `Object
      [ "kind", `String "fixture_missing"
      ; "next_step", `Number (Int.to_string next_step)
      ]
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

let level_string = function
  | Log.Level.Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"
;;

let to_json t ~poll =
  match t with
  | Interrupted ->
    `Object [ "kind", `String "fixture_run"; "state", `String "interrupted" ]
  | Cancelled -> `Object [ "kind", `String "fixture_run"; "state", `String "cancelled" ]
  | Completed complete ->
    let outcome =
      match complete.outcome with
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
    in
    `Object
      ([ "kind", `String "fixture_run"
       ; "state", `String "completed"
       ; "graph_fingerprint", `String (S.Digest.to_hex complete.graph_fingerprint)
       ; "fixture_digest", `String (S.Digest.to_hex complete.fixture_digest)
       ; "matched_steps", `Number (Int.to_string complete.matched_steps)
       ; ( "logs"
         , `Array
             (List.map complete.logs ~f:(fun (log : Log.t) ->
                poll ();
                `Object
                  [ "level", `String (level_string log.level)
                  ; "message", `String log.message
                  ])) )
       ; "outcome", outcome
       ]
       @
       match complete.tool_selection_fingerprint with
       | None -> []
       | Some fingerprint ->
         [ "tool_selection_fingerprint", `String (S.Digest.to_hex fingerprint) ])
;;
