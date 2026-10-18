open! Core
module D = Document_schema
module S = Chatml_source
module P = Agent_protocol

let invalid name reason = Error (D.Error.Invalid_field { path = [ name ]; reason })
let ( let* ) result f = Result.bind result ~f

module Id = struct
  type t = P.Developer_command_id.t [@@deriving compare, equal, sexp_of]

  let of_string value =
    P.Developer_command_id.of_string value
    |> Result.map_error ~f:(fun _ ->
      D.Error.Invalid_field
        { path = [ "id" ]; reason = "command receipt ID must be 1..256 bytes" })
  ;;

  let to_string = P.Developer_command_id.to_string
end

module Kind = struct
  type t =
    | Check
    | Fixture_run
    | Live_run
    | Repl_task_run
  [@@deriving equal, sexp_of]
end

module Diagnostic = Developer_command_diagnostic

module Check = struct
  type t =
    | Ready
    | Rejected of Diagnostic.t
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

module Fixture = Developer_command_fixture

module Live = struct
  type t =
    | Succeeded of Jsonaf.t
    | Failed of P.Error.t
    | Limited of P.Error.t
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

module Task = struct
  type t =
    | Succeeded of { value_preview : string option }
    | Failed of
        { error : P.Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Limited of
        { error : P.Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Interrupted
    | Cancelled
  [@@deriving sexp_of]
end

type value =
  | Check of Check.t
  | Fixture of Fixture.t
  | Live of Live.t
  | Task of Task.t
[@@deriving sexp_of]

type t =
  { value : value
  ; encoded_bytes : int
  ; json : Jsonaf.t
  }

let value t = t.value
let encoded_bytes t = t.encoded_bytes
let to_json t = t.json

let kind t =
  match t.value with
  | Check _ -> Kind.Check
  | Fixture _ -> Fixture_run
  | Live _ -> Live_run
  | Task _ -> Repl_task_run
;;

let equal a b = D.Json.equal a.json b.json
let decimal n = `Number (Int.to_string n)
let digest d = `String (S.Digest.to_hex d)

let task_failure_json ~state ~error ~diagnostic =
  `Object
    ([ "kind", `String "repl_task_run"
     ; "state", `String state
     ; "error", P.Error.to_json error
     ]
     @
     match diagnostic with
     | None -> []
     | Some diagnostic -> [ "diagnostic", Diagnostic.to_json diagnostic ])
;;

let json_of_value ~poll = function
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
  | Fixture fixture -> Fixture.to_json fixture ~poll
  | Live (Succeeded value) ->
    `Object [ "kind", `String "live_run"; "state", `String "succeeded"; "value", value ]
  | Live (Failed error) ->
    `Object
      [ "kind", `String "live_run"
      ; "state", `String "failed"
      ; "error", P.Error.to_json error
      ]
  | Live (Limited error) ->
    `Object
      [ "kind", `String "live_run"
      ; "state", `String "limited"
      ; "error", P.Error.to_json error
      ]
  | Live Interrupted ->
    `Object [ "kind", `String "live_run"; "state", `String "interrupted" ]
  | Live Cancelled -> `Object [ "kind", `String "live_run"; "state", `String "cancelled" ]
  | Task (Succeeded { value_preview }) ->
    `Object
      ([ "kind", `String "repl_task_run"; "state", `String "succeeded" ]
       @
       match value_preview with
       | None -> []
       | Some preview -> [ "value_preview", `String preview ])
  | Task (Failed { error; diagnostic }) ->
    task_failure_json ~state:"failed" ~error ~diagnostic
  | Task (Limited { error; diagnostic }) ->
    task_failure_json ~state:"limited" ~error ~diagnostic
  | Task Interrupted ->
    `Object [ "kind", `String "repl_task_run"; "state", `String "interrupted" ]
  | Task Cancelled ->
    `Object [ "kind", `String "repl_task_run"; "state", `String "cancelled" ]
;;

let validate_value ~limits ~poll = function
  | Check (Rejected diagnostic) -> Diagnostic.validate diagnostic
  | Fixture fixture -> Fixture.validate fixture ~limits ~poll
  | Check (Ready | Interrupted | Cancelled)
  | Live (Succeeded _ | Failed _ | Limited _ | Interrupted | Cancelled) -> Ok ()
  | Task (Failed { diagnostic; _ } | Limited { diagnostic; _ }) ->
    Option.value_map diagnostic ~default:(Ok ()) ~f:Diagnostic.validate
  | Task (Succeeded _ | Interrupted | Cancelled) -> Ok ()
;;

let create value ~limits ~poll =
  let* () = validate_value ~limits ~poll value in
  let json = json_of_value value ~poll in
  let* encoded_bytes =
    D.Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:poll json
  in
  Ok { value; encoded_bytes; json }
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent | Null -> invalid name "required non-null field is absent"
;;

let present_field json name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some value -> Ok value
     | None -> invalid name "required field is absent")
  | _ -> invalid name "expected named object"
;;

let exact json ~required ~optional =
  match json with
  | `Object fields ->
    let names = List.map fields ~f:fst in
    let expected = required @ optional in
    if
      List.length names <> List.length (List.dedup_and_sort names ~compare:String.compare)
      || (not (List.for_all required ~f:(List.mem names ~equal:String.equal)))
      || not (List.for_all names ~f:(List.mem expected ~equal:String.equal))
    then invalid "fields" "missing, duplicate or unexpected member"
    else Ok ()
  | _ -> invalid "value" "expected named object"
;;

let text json name =
  let* value = field json name in
  match value with
  | `String text -> Ok text
  | _ -> invalid name "expected string"
;;

let nonnegative json name =
  let* value = field json name in
  match value with
  | `Number encoded ->
    (match Int.of_string_opt encoded with
     | Some number when number >= 0 && String.equal (Int.to_string number) encoded ->
       Ok number
     | Some _ | None -> invalid name "expected nonnegative decimal integer")
  | _ -> invalid name "expected decimal integer"
;;

let digest_field json name =
  let* hex = text json name in
  S.Digest.of_hex hex
  |> Result.map_error ~f:(fun _ ->
    D.Error.Invalid_field { path = [ name ]; reason = "invalid digest" })
;;

let decode_location json =
  let* () =
    exact
      json
      ~required:
        [ "namespace"; "member"; "digest"; "source_bytes"; "start_byte"; "end_byte" ]
      ~optional:[]
  in
  let* namespace = text json "namespace" in
  let* member = text json "member" in
  let* source =
    S.Id.create ~package_namespace:namespace ~relative_path:member
    |> Result.map_error ~f:(fun _ ->
      D.Error.Invalid_field { path = [ "location" ]; reason = "invalid source identity" })
  in
  let* digest = digest_field json "digest" in
  let* source_bytes = nonnegative json "source_bytes" in
  let* start_byte = nonnegative json "start_byte" in
  let* end_byte = nonnegative json "end_byte" in
  Ok { Diagnostic.Location.source; digest; source_bytes; start_byte; end_byte }
;;

let decode_diagnostic json =
  let* () = exact json ~required:[ "code"; "message" ] ~optional:[ "location" ] in
  let* code = text json "code" in
  let* message = text json "message" in
  let* location =
    match D.Json.field json ~name:"location" with
    | Absent -> Ok None
    | Null -> invalid "location" "explicit null is not absence"
    | Value value -> Result.map (decode_location value) ~f:Option.some
  in
  Ok { Diagnostic.code; message; location }
;;

let decode_failure json =
  let* kind = text json "kind" in
  match kind with
  | "compile" | "runtime" | "invalid_result" ->
    let* constructor =
      match kind with
      | "compile" -> Ok (fun diagnostic -> Fixture.Failure.Compile diagnostic)
      | "runtime" -> Ok (fun diagnostic -> Fixture.Failure.Runtime diagnostic)
      | "invalid_result" ->
        Ok (fun diagnostic -> Fixture.Failure.Invalid_result diagnostic)
      | _ -> invalid "kind" "unsupported fixture failure"
    in
    let* () = exact json ~required:[ "kind"; "diagnostic" ] ~optional:[] in
    let* diagnostic = field json "diagnostic" |> Result.bind ~f:decode_diagnostic in
    Ok (constructor diagnostic)
  | "fixture_mismatch" ->
    let* () =
      exact json ~required:[ "kind"; "expected_step"; "actual_tool" ] ~optional:[]
    in
    let* expected_step = nonnegative json "expected_step" in
    let* actual_tool = text json "actual_tool" in
    Ok (Fixture.Failure.Fixture_mismatch { expected_step; actual_tool })
  | "fixture_missing" ->
    let* () = exact json ~required:[ "kind"; "next_step" ] ~optional:[] in
    let* next_step = nonnegative json "next_step" in
    Ok (Fixture.Failure.Fixture_missing { next_step })
  | "unsupported_effect" ->
    let* () = exact json ~required:[ "kind"; "operation" ] ~optional:[] in
    let* operation = text json "operation" in
    Ok (Fixture.Failure.Unsupported_effect { operation })
  | "output_limit" ->
    let* () = exact json ~required:[ "kind" ] ~optional:[] in
    Ok Fixture.Failure.Output_limit
  | "pending_without_owner" ->
    let* () = exact json ~required:[ "kind" ] ~optional:[] in
    Ok Fixture.Failure.Pending_without_owner
  | "admission_revoked" ->
    let* () = exact json ~required:[ "kind" ] ~optional:[] in
    Ok Fixture.Failure.Admission_revoked
  | _ -> invalid "kind" "unsupported fixture failure"
;;

let decode_log json =
  let* () = exact json ~required:[ "level"; "message" ] ~optional:[] in
  let* level = text json "level" in
  let* level =
    match level with
    | "debug" -> Ok Fixture.Log.Level.Debug
    | "info" -> Ok Info
    | "warn" -> Ok Warn
    | "error" -> Ok Error
    | _ -> invalid "level" "unsupported log level"
  in
  let* message = text json "message" in
  Ok { Fixture.Log.level; message }
;;

let decode_logs json ~poll =
  let* value = field json "logs" in
  match value with
  | `Array logs ->
    Result.all
      (List.map logs ~f:(fun log ->
         poll ();
         decode_log log))
  | _ -> invalid "logs" "expected array"
;;

let decode_fixture_outcome json =
  let* kind = text json "kind" in
  match kind with
  | "succeeded" ->
    let* () = exact json ~required:[ "kind"; "value" ] ~optional:[] in
    let* value = present_field json "value" in
    Ok (Fixture.Succeeded value)
  | "failed" ->
    let* () = exact json ~required:[ "kind"; "failure" ] ~optional:[] in
    let* failure = field json "failure" |> Result.bind ~f:decode_failure in
    Ok (Fixture.Failed failure)
  | "tool_failed" ->
    let* () =
      exact
        json
        ~required:[ "kind"; "code"; "message"; "retryable"; "details" ]
        ~optional:[]
    in
    let* code = text json "code" in
    let* message = text json "message" in
    let* retryable =
      match D.Json.field json ~name:"retryable" with
      | Value `True -> Ok true
      | Value `False -> Ok false
      | Absent | Null | Value _ -> invalid "retryable" "expected boolean"
    in
    let* details = present_field json "details" in
    Ok
      (Fixture.Tool_failed { Agent_protocol.Invocation.code; message; retryable; details })
  | _ -> invalid "kind" "unsupported fixture outcome"
;;

let decode_fixture json ~poll =
  let* state = text json "state" in
  match state with
  | "interrupted" | "cancelled" ->
    let* () = exact json ~required:[ "kind"; "state" ] ~optional:[] in
    Ok
      (Fixture
         (if String.equal state "interrupted" then Fixture.Interrupted else Cancelled))
  | "completed" ->
    let* () =
      exact
        json
        ~required:
          [ "kind"
          ; "state"
          ; "graph_fingerprint"
          ; "fixture_digest"
          ; "matched_steps"
          ; "logs"
          ; "outcome"
          ]
        ~optional:[ "tool_selection_fingerprint" ]
    in
    let* graph_fingerprint = digest_field json "graph_fingerprint" in
    let* fixture_digest = digest_field json "fixture_digest" in
    let* tool_selection_fingerprint =
      match D.Json.field json ~name:"tool_selection_fingerprint" with
      | Absent -> Ok None
      | Null -> invalid "tool_selection_fingerprint" "explicit null is not absence"
      | Value _ ->
        Result.map (digest_field json "tool_selection_fingerprint") ~f:Option.some
    in
    let* matched_steps = nonnegative json "matched_steps" in
    let* logs = decode_logs json ~poll in
    let* outcome = field json "outcome" |> Result.bind ~f:decode_fixture_outcome in
    Ok
      (Fixture
         (Completed
            { Fixture.graph_fingerprint
            ; fixture_digest
            ; tool_selection_fingerprint
            ; matched_steps
            ; logs
            ; outcome
            }))
  | _ -> invalid "state" "unsupported fixture state"
;;

let decode_protocol_error json =
  P.Error.of_json json
  |> Result.map_error ~f:(fun _ ->
    D.Error.Invalid_field { path = [ "error" ]; reason = "invalid protocol error" })
;;

let decode_value json ~poll =
  let* kind = text json "kind" in
  let* state = text json "state" in
  match kind, state with
  | "check", "ready" | "check", "interrupted" | "check", "cancelled" ->
    let* () = exact json ~required:[ "kind"; "state" ] ~optional:[] in
    let* outcome =
      match state with
      | "ready" -> Ok Check.Ready
      | "interrupted" -> Ok Check.Interrupted
      | "cancelled" -> Ok Check.Cancelled
      | _ -> invalid "state" "unsupported check state"
    in
    Ok (Check outcome)
  | "check", "rejected" ->
    let* () = exact json ~required:[ "kind"; "state"; "diagnostic" ] ~optional:[] in
    let* diagnostic = field json "diagnostic" |> Result.bind ~f:decode_diagnostic in
    Ok (Check (Rejected diagnostic))
  | "fixture_run", _ -> decode_fixture json ~poll
  | "live_run", "succeeded" ->
    let* () = exact json ~required:[ "kind"; "state"; "value" ] ~optional:[] in
    let* value = present_field json "value" in
    Ok (Live (Succeeded value))
  | "live_run", "failed" | "live_run", "limited" ->
    let* () = exact json ~required:[ "kind"; "state"; "error" ] ~optional:[] in
    let* error = field json "error" |> Result.bind ~f:decode_protocol_error in
    Ok (Live (if String.equal state "failed" then Failed error else Limited error))
  | "live_run", "interrupted" | "live_run", "cancelled" ->
    let* () = exact json ~required:[ "kind"; "state" ] ~optional:[] in
    Ok (Live (if String.equal state "interrupted" then Interrupted else Cancelled))
  | "repl_task_run", "succeeded" ->
    let* () = exact json ~required:[ "kind"; "state" ] ~optional:[ "value_preview" ] in
    let* value_preview =
      match D.Json.field json ~name:"value_preview" with
      | Absent -> Ok None
      | Null -> invalid "value_preview" "explicit null is not absence"
      | Value (`String preview) -> Ok (Some preview)
      | Value _ -> invalid "value_preview" "expected string"
    in
    Ok (Task (Succeeded { value_preview }))
  | "repl_task_run", "failed" | "repl_task_run", "limited" ->
    let* () =
      exact json ~required:[ "kind"; "state"; "error" ] ~optional:[ "diagnostic" ]
    in
    let* error = field json "error" |> Result.bind ~f:decode_protocol_error in
    let* diagnostic =
      match D.Json.field json ~name:"diagnostic" with
      | Absent -> Ok None
      | Null -> invalid "diagnostic" "explicit null is not absence"
      | Value diagnostic -> Result.map (decode_diagnostic diagnostic) ~f:Option.some
    in
    Ok
      (Task
         (if String.equal state "failed"
          then Failed { error; diagnostic }
          else Limited { error; diagnostic }))
  | "repl_task_run", "interrupted" | "repl_task_run", "cancelled" ->
    let* () = exact json ~required:[ "kind"; "state" ] ~optional:[] in
    Ok (Task (if String.equal state "interrupted" then Interrupted else Cancelled))
  | _ -> invalid "kind" "unsupported receipt kind or state"
;;

let of_json json ~limits ~poll =
  let* _ = D.Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:poll json in
  let* value = decode_value json ~poll in
  let* receipt = create value ~limits ~poll in
  if D.Json.equal json (to_json receipt)
  then Ok receipt
  else invalid "receipt" "decoded fields changed during canonical projection"
;;
