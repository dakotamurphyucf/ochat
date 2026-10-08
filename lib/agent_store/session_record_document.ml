open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = P.Session.t

let v = D.Shape.value
let o = F.shape
let f names = o (List.map names ~f:(fun name -> name, v))

let tag discriminator cases =
  D.Shape.tagged_object ~discriminator cases
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let cases field names = List.map names ~f:(fun name -> name, f [ field ])
let error = f [ "code"; "message"; "retryable"; "data" ]

let observed =
  tag
    "type"
    (cases
       "type"
       [ "stopped"; "queued_for_slot"; "starting"; "recovering"; "idle"; "stopping" ]
     @ [ "running_turn", f [ "type"; "operation_id" ]
       ; "compacting", f [ "type"; "operation_id" ]
       ; "waiting_for_permission", f [ "type"; "permission_id" ]
       ; "failed", o [ "type", v; "error", error ]
       ])
;;

let prompt =
  tag
    "type"
    [ "catalog", f [ "type"; "prompt_id" ]
    ; "local_path", f [ "type"; "path" ]
    ; "generated", f [ "type"; "revision_id" ]
    ]
;;

let workspace_request =
  tag
    "type"
    [ "current", f [ "type" ]
    ; "configured", f [ "type"; "workspace_id" ]
    ; "local_path", f [ "type"; "path" ]
    ]
;;

let liveness =
  tag
    "type"
    [ "detached", f [ "type" ]
    ; "process_bound", f [ "type" ]
    ; "owner_bound", f [ "type"; "disconnect_grace_ms"; "stop_mode" ]
    ]
;;

let protocol_spec =
  o
    [ "execution_host", v
    ; "prompt", prompt
    ; "workspace", workspace_request
    ; "liveness", liveness
    ; "persistence", v
    ; "permission_profile", v
    ; "start_immediately", v
    ; "display_name", v
    ; "labels", v
    ]
;;

let operation =
  o
    [ "id", v
    ; "generation", v
    ; "kind", tag "type" [ "turn", f [ "type"; "reason" ]; "compaction", f [ "type" ] ]
    ; ( "state"
      , tag
          "type"
          (cases "type" [ "starting"; "running"; "cancelling"; "completed"; "cancelled" ]
           @ [ "failed", o [ "type", v; "error", error ]
             ; "interrupted", f [ "type"; "reason"; "retryable" ]
             ]) )
    ; "started_at", v
    ; "updated_at", v
    ]
;;

let metric_sum = tag "kind" [ "tokens", f [ "kind"; "tokens" ]; "overflow", f [ "kind" ] ]

let metric =
  o
    [ "actual", metric_sum
    ; "actual_attempts", v
    ; "estimated", metric_sum
    ; "estimated_attempts", v
    ; "mixed_estimators", v
    ; ( "unknown"
      , f
          [ "not_reported"
          ; "explicit_null"
          ; "interrupted"
          ; "not_submitted"
          ; "before_tracking"
          ] )
    ]
;;

let inference_summary =
  o
    [ "retained_attempts", v
    ; "turns", f [ "pending"; "completed"; "failed"; "cancelled"; "interrupted" ]
    ; ( "components"
      , o
          (List.map
             [ "input"
             ; "output"
             ; "reported_total"
             ; "cached_input"
             ; "cache_write_input"
             ; "reasoning_output"
             ]
             ~f:(fun name -> name, metric)) )
    ; ( "coverage"
      , f
          [ "before_tracking_unknown"
          ; "retired_attempts"
          ; "untracked_attempts"
          ; "retired_turns"
          ; "untracked_turns"
          ; "tracking_limit"
          ] )
    ; "accounting_revision", v
    ]
;;

let shape =
  o
    [ "id", v
    ; "creator", v
    ; "created_at", v
    ; "updated_at", v
    ; "generation", v
    ; "spec", protocol_spec
    ; "desired_state", v
    ; "observed_state", observed
    ; "prompt_revision", v
    ; "workspace_instance", v
    ; "active_operation", operation
    ; "revision", v
    ; "metadata_revision", v
    ; "latest_event_sequence", v
    ; "inference_summary", D.Shape.nullable inference_summary
    ]
;;

let counters = [ "revision"; "metadata_revision"; "latest_event_sequence" ]

let of_json json =
  let open Result.Let_syntax in
  let%bind fields =
    match json with
    | `Object fields -> Ok fields
    | _ -> F.invalid "session" "expected object"
  in
  let%bind fields =
    List.fold_result counters ~init:fields ~f:(fun fields name ->
      let%map value = F.required json name F.decimal in
      List.Assoc.add fields ~equal:String.equal name (`Number (Int64.to_string value)))
  in
  P.Session.of_json (`Object fields) |> F.protocol
;;

let to_json value =
  let open Result.Let_syntax in
  let json = P.Session.to_json value in
  let%bind _ = P.Session.of_json json |> F.protocol in
  match json with
  | `Object fields ->
    Ok
      (`Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if List.mem counters name ~equal:String.equal
               then (
                 match value with
                 | `Number text -> `String text
                 | _ -> value)
               else value ))))
  | _ -> F.invalid "session" "expected object"
;;

let stored_id json =
  F.required json "id" (fun value -> P.Id.Session.of_json value |> F.protocol)
;;
