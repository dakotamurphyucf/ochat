open Core
module P = Agent_protocol

type t =
  { status :
      Native_tool_invocation.borrowed
      -> P.Id.Session.t
      -> (Jsonaf.t, P.Invocation.tool_error) result
  }

let status_json (state : Session_state.t) =
  let observed =
    match state.lifecycle.observed with
    | Stopped -> "stopped"
    | Queued_for_slot -> "queued"
    | Starting -> "starting"
    | Recovering -> "recovering"
    | Idle -> "idle"
    | Running_turn _ -> "running"
    | Compacting _ -> "compacting"
    | Waiting_for_permission _ -> "waiting_for_permission"
    | Stopping -> "stopping"
    | Failed _ -> "failed"
  in
  let operation =
    match state.active_operation with
    | None -> `Null
    | Some operation ->
      let kind =
        match operation.kind with
        | Turn _ -> "turn"
        | Compaction -> "compaction"
      in
      let status =
        match operation.state with
        | Starting -> "starting"
        | Running -> "running"
        | Cancelling -> "cancelling"
        | Completed -> "completed"
        | Failed _ -> "failed"
        | Cancelled -> "cancelled"
        | Interrupted _ -> "interrupted"
      in
      `Object
        [ "id", `String (P.Id.Operation.to_string operation.id)
        ; "kind", `String kind
        ; "state", `String status
        ]
  in
  let waiting_permissions =
    List.count state.permissions ~f:(fun permission ->
      match permission.P.Permission.state with
      | Pending -> true
      | _ -> false)
  in
  `Object
    [ "version", `Number "1"
    ; "session_id", `String (P.Id.Session.to_string state.identity.session_id)
    ; "generation", `Number (Int.to_string state.identity.generation)
    ; "revision", `Number (Int64.to_string state.counters.revision)
    ; "desired_state", `String (P.Session.desired_state_to_string state.lifecycle.desired)
    ; "state", `String observed
    ; "operation", operation
    ; "waiting_permissions", `Number (Int.to_string waiting_permissions)
    ; ("halted", if state.halted then `True else `False)
    ; ("failed", if Option.is_some state.failure then `True else `False)
    ]
;;
