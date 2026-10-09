open! Core
module P = Agent_protocol

module Boundary = struct
  type t =
    | Worker of P.Id.Operation.t
    | Idle_start
  [@@deriving equal, sexp]
end

type t =
  { generation : int
  ; boundary : Boundary.t
  ; available : bool
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let create (state : Session_state.t) ~boundary ~runtime_admission_open =
  let open Result.Let_syntax in
  let%bind owner_available =
    match boundary, state.active_operation with
    | Boundary.Worker id, Some operation ->
      if
        (not (P.Id.Operation.equal id operation.id))
        || not (Int.equal state.identity.generation operation.generation)
      then conflict "pending consumption root differs from current operation"
      else (
        match operation.kind, operation.state with
        | Turn _, (Starting | Running) -> Ok true
        | Turn _, Cancelling -> Ok false
        | Turn _, (Completed | Cancelled | Failed _ | Interrupted _)
        | ( Compaction
          , ( Starting
            | Running
            | Cancelling
            | Completed
            | Cancelled
            | Failed _
            | Interrupted _ ) ) ->
          conflict "pending worker consumption requires an actual live Turn root")
    | Worker _, None -> conflict "pending worker consumption has no current root"
    | Idle_start, Some _ -> Ok false
    | Idle_start, None ->
      (match state.lifecycle.observed with
       | Idle -> Ok true
       | Stopped
       | Queued_for_slot
       | Starting
       | Running_turn _
       | Compacting _
       | Waiting_for_permission _
       | Recovering
       | Stopping
       | Failed _ -> Ok false)
  in
  let available =
    owner_available
    && runtime_admission_open
    && P.Session.equal_desired_state state.lifecycle.desired Running
    && Session_state.Runtime_initialization.equal state.runtime_initialization Ready
    && (not state.halted)
    && Option.is_none state.failure
  in
  Ok { generation = state.identity.generation; boundary; available }
;;

let eligible t input =
  if not t.available
  then false
  else (
    match P.Pending_input.binding input, t.boundary with
    | Safe_boundary, (Worker _ | Idle_start) -> true
    | Await_idle, Idle_start -> true
    | After_root { terminal = Some _; _ }, Idle_start -> true
    | After_root { operation_id; terminal = Some _; _ }, Worker current ->
      not (P.Id.Operation.equal operation_id current)
    | Await_idle, Worker _ | After_root { terminal = None; _ }, (Worker _ | Idle_start) ->
      false)
;;

let eligible_prefix t queue =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result queue ~init:() ~f:(fun () document ->
      let input = Pending_input_document.value document in
      if Int.equal t.generation (P.Pending_input.generation input)
      then Ok ()
      else conflict "pending input belongs to another session generation")
  in
  let rec take reversed = function
    | [] -> List.rev reversed
    | document :: rest ->
      if eligible t (Pending_input_document.value document)
      then take (document :: reversed) rest
      else List.rev reversed
  in
  Ok (take [] queue)
;;
