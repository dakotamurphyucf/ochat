open! Core
module P = Agent_protocol
module F = P.Recovery_followup
module A = Agent_session
module Actor = A.Session_actor
module C = A.Recovery_rollover_continuation
module Ledger = A.Recovery_window_ledger

let phase : C.phase -> F.Reply.phase = function
  | Awaiting -> Awaiting
  | Waiting -> Waiting
  | Preparing _ -> Preparing
  | Held Capacity -> Held_capacity
  | Held Capacity_unknown -> Held_capacity_unknown
  | Held Authority_unavailable -> Held_authority
  | Held Required_input_unavailable -> Held_input
  | Held Preparation_interrupted -> Held_interrupted
  | Admitted _ -> Admitted
  | Finished Script_finished -> Finished
  | Finished Cancelled -> Cancelled
;;

let action : F.action -> C.action = function
  | Continue -> Continue
  | Wait -> Wait_for_wake
  | Finish -> Finish_followup
  | Cancel -> Cancel_followup
;;

let error code message = P.Error.create code ~message ~retryable:false ()

let execute runtime ~actor ~authorize ~on_release (request : F.t) =
  let open Result.Let_syntax in
  let%bind observation = Actor.observe_resource_state runtime in
  let inspect () =
    let%bind state =
      Actor.Resource_observation.check_recovery_window_writer
        observation
        ~actor
        ~attachment_id:request.attachment_id
    in
    let%bind () = authorize state in
    let%bind windows =
      Result.of_option
        state.recovery_windows
        ~error:(error Invalid_state "recovery windows are disabled")
    in
    let scope = Ledger.Window.scope (Ledger.active windows) in
    let%bind basis = F.Basis.create scope ~revision:state.counters.revision in
    let current = A.Session_delta.foreground_continuation state in
    let reply =
      { F.Reply.basis
      ; phase =
          Option.value_map current ~default:F.Reply.Idle ~f:(fun follow ->
            phase follow.phase)
      }
    in
    Ok (current, reply)
  in
  let%bind current, before = inspect () in
  let%bind reply =
    match request.selection with
    | Inspect _ -> Ok before
    | Apply (expected, selected) ->
      let%bind () =
        if F.Basis.equal expected before.basis
        then Ok ()
        else Error (error Conflict "recovery follow-up basis changed")
      in
      let%bind follow =
        Result.of_option
          current
          ~error:(error Invalid_state "no current rollover follow-up")
      in
      let%bind exact =
        A.Recovery_rollover.Expected.create expected.scope ~revision:expected.revision
      in
      let%bind _ =
        Actor.control_context_rollover
          runtime
          ~actor
          ~attachment_id:request.attachment_id
          ~authorize
          ~id:follow.owner
          exact
          (action selected)
      in
      let%bind _, result = inspect () in
      let%map () = F.validate result ~request in
      result
  in
  let check () =
    let%bind _, fresh = inspect () in
    if F.Reply.equal reply fresh
    then Ok ()
    else Error (error Conflict "recovery follow-up changed before release")
  in
  on_release check;
  Ok reply
;;
