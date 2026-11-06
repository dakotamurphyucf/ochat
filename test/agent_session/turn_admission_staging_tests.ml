open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol

let%expect_test "Turn stages retain exact candidate, attempted writer and one ACK suffix" =
  (* Pure candidate/phase law only. These DATA projections do not acknowledge a
     Store, authorize an Actor, launch a worker or prove resources joined. *)
  with_actor_workspace (fun _env workspace_instance ->
    let before =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let operation : P.Operation.t =
      { id = P.Id.Operation.create ()
      ; generation = before.identity.generation
      ; kind = Turn User_submit
      ; state = Starting
      ; started_at = timestamp
      ; updated_at = timestamp
      }
    in
    let candidate =
      A.Session_transition.apply
        ~now:timestamp
        before
        ~delta:
          (Batch
             [ Active_operation_changed (Some operation)
             ; Lifecycle_changed
                 { desired = Running; observed = Running_turn operation.id }
             ])
        ~payloads:[ Operation_started operation ]
      |> protocol_ok
    in
    let module T = A.Turn_admission_staging in
    let owner = A.Preparation_tracking.Owner.create () in
    let attempted = ref false in
    let staged =
      T.capture
        ~owner
        ~before
        ~candidate
        ~operation
        ~remaining_audit:None
        ~commit_attempted:attempted
        ~plan:operation.id
      |> protocol_ok
    in
    assert (phys_equal attempted (T.commit_attempted staged));
    assert (phys_equal candidate (T.candidate staged));
    T.check_before_writer staged ~owner ~state:before |> protocol_ok;
    assert (
      Result.is_error
        (T.check_before_writer
           staged
           ~owner:(A.Preparation_tracking.Owner.create ())
           ~state:before));
    assert (Result.is_error (T.check_before_writer staged ~owner ~state:candidate.state));
    assert (Result.is_error (T.acknowledge staged ~owner ~state:before));
    attempted := true;
    (* A writer attempt without canonical installation is not an ACK. *)
    assert (Result.is_error (T.acknowledge staged ~owner ~state:before));
    assert (Result.is_error (T.seal_before_writer staged ~owner));
    let first = T.acknowledge staged ~owner ~state:candidate.state |> protocol_ok in
    let second = T.acknowledge staged ~owner ~state:candidate.state |> protocol_ok in
    let actual = T.finish first ~owner ~state:candidate.state |> protocol_ok in
    [%test_eq: P.Id.Operation.t] operation.id actual;
    assert (Result.is_error (T.finish second ~owner ~state:candidate.state));
    let unattempted =
      T.capture
        ~owner
        ~before
        ~candidate
        ~operation
        ~remaining_audit:None
        ~commit_attempted:(ref false)
        ~plan:()
      |> protocol_ok
    in
    ignore
      (T.seal_before_writer unattempted ~owner |> protocol_ok : T.Closed_before_writer.t);
    assert (Result.is_error (T.check_before_writer unattempted ~owner ~state:before));
    assert (Result.is_error (T.acknowledge unattempted ~owner ~state:candidate.state));
    print_s
      [%sexp
        { exact_candidate = true
        ; foreign_and_displaced_reject = true
        ; attempted_is_not_closed_before_writer = true
        ; duplicate_ack_shares_one_suffix = true
        ; sealed_preparation_cannot_acknowledge = true
        }]);
  [%expect
    {|
    ((exact_candidate true) (foreign_and_displaced_reject true)
     (attempted_is_not_closed_before_writer true)
     (duplicate_ack_shares_one_suffix true)
     (sealed_preparation_cannot_acknowledge true))
    |}]
;;
