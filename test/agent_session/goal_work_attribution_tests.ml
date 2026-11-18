open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Origin = A.Goal_work_attribution

let%expect_test "stored Run identity survives absence of a Goal; missing origin rejects" =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "accounting-run-origin") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    let state, job = Job_inference_provenance_tests.associated basis ~attempt:0 in
    let run = Origin.run_of_job state job |> protocol_ok |> Option.value_exn in
    assert (Option.is_none (Origin.of_job state job |> protocol_ok));
    assert (
      match job.inference_origin with
      | Some (Run id) -> P.Id.Run.equal id run.id
      | _ -> false);
    let missing = { job with inference_origin = None } in
    assert (Result.is_error (Origin.of_job state missing));
    let unbound =
      { job with
        id = P.Id.Job.of_string "job_explicitly_unbound" |> protocol_ok
      ; inference_origin = Some Unbound
      }
    in
    assert (Option.is_none (Origin.run_of_job state unbound |> protocol_ok));
    assert (Option.is_none (Origin.of_job state unbound |> protocol_ok));
    (* Pure retained evidence only: no live invocation or Run permission is
       manufactured by this imported historical row. *)
    let historical = invocation_fixture () in
    assert (Result.is_error (Origin.run_of_invocation state historical));
    let invocation =
      P.Invocation.capture_inference_origin historical (Run run.id) |> protocol_ok
    in
    assert (
      P.Id.Run.equal
        run.id
        (Origin.run_of_invocation state invocation |> protocol_ok |> Option.value_exn).id);
    let unrelated =
      P.Invocation.capture_inference_origin historical Unbound |> protocol_ok
    in
    assert (Option.is_none (Origin.run_of_invocation state unrelated |> protocol_ok));
    let planned = A.Run_job_occurrence.work job |> protocol_ok in
    assert (
      P.Id.Run.equal
        (Origin.run_of_work state planned |> protocol_ok |> Option.value_exn).id
        run.id));
  print_endline
    "unavailable rejects; explicit unbound remains unrelated; unassociated Run keeps its \
     identity";
  [%expect
    {| unavailable rejects; explicit unbound remains unrelated; unassociated Run keeps its identity |}]
;;
