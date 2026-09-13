open Core
open Agent_server_test_support
open Retired_delivery_checks
module P = Agent_protocol
module A = Agent_session.Session_actor
module State = Agent_session.Session_state

let check env ~before ~upgraded =
  let job = List.hd_exn before.State.jobs in
  with_actor env before (fun actor backend _ ->
    let retired =
      A.retire_obsolete_moderator_delivery actor ~revision:before.counters.revision ~job
      |> protocol_ok
    in
    assert (not retired);
    same before (A.state actor |> protocol_ok);
    same before (Agent_session.Memory_backend.state backend));
  with_actor env upgraded (fun actor backend reject_save ->
    let revision = upgraded.State.counters.revision in
    let retire ~revision job =
      A.retire_obsolete_moderator_delivery actor ~revision ~job
    in
    rejected (retire ~revision:(Int64.pred revision) job);
    rejected (retire ~revision { job with attempt = job.attempt + 1 });
    reject_save := true;
    rejected (retire ~revision job);
    same upgraded (A.state actor |> protocol_ok);
    same upgraded (Agent_session.Memory_backend.state backend);
    reject_save := false;
    assert (retire ~revision job |> protocol_ok);
    let state = A.state actor |> protocol_ok in
    let retained = List.hd_exn state.jobs in
    (match retained.delivery with
     | Discarded { reason = Authority_changed; _ } -> ()
     | _ -> failwith "obsolete moderator delivery not retired");
    same
      { upgraded with
        jobs = [ retained ]
      ; identity = state.identity
      ; counters = state.counters
      }
      state;
    same state (Agent_session.Memory_backend.state backend);
    rejected (retire ~revision:state.counters.revision retained);
    same state (A.state actor |> protocol_ok))
;;
