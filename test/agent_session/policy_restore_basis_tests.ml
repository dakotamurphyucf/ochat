open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module E = P.Moderator_execution
module S = Moderator_lifecycle_evidence_tests
module Basis = A.Policy_restore_basis

(* These are pure State/transition DATA fixtures. They do not create an Actor
   admission, restore a physical Product or demonstrate cold policy authority. *)
let%expect_test
    "policy restoration basis keeps raw construction and current receipt facts"
  =
  with_actor_workspace (fun env workspace ->
    let manager, _, _ = handoff_definition env ~declare_tool:false in
    let snapshot =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let checkpoint = A.Moderator_checkpoint.encode snapshot in
    let observer =
      A.Moderator_checkpoint.observer (Some checkpoint) |> protocol_ok |> Option.value_exn
    in
    let installation =
      A.Run_source_installation.apply
        A.Run_source_installation.initial
        ~change:(Replace observer)
      |> protocol_ok
    in
    let index =
      A.Run_state.replace_installation A.Run_state.empty ~installation ~retired_runs:[]
      |> protocol_ok
    in
    let initial =
      actor_state
        ~workspace_instance:workspace
        ~liveness:Detached
        ~start_immediately:false
    in
    let initial = { initial with moderator = Some checkpoint; run_state = Some index } in
    let source = S.current_source initial in
    let queue =
      A.Moderator_queue.bootstrap ~source ~queue:snapshot.queued_internal_events
      |> protocol_ok
      |> A.Moderator_queue.to_jsonaf
      |> fun queue -> S.set queue "future_queue" (`Array [ `String "retained" ])
    in
    let checkpoint =
      S.set
        checkpoint
        "identity_snapshot"
        (S.set (S.field checkpoint "identity_snapshot") "future_identity" `True)
      |> fun checkpoint ->
      S.set checkpoint "queue_provenance" queue
      |> fun checkpoint -> S.set checkpoint "future_checkpoint" (`String "original")
    in
    let initial = { initial with moderator = Some checkpoint } in
    let first = S.make_receipt initial "mex_policy_basis_first" Session_start in
    let claimed = S.apply initial (S.paired initial first) in
    let completed = S.apply claimed (Moderator_execution_changed (S.complete first)) in
    let basis = Basis.of_state completed |> protocol_ok in
    Basis.check_construction basis ~state:completed |> protocol_ok;
    Basis.check_policy basis ~state:completed |> protocol_ok;
    assert (
      A.Moderator_lifecycle.equal
        (Basis.lifecycle basis)
        (A.Run_state.moderator_lifecycle (S.index completed) |> Option.value_exn));
    let metadata_patch =
      P.Session_metadata.Patch.create
        ~name:(Set "metadata does not replace a resource")
        ~set_labels:[ "basis", "same" ]
        ~remove_labels:[]
      |> protocol_ok
    in
    let metadata =
      A.Session_metadata_transition.apply
        completed
        ~expected_metadata_revision:completed.identity.metadata_revision
        ~patch:metadata_patch
      |> protocol_ok
      |> Option.value_exn
      |> S.apply completed
    in
    A.Session_state.validate metadata |> protocol_ok;
    Basis.check_construction basis ~state:metadata |> protocol_ok;
    Basis.check_policy basis ~state:metadata |> protocol_ok;
    (* Independent validated DATA candidates test full raw equality only. They
       are NOT admitted replacements for the original's retained unknown fields. *)
    let unknown_changed =
      { completed with
        moderator = Some (S.set checkpoint "future_checkpoint" (`String "changed"))
      }
    in
    A.Session_state.validate unknown_changed |> protocol_ok;
    assert (Result.is_error (Basis.check_construction basis ~state:unknown_changed));
    let queue_changed =
      { completed with
        moderator =
          Some
            (S.set
               checkpoint
               "queue_provenance"
               (S.set queue "future_queue" (`Array [ `String "changed" ])))
      }
    in
    A.Session_state.validate queue_changed |> protocol_ok;
    assert (Result.is_error (Basis.check_construction basis ~state:queue_changed));
    let checkpoint_component =
      A.Moderator_checkpoint.of_jsonaf checkpoint |> protocol_ok
    in
    let updated_checkpoint =
      A.Moderator_checkpoint.update
        checkpoint_component
        ~snapshot:{ snapshot with current_state = Array [ Int 7 ] }
        ~queue:Keep
      |> protocol_ok
      |> A.Moderator_checkpoint.to_jsonaf
    in
    let own_checkpoint =
      S.apply completed (Moderator_changed (Some updated_checkpoint))
    in
    A.Session_state.validate own_checkpoint |> protocol_ok;
    assert (Result.is_error (Basis.check_construction basis ~state:own_checkpoint));
    Basis.check_policy basis ~state:own_checkpoint |> protocol_ok;
    (* An older Completed receipt is still present, but the selected newer claim
       controls eligibility. No DATA constructor is used as execution permission. *)
    let newer = S.make_receipt completed "mex_policy_basis_newer" Session_resume in
    let running = S.apply completed (S.paired completed newer) in
    assert (Result.is_error (Basis.of_state running));
    assert (Result.is_error (Basis.check_policy basis ~state:running));
    List.iter [ `Failed; `Interrupted; `Completed ] ~f:(fun status ->
      let next =
        S.apply running (Moderator_execution_changed (S.terminal newer status))
      in
      A.Session_state.validate next |> protocol_ok;
      assert (List.exists next.moderator_executions ~f:(E.equal (S.complete first)));
      assert (Result.is_error (Basis.check_policy basis ~state:next));
      match status with
      | `Failed | `Interrupted -> assert (Result.is_error (Basis.of_state next))
      | `Completed ->
        let fresh = Basis.of_state next |> protocol_ok in
        Basis.check_construction fresh ~state:next |> protocol_ok;
        assert (
          not
            (A.Moderator_lifecycle.equal (Basis.lifecycle basis) (Basis.lifecycle fresh))));
    let absent =
      { completed with
        run_state = Some (A.Run_state.clear_moderator_lifecycle (S.index completed))
      }
    in
    A.Session_state.validate absent |> protocol_ok;
    assert (Result.is_error (Basis.of_state absent));
    assert (Result.is_error (Basis.check_policy basis ~state:absent));
    let no_index =
      { initial with
        moderator = Some (A.Moderator_checkpoint.encode snapshot)
      ; run_state = None
      }
    in
    A.Session_state.validate no_index |> protocol_ok;
    assert (Result.is_error (Basis.of_state no_index));
    print_endline
      "DATA only: full raw construction, own checkpoint and metadata, exact latest \
       receipt");
  [%expect
    {| DATA only: full raw construction, own checkpoint and metadata, exact latest receipt |}]
;;
