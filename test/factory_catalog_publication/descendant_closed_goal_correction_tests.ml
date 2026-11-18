open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Source = Descendant_ancestry_support
module Usage = Child_usage_fault_tests
module Goal = Goal_service_tests
module G = P.Goal
module GD = A.Goal_descendants
module S = A.Child_usage_snapshot
module L = A.Inference_ledger
module Included = A.Goal_descendant_included
module Observation = Inference.Observation
module Delegations = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Handle = Agent_client.Session_handle

let input partition =
  S.Partition.usage partition
  |> fun usage -> A.Goal_usage.component usage Input |> A.Goal_usage.Metric.actual
;;

let assert_input expected partition =
  assert (P.Inference_query.Metric.equal_sum (Tokens expected) (input partition))
;;

let assert_marks partition =
  let marks = S.Partition.marks partition in
  [%test_eq: int64] 1L marks.turn;
  [%test_eq: int64] 1L marks.attempt;
  [%test_eq: int64] 0L marks.untracked_turns;
  [%test_eq: int64] 0L marks.untracked_attempts
;;

let import_replacement daemon parent child =
  match Usage.import daemon parent child |> protocol_ok with
  | Imported Replaced -> ()
  | Imported (Added | Duplicate | Stale) | Historical_absence ->
    failwith "new retained usage did not replace the actual scoped contribution"
;;

let assert_duplicate daemon parent child =
  let before = state parent in
  (match Usage.import daemon parent child |> protocol_ok with
   | Imported Duplicate -> ()
   | Imported (Added | Replaced | Stale) | Historical_absence ->
     failwith "identical child observation was not a duplicate");
  Existing.assert_same before parent
;;

module Evidence = struct
  type t =
    { closed : G.t
    ; root_account : A.Goal_accounting.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; scope : Agent_store.Delegation_goal_origin.Scope.t
    ; included : Included.t
    ; handle : L.Handle.t
    }

  let run current goal =
    let admission = G.episode goal |> Option.value_exn |> G.Episode.admission in
    A.Run_state.find (Option.value_exn current.A.Session_state.run_state) admission.run_id
    |> Option.value_exn
  ;;

  let check t ~parent ~child ~expected =
    let root = state parent in
    let child = state child in
    assert (G.equal t.closed (Goal.goal root));
    assert (A.Goal_accounting.equal t.root_account (Goal.accounting root));
    assert (Option.is_none child.active_operation);
    Source.assert_no_local_goal child;
    let branch =
      GD.branches (Option.value_exn root.goal_descendants) ~goal_id:Goal.goal_id
      |> List.find_exn ~f:(fun branch ->
        Agent_store.Delegation_store.Reference.equal
          t.reference
          (GD.Branch.reference branch)
        && Agent_store.Delegation_goal_origin.Scope.equal t.scope (GD.Branch.scope branch))
    in
    [%test_eq: int64] 2L (GD.Branch.confirmed_prefix branch);
    [%test_eq: int64] 2L (GD.Branch.last_nonce branch);
    assert (Option.is_none (GD.Branch.pending branch));
    assert (Included.equal t.included (Birth.Provider_probe.included branch));
    assert (
      List.equal
        Included.equal
        [ t.included ]
        (GD.local_included (Option.value_exn child.goal_descendants)));
    let observed = Source.snapshot root t.reference in
    S.validate_origin
      observed
      ~origin:(Option.value_exn (GD.origin (Option.value_exn child.goal_descendants)))
    |> protocol_ok;
    let scoped =
      match S.goal_scopes observed |> Option.value_exn with
      | [ scoped ] -> scoped
      | [] | _ :: _ :: _ -> failwith "closed Goal lost its one actual inherited scope"
    in
    assert (
      Agent_store.Delegation_goal_origin.Scope.equal t.scope (S.Goal_scope.scope scoped));
    assert (Option.exists (S.Goal_scope.included scoped) ~f:(Included.equal t.included));
    assert (S.Partition.equal (S.own observed) (S.Goal_scope.own scoped));
    Source.assert_coverage (S.coverage observed);
    Source.assert_coverage (S.Goal_scope.coverage scoped);
    assert (
      S.Partition.equal
        S.Partition.empty
        (S.Descendants.partition (S.descendants observed)));
    List.iter
      [ S.own observed; S.Goal_scope.own scoped ]
      ~f:(fun partition ->
        assert_marks partition;
        assert_input expected partition);
    let aggregate = Source.root_descendants root in
    assert_marks (S.Descendants.partition aggregate);
    assert_input expected (S.Descendants.partition aggregate);
    let actual = A.Child_accounting.snapshot child |> protocol_ok |> Option.value_exn in
    assert_marks (S.own actual);
    assert_input expected (S.own actual);
    let row =
      L.find child.inference_ledger ~ordinal:(L.Handle.ordinal t.handle)
      |> Option.value_exn
    in
    assert (L.Handle.equal t.handle (L.Row.handle row));
    (match Observation.Attempt_record.state (L.Row.record row) with
     | Terminal terminal ->
       assert (
         Inference.Event.Terminal.equal_outcome
           (Inference.Event.Terminal.outcome terminal)
           Completed)
     | Prepared | Running | Interrupted _ ->
       failwith "late correction changed the completed actual Attempt");
    assert (
      not
        (List.exists (L.rows root.inference_ledger) ~f:(fun row ->
           L.Handle.equal t.handle (L.Row.handle row))))
  ;;
end

let next_usage_revision child handle =
  L.find (state child).inference_ledger ~ordinal:(L.Handle.ordinal handle)
  |> Option.value_exn
  |> L.Row.record
  |> Observation.Attempt_record.observations
  |> List.filter_map ~f:(fun observation ->
    match Observation.payload observation with
    | Usage _ -> Some (Observation.revision observation)
    | Configuration _ | Context_estimate _ | Transport_selection _ | Diagnostic _ -> None)
  |> List.fold ~init:0L ~f:Int64.max
  |> Int64.succ
;;

let correct child handle tokens =
  let revision = next_usage_revision child handle in
  Actor.observe_inference
    child.Registry.actor
    ~handle
    (Usage.correction handle revision tokens)
  |> protocol_ok
;;

let refuse_linked_child_removal env sw daemon child_id =
  let connection = connection daemon principal in
  protect_cleanup
    ~finally:(fun () -> Agent_client.Connection.close connection)
    (fun () ->
       initialize connection;
       let handle =
         Handle.attach
           ~sw
           ~clock:env#clock
           ~connection
           ~session_id:child_id
           ~mode:Read_write
           ~subscribe:false
           ()
         |> protocol_ok
       in
       protect_cleanup
         ~finally:(fun () -> Handle.close handle)
         (fun () ->
            let stopped = Handle.stop handle ~mode:Cancel |> protocol_ok in
            [%test_eq: P.Id.Session.t] child_id stopped.id;
            let refusal =
              Handle.delete
                handle
                ~expected_revision:stopped.revision
                ~policy:Remove
                ~confirmation:(P.Id.Session.to_string child_id)
              |> rejected
            in
            [%test_eq: P.Error.code] Conflict refusal.code;
            [%test_eq: string]
              "session has a live durable delegation obligation"
              refusal.message))
;;

let%expect_test "closed Goal retains corrected descendant usage through physical reopen" =
  let probe = ref None in
  let provider_failure = ref None in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ~timeout:90.
    ~budget
    ~child_sources:Birth.attempt_sources
    ~post_creation_stream:(fun _ ->
      Option.iter !probe ~f:(fun probe ->
        try Birth.Provider_probe.before_submit probe with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          provider_failure := Some (exn, backtrace);
          Exn.raise_with_original_backtrace exn backtrace);
      completed_response "554 retained provider observation")
    (fun env sw daemon parent child scope reference counters ->
       let within f = Eio.Time.with_timeout_exn env#clock 30. f in
       Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
       let parent_before = state parent in
       let root_account = Goal.accounting parent_before in
       let actual_probe =
         Birth.Provider_probe.create
           ~parent
           ~child
           ~scope
           ~reference
           ~parent_attempts_before:(A.Goal_accounting.admitted_attempts root_account)
       in
       Source.start_legacy daemon child;
       Source.with_writer child (fun writer ->
         let message = Source.message child "554 genuine provider before Goal closure" in
         probe := Some actual_probe;
         let submitted =
           Actor.submit_message_authenticated
             child.actor
             ~actor:original
             ~attachment_id:writer.id
             message
           |> protocol_ok
         in
         assert (Option.is_some submitted.operation_id);
         within (fun () -> Goal_native_test_support.await_quiet child) |> ignore;
         Option.iter !provider_failure ~f:(fun (exn, backtrace) ->
           Exn.raise_with_original_backtrace exn backtrace);
         Birth.Provider_probe.after_terminal actual_probe);
       probe := None;
       let included, handle = Option.value_exn actual_probe.submitted in
       [%test_eq: int] 3 !(counters.providers);
       correct child handle 7L;
       import_replacement daemon parent child;
       let before_close = state parent in
       let root_run = Evidence.run before_close (Goal.goal before_close) in
       let reason =
         G.Reason.of_string "operator closes completed foreground work" |> protocol_ok
       in
       Goal_admitted_guard_tests.command
         daemon
         parent
         ~key:"554-close-before-usage-correction"
         (Abandon (Goal.goal_id, reason))
       |> protocol_ok
       |> ignore;
       let closed = Goal.goal (state parent) in
       assert (G.State.equal (Closed (Abandoned reason)) (G.state closed));
       assert (P.Run_source.equal (G.source (Goal.goal before_close)) (G.source closed));
       assert (
         Option.equal
           G.Episode.equal
           (G.episode (Goal.goal before_close))
           (G.episode closed));
       let evidence =
         Evidence.{ closed; root_account; reference; scope; included; handle }
       in
       Evidence.check evidence ~parent ~child ~expected:7L;
       correct child handle 11L;
       import_replacement daemon parent child;
       Evidence.check evidence ~parent ~child ~expected:11L;
       assert (P.Run.equal root_run (Evidence.run (state parent) closed));
       assert_duplicate daemon parent child;
       [%test_eq: int] 3 !(counters.providers);
       let parent_id = (state parent).identity.session_id in
       let child_id = (state child).identity.session_id in
       let root =
         Agent_store.Session_store.data_root (Daemon.store daemon)
         |> Agent_store.Data_root.path
         |> Filename.dirname
       in
       let replayed = ref 0 in
       let policy =
         inference_policy
           ~default_model:"fixture-model"
           ~post_stream:(fun ~sw:_ ~inputs:_ ->
             Int.incr replayed;
             completed_response "unexpected provider replay after Goal closure")
       in
       Usage.with_reopened env sw root daemon policy (fun recovered ->
         let parent =
           Registry.load (Daemon.registry recovered) parent_id |> protocol_ok
         in
         let child = Registry.load (Daemon.registry recovered) child_id |> protocol_ok in
         within (fun () -> Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()))
         |> protocol_ok;
         within (fun () -> Actor.with_moderator_checkpoint child.actor (fun () -> Ok ()))
         |> protocol_ok;
         Evidence.check evidence ~parent ~child ~expected:11L;
         (* Recovery can advance factual session metadata. Import that exact
            observation before testing duplicate stability, without new work. *)
         (match Usage.import recovered parent child |> protocol_ok with
          | Imported (Replaced | Duplicate) -> ()
          | Imported (Added | Stale) | Historical_absence ->
            failwith "recovered actual child lost its contribution");
         Evidence.check evidence ~parent ~child ~expected:11L;
         assert_duplicate recovered parent child;
         let read_record daemon =
           Delegations.resolve (Store.delegations (Daemon.store daemon)) reference
           |> Existing.store_ok
         in
         let record = read_record recovered in
         assert (Delegations.equal_stage record.stage Linked);
         assert (Option.is_none record.revocation);
         let before_removal = Source.snapshot (state parent) reference in
         let assert_retained daemon parent =
           let session_path =
             Agent_store.Data_root.session_path
               (Store.data_root (Daemon.store daemon))
               child_id
           in
           assert (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / session_path));
           assert (Delegations.equal_record record (read_record daemon));
           let observed = Source.snapshot (state parent) reference in
           [%test_eq: string] (S.digest before_removal) (S.digest observed);
           let output =
             A.Goal_usage.component (S.Partition.usage (S.own observed)) Output
           in
           [%test_eq: int64]
             1L
             (A.Goal_usage.Unknown.count
                (A.Goal_usage.Metric.unknown output)
                Not_reported)
         in
         within (fun () -> refuse_linked_child_removal env sw recovered child_id);
         assert_retained recovered parent;
         Usage.with_reopened env sw root recovered policy (fun reopened ->
           let parent =
             Registry.load (Daemon.registry reopened) parent_id |> protocol_ok
           in
           let child = Registry.load (Daemon.registry reopened) child_id |> protocol_ok in
           assert (P.Session.equal_desired_state Stopped (state child).lifecycle.desired);
           assert_retained reopened parent;
           Evidence.check evidence ~parent ~child ~expected:11L;
           [%test_eq: int] 0 !replayed)));
  print_endline
    "closed Goal: retained child usage 7 -> 11, never 18; reopen preserves counts and \
     closure";
  print_endline
    "linked child: Remove refused; its last known input and unknown output survive a \
     second reopen";
  [%expect
    {|
    closed Goal: retained child usage 7 -> 11, never 18; reopen preserves counts and closure
    linked child: Remove refused; its last known input and unknown output survive a second reopen
    |}]
;;
