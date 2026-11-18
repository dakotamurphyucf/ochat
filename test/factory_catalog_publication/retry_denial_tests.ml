open! Core
open Agent_server_test_support
open Retry_support
module Result_attempt = Factory.Deferred_run_attempt

(* All denial checks use real Factory-issued source custody. This snapshot also
   covers the complete catalog, Run index, startup receipts and physical journal. *)
let unchanged env entries (counters : counters) =
  let saved =
    List.map entries ~f:(fun entry -> entry, state entry, Existing.frames env entry)
  in
  let inference_ports = !(counters.inference_ports) in
  let providers = !(counters.providers) in
  let transcripts = !(counters.transcripts) in
  fun () ->
    List.iter saved ~f:(fun (entry, previous, frames) ->
      Existing.assert_same previous entry;
      [%test_eq: string list] frames (Existing.frames env entry));
    [%test_eq: int] inference_ports !(counters.inference_ports);
    [%test_eq: int] providers !(counters.providers);
    [%test_eq: string list] transcripts !(counters.transcripts)
;;

let error code message = P.Error.create code ~message ~retryable:false ()

let%expect_test
    "retry source denies foreign ownership and current authorization without effects"
  =
  with_fixture ~max_root_agents:2 (fun env sw daemon entry _runtime _revision counters ->
    let occurrence, observation =
      with_planner env sw daemon entry (fun () ->
        let occurrence = create_occurrence daemon entry entry in
        occurrence, initial_absence env daemon entry occurrence)
    in
    Source.with_loaded_entry daemon (fun ~entry:foreign ~runtime:_ ~revision:_ ->
      let foreign_occurrence =
        with_planner env sw daemon foreign (fun () ->
          create_occurrence daemon foreign foreign)
      in
      let request = next_request entry occurrence in
      let callbacks = ref 0 in
      let invoke target ~actor ~authorize ~intent =
        Factory.with_recurrence_retry_run_source
          (Daemon.factory daemon)
          target
          ~observation
          ~actor
          ~authorize
          ~intent
          ~previous:occurrence.previous
          ~request
          (fun _ _ ->
             Int.incr callbacks;
             Ok ())
      in
      let assert_denied expected f =
        let assert_unchanged = unchanged env [ entry; foreign ] counters in
        assert_error expected (f () |> rejected);
        [%test_eq: int] 0 !callbacks;
        assert_unchanged ();
        Actor.No_effects_observation.check_current observation |> protocol_ok
      in
      assert_denied
        (error Conflict "retry observation belongs to another request or target")
        (fun () -> invoke foreign ~actor:original ~authorize ~intent:occurrence.intent);
      assert_denied
        (P.Error.invalid_request "recurrence attempt belongs to another immutable intent")
        (fun () ->
           invoke entry ~actor:original ~authorize ~intent:foreign_occurrence.intent);
      let now = ref (P.Timestamp.now ()) in
      let expires_at = P.Timestamp.add_ms !now 1 |> protocol_ok in
      let expired =
        Operator_authorization.bounded ~principal ~now:(fun () -> !now) ~expires_at
        |> protocol_ok
      in
      now := expires_at;
      assert (not (Operator_authorization.is_current expired));
      assert_denied
        (error
           Permission_denied
           "headless run command authority expired, was revoked or lacks scope")
        (fun () -> invoke entry ~actor:expired ~authorize ~intent:occurrence.intent);
      let denied =
        P.Error.create
          Permission_denied
          ~message:"539 current source policy denies retry"
          ~retryable:false
          ~data:(`Object [ "policy", `String "current-target-source" ])
          ()
      in
      let policies = ref 0 in
      assert_denied denied (fun () ->
        invoke
          entry
          ~actor:original
          ~authorize:(fun _ ->
            Int.incr policies;
            Error denied)
          ~intent:occurrence.intent);
      [%test_eq: int] 1 !policies;
      print_endline
        "foreign target and intent, expired original and current policy reject unchanged"));
  [%expect
    {| foreign target and intent, expired original and current policy reject unchanged |}]
;;

let%expect_test
    "new retry caller is authenticated once and cannot replace its frozen resume \
     principal"
  =
  with_fixture (fun env sw daemon entry _runtime _revision counters ->
    with_planner env sw daemon entry (fun () ->
      let occurrence = create_occurrence daemon entry entry in
      let observation = initial_absence env daemon entry occurrence in
      let request = next_request entry occurrence in
      let next_principal = principal_with_id "pri_539_fresh_retry_caller" in
      let next_original = Operator_authorization.trusted_local next_principal in
      let authorize_next current =
        if
          Agent_server.Authorization.session_visible_to
            next_principal
            (A.Session_state.summary current)
        then Ok ()
        else Error (error Permission_denied "539 fresh caller cannot view target")
      in
      let selected =
        error Invalid_request "539 stop after authenticated retry source selection"
      in
      let assert_unchanged = unchanged env [ entry ] counters in
      let issued = ref None in
      let callbacks = ref 0 in
      let attempt =
        Factory.with_recurrence_retry_run_source
          (Daemon.factory daemon)
          entry
          ~observation
          ~actor:next_original
          ~authorize:authorize_next
          ~intent:occurrence.intent
          ~previous:occurrence.previous
          ~request
          (fun retry prepared ->
             Int.incr callbacks;
             issued := Some retry;
             let next = Actor.Recurrence_retry.next_attempt retry in
             assert (P.Id.Principal.equal next_principal.id (Attempt.principal_id next));
             assert (not (P.Id.Principal.equal principal.id (Attempt.principal_id next)));
             [%test_eq: int] 1 (Attempt.number next);
             [%test_eq: int64] request.expected_revision (Attempt.expected_revision next);
             assert (P.Idempotency_key.equal request.key (Attempt.key next));
             Owner.Prepared.check_basis prepared |> protocol_ok;
             Error selected)
        |> protocol_ok
      in
      assert_error selected (Result_attempt.result attempt |> rejected);
      let retry = Result_attempt.retry attempt |> Option.value_exn in
      assert (phys_equal retry (Option.value_exn !issued));
      assert (Option.is_none (Result_attempt.no_effects attempt));
      [%test_eq: int] 1 !callbacks;
      assert_unchanged ();
      Actor.Recurrence_retry.check_basis retry |> protocol_ok;
      let assert_denied expected f =
        assert_error expected (f () |> rejected);
        [%test_eq: int] 1 !callbacks;
        assert_unchanged ();
        Actor.Recurrence_retry.check_basis retry |> protocol_ok
      in
      assert_denied
        (error Conflict "no-effects observation lost its actual writer or attempt")
        (fun () ->
           Factory.with_recurrence_retry_run_source
             (Daemon.factory daemon)
             entry
             ~observation
             ~actor:next_original
             ~authorize:authorize_next
             ~intent:occurrence.intent
             ~previous:occurrence.previous
             ~request
             (fun _ _ ->
                Int.incr callbacks;
                Ok ()));
      assert_denied
        (error Permission_denied "retry resume changed its frozen next principal")
        (fun () ->
           Factory.resume_recurrence_retry_run_source
             (Daemon.factory daemon)
             entry
             ~retry
             ~actor:original
             ~authorize
             (fun _ _ ->
                Int.incr callbacks;
                Ok ()));
      print_endline
        "fresh authorized caller accepted; consumed observation and changed resume \
         caller reject unchanged"));
  [%expect
    {| fresh authorized caller accepted; consumed observation and changed resume caller reject unchanged |}]
;;
