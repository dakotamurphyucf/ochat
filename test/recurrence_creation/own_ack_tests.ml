open! Core
open Creation_support
module Builder = A.Runtime_builder
module Owner = Agent_server.Runtime_owner
module Binding = A.Session_actor.Source_binding
module Held = A.Session_actor.Held_run
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Manager = Chat_response.Moderator_manager

(* The controller was initialized BEFORE this staged admission. Its real Manager
   checkpoint is compiled from the exact captured revision, then installed by the
   actual Actor. This is not the Factory/cold-Ready initialization path. *)
let with_controller ?clock_read ?before_commit f =
  Held_tests.with_captured_target ~controller:true (fun fixture source root_file ->
    let revision = Builder.Source.revision source in
    let _, artifact =
      Manager.Registry.of_elements
        Manager.Registry.empty
        (A.Prompt_revision.elements revision)
      |> Result.ok_or_failwith
    in
    let allocator =
      History_entry.Allocator.create ~namespace:"own-ack-controller" ~next_sequence:0
      |> Result.ok_or_failwith
    in
    let manager =
      Manager.create_entries
        ~env:fixture.env
        ~artifact:(Option.value_exn artifact)
        ~capabilities:Chat_response.Moderation.Capabilities.default
        ~allocator
        ()
      |> Result.ok_or_failwith
    in
    let checkpoint =
      Manager.identity_snapshot manager
      |> Result.ok_or_failwith
      |> A.Moderator_checkpoint.encode
    in
    let observer =
      A.Moderator_checkpoint.observer (Some checkpoint) |> protocol_ok |> Option.value_exn
    in
    (match Builder.Source.moderator source with
     | Legacy expected -> assert (P.Invocation.equal_observer expected observer)
     | Extension _ | No_moderator -> failwith "expected captured legacy controller");
    let actor = create_actor fixture ~sw:fixture.sw ?clock_read ?before_commit () in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      ~f:(fun () ->
        A.Session_actor.change_moderator actor (Some checkpoint) |> protocol_ok |> ignore;
        let writer, _ =
          A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
        in
        A.Session_actor.start ~actor:proof actor ~attachment_id:writer.id
        |> protocol_ok
        |> ignore;
        (match (state actor).lifecycle with
         | { desired = Running; observed = Idle } -> ()
         | _ -> failwith "authenticated controller Start did not establish Idle");
        assert (Option.is_none (state actor).active_operation);
        let host = open_host fixture () in
        Exn.protect
          ~finally:(fun () -> Host.close_and_wait host)
          ~f:(fun () ->
            install actor host;
            create
              actor
              "own_ack"
              (interval ~first_due:(P.Timestamp.to_time_ns timestamp) ())
            |> ignore;
            (* With no prior Run index, the actual recurrence capture command
               installs its first source index from the real checkpoint. *)
            let installed = A.Recurrence_source.installed (state actor) |> protocol_ok in
            assert (P.Invocation.equal_observer installed.observer observer);
            sweep actor;
            let intent, attempt = Record_tests.original_request actor "own_ack" in
            let builds = ref 0 in
            let owner =
              Owner.create ~actor ~initial:None ~build:(fun () ->
                Int.incr builds;
                failwith "own ACK staging entered eager runtime construction")
            in
            Exn.protect
              ~finally:(fun () -> Owner.close_and_wait owner)
              ~f:(fun () ->
                f fixture actor source owner intent attempt;
                [%test_eq: int] !builds 0;
                assert (not (Eio.Path.is_file root_file))))))
;;

let with_prepared actor owner source ~target_proof intent attempt f =
  let preparation =
    Held_tests.begin_preparation
      actor
      ~proof:target_proof
      ~policy:authorize
      intent
      attempt
  in
  Exn.protect
    ~finally:(fun () ->
      A.Session_actor.end_run_preparation actor preparation |> protocol_ok)
    ~f:(fun () ->
      Owner.with_prepared_source owner ~preparation ~source (fun prepared ->
        f preparation prepared;
        Ok ())
      |> protocol_ok)
;;

let assert_original preparation attempt =
  let request = A.Run_preparation.request preparation in
  [%test_eq: int64] request.expected_revision (Attempt.expected_revision attempt);
  assert (P.Idempotency_key.equal request.key (Attempt.key attempt));
  [%test_eq: string]
    (A.Run_preparation.request_sha256 preparation)
    (Attempt.request_sha256 attempt);
  [%test_eq: string]
    (A.Run_admission_request.request_sha256 request |> protocol_ok)
    (Attempt.request_sha256 attempt)
;;

let record
      actor
      binding
      ~proof
      ?(authorize = Record_tests.authorize_record)
      intent
      attempt
  =
  A.Session_actor.record_prepared_recurrence_execution
    actor
    ~binding
    ~actor:proof
    ~authorize
    ~intent
    ~attempt
;;

let import actor held ~proof observation =
  A.Session_actor.import_held_recurrence_execution
    actor
    ~held
    ~actor:proof
    ~authorize:Record_tests.authorize_record
    ~automatic_policy:(fun _ _ -> Ok true)
    ~observation
;;

let foreign_controller_request actor intent attempt =
  let current = E.current (entry actor "own_ack") in
  let controller =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_foreign_own_ack" |> protocol_ok)
      ~session_id:(P.Session_ref.session_id session)
  in
  let definition =
    F.create
      ~key:
        (I.Definition.create ~controller ~recurrence_id:(id "own_ack") ~revision:0L
         |> protocol_ok)
      ~creator:(F.creator current)
      ~source:(F.source current)
      ~birth_lifecycle_revision:(F.birth_lifecycle_revision current)
      ~schedule:(F.schedule current)
      ~target:(F.target current)
      ~policy:(F.policy current)
      ~created_at:(F.created_at current)
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:0L
      ~nominal_json:(N.create_interval definition ~slot:0L |> protocol_ok |> N.to_json)
      ~target:(Intent.target intent)
      ~source:(Intent.source intent)
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:(Attempt.expected_revision attempt)
      ~key:(Attempt.key attempt)
    |> protocol_ok
  in
  intent, attempt
;;

let assert_one_ack
      fixture
      (before : A.Session_state.t)
      after_frames
      (after : A.Session_state.t)
  =
  [%test_eq: int64] after.counters.revision Int64.(before.counters.revision + 1L);
  [%test_eq: int64]
    after.counters.transaction_sequence
    Int64.(before.counters.transaction_sequence + 1L);
  [%test_eq: int] (List.length (Held_tests.frames fixture)) (List.length after_frames + 1);
  Held_tests.assert_pristine_effects
    before
    { after with recurrence_catalog = before.recurrence_catalog };
  Held_tests.assert_same_state after (replayed fixture)
;;

let%expect_test
    "same prepared self-target retains its original request through three ACKs"
  =
  with_controller (fun fixture actor source owner intent attempt ->
    let original_attempt = Attempt.to_json attempt in
    let target_guards = ref 0
    and controller_guards = ref 0 in
    let target_proof =
      Operator_authorization.guarded ~principal ~is_current:(fun () ->
        Int.incr target_guards;
        true)
    in
    let proof =
      Operator_authorization.guarded ~principal ~is_current:(fun () ->
        Int.incr controller_guards;
        true)
    in
    with_prepared
      actor
      owner
      source
      ~target_proof
      intent
      attempt
      (fun preparation prepared ->
         let binding = Owner.Prepared.binding prepared in
         let check_owners () =
           Owner.Prepared.check_basis prepared |> protocol_ok;
           Binding.check_basis binding |> protocol_ok;
           assert_original preparation attempt;
           assert (Jsonaf.exactly_equal original_attempt (Attempt.to_json attempt))
         in
         let before = state actor in
         let frames = Held_tests.frames fixture in
         let foreign_intent, foreign_attempt =
           foreign_controller_request actor intent attempt
         in
         [%test_eq: string]
           (Attempt.request_sha256 foreign_attempt)
           (Attempt.request_sha256 attempt);
         target_guards := 0;
         controller_guards := 0;
         let policies = ref 0 in
         Record_tests.assert_code
           Conflict
           (record
              actor
              binding
              ~proof
              ~authorize:(fun state definition ->
                Int.incr policies;
                Record_tests.authorize_record state definition)
              foreign_intent
              foreign_attempt);
         [%test_eq: int] !target_guards 0;
         [%test_eq: int] !controller_guards 0;
         [%test_eq: int] !policies 0;
         Held_tests.assert_same_state before (state actor);
         [%test_eq: string list] frames (Held_tests.frames fixture);
         let row = record actor binding ~proof intent attempt |> protocol_ok in
         let recorded = state actor in
         assert_one_ack fixture before frames recorded;
         assert (Intent.equal (Execution.intent row) intent);
         assert (
           Attempt.equal
             (Execution.Attempt.request (Execution.current_attempt row))
             attempt);
         [%test_eq: int] (List.length (H.ready (frontier actor "own_ack"))) 0;
         check_owners ();
         let frames = Held_tests.frames fixture in
         assert (
           Execution.equal row (record actor binding ~proof intent attempt |> protocol_ok));
         Held_tests.assert_same_state recorded (state actor);
         [%test_eq: string list] frames (Held_tests.frames fixture);
         check_owners ();
         let held =
           match Held_tests.admit actor binding intent attempt session |> protocol_ok with
           | Held held -> held
           | Retained _ -> failwith "new self-target admission unexpectedly retained DATA"
           | Rejected failure ->
             raise_s [%sexp "self-target Held admission rejected", (failure : P.Error.t)]
           | Uncertain failure ->
             raise_s [%sexp "self-target Held admission uncertain", (failure : P.Error.t)]
         in
         let admitted = state actor in
         assert_one_ack fixture recorded frames admitted;
         check_owners ();
         Held.check_basis held |> protocol_ok;
         let admission = Held.admission held in
         let index = Option.value_exn admitted.run_state in
         let run = A.Run_state.find index admission.run_id |> Option.value_exn in
         assert (P.Run.Lifecycle.equal run.lifecycle Admitted);
         [%test_eq: int64] run.revision 0L;
         assert (List.is_empty run.owned_work);
         let origin =
           A.Run_state.recurrence_origin index admission.run_id |> Option.value_exn
         in
         Origin.validate origin ~intent ~attempt |> protocol_ok;
         Origin.validate_run origin ~run ~admission |> protocol_ok;
         let observation = Import_tests.observe actor intent attempt in
         let frames = Held_tests.frames fixture in
         let imported = import actor held ~proof observation |> protocol_ok in
         (match imported.settlement with
          | Imported { execution; next = Evidence_only } ->
            let saved =
              Execution.current_attempt execution
              |> Execution.Attempt.origin
              |> Option.value_exn
            in
            assert (Origin.equal saved origin)
          | Imported _ | Retained_exact _ | Detail_not_retained _ ->
            failwith "expected reciprocal association ACK");
         let associated = state actor in
         assert_one_ack fixture admitted frames associated;
         check_owners ();
         Held.check_basis held |> protocol_ok;
         Record_tests.assert_code
           Conflict
           (A.Session_actor.Recurrence_run_observation.check_current observation);
         let fresh = Import_tests.observe actor intent attempt in
         let frames = Held_tests.frames fixture in
         let duplicate = import actor held ~proof fresh |> protocol_ok in
         (match duplicate.settlement with
          | Retained_exact saved ->
            assert (
              Origin.equal
                (Execution.current_attempt saved
                 |> Execution.Attempt.origin
                 |> Option.value_exn)
                origin)
          | Imported _ | Detail_not_retained _ ->
            failwith "retained import changed its disposition");
         Held_tests.assert_same_state associated (state actor);
         [%test_eq: string list] frames (Held_tests.frames fixture);
         check_owners ();
         let fresh = Import_tests.observe actor intent attempt in
         let permit = Activation_tests.acquire actor fresh |> protocol_ok in
         let checked = Activation_tests.refresh actor permit fresh |> protocol_ok in
         A.Session_actor.Recurrence_activation.Checked.check_current checked
         |> protocol_ok;
         check_owners ();
         Held.check_basis held |> protocol_ok;
         Activation_tests.release actor permit;
         Held_tests.assert_same_state associated (state actor);
         [%test_eq: string list] frames (Held_tests.frames fixture)));
  print_endline
    "record / inert admission / reciprocal association preserve original CAS and custody";
  [%expect
    {| record / inert admission / reciprocal association preserve original CAS and custody |}]
;;

let%expect_test "final prewrite grace clocks cannot revoke either self-target original" =
  let learned = ref None in
  let run ~revoke_target ~learn =
    let target_current = ref true
    and controller_current = ref true
    and tracing = ref false
    and reads = ref 0
    and writes = ref 0
    and hit = ref false in
    let clock_read () =
      if !tracing
      then (
        Int.incr reads;
        if (not learn) && Option.equal Int.equal (Some !reads) !learned
        then (
          hit := true;
          if revoke_target then target_current := false else controller_current := false))
    in
    let before_commit () =
      if !tracing
      then (
        Int.incr writes;
        if learn then learned := Some !reads)
    in
    with_controller
      ~clock_read
      ~before_commit
      (fun fixture actor source owner intent attempt ->
         let target_proof =
           Operator_authorization.guarded ~principal ~is_current:(fun () ->
             !target_current)
         in
         let controller_proof =
           Operator_authorization.guarded ~principal ~is_current:(fun () ->
             !controller_current)
         in
         with_prepared
           actor
           owner
           source
           ~target_proof
           intent
           attempt
           (fun preparation prepared ->
              let before = state actor in
              let frames = Held_tests.frames fixture in
              let binding = Owner.Prepared.binding prepared in
              tracing := true;
              let result = record actor binding ~proof:controller_proof intent attempt in
              tracing := false;
              if learn
              then (
                ignore (protocol_ok result);
                [%test_eq: int] !writes 1;
                assert (Option.value_exn !learned > 0);
                assert_original preparation attempt)
              else (
                assert !hit;
                Record_tests.assert_code Permission_denied result;
                [%test_eq: int] !writes 0;
                Held_tests.assert_same_state before (state actor);
                [%test_eq: string list] frames (Held_tests.frames fixture);
                assert_original preparation attempt;
                [%test_eq: int] (List.length (H.ready (frontier actor "own_ack"))) 1)))
  in
  run ~revoke_target:false ~learn:true;
  run ~revoke_target:true ~learn:false;
  run ~revoke_target:false ~learn:false;
  print_endline
    "learned final writer-bound clock cut: both originals deny before any write";
  [%expect
    {| learned final writer-bound clock cut: both originals deny before any write |}]
;;
