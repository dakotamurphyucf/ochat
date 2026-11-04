open! Core
open Creation_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference
module Observation = A.Session_actor.Recurrence_run_observation

(* Actual pure admission/recovery producers supply the retained facts. The
   journal and real owned Store projection publish them before the actor reads
   them; this does not claim to activate a recurrence Run or a provider. *)
let retained_run fixture =
  let initial = fixture.initial in
  let observer =
    A.Moderator_checkpoint.observer initial.moderator |> protocol_ok |> Option.value_exn
  in
  let installation =
    A.Run_source_installation.apply
      A.Run_source_installation.initial
      ~change:(Replace observer)
    |> protocol_ok
  in
  let source =
    A.Run_source_installation.captured
      installation
      ~generation:initial.identity.generation
    |> protocol_ok
  in
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source
      ~configuration_revision:initial.spec.configuration_revision
      ~input:{ kind = Plain_text; text = "observed input"; attachments = [] }
    |> protocol_ok
  in
  let definition =
    F.create
      ~key:
        (I.Definition.create
           ~controller:session
           ~recurrence_id:(id "observe")
           ~revision:0L
         |> protocol_ok)
      ~creator:principal_id
      ~source
      ~birth_lifecycle_revision:P.Session_lifecycle.Revision.zero
      ~schedule:(interval ~first_due:(P.Timestamp.to_time_ns timestamp) ())
      ~target
      ~policy:A.Recurrence_policy.default
      ~created_at:timestamp
    |> protocol_ok
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:0L
      ~nominal_json:(N.create_interval definition ~slot:0L |> protocol_ok |> N.to_json)
      ~target:session
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:initial.counters.revision
      ~key:(P.Idempotency_key.of_string "observed-admission" |> protocol_ok)
    |> protocol_ok
  in
  let scope =
    A.Run_admission.Scope.create_authenticated
      ~actor:proof
      ~observer
      ~startup_pending:(fun () -> false)
      ~authorize
    |> protocol_ok
  in
  let operation : P.Operation.t =
    { id = operation_id
    ; generation = initial.identity.generation
    ; kind = Turn User_submit
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
  in
  let admitted =
    A.Run_admission.prepare_request
      initial
      ~scope
      ~request:
        (Attempt.request attempt ~intent
         |> protocol_ok
         |> A.Run_admission_request.of_headless)
      ~session
      ~run_id:(P.Id.Run.of_string "run_observed" |> protocol_ok)
      ~operation:(Some operation)
      ~request_sha256:(Attempt.request_sha256 attempt)
      ~now:timestamp
    |> protocol_ok
  in
  let run = A.Run_admission.run admitted in
  let receipt = A.Run_admission.receipt admitted in
  let origin = Origin.create ~intent ~attempt ~run ~admission:receipt |> protocol_ok in
  let before =
    A.Run_state.replace_installation A.Run_state.empty ~installation ~retired_runs:[]
    |> protocol_ok
  in
  let index =
    A.Run_state.commit_recurrence_admission before ~run ~receipt ~origin |> protocol_ok
  in
  let publish previous index =
    let transition =
      A.Session_transition.apply
        ~now:timestamp
        previous
        ~delta:(Run_state_changed index)
        ~payloads:[]
      |> protocol_ok
    in
    A.Session_persistence.commit fixture.durable ~command_audit:None ~previous transition
    |> protocol_ok;
    S.write_metadata
      ~entry:(index_entry transition.state)
      fixture.store
      fixture.handle
      (metadata transition.state)
    |> store_ok;
    transition.state
  in
  let admitted_state = publish initial index in
  let retired =
    A.Run_retirement.recover
      index
      ~session_revision:Int64.(admitted_state.counters.revision + 1L)
      ~now:timestamp
    |> protocol_ok
  in
  let final = publish admitted_state retired in
  { fixture with initial = final }, intent, attempt, origin
;;

let observe actor ?(actor_proof = proof) ?(authorize = authorize) intent attempt =
  A.Session_actor.observe_recurrence_run
    actor
    ~actor:actor_proof
    ~authorize
    ~intent
    ~attempt
;;

let assert_error expected = function
  | Error error ->
    [%test_eq: string]
      (P.Error.code_to_string error.P.Error.code)
      (P.Error.code_to_string expected)
  | Ok _ -> failwith "expected observation fence"
;;

let%expect_test
    "owned target observation authenticates retained terminal evidence without runtime \
     loading"
  =
  with_store (fun fixture ->
    let fixture, intent, attempt, origin = retained_run fixture in
    let revoke_on_observe = ref false in
    let current = ref true in
    let actor =
      create_actor
        fixture
        ~sw:fixture.sw
        ~before_observe:(fun () -> if !revoke_on_observe then current := false)
        ()
    in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      ~f:(fun () ->
        let original =
          Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
        in
        let before = state actor in
        let observed =
          observe actor ~actor_proof:original intent attempt
          |> protocol_ok
          |> Option.value_exn
        in
        assert (Origin.equal origin (Observation.origin observed));
        let terminal = Observation.terminal observed |> Option.value_exn in
        assert (Reference.Outcome.equal (Reference.outcome terminal) Interrupted);
        let index = (state actor).run_state |> Option.value_exn in
        let actual = A.Run_state.find index (Origin.run_id origin) |> Option.value_exn in
        Reference.validate
          terminal
          ~origin
          ~run:actual
          ~receipt:(Reference.receipt terminal)
        |> protocol_ok;
        Observation.check_current observed |> protocol_ok;
        assert (equal_state before (state actor));
        current := false;
        assert_error Permission_denied (Observation.check_current observed);
        current := true;
        revoke_on_observe := true;
        assert_error
          Permission_denied
          (observe actor ~actor_proof:original intent attempt);
        revoke_on_observe := false;
        current := true;
        let revoke state =
          let result = authorize state in
          current := false;
          result
        in
        assert_error
          Permission_denied
          (observe actor ~actor_proof:original ~authorize:revoke intent attempt);
        current := true;
        let absent =
          Attempt.create
            ~intent
            ~number:0
            ~principal_id
            ~expected_revision:(Attempt.expected_revision attempt)
            ~key:(P.Idempotency_key.of_string "not-retained" |> protocol_ok)
          |> protocol_ok
        in
        assert (Option.is_none (observe actor intent absent |> protocol_ok));
        let observed = observe actor intent attempt |> protocol_ok |> Option.value_exn in
        (* A real metadata publication rotates the genuine Handle epoch. A scalar
         revision or unchanged retained Run cannot keep this observation current. *)
        S.write_metadata
          ~entry:(index_entry (state actor))
          fixture.store
          fixture.handle
          (metadata (state actor))
        |> store_ok;
        assert_error Conflict (Observation.check_current observed);
        let last = observe actor intent attempt |> protocol_ok |> Option.value_exn in
        A.Session_actor.shutdown actor;
        assert_error Conflict (Observation.check_current last)));
  print_endline
    "actual journal/Store/Actor; exact Interrupted receipt; read and owner fences";
  [%expect
    {| actual journal/Store/Actor; exact Interrupted receipt; read and owner fences |}]
;;
