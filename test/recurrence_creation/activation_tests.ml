open! Core
open Creation_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Activation = A.Session_actor.Recurrence_activation
module Checked = Activation.Checked

let acquire
      actor
      ?(actor_proof = proof)
      ?(authorize = Record_tests.authorize_record)
      observation
  =
  A.Session_actor.acquire_recurrence_activation
    actor
    ~actor:actor_proof
    ~authorize
    ~observation
;;

let refresh actor permit observation =
  A.Session_actor.check_recurrence_activation actor permit ~observation
;;

let release actor permit =
  A.Session_actor.release_recurrence_activation actor permit |> protocol_ok
;;

type target_owner =
  { fixture : Creation_support.t
  ; mutable actor : A.Session_actor.t
  }

(* Shut down the data-only target actor before changing its durable facts, then
   rehydrate through the actual Store-backed actor. No live actor is bypassed. *)
let complete_target target (intent, attempt) =
  let previous = state target.actor in
  let index = Option.value_exn previous.run_state in
  let admission =
    A.Run_state.receipt
      index
      ~principal_id:(Attempt.principal_id attempt)
      ~key:(Attempt.key attempt)
      ~request_sha256:(Attempt.request_sha256 attempt)
    |> protocol_ok
    |> Option.value_exn
  in
  let prior = A.Run_state.find index admission.run_id |> Option.value_exn in
  let outcome = P.Run.Terminal.Completed None in
  let completed =
    P.Run.create
      ~id:prior.id
      ~session:prior.session
      ~principal_id:prior.principal_id
      ~source:prior.source
      ~mode:prior.mode
      ~lifecycle:(Terminal outcome)
      ~revision:(Int64.succ prior.revision)
      ~owned_work:prior.owned_work
      ~relinquished_work:prior.relinquished_work
      ~terminal_work:prior.terminal_work
      ~created_at:prior.created_at
      ~updated_at:timestamp
    |> protocol_ok
  in
  let receipt =
    P.Run_receipt.create
      ~run_id:prior.id
      ~principal_id:(Attempt.principal_id attempt)
      ~source:(Intent.source intent)
      ~key:(P.Idempotency_key.of_string "activation-terminal" |> protocol_ok)
      ~request_sha256:
        (P.Run.Terminal.to_json outcome
         |> Jsonaf.to_string
         |> Digestif.SHA256.digest_string
         |> Digestif.SHA256.to_hex)
      ~kind:Terminal
      ~run_revision:completed.revision
      ~session_revision:(Int64.succ previous.counters.revision)
      ~committed_at:timestamp
    |> protocol_ok
  in
  let index =
    A.Run_state.commit index ~run:completed ~receipt ~intent:None |> protocol_ok
  in
  A.Session_actor.shutdown target.actor;
  let current = Import_tests.publish target.fixture previous (Run_state_changed index) in
  target.actor
  <- create_actor { target.fixture with initial = current } ~sw:target.fixture.sw ();
  Import_tests.observe target.actor intent attempt
;;

(* Actual Store/opaque observations and controller ACK, but seeded target Run
   constructors are DATA. No Run_authorities, held initialization owner, provider
   or first-effect consumer is fabricated by this fixture. *)
let with_admitted_target f =
  with_store (fun base ->
    Import_tests.with_target base (fun target source ->
      let controller = create_actor base ~sw:base.sw () in
      Exn.protect
        ~finally:(fun () ->
          base.reject := false;
          A.Session_actor.shutdown controller)
        ~f:(fun () ->
          let host = open_host base () in
          Exn.protect
            ~finally:(fun () -> Host.close_and_wait host)
            ~f:(fun () ->
              install controller host;
              let target_session =
                P.Session_ref.create
                  ~server_id
                  ~session_id:target.initial.identity.session_id
              in
              let selected_target =
                A.Recurrence_target.create_existing
                  ~session:target_session
                  ~source
                  ~configuration_revision:target.initial.spec.configuration_revision
                  ~input:{ kind = Plain_text; text = "activation data"; attachments = [] }
                |> protocol_ok
              in
              List.iter [ "activation_a"; "activation_b" ] ~f:(fun name ->
                mutate
                  controller
                  ("create-" ^ name)
                  (Create
                     ( id name
                     , { schedule =
                           interval
                             ~first_due:(P.Timestamp.to_time_ns (wall_at_ms 1000))
                             ()
                       ; target = selected_target
                       ; policy = A.Recurrence_policy.default
                       } ))
                |> ignore;
                mutate controller ("enable-" ^ name) (Enable (id name)) |> ignore);
              sweep controller;
              base.elapsed_ms := 1000;
              base.wall := wall_at_ms (-3_600_000);
              sweep controller;
              let target_state, requests =
                List.fold_map
                  [ "activation_a"; "activation_b" ]
                  ~init:target.initial
                  ~f:(fun target_state name ->
                    let intent, original =
                      Record_tests.original_request controller name
                    in
                    let attempt =
                      Attempt.create
                        ~intent
                        ~number:0
                        ~principal_id
                        ~expected_revision:target_state.A.Session_state.counters.revision
                        ~key:(Attempt.key original)
                      |> protocol_ok
                    in
                    Record_tests.record controller intent attempt |> protocol_ok |> ignore;
                    let target_state, _, _, _, _ =
                      Import_tests.publish_admitted
                        target
                        target_state
                        name
                        intent
                        attempt
                    in
                    target_state, (intent, attempt))
              in
              let target_actor =
                create_actor { target with initial = target_state } ~sw:base.sw ()
              in
              let target_owner = { fixture = target; actor = target_actor } in
              Exn.protect
                ~finally:(fun () -> A.Session_actor.shutdown target_owner.actor)
                ~f:(fun () ->
                  let observe ?(authorize = authorize) (intent, attempt) =
                    Import_tests.observe target_owner.actor ~authorize intent attempt
                  in
                  List.iter requests ~f:(fun request ->
                    Import_tests.import controller (observe request)
                    |> protocol_ok
                    |> ignore);
                  f base controller observe requests target_owner)))))
;;

let with_admitted f =
  with_admitted_target (fun base controller observe requests _target ->
    f base controller observe requests)
;;

let%expect_test "actual controller permits separate live custody from checked Current" =
  with_admitted (fun fixture controller observe requests ->
    let request_a, request_b =
      match requests with
      | [ a; b ] -> a, b
      | _ -> assert false
    in
    let current = ref true in
    let revoke_in_policy = ref false in
    let policy_calls = ref 0 in
    let read_calls = ref 0 in
    let original =
      Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
    in
    let policy state definition =
      Int.incr policy_calls;
      let result = Record_tests.authorize_record state definition in
      if !revoke_in_policy then current := false;
      result
    in
    let read state =
      Int.incr read_calls;
      authorize state
    in
    let observe_a () = observe ~authorize:read request_a in
    let observation_a = observe_a () in
    let permit_a =
      acquire controller ~actor_proof:original ~authorize:policy observation_a
      |> protocol_ok
    in
    let observation_b = observe request_b in
    let permit_b = acquire controller observation_b |> protocol_ok in
    let checked_a = refresh controller permit_a observation_a |> protocol_ok in
    Checked.check_current checked_a |> protocol_ok;
    with_store (fun foreign ->
      let other = create_actor foreign ~sw:foreign.sw () in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown other)
        ~f:(fun () ->
          policy_calls := 0;
          read_calls := 0;
          Record_tests.assert_code Conflict (refresh other permit_a observation_a);
          [%test_eq: int] !policy_calls 0;
          [%test_eq: int] !read_calls 0));
    (* B's actual Pause changes the physical controller Current but not A's
       immutable control/attempt/source. Only B's cell is irrevocably closed. *)
    mutate controller "pause-activation-b" (Pause (id "activation_b")) |> ignore;
    Record_tests.assert_code Conflict (Checked.check_basis checked_a);
    let checked_a = refresh controller permit_a (observe_a ()) |> protocol_ok in
    Checked.check_current checked_a |> protocol_ok;
    policy_calls := 0;
    read_calls := 0;
    Record_tests.assert_code Conflict (refresh controller permit_b observation_b);
    [%test_eq: int] !policy_calls 0;
    [%test_eq: int] !read_calls 0;
    release controller permit_a;
    release controller permit_a;
    policy_calls := 0;
    read_calls := 0;
    Record_tests.assert_code Conflict (Checked.check_current checked_a);
    Record_tests.assert_code Conflict (refresh controller permit_a observation_a);
    [%test_eq: int] !policy_calls 0;
    [%test_eq: int] !read_calls 0;
    let permit_a =
      acquire controller ~actor_proof:original ~authorize:policy (observe_a ())
      |> protocol_ok
    in
    let checked_a = refresh controller permit_a (observe_a ()) |> protocol_ok in
    let before = state controller in
    revoke_in_policy := true;
    Record_tests.assert_code Permission_denied (Checked.check_current checked_a);
    assert (not !current);
    assert (equal_state before (state controller));
    release controller permit_a;
    (* Reauthorization uses a fresh actual host proof, never resurrects the
       revoked original or changes the durable attempt's audit principal. *)
    let permit_a = acquire controller (observe request_a) |> protocol_ok in
    let checked_a = refresh controller permit_a (observe request_a) |> protocol_ok in
    Checked.check_basis checked_a |> protocol_ok;
    fixture.elapsed_ms := 61_001;
    fixture.wall := wall_at_ms 1000;
    Record_tests.assert_code Conflict (Checked.check_current checked_a);
    Record_tests.assert_code Conflict (refresh controller permit_a (observe request_a));
    assert (equal_state before (state controller));
    release controller permit_a;
    assert (equal_state before (replayed fixture)));
  print_endline
    "Admitted DATA; reciprocal ACK; foreign/closed policy0; unrelated Current refresh; \
     original proof; elapsed grace";
  [%expect
    {| Admitted DATA; reciprocal ACK; foreign/closed policy0; unrelated Current refresh; original proof; elapsed grace |}]
;;

module Hook = struct
  type t =
    | Rejected_pause
    | Internal_stop
    | Idle_failure
    | Source_replacement
    | Terminal_import
    | Lifecycle_reservation
    | Runtime_retirement
    | Owner_shutdown
  [@@deriving sexp_of]
end

let journal_facts fixture =
  let journal, _, _ = restored_journal fixture in
  let scan = Agent_store.Journal.scan journal |> store_ok in
  List.map scan.entries ~f:(fun entry ->
    let frame = entry.Agent_store.Journal.frame in
    ( Agent_store.Frame.flags frame
    , Agent_store.Frame.payload frame
    , Agent_store.Frame.checksum_hex frame ))
;;

let%expect_test "actual invalidating controller hooks irreversibly close exact custody" =
  List.iter
    Hook.
      [ Rejected_pause
      ; Internal_stop
      ; Idle_failure
      ; Source_replacement
      ; Terminal_import
      ; Lifecycle_reservation
      ; Runtime_retirement
      ; Owner_shutdown
      ]
    ~f:(fun hook ->
      with_admitted_target (fun fixture controller observe requests target ->
        let request_a, request_b =
          match requests with
          | [ a; b ] -> a, b
          | _ -> assert false
        in
        (* The data fixture starts with desired Running but observed Stopped.
           The legacy idle borrow requires an actual Idle lifecycle. Establish
           it through authenticated Start before acquiring any custody. *)
        (match hook with
         | Internal_stop | Idle_failure ->
           let writer, _ =
             A.Session_actor.attach controller ~mode:Read_write ~subscribe:false
             |> protocol_ok
           in
           A.Session_actor.start ~actor:proof controller ~attachment_id:writer.id
           |> protocol_ok
           |> ignore;
           (match (state controller).lifecycle.observed with
            | Idle -> ()
            | _ -> failwith "authenticated Start did not establish Idle")
         | Rejected_pause
         | Source_replacement
         | Terminal_import
         | Lifecycle_reservation
         | Runtime_retirement
         | Owner_shutdown -> ());
        let permit_a = acquire controller (observe request_a) |> protocol_ok in
        let permit_b = acquire controller (observe request_b) |> protocol_ok in
        let checked_a = refresh controller permit_a (observe request_a) |> protocol_ok in
        let before = state controller in
        let frames = journal_facts fixture in
        (match hook with
         | Rejected_pause ->
           let pause =
             request controller "rejected-activation-pause" (Pause (id "activation_a"))
           in
           let result =
             Exn.protect
               ~finally:(fun () -> fixture.reject := false)
               ~f:(fun () ->
                 fixture.reject := true;
                 A.Session_actor.recurrence_command
                   controller
                   ~actor:proof
                   ~authorize
                   ~request:pause)
           in
           (match result with
            | Error error ->
              assert (P.Error.equal_code error.code Persistence_error);
              assert (String.equal error.message "definite no-write runtime fixture");
              assert error.retryable
            | Ok _ -> failwith "rejected Pause acknowledged");
           assert (equal_state before (state controller));
           assert (equal_state before (replayed fixture));
           [%test_eq: (int * string * string) list] (journal_facts fixture) frames;
           Record_tests.assert_code Conflict (Checked.check_current checked_a);
           (* The rejected write did not rotate Store Current. Reobserving B
              succeeds, while A remains closed even with fresh target proof. *)
           refresh controller permit_b (observe request_b)
           |> protocol_ok
           |> Checked.check_current
           |> protocol_ok;
           Record_tests.assert_code
             Conflict
             (refresh controller permit_a (observe request_a));
           assert (equal_state before (state controller));
           [%test_eq: (int * string * string) list] (journal_facts fixture) frames
         | Internal_stop ->
           assert (
             Option.is_some
               (A.Session_actor.claim_idle_moderator controller |> protocol_ok));
           A.Session_actor.complete_idle_moderator
             controller
             { moderator_snapshot = before.moderator
             ; runtime_requests = [ End_session "activation hook data" ]
             ; notifications = []
             ; remaining_events = false
             }
           |> protocol_ok;
           let stopped = state controller in
           assert stopped.halted;
           assert (P.Session.equal_desired_state stopped.lifecycle.desired Stopped);
           (* This actual internal result bypasses public Stop's prior pause. *)
           assert (E.equal_control (E.control (entry controller "activation_a")) Enabled)
         | Idle_failure ->
           assert (
             Option.is_some
               (A.Session_actor.claim_idle_moderator controller |> protocol_ok));
           let failure =
             P.Error.create
               Invalid_state
               ~message:"actual idle failure data"
               ~retryable:false
               ()
           in
           A.Session_actor.fail_idle_moderator controller failure |> protocol_ok;
           let failed = state controller in
           assert (Option.is_some failed.failure);
           (match failed.lifecycle.observed with
            | Failed actual -> assert (P.Error.equal_code actual.code failure.code)
            | Stopped
            | Queued_for_slot
            | Starting
            | Recovering
            | Idle
            | Running_turn _
            | Compacting _
            | Waiting_for_permission _
            | Stopping -> failwith "idle failure did not record observed Failed")
         | Source_replacement ->
           A.Session_actor.change_moderator controller None |> protocol_ok |> ignore;
           assert (Option.is_none (state controller).moderator);
           assert (
             E.equal_control
               (E.control (entry controller "activation_a"))
               (Paused Source_review))
         | Terminal_import ->
           let terminal = complete_target target request_a in
           Import_tests.import controller terminal |> protocol_ok |> ignore;
           [%test_eq: int] (H.pending_count (frontier controller "activation_a")) 0;
           [%test_eq: int64] (H.summary (frontier controller "activation_a")).settled 1L;
           refresh controller permit_b (observe request_b)
           |> protocol_ok
           |> Checked.check_current
           |> protocol_ok
         | Lifecycle_reservation ->
           let fence =
             A.Session_actor.begin_lifecycle
               controller
               ~attachment_id:None
               ~expected_generation:before.identity.generation
               ~expected_revision:before.counters.revision
             |> protocol_ok
           in
           Record_tests.assert_code Conflict (Checked.check_basis checked_a);
           A.Session_actor.abort_lifecycle controller fence |> protocol_ok;
           assert (equal_state before (state controller));
           [%test_eq: (int * string * string) list] (journal_facts fixture) frames
         | Runtime_retirement ->
           let retirement =
             A.Session_actor.retire_runtime_worker controller ~closing:false
             |> protocol_ok
           in
           A.Session_actor.Runtime_retirement.await retirement |> protocol_ok;
           (* Clear the finished nonclosing retirement through the real setter,
              without inventing a physical worker to claim its Some branch. *)
           A.Session_actor.set_operation_worker controller None |> protocol_ok;
           assert (equal_state before (state controller));
           [%test_eq: (int * string * string) list] (journal_facts fixture) frames
         | Owner_shutdown ->
           A.Session_actor.shutdown controller;
           Record_tests.assert_code
             Server_shutting_down
             (refresh controller permit_a (observe request_a));
           assert (equal_state before (replayed fixture));
           [%test_eq: (int * string * string) list] (journal_facts fixture) frames);
        Record_tests.assert_code Conflict (Checked.check_basis checked_a);
        (match hook with
         | Owner_shutdown -> ()
         | Rejected_pause
         | Internal_stop
         | Idle_failure
         | Source_replacement
         | Terminal_import
         | Lifecycle_reservation
         | Runtime_retirement ->
           Record_tests.assert_code
             Conflict
             (refresh controller permit_a (observe request_a));
           assert (equal_state (state controller) (replayed fixture)));
        release controller permit_a;
        release controller permit_b;
        print_s [%sexp (hook : Hook.t)]));
  [%expect
    {|
    Rejected_pause
    Internal_stop
    Idle_failure
    Source_replacement
    Terminal_import
    Lifecycle_reservation
    Runtime_retirement
    Owner_shutdown
  |}]
;;
