open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Owner = Agent_server.Runtime_owner
module Actor = A.Session_actor
module Held = Actor.Held_run
module Intent = A.Recurrence_execution_intent
module Execution_attempt = A.Recurrence_execution_attempt
module Attempt = Factory.Deferred_run_attempt
module Existing = Source_preparation_tests

(* Immutable request DATA is shared by the real source CPS, real Admission
   writer, and fault predicate. It grants no preparation or Held authority. *)
module Request = struct
  type t =
    { session : P.Session_ref.t
    ; intent : Intent.t
    ; attempt : Execution_attempt.t
    ; request : A.Run_admission_request.Headless.t
    }

  let create daemon entry key =
    let current = Existing.state entry in
    let session =
      P.Session_ref.create
        ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
        ~session_id:current.identity.session_id
    in
    let source =
      A.Run_source_installation.captured
        (A.Run_state.installation (Option.value_exn current.run_state))
        ~generation:current.identity.generation
      |> protocol_ok
    in
    let target =
      A.Recurrence_target.create_existing
        ~session
        ~source
        ~configuration_revision:current.spec.configuration_revision
        ~input:{ kind = Plain_text; text = "538 deferred admission"; attachments = [] }
      |> protocol_ok
    in
    let period =
      Recurrence_rules.Rule.Period.create_ms 600_000L
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
      |> Result.ok_or_failwith
    in
    let definition =
      A.Recurrence_definition.create
        ~key:
          (A.Recurrence_identity.Definition.create
             ~controller:session
             ~recurrence_id:(P.Id.Recurrence.of_string key |> protocol_ok)
             ~revision:0L
           |> protocol_ok)
        ~creator:(principal ()).id
        ~source
        ~birth_lifecycle_revision:P.Session_lifecycle.Revision.zero
        ~schedule:
          (Interval
             { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
             ; first_due = P.Timestamp.to_time_ns current.identity.updated_at
             })
        ~target
        ~policy:A.Recurrence_policy.default
        ~created_at:current.identity.updated_at
      |> protocol_ok
    in
    let intent =
      Intent.create
        ~definition
        ~control_revision:0L
        ~nominal_json:
          (A.Recurrence_nominal.create_interval definition ~slot:0L
           |> protocol_ok
           |> A.Recurrence_nominal.to_json)
        ~target:session
        ~source
        ~child:None
      |> protocol_ok
    in
    let attempt =
      Execution_attempt.create
        ~intent
        ~number:0
        ~principal_id:(principal ()).id
        ~expected_revision:current.counters.revision
        ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      |> protocol_ok
    in
    let request = Execution_attempt.request attempt ~intent |> protocol_ok in
    [%test_eq: string]
      (Execution_attempt.request_sha256 attempt)
      (A.Run_admission_request.request_sha256
         (A.Run_admission_request.of_headless request)
       |> protocol_ok);
    { session; intent; attempt; request }
  ;;

  let receipt t index =
    A.Run_state.receipt
      index
      ~principal_id:(Execution_attempt.principal_id t.attempt)
      ~key:(Execution_attempt.key t.attempt)
      ~request_sha256:(Execution_attempt.request_sha256 t.attempt)
    |> protocol_ok
  ;;

  let evidence t index =
    let admission = receipt t index |> Option.value_exn in
    assert (P.Run_receipt.Kind.equal admission.kind Admission);
    let run = A.Run_state.find index admission.run_id |> Option.value_exn in
    assert (P.Session_ref.equal run.session t.session);
    assert (P.Run.Lifecycle.equal run.lifecycle Admitted);
    [%test_eq: int64] 0L run.revision;
    assert (List.is_empty run.owned_work);
    let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
    A.Recurrence_run_origin.validate origin ~intent:t.intent ~attempt:t.attempt
    |> protocol_ok;
    A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
    admission
  ;;
end

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

let admission_index (request : Request.t) (transaction : Agent_store.Transaction.t) =
  if not (P.Id.Session.equal transaction.session_id request.request.session_id)
  then None
  else (
    let delta =
      A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
      |> function
      | Ok document -> A.Session_delta_document.value document
      | Error error -> raise_s [%sexp (error : D.Error.t)]
    in
    List.find_map (atoms delta) ~f:(function
      | Run_state_changed index ->
        Option.map (Request.receipt request index) ~f:(fun admission ->
          [%test_eq: int64] transaction.session_revision admission.session_revision;
          assert (P.Run_receipt.equal admission (Request.evidence request index));
          index)
      | _ -> None))
;;

module Fault = struct
  type t =
    { mutable armed : Request.t option
    ; mutable candidate : Agent_store.Transaction.t option
    ; mutable hits : int
    }

  let create () = { armed = None; candidate = None; hits = 0 }

  let reached t env path =
    match t.armed with
    | None -> ()
    | Some request ->
      (* Inspect the actual appended candidate through the unwrapped filesystem.
         It is visible at this boundary, but the writer has NOT acknowledged it. *)
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
           |> Existing.store_ok
         in
         if Option.is_some (admission_index request transaction)
         then (
           t.armed <- None;
           t.candidate <- Some transaction;
           t.hits <- t.hits + 1;
           raise (Core_unix.Unix_error (EIO, "538 admission before sync", path)))
       | Some _ | None -> ())
  ;;
end

module Cut = struct
  type t =
    | Known_held
    | Admission_before_sync
  [@@deriving sexp_of]
end

(* Raised only AFTER the failed-writer oracles and the outer Factory result have
   been checked. The exact observed primary survives poisoned fixture teardown. *)
exception Observed_persistence_failure of P.Error.t

let assert_error expected actual =
  assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
;;

let assert_pristine_effects (before : A.Session_state.t) (after : A.Session_state.t) =
  assert (
    Jsonaf.exactly_equal
      (state_document before |> Existing.store_ok |> D.Document.json)
      (state_document
         { after with
           counters = before.counters
         ; identity = { after.identity with updated_at = before.identity.updated_at }
         ; run_state = before.run_state
         }
       |> Existing.store_ok
       |> D.Document.json))
;;

let run (cut : Cut.t) =
  let fault = Fault.create () in
  let constructions = ref 0
  and providers = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "inert deferred admission dispatched inference")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr constructions;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~qualify_chatml_extensions:true
    ~prompt_text:Deferred_source_support.prompt_text
    ~policy
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_sync
        ~matches:(fun path ->
          String.is_substring path ~substring:"/journal/"
          && String.is_suffix path ~suffix:".log")
        ~reached:(Fault.reached fault env))
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Deferred_source_support.with_loaded_entry
           daemon
           (fun ~entry ~runtime:_ ~revision:_ ->
              let factory = Daemon.factory daemon in
              let request = Request.create daemon entry "rec_538_deferred_admission" in
              let before = Existing.state entry in
              let frames = Existing.frames env entry in
              let counts = !constructions, !providers in
              let expected = ref None
              and held = ref None
              and escaped = ref None in
              let original = Operator_authorization.trusted_local (principal ()) in
              let outcome =
                Factory.with_effect_deferred_run_source
                  factory
                  entry
                  ~actor:original
                  ~authorize:(fun _ -> Ok ())
                  ~request:request.request
                  (fun deferred prepared ->
                     escaped := Some prepared;
                     [%test_eq: string]
                       (Execution_attempt.request_sha256 request.attempt)
                       (A.Run_preparation.request_sha256
                          (A.Run_preparation.Deferred.preparation deferred));
                     (match cut with
                      | Known_held -> ()
                      | Admission_before_sync -> fault.armed <- Some request);
                     let binding = Owner.Prepared.binding prepared in
                     let admission =
                       Actor.admit_held_recurrence_run
                         entry.actor
                         ~binding
                         ~intent:request.intent
                         ~attempt:request.attempt
                         ~session:request.session
                       |> protocol_ok
                     in
                     let failure =
                       match cut, admission with
                       | Known_held, Held actual ->
                         held := Some actual;
                         Held.check_basis actual |> protocol_ok;
                         let current = Existing.state entry in
                         let receipt =
                           Request.evidence request (Option.value_exn current.run_state)
                         in
                         assert (P.Run_receipt.equal receipt (Held.admission actual));
                         assert_pristine_effects before current;
                         let after_frames = Existing.frames env entry in
                         [%test_eq: int]
                           (List.length frames + 1)
                           (List.length after_frames);
                         let transaction =
                           Agent_store.Transaction.decode (List.last_exn after_frames)
                           |> Existing.store_ok
                         in
                         assert (Option.is_some (admission_index request transaction));
                         P.Error.create
                           Conflict
                           ~message:"538 caller retains its complete selected failure"
                           ~retryable:false
                           ~data:(`Object [ "cut", `String "known-held" ])
                           ()
                       | Admission_before_sync, Uncertain failure ->
                         [%test_eq: P.Error.code] Persistence_error failure.code;
                         [%test_eq: int] 1 fault.hits;
                         let candidate = Option.value_exn fault.candidate in
                         assert (Option.is_some (admission_index request candidate));
                         [%test_eq: int64]
                           (Int64.succ before.counters.revision)
                           candidate.session_revision;
                         Existing.assert_same before entry;
                         assert (
                           Option.is_none
                             (Request.receipt
                                request
                                (Option.value_exn (Existing.state entry).run_state)));
                         (* The same closed physical source cannot retry. No new
                         request/CAS/preparation is synthesized after uncertainty. *)
                         (match
                            Actor.admit_held_recurrence_run
                              entry.actor
                              ~binding
                              ~intent:request.intent
                              ~attempt:request.attempt
                              ~session:request.session
                          with
                          | Error error -> [%test_eq: P.Error.code] Conflict error.code
                          | Ok _ ->
                            failwith "uncertain source recreated admission authority");
                         Existing.assert_same before entry;
                         failure
                       | _ -> failwith "actual Admission did not reach its selected cut"
                     in
                     [%test_eq: int * int] counts (!constructions, !providers);
                     expected := Some failure;
                     Error failure)
                |> protocol_ok
              in
              let actual =
                match outcome with
                | Attempt.Outcome.Attempted actual -> actual
                | Retained _ -> failwith "fresh attempt returned retained DATA"
              in
              let failure = Option.value_exn !expected in
              (match Attempt.result actual with
               | Error error -> assert_error failure error
               | Ok _ -> failwith "Factory erased the selected primary failure");
              assert (Option.is_none (Attempt.no_effects actual));
              assert (
                Result.is_error (Owner.Prepared.check_basis (Option.value_exn !escaped)));
              [%test_eq: int * int] counts (!constructions, !providers);
              [%test_eq: int] 0 !providers;
              match cut with
              | Admission_before_sync ->
                [%test_eq: int] 1 fault.hits;
                Existing.assert_same before entry;
                raise (Observed_persistence_failure failure)
              | Known_held ->
                let actual_held = Option.value_exn !held in
                assert (Result.is_error (Held.check_basis actual_held));
                let confirmed = Existing.state entry in
                let receipt = Held.admission actual_held in
                assert (
                  P.Run_receipt.equal
                    receipt
                    (Request.evidence request (Option.value_exn confirmed.run_state)));
                let callbacks = ref 0 in
                (match
                   Factory.with_effect_deferred_run_source
                     factory
                     entry
                     ~actor:original
                     ~authorize:(fun _ -> Ok ())
                     ~request:request.request
                     (fun _ _ ->
                        Int.incr callbacks;
                        Ok ())
                   |> protocol_ok
                 with
                 | Retained saved -> assert (P.Run_receipt.equal saved receipt)
                 | Attempted _ -> failwith "known receipt replayed source preparation");
                [%test_eq: int] 0 !callbacks;
                Existing.assert_same confirmed entry;
                [%test_eq: int]
                  (List.length frames + 1)
                  (List.length (Existing.frames env entry));
                [%test_eq: int * int] counts (!constructions, !providers))))
;;

let%test_unit "actual Factory held and uncertain Admissions refuse no-effects proof" =
  List.iter
    Cut.[ Known_held; Admission_before_sync ]
    ~f:(fun cut ->
      try
        match cut with
        | Known_held -> run cut
        | Admission_before_sync ->
          (match run cut with
           | () -> failwith "fault cut lost the observed persistence primary"
           | exception Observed_persistence_failure error ->
             [%test_eq: P.Error.code] Persistence_error error.code)
      with
      | exn ->
        Exn.reraise
          exn
          (Sexp.to_string_hum [%sexp "Factory deferred Admission cut", (cut : Cut.t)]))
;;
