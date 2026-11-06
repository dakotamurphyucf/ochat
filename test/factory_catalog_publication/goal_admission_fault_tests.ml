open! Core
open Agent_server_test_support
open Retry_support
module Goal_service = Goal_service_tests
module Service = Agent_server.Goal_run_service
module Goal = P.Goal

let request (before : A.Session_state.t) ~stale =
  let goal = Goal_service.goal before in
  let headless =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input:
        (User_submission
           { kind = Plain_text
           ; text = "542 admission must not dispatch"
           ; attachments = []
           })
      ~key:(P.Idempotency_key.of_string "goal-admission-fault" |> protocol_ok)
    |> protocol_ok
  in
  A.Goal_activation_request.create
    ~goal_id:(Goal.id goal)
    ~expected_goal_revision:
      (if stale then Int64.succ (Goal.revision goal) else Goal.revision goal)
    ~expected_catalog_revision:
      (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
    ~request:headless
  |> protocol_ok
;;

let service daemon =
  Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
;;

let call daemon entry session request =
  Service.start_episode (service daemon) ~entry ~actor:original ~session ~request
  |> protocol_ok
;;

let attempted = function
  | Service.Outcome.Attempted attempt -> attempt
  | Retained _ -> failwith "new Goal admission unexpectedly returned retained DATA"
;;

let assert_inert env entry before frames counters ports =
  Existing.assert_same before entry;
  [%test_eq: string list] frames (Existing.frames env entry);
  [%test_eq: int] 0 !(counters.providers);
  [%test_eq: int] ports !(counters.inference_ports);
  assert (Option.is_none (Goal.episode (Goal_service.goal (state entry))));
  assert (Option.is_none (state entry).active_operation)
;;

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

module Fault = struct
  type selection =
    { session : P.Id.Session.t
    ; request : A.Goal_activation_request.t
    }

  type candidate =
    { index : A.Run_state.t
    ; catalog : A.Goal_catalog.t
    ; receipt : P.Run_receipt.t
    ; delta : A.Session_delta.t
    }

  type t =
    { mutable selected : selection option
    ; mutable candidate : candidate option
    ; mutable hits : int
    }

  let create () = { selected = None; candidate = None; hits = 0 }

  let reached t env path =
    match t.selected with
    | None -> ()
    | Some selected ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id selected.session
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           let changes = atoms delta in
           let index =
             List.find_map changes ~f:(function
               | A.Session_delta.Run_state_changed index -> Some index
               | _ -> None)
           in
           let catalog =
             List.find_map changes ~f:(function
               | A.Session_delta.Goal_catalog_changed catalog -> Some catalog
               | _ -> None)
           in
           match index, catalog with
           | Some index, Some catalog ->
             let request = A.Goal_activation_request.request selected.request in
             let digest =
               A.Run_admission_request.request_sha256
                 (A.Run_admission_request.of_headless request)
               |> protocol_ok
             in
             (match
                A.Run_state.receipt
                  index
                  ~principal_id:principal.id
                  ~key:request.key
                  ~request_sha256:digest
                |> protocol_ok
              with
              | None -> ()
              | Some receipt ->
                t.selected <- None;
                t.candidate <- Some { index; catalog; receipt; delta };
                t.hits <- t.hits + 1;
                raise (Core_unix.Unix_error (EIO, "542 atomic admission sync", path)))
           | None, _ | _, None -> ())
       | Some _ | None -> ())
  ;;
end

let assert_candidate (before : A.Session_state.t) request (candidate : Fault.candidate) =
  [%test_eq: int] 2 (List.length (atoms candidate.delta));
  let admission = candidate.receipt in
  let run = A.Run_state.find candidate.index admission.run_id |> Option.value_exn in
  let link = A.Run_state.goal_link candidate.index run.id |> Option.value_exn in
  let goal =
    A.Goal_catalog.find candidate.catalog Goal_service.goal_id |> Option.value_exn
  in
  let episode = Goal.episode goal |> Option.value_exn in
  assert (P.Run.Mode.equal Workflow run.mode);
  (match run.lifecycle with
   | Admitted -> ()
   | Active | Waiting _ | Terminal _ -> failwith "admission candidate already executed");
  assert (List.is_empty run.owned_work);
  [%test_eq: int64] 0L run.revision;
  assert (Goal.State.equal (Open Enabled) (Goal.state goal));
  [%test_eq: int64]
    (Int64.succ (Goal.revision (Goal_service.goal before)))
    (Goal.revision goal);
  assert (P.Goal_episode.matches_run episode run);
  assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
  assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
  [%test_eq: string]
    (A.Goal_episode_admission.digest (A.Goal_activation_request.intent request))
    (A.Run_goal_link.intent_sha256 link);
  assert (P.Id.Goal.equal (Goal.id goal) (A.Run_goal_link.goal_id link));
  assert (P.Run_source.equal (Goal.source goal) run.source);
  [%test_eq: int]
    1
    (List.count (A.Run_state.receipts candidate.index) ~f:(fun receipt ->
       P.Id.Run.equal receipt.P.Run_receipt.run_id run.id
       && P.Run_receipt.Kind.equal Admission receipt.kind))
;;

let assert_recovered candidate current =
  let index = Option.value_exn current.A.Session_state.run_state in
  let admission = candidate.Fault.receipt in
  let actual =
    A.Run_state.receipt
      index
      ~principal_id:admission.principal_id
      ~key:admission.key
      ~request_sha256:admission.request_sha256
    |> protocol_ok
    |> Option.value_exn
  in
  assert (P.Run_receipt.equal admission actual);
  assert (
    A.Run_goal_link.equal
      (A.Run_state.goal_link candidate.index admission.run_id |> Option.value_exn)
      (A.Run_state.goal_link index admission.run_id |> Option.value_exn));
  [%test_eq: int]
    1
    (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
       P.Id.Run.equal admission.run_id receipt.P.Run_receipt.run_id
       && P.Run_receipt.Kind.equal Admission receipt.kind));
  let account = Goal_service.accounting current in
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_attempts account);
  [%test_eq: int64] 0L (A.Goal_accounting.admitted_turns account);
  assert (List.is_empty (A.Goal_accounting.bindings account));
  assert (List.is_empty (A.Inference_ledger.rows current.inference_ledger))
;;

let reopen env sw root daemon policy entry before counters request candidate =
  let old_store = Daemon.store daemon in
  Daemon.shutdown daemon |> protocol_ok;
  assert (Agent_store.Session_store.is_closed old_store);
  let recovered =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () ->
       assert (not (phys_equal old_store (Daemon.store recovered)));
       let loaded =
         Registry.load
           (Daemon.registry recovered)
           before.A.Session_state.identity.session_id
         |> protocol_ok
       in
       assert (not (phys_equal entry.Registry.actor loaded.actor));
       let current = Retry_rotation_tests.await_recovered loaded before in
       assert_recovered candidate current;
       let frames = Existing.frames env loaded in
       let counts = !(counters.providers), !(counters.inference_ports) in
       let session =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (Daemon.store recovered))
           ~session_id:current.identity.session_id
       in
       (match call recovered loaded session request with
        | Retained admission -> assert (P.Run_receipt.equal candidate.receipt admission)
        | Attempted _ -> failwith "recovered association acquired fresh source custody");
       Existing.assert_same current loaded;
       [%test_eq: string list] frames (Existing.frames env loaded);
       [%test_eq: int * int] counts (!(counters.providers), !(counters.inference_ports));
       [%test_eq: int] 0 !(counters.providers))
;;

exception Observed_admission_failure of P.Error.t

let fault_case boundary ~recover =
  let fault = Fault.create () in
  (match
     with_recoverable_fixture
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary
           ~matches:(fun path ->
             String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(Fault.reached fault env))
       (fun env sw root daemon policy entry _runtime _revision counters ->
          let session, before = Goal_service.create_goal daemon entry in
          let request = request before ~stale:false in
          let ports = !(counters.inference_ports) in
          fault.selected <- Some { session = before.identity.session_id; request };
          let attempt = call daemon entry session request |> attempted in
          let failure =
            match Service.Attempt.result attempt |> protocol_ok with
            | Admission_uncertain error -> error
            | Activated _ | Retained _ | Activation_failed _ | Controlled _ | Settled _ ->
              failwith "failed admission write was not classified uncertain"
          in
          [%test_eq: P.Error.code] Persistence_error failure.code;
          [%test_eq: int] 1 fault.hits;
          assert (Option.is_none (Service.Attempt.no_effects attempt));
          let candidate = Option.value_exn fault.candidate in
          assert_candidate before request candidate;
          Existing.assert_same before entry;
          [%test_eq: int] 0 !(counters.providers);
          [%test_eq: int] ports !(counters.inference_ports);
          assert (Option.is_none (Goal.episode (Goal_service.goal (state entry))));
          if recover
          then reopen env sw root daemon policy entry before counters request candidate;
          (* Preserve the actual primary failure through the full fixture's
             factual cleanup, including a poisoned writer. *)
          raise (Observed_admission_failure failure))
   with
   | () -> failwith "atomic admission fault was not reached"
   | exception Observed_admission_failure failure ->
     [%test_eq: P.Error.code] Persistence_error failure.code);
  [%test_eq: int] 1 fault.hits
;;

let%expect_test
    "public Goal admission rejects before write and reconciles durable uncertainty"
  =
  with_fixture (fun env _sw daemon entry _runtime _revision counters ->
    let session, before = Goal_service.create_goal daemon entry in
    let frames = Existing.frames env entry in
    let ports = !(counters.inference_ports) in
    let attempt = call daemon entry session (request before ~stale:true) |> attempted in
    let failure = Service.Attempt.result attempt |> rejected in
    [%test_eq: P.Error.code] Conflict failure.code;
    assert (not failure.retryable);
    let observation = Service.Attempt.no_effects attempt |> Option.value_exn in
    Actor.No_effects_observation.check_current observation |> protocol_ok;
    Actor.No_effects_observation.check_basis observation |> protocol_ok;
    assert_inert env entry before frames counters ports);
  fault_case Before_sync ~recover:false;
  fault_case After_sync ~recover:true;
  print_endline "stale Goal CAS writes nothing and releases no execution";
  print_endline "atomic Goal/Run write uncertainty releases no admission or NoEffects";
  print_endline
    "durable sync without writer ACK reopens one original association; no replay";
  [%expect
    {|
    stale Goal CAS writes nothing and releases no execution
    atomic Goal/Run write uncertainty releases no admission or NoEffects
    durable sync without writer ACK reopens one original association; no replay
    |}]
;;
