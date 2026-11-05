open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module Service = Agent_server.Goal_run_service
module Resume = A.Goal_resume_request
module Report = Factory.Goal_resume_source
module Continuation = Actor.Goal_continuation

let resume_principal = principal_with_id "pri_543_resume_operator"

let authorize_as principal current =
  if
    Agent_server.Authorization.session_visible_to
      principal
      (A.Session_state.summary current)
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"Goal resume fixture is hidden"
         ~retryable:false
         ())
;;

let guarded principal live =
  Operator_authorization.guarded ~principal ~is_current:(fun () -> !live)
;;

let with_recoverable_episode ?(wrap_env = Fn.id) f =
  with_recoverable_fixture
    ~wrap_env
    (fun env sw root daemon policy entry runtime _revision counters ->
       let old_live = ref true in
       let old_original = guarded principal old_live in
       let session, before = Goal.create_goal daemon entry in
       let request =
         A.Run_admission_request.Headless.create
           ~session_id:before.identity.session_id
           ~generation:before.identity.generation
           ~expected_revision:before.counters.revision
           ~mode:Workflow
           ~input:
             (User_submission
                { kind = Plain_text; text = "543 original Goal input"; attachments = [] })
           ~key:(P.Idempotency_key.of_string "goal-resume-initial-episode" |> protocol_ok)
         |> protocol_ok
         |> fun request ->
         A.Goal_activation_request.create
           ~goal_id:Goal.goal_id
           ~expected_goal_revision:(G.revision (Goal.goal before))
           ~expected_catalog_revision:
             (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
           ~request
         |> protocol_ok
       in
       let service =
         Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun actor ->
           authorize_as (Operator_authorization.principal actor))
       in
       let admission =
         Service.start_episode service ~entry ~actor:old_original ~session ~request
         |> protocol_ok
         |> Goal.activated
       in
       let initial = Goal.await_input env entry counters in
       let run =
         A.Run_state.find (Option.value_exn initial.run_state) admission.run_id
         |> Option.value_exn
       in
       assert (P.Run.Mode.equal Workflow run.mode);
       (match run.lifecycle with
        | Admitted | Active -> ()
        | Waiting _ | Terminal _ ->
          raise_s [%sexp "initial Goal is not active", (run : P.Run.t)]);
       assert (P.Id.Principal.equal principal.id run.principal_id);
       Goal_admitted_guard_tests.command
         daemon
         entry
         ~key:"goal-resume-operator-hold"
         (Hold (Goal.goal_id, Operator_pause))
       |> protocol_ok
       |> ignore;
       let held = state entry in
       assert (G.State.equal (Open (Held Operator_pause)) (G.state (Goal.goal held)));
       assert (
         P.Run.equal
           run
           (A.Run_state.find (Option.value_exn held.run_state) admission.run_id
            |> Option.value_exn));
       [%test_eq: int] 1 !(counters.providers);
       f env sw root daemon policy entry runtime counters session admission old_live held)
;;

let with_episode ?wrap_env f =
  with_recoverable_episode
    ?wrap_env
    (fun
        env
         _sw
         _root
         daemon
         _policy
         entry
         runtime
         counters
         session
         admission
         old_live
         held
       -> f env daemon entry runtime counters session admission old_live held)
;;

let request (held : A.Session_state.t) ~key ~goal_revision ~run_revision ~source =
  let goal = Goal.goal held in
  let episode = G.episode goal |> Option.value_exn in
  Resume.create
    ~session_id:held.identity.session_id
    ~generation:held.identity.generation
    ~goal_id:Goal.goal_id
    ~run_id:(P.Goal_episode.admission episode).run_id
    ~source
    ~expected_goal_revision:goal_revision
    ~expected_catalog_revision:
      (A.Goal_catalog.revision (Option.value_exn held.goal_catalog))
    ~expected_run_revision:run_revision
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let exact_request held ~key =
  let goal = Goal.goal held in
  let admission = G.episode goal |> Option.value_exn |> P.Goal_episode.admission in
  let run =
    A.Run_state.find (Option.value_exn held.A.Session_state.run_state) admission.run_id
    |> Option.value_exn
  in
  request
    held
    ~key
    ~goal_revision:(G.revision goal)
    ~run_revision:run.revision
    ~source:run.source
;;

let stage daemon entry session ~actor request f =
  Factory.with_goal_resume_source_reported
    (Daemon.factory daemon)
    entry
    ~actor
    ~authorize:(authorize_as (Operator_authorization.principal actor))
    ~session
    ~request
    f
  |> protocol_ok
;;

let historical entry session ~actor request =
  Actor.goal_resume_receipt
    entry.Registry.actor
    ~actor
    ~authorize:(authorize_as (Operator_authorization.principal actor))
    ~session
    ~request
  |> protocol_ok
;;

let ledger_json ledger =
  match A.Inference_ledger.to_document ledger with
  | Ok document -> Document_schema.Document.json document
  | Error error -> raise_s [%sexp (error : A.Inference_ledger.Error.t)]
;;

let assert_existing_episode (before : A.Session_state.t) (after : A.Session_state.t) =
  assert (
    A.Run_state.equal
      (Option.value_exn before.run_state)
      (Option.value_exn after.run_state));
  assert (A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting after));
  assert (
    Jsonaf.exactly_equal
      (A.Goal_accounts.to_jsonaf (Option.value_exn before.goal_accounts) |> protocol_ok)
      (A.Goal_accounts.to_jsonaf (Option.value_exn after.goal_accounts) |> protocol_ok));
  assert (
    Jsonaf.exactly_equal
      (ledger_json before.inference_ledger)
      (ledger_json after.inference_ledger));
  assert (
    Option.equal
      P.Goal_episode.equal
      (G.episode (Goal.goal before))
      (G.episode (Goal.goal after)));
  assert (P.Run_source.equal (G.source (Goal.goal before)) (G.source (Goal.goal after)));
  assert (
    List.equal
      P.Moderator_execution.equal
      before.moderator_executions
      after.moderator_executions);
  assert (Option.is_none after.active_operation)
;;

let assert_receipt
      request
      (before : A.Session_state.t)
      (receipt : A.Goal_command_receipt.t)
  =
  assert (P.Id.Principal.equal resume_principal.id receipt.principal_id);
  assert (P.Idempotency_key.equal (Resume.key request) receipt.key);
  [%test_eq: string] (Resume.digest request |> protocol_ok) receipt.request_sha256;
  assert (P.Id.Goal.equal Goal.goal_id receipt.goal_id);
  assert (Option.exists receipt.source ~f:(P.Run_source.equal (Resume.source request)));
  [%test_eq: int64] (Int64.succ (G.revision (Goal.goal before))) receipt.goal_revision;
  [%test_eq: int64]
    (Int64.succ (A.Goal_catalog.revision (Option.value_exn before.goal_catalog)))
    receipt.catalog_revision
;;

let%expect_test "Goal resume source staging acknowledges the existing episode only" =
  with_episode (fun env daemon entry runtime counters session admission _old_live held ->
    let actor = guarded resume_principal (ref true) in
    assert (not (P.Id.Principal.equal principal.id resume_principal.id));
    let request = exact_request held ~key:"goal-resume-source-ack" in
    assert (Option.is_none (historical entry session ~actor request));
    let transactions = Retry_tests.transaction_payloads env entry in
    let ports = !(counters.inference_ports) in
    let escaped = ref None in
    let callbacks = ref 0 in
    let report =
      stage daemon entry session ~actor request (fun prepared ->
        Int.incr callbacks;
        Owner.Prepared_goal_continuation.check_basis prepared |> protocol_ok;
        let binding = Owner.Prepared_goal_continuation.binding prepared in
        escaped := Some binding;
        match Actor.admit_goal_resume entry.actor ~binding |> protocol_ok with
        | Ready continuation -> Ok continuation
        | Retained _ | Waiting _ | Rejected _ | Uncertain _ | Resume_failed _ ->
          failwith "active existing episode did not acknowledge a Ready staging owner")
    in
    let continuation =
      match Report.result report |> protocol_ok with
      | Fresh continuation -> continuation
      | Retained _ -> failwith "fresh resume key unexpectedly retained"
    in
    [%test_eq: int] 1 !callbacks;
    let receipt = Continuation.receipt continuation in
    assert_receipt request held receipt;
    assert (
      Option.exists (Report.receipt report) ~f:(A.Goal_command_receipt.equal receipt));
    (match Owner.Scope_outcome.Cleanup.status (Report.cleanup report) with
     | Joined -> ()
     | Never_acquired | Failed _ -> failwith "actual source staging cleanup did not join");
    assert (
      Result.is_error
        (Continuation.Source_binding.check_basis (Option.value_exn !escaped)));
    let current = state entry in
    assert_existing_episode held current;
    assert (G.State.equal (Open Enabled) (G.state (Goal.goal current)));
    [%test_eq: int64] (Int64.succ held.counters.revision) current.counters.revision;
    let after_transactions = Retry_tests.transaction_payloads env entry in
    [%test_eq: int] (List.length transactions + 1) (List.length after_transactions);
    [%test_eq: string list]
      transactions
      (List.take after_transactions (List.length transactions));
    [%test_eq: int] 1 !(counters.providers);
    [%test_eq: int] ports !(counters.inference_ports);
    Owner.with_loaded_runtime entry.runtime (fun actual ->
      assert (phys_equal runtime actual);
      Ok ())
    |> protocol_ok;
    assert (
      Option.exists
        (historical entry session ~actor request)
        ~f:(A.Goal_command_receipt.equal receipt));
    let frames = Existing.frames env entry in
    let repeated =
      stage daemon entry session ~actor request (fun _ ->
        Int.incr callbacks;
        Ok ())
    in
    (match Report.result repeated |> protocol_ok with
     | Retained actual -> assert (A.Goal_command_receipt.equal receipt actual)
     | Fresh () -> failwith "retained command recreated source custody");
    assert (
      Option.exists (Report.receipt repeated) ~f:(A.Goal_command_receipt.equal receipt));
    [%test_eq: int] 1 !callbacks;
    Existing.assert_same current entry;
    [%test_eq: string list] frames (Existing.frames env entry);
    [%test_eq: int] ports !(counters.inference_ports);
    [%test_eq: int] 1 !(counters.providers);
    let index = Option.value_exn current.run_state in
    [%test_eq: int] 1 (List.length (A.Run_state.runs index));
    assert (
      P.Id.Principal.equal
        principal.id
        (A.Run_state.find index admission.run_id |> Option.value_exn).principal_id);
    print_endline
      "one resume command ACK preserves the original Run, episode, account and runtime";
    print_endline
      "closed Ready is staging DATA; exact command replay creates no execution owner");
  [%expect
    {|
    one resume command ACK preserves the original Run, episode, account and runtime
    closed Ready is staging DATA; exact command replay creates no execution owner
    |}]
;;

let%expect_test "Goal resume rejects stale source fences and either expired original" =
  with_episode (fun env daemon entry _runtime counters session _admission old_live held ->
    let new_live = ref true in
    let actor = guarded resume_principal new_live in
    let original_request = exact_request held ~key:"goal-resume-denials" in
    let source = Resume.source original_request in
    let changed_source =
      P.Run_source.create
        ~observer:source.observer
        ~generation:source.generation
        ~installation_epoch:(Int64.succ source.installation_epoch)
      |> protocol_ok
    in
    let wrong key ~goal_revision ~run_revision ~source =
      request held ~key ~goal_revision ~run_revision ~source
    in
    let frames = Existing.frames env entry in
    let ports = !(counters.inference_ports) in
    let callbacks = ref 0 in
    let denies actor expected request =
      let report =
        stage daemon entry session ~actor request (fun _ ->
          Int.incr callbacks;
          Ok ())
      in
      let (error : P.Error.t) = Report.result report |> rejected in
      [%test_eq: P.Error.code] expected error.code;
      assert (not error.retryable);
      assert (Option.is_none (Report.receipt report));
      [%test_eq: int] 0 !callbacks;
      Existing.assert_same held entry;
      [%test_eq: string list] frames (Existing.frames env entry);
      [%test_eq: int] ports !(counters.inference_ports);
      [%test_eq: int] 1 !(counters.providers)
    in
    denies
      actor
      Conflict
      (wrong
         "goal-resume-wrong-goal-cas"
         ~goal_revision:(Int64.succ (Resume.expected_goal_revision original_request))
         ~run_revision:(Resume.expected_run_revision original_request)
         ~source);
    denies
      actor
      Conflict
      (wrong
         "goal-resume-wrong-run-cas"
         ~goal_revision:(Resume.expected_goal_revision original_request)
         ~run_revision:(Int64.succ (Resume.expected_run_revision original_request))
         ~source);
    denies
      actor
      Conflict
      (wrong
         "goal-resume-wrong-source-epoch"
         ~goal_revision:(Resume.expected_goal_revision original_request)
         ~run_revision:(Resume.expected_run_revision original_request)
         ~source:changed_source);
    new_live := false;
    denies actor Permission_denied original_request;
    assert !old_live;
    let fresh_actor = guarded resume_principal (ref true) in
    old_live := false;
    denies fresh_actor Permission_denied original_request;
    print_endline "Goal/Run CAS and complete source epoch reject without writes";
    print_endline "fresh command authority cannot replace the original Run authorization");
  [%expect
    {|
    Goal/Run CAS and complete source epoch reject without writes
    fresh command authority cannot replace the original Run authorization
    |}]
;;

module Fault = struct
  type selection =
    { request : Resume.t
    ; principal_id : P.Id.Principal.t
    }

  type candidate =
    { transaction : Agent_store.Transaction.t
    ; catalog : A.Goal_catalog.t
    ; receipt : A.Goal_command_receipt.t
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
       | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id (Resume.session_id selected.request)
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           match Goal_admission_fault_tests.atoms delta with
           | [ Goal_catalog_changed catalog ] ->
             (match
                A.Goal_catalog.receipt
                  catalog
                  ~principal_id:selected.principal_id
                  ~key:(Resume.key selected.request)
                  ~request_sha256:(Resume.digest selected.request |> protocol_ok)
                |> protocol_ok
              with
              | None -> ()
              | Some receipt ->
                t.selected <- None;
                t.candidate <- Some { transaction; catalog; receipt };
                t.hits <- t.hits + 1;
                raise (Core_unix.Unix_error (EIO, "543 resume command before sync", path)))
           | _ -> ())
       | Some _ | None -> ())
  ;;
end

exception Observed_resume_failure of P.Error.t

let%expect_test "unknown first Goal resume writer releases no acknowledged owner" =
  let fault = Fault.create () in
  (match
     with_episode
       ~wrap_env:(fun env ->
         Crash_fault_io.wrap
           env
           ~boundary:Before_sync
           ~matches:(fun path ->
             String.is_substring path ~substring:"/journal/"
             && String.is_suffix path ~suffix:".log")
           ~reached:(Fault.reached fault env))
       (fun env daemon entry _runtime counters session _admission _old_live held ->
          let actor = guarded resume_principal (ref true) in
          let request = exact_request held ~key:"goal-resume-unknown-command" in
          let ports = !(counters.inference_ports) in
          let transactions = Retry_tests.transaction_payloads env entry in
          fault.selected <- Some { request; principal_id = resume_principal.id };
          let failure = ref None in
          let callbacks = ref 0 in
          let report =
            stage daemon entry session ~actor request (fun prepared ->
              Int.incr callbacks;
              match
                Actor.admit_goal_resume
                  entry.actor
                  ~binding:(Owner.Prepared_goal_continuation.binding prepared)
                |> protocol_ok
              with
              | Uncertain error ->
                failure := Some error;
                Error error
              | Retained _ | Waiting _ | Ready _ | Rejected _ | Resume_failed _ ->
                failwith "unknown first command writer returned an acknowledged owner")
          in
          let (actual : P.Error.t) = Report.result report |> rejected in
          [%test_eq: P.Error.code] Persistence_error actual.code;
          assert_error (Option.value_exn !failure) actual;
          [%test_eq: int] 1 !callbacks;
          [%test_eq: int] 1 fault.hits;
          assert (Option.is_none (Report.receipt report));
          let candidate = Option.value_exn fault.candidate in
          assert_receipt request held candidate.receipt;
          [%test_eq: int64]
            (Int64.succ held.counters.revision)
            candidate.transaction.session_revision;
          let candidate_goal =
            A.Goal_catalog.find candidate.catalog Goal.goal_id |> Option.value_exn
          in
          assert (G.equal (G.resume (Goal.goal held) |> protocol_ok) candidate_goal);
          assert (
            Option.equal
              P.Goal_episode.equal
              (G.episode (Goal.goal held))
              (G.episode candidate_goal));
          Existing.assert_same held entry;
          let written = Retry_tests.transaction_payloads env entry in
          [%test_eq: int] (List.length transactions + 1) (List.length written);
          [%test_eq: string list]
            transactions
            (List.take written (List.length transactions));
          [%test_eq: string]
            (Agent_store.Transaction.encode candidate.transaction)
            (List.last_exn written);
          let (read_error : P.Error.t) =
            Actor.goal_resume_receipt
              entry.actor
              ~actor
              ~authorize:(authorize_as (Operator_authorization.principal actor))
              ~session
              ~request
            |> rejected
          in
          [%test_eq: P.Error.code] Journal_corrupt read_error.code;
          [%test_eq: int] 1 !(counters.providers);
          [%test_eq: int] ports !(counters.inference_ports);
          (* The real writer is poisoned; preserve this actual primary failure
            through all lexical cleanup rather than pretending cleanup success. *)
          raise (Observed_resume_failure actual))
   with
   | () -> failwith "resume writer fault was not reached"
   | exception Observed_resume_failure error ->
     [%test_eq: P.Error.code] Persistence_error error.code);
  [%test_eq: int] 1 fault.hits;
  print_endline
    "written resume candidate is unconfirmed; original held episode and account remain \
     exact";
  print_endline "uncertain first writer yields no command ACK or executable continuation";
  [%expect
    {|
    written resume candidate is unconfirmed; original held episode and account remain exact
    uncertain first writer yields no command ACK or executable continuation
    |}]
;;
