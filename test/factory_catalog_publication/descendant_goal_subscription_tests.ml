open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_authored_birth_support
module Direct = Descendant_turn_adapter_tests
module Subscription = Goal_subscription_runtime_tests
module Resume = Goal_resume_runtime_tests
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module Service = Agent_server.Goal_run_service
module G = P.Goal
module GD = A.Goal_descendants
module L = A.Inference_ledger
module S = A.Child_usage_snapshot
module Gate = Subscription.Gate
module Origin = Agent_store.Delegation_goal_origin
module Delegations = Agent_store.Delegation_store

let prompt =
  let before = "| `Turn_end ->\n  if state.waiting then" in
  assert (String.is_substring Subscription.prompt ~substring:before);
  {|<authoring_context policy="manual"/><config model="fixture-model"/>|}
  ^ String.substr_replace_all
      Subscription.prompt
      ~pattern:before
      ~with_:
        "| `Turn_end ->\n\
        \  if state.subscription == \"\" then Task.pure(state) else\n\
        \  if state.waiting then"
;;

let raise_recorded_failure failure =
  Option.iter !failure ~f:(fun (exn, trace) ->
    Exn.raise_with_original_backtrace exn trace)
;;

let completed_turns current =
  let turns =
    P.Inference_query.Summary.turns (L.summary current.A.Session_state.inference_ledger)
  in
  Int64.to_int_exn turns.completed
;;

let budget ~turns ~attempts =
  G.Budget.create
    ~admitted_turns:(Some turns)
    ~admitted_attempts:(Some attempts)
    ~observed_tokens:None
    ~deadline:None
  |> protocol_ok
;;

let change_budget daemon entry key value =
  Goal_admitted_guard_tests.command
    daemon
    entry
    ~key
    (Change_budget (Goal.goal_id, value))
  |> protocol_ok
  |> ignore
;;

let root_total parent =
  let current = state parent in
  let goal = Goal.goal current in
  let total =
    A.Child_contributions.aggregate_root
      (Option.value_exn current.child_contributions)
      ~owner:(G.session goal)
      ~goal_id:Goal.goal_id
    |> protocol_ok
  in
  let coverage = S.Descendants.coverage total in
  assert (not (S.Coverage.historical_children_unknown coverage));
  [%test_eq: int64] 0L (S.Coverage.unobserved_children coverage);
  S.Descendants.partition total |> S.Partition.marks
;;

module Probe = struct
  type t =
    { parent : Registry.entry
    ; child : Registry.entry
    ; scope : Origin.Scope.t
    ; reference : Delegations.Reference.t
    ; admission : P.Run_receipt.t
    ; before_child : A.Session_state.t
    ; before_parent : A.Session_state.t
    ; before_prefix : int64
    ; turns : int64
    ; attempts : int64
    ; local_budget : G.Budget.t
    ; root_budget : G.Budget.t
    ; mutable observed :
        (A.Session_state.t * A.Goal_descendant_included.t * L.Handle.t) option
    }

  let check_root t =
    let parent = state t.parent in
    let before = Goal.accounting t.before_parent in
    let actual = Goal.accounting parent in
    [%test_eq: int64]
      (A.Goal_accounting.admitted_turns before)
      (A.Goal_accounting.admitted_turns actual);
    [%test_eq: int64]
      (A.Goal_accounting.admitted_attempts before)
      (A.Goal_accounting.admitted_attempts actual);
    assert (G.Budget.equal t.root_budget (G.budget (Goal.goal parent)));
    let descendants = root_total t.parent in
    [%test_eq: int64]
      Int64.(A.Goal_accounting.admitted_turns actual + descendants.turn)
      (Option.value_exn (G.Budget.admitted_turns t.root_budget));
    [%test_eq: int64]
      Int64.(A.Goal_accounting.admitted_attempts actual + descendants.attempt)
      (Option.value_exn (G.Budget.admitted_attempts t.root_budget));
    List.iter [ A.Goal_admission_policy.Host_turn; Attempt ] ~f:(fun dimension ->
      let count, ceiling =
        match dimension with
        | Host_turn ->
          A.Goal_accounting.admitted_turns actual, G.Budget.admitted_turns t.root_budget
        | Attempt ->
          ( A.Goal_accounting.admitted_attempts actual
          , G.Budget.admitted_attempts t.root_budget )
      in
      let available =
        GD.available
          (Option.value_exn parent.goal_descendants)
          ~children:(Option.value_exn parent.child_contributions)
          ~goal_id:Goal.goal_id
          ~local_count:count
          ~ceiling
          ~dimension
        |> protocol_ok
      in
      assert (GD.Availability.equal available Exhausted))
  ;;

  let at_provider t =
    assert (Option.is_none t.observed);
    let child = state t.child in
    let operation = Option.value_exn child.active_operation in
    [%test_eq: P.Operation.kind] (Turn Idle_followup) operation.kind;
    Subscription.assert_operation_owned child t.admission operation;
    let run = Resume.run child t.admission in
    let turn =
      L.find_turn_handle
        child.inference_ledger
        ~operation_id:operation.id
        ~generation:child.identity.generation
      |> Option.value_exn
    in
    assert (P.Id.Operation.equal (L.Turn_handle.operation_id turn) operation.id);
    let receipt =
      Direct.confirmed
        t.parent
        t.child
        t.scope
        t.reference
        ~turns:t.turns
        ~attempts:t.attempts
    in
    let branch = Direct.branch t.parent t.scope t.reference in
    [%test_eq: int64] Int64.(t.before_prefix + 2L) (GD.Branch.confirmed_prefix branch);
    let occurrence =
      A.Goal_descendant_included.pending receipt |> A.Goal_descendant_pending.occurrence
    in
    let imported =
      A.Child_contributions.snapshot
        (Option.value_exn (state t.parent).child_contributions)
        ~reference:t.reference
      |> protocol_ok
      |> Option.value_exn
    in
    assert (
      Int64.(
        S.revision imported
        >= A.Goal_descendant_occurrence.Frame.candidate_session_revision
             (A.Goal_descendant_occurrence.frame occurrence)));
    let scopes = S.goal_scopes imported |> Option.value_exn in
    [%test_eq: int] 1 (List.length scopes);
    assert (Origin.Scope.equal (S.Goal_scope.scope (List.hd_exn scopes)) t.scope);
    let ordinal =
      match A.Goal_descendant_occurrence.kind occurrence with
      | Attempt { ordinal; operation_id; invocation_id } ->
        assert (Option.equal P.Id.Operation.equal (Some operation.id) operation_id);
        assert (Option.is_none invocation_id);
        ordinal
      | Host_turn _ -> failwith "subscription provider preceded inherited Attempt ACK"
    in
    let row = L.find child.inference_ledger ~ordinal |> Option.value_exn in
    let handle = L.Row.handle row in
    assert (P.Id.Session.equal (L.Handle.session_id handle) child.identity.session_id);
    assert (
      Option.equal P.Id.Operation.equal (Some operation.id) (L.Handle.operation_id handle));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ ->
       failwith "subscription provider preceded Running canonical Attempt ACK");
    let account = Goal.accounting child in
    let before_account = Goal.accounting t.before_child in
    [%test_eq: int64]
      (Int64.succ (A.Goal_accounting.admitted_turns before_account))
      (A.Goal_accounting.admitted_turns account);
    [%test_eq: int64]
      (Int64.succ (A.Goal_accounting.admitted_attempts before_account))
      (A.Goal_accounting.admitted_attempts account);
    assert (G.Budget.equal t.local_budget (G.budget (Goal.goal child)));
    [%test_eq: int64]
      (Option.value_exn (G.Budget.admitted_turns t.local_budget))
      (A.Goal_accounting.admitted_turns account);
    [%test_eq: int64]
      (Option.value_exn (G.Budget.admitted_attempts t.local_budget))
      (A.Goal_accounting.admitted_attempts account);
    assert (
      List.exists (A.Goal_accounting.bindings account) ~f:(fun binding ->
        L.Handle.equal (A.Goal_accounting.Binding.handle binding) handle
        && P.Id.Run.equal (A.Goal_accounting.Binding.run_id binding) run.id
        && P.Run_source.equal (A.Goal_accounting.Binding.source binding) run.source));
    Subscription.assert_published_proof child t.admission "545-subscription";
    Subscription.assert_published_proof child t.admission "545-ordinary";
    check_root t;
    t.observed <- Some (child, receipt, handle)
  ;;

  let after_terminal t =
    let _, receipt, handle = Option.value_exn t.observed in
    let current = state t.child in
    let actual =
      Direct.confirmed
        t.parent
        t.child
        t.scope
        t.reference
        ~turns:t.turns
        ~attempts:t.attempts
    in
    assert (A.Goal_descendant_included.equal receipt actual);
    let row =
      L.find current.inference_ledger ~ordinal:(L.Handle.ordinal handle)
      |> Option.value_exn
    in
    assert (L.Handle.equal handle (L.Row.handle row));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Terminal terminal ->
       assert (
         Inference.Event.Terminal.equal_outcome
           (Inference.Event.Terminal.outcome terminal)
           Completed)
     | Prepared | Running | Interrupted _ ->
       failwith "subscription Attempt did not complete");
    check_root t
  ;;
end

module Waiting_fixture = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; real_env : Eio_unix.Stdenv.base
    ; sw : Eio.Switch.t
    ; daemon : Daemon.t
    ; parent : Registry.entry
    ; child : Registry.entry
    ; scope : Origin.Scope.t
    ; reference : Delegations.Reference.t
    ; counters : Retry_support.counters
    ; session : P.Session_ref.t
    ; admission : P.Run_receipt.t
    ; run : P.Run.t
    ; wake : P.Run_wake.t
    ; held : A.Session_state.t
    ; ordinary : A.Session_state.t
    ; old_runtime : A.Runtime_builder.t
    ; providers_before : int
    ; completed_before : int
    ; child_calls : int ref
    ; failure : (exn * Stdlib.Printexc.raw_backtrace) option ref
    ; entered : unit Eio.Promise.t
    ; resumed : Gate.t
    }

  let write_context env child (run : P.Run.t) ?finish () =
    let directory =
      Option.value_exn (state child).spec.workspace_instance.configured_root
    in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / directory / Subscription.context_file)
      (Jsonaf.to_string
         (`Object
             ([ "run_id", P.Id.Run.to_json run.id
              ; "source", P.Run_source.to_json run.source
              ]
              @ Option.to_list
                  (Option.map finish ~f:(fun intent ->
                     "finish", P.Goal_achievement.Intent.to_json intent))))
       ^ "\n")
  ;;

  let save_context t ?finish () = write_context t.env t.child t.run ?finish ()
  let raise_failure t = raise_recorded_failure t.failure
  let release_wake_provider t = Gate.release t.resumed

  let await_wake_provider t =
    let rec await () =
      raise_failure t;
      match Eio.Promise.peek t.entered with
      | Some () -> ()
      | None ->
        Eio.Fiber.yield ();
        await ()
    in
    await ()
  ;;

  let with_waiting ~phase ~wrap_env ~at_wake_provider f =
    let initial = Gate.create () in
    let resumed = Gate.create () in
    let cache = Gate.create () in
    let cache_entered, cache_enter = Eio.Promise.create () in
    let entered, enter = Eio.Promise.create () in
    let context = ref None in
    let real_env = ref None in
    let cache_directory = ref None in
    let cache_hits = ref 0 in
    let child_calls = ref 0 in
    let selected = ref None in
    let failure = ref None in
    let raise_failure () = raise_recorded_failure failure in
    let policy =
      let defaults = Chat_response.Runtime_semantics.default_policy in
      { defaults with budget = { defaults.budget with max_followup_turns = 2 } }
    in
    let await_signal promise =
      let rec await () =
        raise_failure ();
        match Eio.Promise.peek promise with
        | Some () -> ()
        | None ->
          Eio.Fiber.yield ();
          await ()
      in
      await ()
    in
    try
      Birth.with_child
        ~phase
        ~prompt
        ~chatml_runtime_policy:policy
        ~post_creation_stream:(fun _ ->
          match !selected with
          | None -> completed_response "570 parent completed genuine child birth"
          | Some _ ->
            Int.incr child_calls;
            (match !child_calls with
             | 1 ->
               Eio.Promise.await initial.waiting;
               Subscription.watch_response ()
             | 2 | 3 -> completed_response "570 actual subscription setup/ordinary turn"
             | 4 ->
               (try at_wake_provider (Option.value_exn !context) with
                | exn -> failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
               Eio.Promise.resolve enter ();
               Eio.Promise.await resumed.waiting;
               completed_response "570 actual associated subscription wake"
             | _ ->
               failure
               := Some
                    ( Failure "subscription provider replayed"
                    , Stdlib.Printexc.get_callstack 16 );
               completed_response "570 unexpected replay"))
        ~wrap_env:(fun env ->
          real_env := Some env;
          let env = wrap_env env in
          Crash_fault_io.wrap
            env
            ~boundary:Before_rename
            ~matches:(fun path ->
              Option.exists !cache_directory ~f:(fun directory ->
                String.equal directory (Filename.dirname path)
                && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
                && String.is_suffix path ~suffix:".tmp"))
            ~reached:(fun _ ->
              cache_directory := None;
              Int.incr cache_hits;
              Eio.Promise.resolve cache_enter ();
              Eio.Promise.await cache.waiting))
        (fun env sw daemon parent child scope reference counters ->
           protect_cleanup
             ~finally:(fun () ->
               Gate.release initial;
               Gate.release resumed;
               Gate.release cache)
             (fun () ->
                selected := Some child;
                phase := "actual authored child bootstrap joined";
                let providers_before = !(counters.providers) in
                let completed_before = completed_turns (state child) in
                let old_runtime =
                  Owner.with_loaded_runtime child.runtime (fun runtime -> Ok runtime)
                  |> protocol_ok
                in
                let session, before = Goal.create_goal daemon child in
                cache_directory
                := Some
                     (Agent_store.Session_store.Handle.responses_directory
                        (Option.value_exn child.store_handle));
                phase := "local Goal start";
                let admission =
                  Resume.start_episode daemon child original session before
                in
                let run = Resume.run (state child) admission in
                write_context env child run ();
                Gate.release initial;
                phase := "actual pending mixed notices after Run.wait";
                Birth.within env !phase (fun () -> await_signal cache_entered);
                [%test_eq: int] 1 !cache_hits;
                [%test_eq: int] 2 !child_calls;
                let passive = Resume.checkpoint child in
                let wake = Subscription.waiting passive admission in
                (match wake.occurrence with
                 | Subscription_delivery { subscription_id; epoch; delivery_id; _ } ->
                   let subscription =
                     Resume.one_actual "actual subscription" passive.subscriptions
                   in
                   assert (P.Id.Subscription.equal subscription.context.id subscription_id);
                   [%test_eq: int] subscription.epoch epoch;
                   assert (
                     P.Id.Delivery.equal
                       (Subscription.delivery passive "545-subscription").context.id
                       delivery_id)
                 | Delivered_timer _ | Job_completion _ ->
                   failwith "native subscription lacks real Wait");
                [%test_eq: int] 2 (List.length passive.deliveries);
                List.iter passive.deliveries ~f:(fun value ->
                  match value.P.Delivery.status, value.wake_disposition with
                  | Pending, None -> ()
                  | Pending, Some _ | Failed _, _ | Committed _, _ ->
                    failwith "mixed notices lost pending state");
                Goal_admitted_guard_tests.command
                  daemon
                  child
                  ~key:"570-hold-mixed"
                  (Hold (Goal.goal_id, Operator_pause))
                |> protocol_ok
                |> ignore;
                let held = Resume.checkpoint child in
                Gate.release cache;
                phase := "ordinary notice remains outside local Run";
                let ordinary =
                  Resume.await_turns
                    ~providers:(providers_before + 3)
                    ~observe:(fun _ -> raise_failure ())
                    env
                    child
                    counters
                    ~completed:(completed_before + 2)
                in
                Subscription.pending ordinary |> ignore;
                let ordinary_operation = Subscription.accepted ordinary "545-ordinary" in
                assert (P.Run_wake.equal wake (Subscription.waiting ordinary admission));
                Resume.assert_episode held ordinary admission;
                assert (
                  G.State.equal
                    (Open (Held Operator_pause))
                    (G.state (Goal.goal ordinary)));
                assert (
                  A.Goal_accounting.equal
                    (Goal.accounting held)
                    (Goal.accounting ordinary));
                assert (
                  not
                    (List.exists
                       (Resume.operation_work (Resume.run ordinary admission))
                       ~f:(fun work ->
                         match work.P.Run_work.key with
                         | Operation id -> P.Id.Operation.equal id ordinary_operation
                         | Retained _ -> false)));
                let stable =
                  Birth.within env "ordinary notice source checkpoint" (fun () ->
                    Resume.settled_snapshot child)
                in
                assert (
                  not
                    (Factory.drain_idle_moderator (Daemon.factory daemon) child
                     |> protocol_ok));
                Existing.assert_same stable child;
                [%test_eq: int] 3 !child_calls;
                let fixture =
                  { env
                  ; real_env = Option.value_exn !real_env
                  ; sw
                  ; daemon
                  ; parent
                  ; child
                  ; scope
                  ; reference
                  ; counters
                  ; session
                  ; admission
                  ; run
                  ; wake
                  ; held
                  ; ordinary = stable
                  ; old_runtime
                  ; providers_before
                  ; completed_before
                  ; child_calls
                  ; failure
                  ; entered
                  ; resumed
                  }
                in
                context := Some fixture;
                f fixture))
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      print_s
        [%sexp ("associated subscription fixture failed" : string), (!phase : string)];
      Stdlib.Printexc.raise_with_backtrace exn backtrace
  ;;
end

let%expect_test "cold associated Goal subscription runs at local and inherited caps once" =
  let phase = ref "birth" in
  let probe = ref None in
  Waiting_fixture.with_waiting
    ~phase
    ~wrap_env:Fn.id
    ~at_wake_provider:(fun _ -> Probe.at_provider (Option.value_exn !probe))
    (fun fixture ->
       let { Waiting_fixture.env
           ; real_env
           ; daemon
           ; parent
           ; child
           ; scope
           ; reference
           ; counters
           ; session
           ; admission
           ; wake
           ; ordinary = stable
           ; old_runtime
           ; providers_before
           ; completed_before
           ; child_calls
           ; _
           }
         =
         fixture
       in
       let raise_failure () = Waiting_fixture.raise_failure fixture in
       let before_parent = state parent in
       let before_root = root_total parent in
       let root_account = Goal.accounting before_parent in
       let local_account = Goal.accounting stable in
       let local_budget =
         budget
           ~turns:(Int64.succ (A.Goal_accounting.admitted_turns local_account))
           ~attempts:(Int64.succ (A.Goal_accounting.admitted_attempts local_account))
       in
       let root_budget =
         budget
           ~turns:
             Int64.(A.Goal_accounting.admitted_turns root_account + before_root.turn + 1L)
           ~attempts:
             Int64.(
               A.Goal_accounting.admitted_attempts root_account + before_root.attempt + 1L)
       in
       change_budget daemon parent "570-root-cap" root_budget;
       change_budget daemon child "570-local-cap" local_budget;
       let capped = Resume.checkpoint child in
       let marks = Direct.marks child in
       let before_prefix =
         GD.Branch.confirmed_prefix (Direct.branch parent scope reference)
       in
       [%test_eq: int64] Int64.(marks.turn + marks.attempt) before_prefix;
       probe
       := Some
            Probe.
              { parent
              ; child
              ; scope
              ; reference
              ; admission
              ; before_child = capped
              ; before_parent
              ; before_prefix
              ; turns = Int64.succ marks.turn
              ; attempts = Int64.succ marks.attempt
              ; local_budget
              ; root_budget
              ; observed = None
              };
       phase := "unload held native runtime";
       Owner.unload_and_wait child.runtime |> protocol_ok;
       assert (not (Owner.is_loaded child.runtime));
       let cold = Resume.checkpoint child in
       assert (P.Run_wake.equal wake (Subscription.waiting cold admission));
       let actor = Staging.guarded Staging.resume_principal (ref true) in
       let request = Staging.exact_request cold ~key:"570-enable-real-wait" in
       phase := "passive public Resume";
       (match
          Service.resume_episode
            (Resume.public daemon)
            ~entry:child
            ~actor
            ~session
            ~request
          |> protocol_ok
        with
        | Resumed { continuation = Waiting actual; _ } ->
          assert (P.Run_wake.equal wake actual)
        | Resumed { continuation = Started _ | Settled _ | Controlled _; _ } | Retained _
          -> failwith "passive Resume fabricated a new Turn/Run"
        | Resume_failed { error; _ } | Rejected error | Uncertain error ->
          raise_s [%sexp (error : P.Error.t)]);
       phase := "true cold subscription provider at both caps";
       Birth.within real_env !phase (fun () ->
         Waiting_fixture.await_wake_provider fixture);
       raise_failure ();
       let probe = Option.value_exn !probe in
       let first_effect, _, _ = Option.value_exn probe.observed in
       let operation = Option.value_exn first_effect.active_operation in
       let rec joined () =
         raise_failure ();
         if not (Owner.source_lane_idle child.runtime)
         then (
           Eio.Fiber.yield ();
           joined ())
       in
       Birth.within real_env "cold wake source lane joined" joined;
       assert (Owner.is_loaded child.runtime);
       Owner.with_loaded_runtime child.runtime (fun runtime ->
         assert (not (phys_equal runtime old_runtime));
         Ok ())
       |> protocol_ok;
       let finish =
         P.Goal_achievement.Intent.create
           ~expected_goal_revision:(G.revision (Goal.goal first_effect))
           ~evidence:
             (G.Reason.of_string "Actual dual-accounted subscription provider"
              |> protocol_ok)
           ~action:(Finish { terminal = Completed None; relinquish = [] })
         |> protocol_ok
       in
       Waiting_fixture.save_context fixture ~finish ();
       Waiting_fixture.release_wake_provider fixture;
       phase := "same Run achieves after actual terminal ACK";
       let final =
         Resume.await_turns
           ~providers:(providers_before + 4)
           ~observe:(fun _ -> raise_failure ())
           env
           child
           counters
           ~completed:(completed_before + 3)
       in
       Resume.assert_episode cold final admission;
       Subscription.assert_succeeded_proof final admission (Operation operation.id);
       Subscription.assert_published_proof final admission "545-subscription";
       Subscription.assert_published_proof final admission "545-ordinary";
       let final_run = Resume.run final admission in
       (match final_run.lifecycle, G.state (Goal.goal final) with
        | Terminal (Completed None), Closed (Achieved achievement) ->
          let terminal =
            Resume.one_actual
              "same Run terminal receipt"
              (List.filter
                 (A.Run_state.receipts (Option.value_exn final.run_state))
                 ~f:(fun receipt ->
                   P.Id.Run.equal receipt.P.Run_receipt.run_id final_run.id
                   && P.Run_receipt.Kind.equal receipt.kind Terminal))
          in
          assert (P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement));
          assert (
            P.Run_receipt.equal
              terminal
              (P.Goal_episode.Settlement.receipt
                 (G.request_settlement (Goal.goal final) |> Option.value_exn)))
        | _ ->
          raise_s
            [%sexp "subscription did not settle SAME actual Goal", (final_run : P.Run.t)]);
       A.Goal_run_evidence.validate ~catalog:final.goal_catalog ~runs:final.run_state
       |> protocol_ok;
       Probe.after_terminal probe;
       let stable =
         Birth.within real_env "associated notice source checkpoint" (fun () ->
           Resume.settled_snapshot child)
       in
       phase := "exact wake duplicate and quiet no-Turn";
       (match
          Factory.deliver_goal_wake (Daemon.factory daemon) child ~wake |> protocol_ok
        with
        | No_work -> ()
        | Suspended_goal _ | Handed_off _ | Controlled _ ->
          failwith "subscription wake replayed");
       Existing.assert_same stable child;
       assert (
         not (Factory.drain_idle_moderator (Daemon.factory daemon) child |> protocol_ok));
       Existing.assert_same stable child;
       [%test_eq: int] 4 !child_calls;
       raise_failure ();
       print_endline
         "genuine cold subscription owns SAME Run and native/inherited ACKs at both \
          caps; ordinary notice stays separate; duplicate/quiet stays unchanged");
  [%expect
    {| genuine cold subscription owns SAME Run and native/inherited ACKs at both caps; ordinary notice stays separate; duplicate/quiet stays unchanged |}]
;;

module Scheduler_parking = struct
  type t =
    { pause : (unit -> unit) ref
    ; resume : (unit -> unit) ref
    ; wall_now : (unit -> float) ref
    ; mono_now : (unit -> Mtime.t) ref
    }

  let create () =
    { pause = ref ignore
    ; resume = ref ignore
    ; wall_now = ref (fun () -> failwith "actual scheduler wall clock absent")
    ; mono_now = ref (fun () -> failwith "actual scheduler Mono clock absent")
    }
  ;;

  let wrap t env =
    let env =
      Goal_background_runtime_tests.with_controlled_polling
        env
        ~pause:t.pause
        ~resume:t.resume
        ~clock_now:t.wall_now
    in
    let mono_clock, pause_mono, resume_mono, _ =
      controlled_monotonic_clock env#mono_clock
    in
    let pause_wall = !(t.pause) in
    let resume_wall = !(t.resume) in
    (t.pause
     := fun () ->
          pause_wall ();
          pause_mono ());
    (t.resume
     := fun () ->
          resume_mono ();
          resume_wall ());
    (t.mono_now := fun () -> Eio.Time.Mono.now mono_clock);
    (object
       method clock = env#clock
       method mono_clock = mono_clock
       method domain_mgr = env#domain_mgr
       method stdin = env#stdin
       method stdout = env#stdout
       method stderr = env#stderr
       method net = env#net
       method process_mgr = env#process_mgr
       method fs = env#fs
       method cwd = env#cwd
       method secure_random = env#secure_random
       method debug = env#debug
       method backend_id = env#backend_id
     end
     : Eio_unix.Stdenv.base)
  ;;

  let resume t = !(t.resume) ()

  let park t (fixture : Waiting_fixture.t) =
    !(t.pause) ();
    let rec future_waits () =
      Waiting_fixture.raise_failure fixture;
      match
        ( Daemon.For_testing.job_scheduler_wait_deadline fixture.daemon
        , Daemon.For_testing.schedule_scheduler_wait_deadline fixture.daemon )
      with
      | Some wall, Some mono
        when Float.(wall > !(t.wall_now) ()) && Mtime.compare mono (!(t.mono_now) ()) > 0
        -> ()
      | Some _, Some _ | Some _, None | None, Some _ | None, None ->
        Eio.Fiber.yield ();
        future_waits ()
    in
    future_waits ();
    Daemon.For_testing.await_schedule_scheduler_idle fixture.daemon
  ;;
end

let boundary_within (fixture : Waiting_fixture.t) = Birth.within fixture.real_env

let assert_no_job_producer (current : A.Session_state.t) =
  assert (List.is_empty current.jobs);
  assert (List.is_empty (A.Run_state.job_deliveries (Option.value_exn current.run_state)))
;;

let resume_same_wait (fixture : Waiting_fixture.t) key =
  let before = Resume.checkpoint fixture.child in
  let actor = Staging.guarded Staging.resume_principal (ref true) in
  let request = Staging.exact_request before ~key in
  (match
     Service.resume_episode
       (Resume.public fixture.daemon)
       ~entry:fixture.child
       ~actor
       ~session:fixture.session
       ~request
     |> protocol_ok
   with
   | Resumed { continuation = Waiting actual; _ } ->
     assert (P.Run_wake.equal fixture.wake actual)
   | Resumed { continuation = Started _ | Settled _ | Controlled _; _ } | Retained _ ->
     failwith "boundary passive Resume fabricated a Turn or Run"
   | Resume_failed { error; _ } | Rejected error | Uncertain error ->
     raise_s [%sexp (error : P.Error.t)]);
  let after = Resume.checkpoint fixture.child in
  Resume.assert_episode before after fixture.admission;
  assert (P.Run_wake.equal fixture.wake (Subscription.waiting after fixture.admission));
  assert (A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting after));
  after
;;

let assert_same_attempts before after =
  assert (
    List.equal
      (fun left right ->
         L.Handle.equal (L.Row.handle left) (L.Row.handle right)
         && Jsonaf.exactly_equal
              (Inference.Observation.Attempt_record.to_json (L.Row.record left))
              (Inference.Observation.Attempt_record.to_json (L.Row.record right)))
      (L.rows before.A.Session_state.inference_ledger)
      (L.rows after.A.Session_state.inference_ledger))
;;

let%expect_test
    "local Hold seals subscription Reserve and cancelled dual ACK survives strong close"
  =
  let module Barrier = Descendant_history_adapter_tests.Root_barrier in
  let reserve = Barrier.create () in
  let imported = Barrier.create () in
  let parking = Scheduler_parking.create () in
  let cache_directory = ref None in
  let cache_closes = ref 0 in
  let phase = ref "birth for actual subscription boundaries" in
  Waiting_fixture.with_waiting
    ~phase
    ~at_wake_provider:(fun _ ->
      failwith "cancelled or Held subscription reached a fourth provider")
    ~wrap_env:(fun env ->
      let env = Scheduler_parking.wrap parking env in
      let env = Barrier.wrap reserve env |> Barrier.wrap imported in
      Crash_fault_io.wrap
        env
        ~boundary:After_rename
        ~matches:(fun path ->
          Option.exists !cache_directory ~f:(fun directory ->
            String.equal directory (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun _ -> Int.incr cache_closes))
    (fun fixture ->
       let { Waiting_fixture.daemon; child; parent; scope; reference; sw; _ } = fixture in
       let within phase f = boundary_within fixture phase f in
       let first = ref None in
       let second = ref None in
       let closing = ref None in
       let caller, signal_caller = Eio.Promise.create () in
       let active_caller = ref None in
       let cancelled = Failure "570 native subscription caller cancelled during Import" in
       let join_cancelled pending =
         match Result.try_with (fun () -> Eio.Promise.await_exn pending) with
         | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause cancelled)
         | Error exn -> raise exn
         | Ok _ -> failwith "actual subscription caller lost cancellation"
       in
       let raise_failure () = Waiting_fixture.raise_failure fixture in
       let source_idle () =
         let rec await () =
           raise_failure ();
           if not (Owner.source_lane_idle child.runtime)
           then (
             Eio.Fiber.yield ();
             await ())
         in
         await ()
       in
       let await_gate barrier pending =
         Eio.Fiber.first
           (fun () -> Eio.Promise.await barrier.Barrier.entered)
           (fun () ->
              let result = Eio.Promise.await_exn pending in
              match result with
              | Error failure -> raise_s [%sexp (failure : P.Error.t)]
              | Ok _ -> failwith "actual subscription source returned before root gate")
       in
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.real_env) 90. (fun () ->
         protect_cleanup
           ~finally:(fun () ->
             Exn.protect
               ~finally:(fun () -> Scheduler_parking.resume parking)
               ~f:(fun () ->
                 (* Always release actual writers before joining their retained owners.
               Scheduler parking never supplies the timeout clock. *)
                 Waiting_fixture.release_wake_provider fixture;
                 Barrier.release reserve;
                 Barrier.release imported;
                 Option.iter !active_caller ~f:(fun context ->
                   Eio.Cancel.cancel context cancelled);
                 Option.iter !first ~f:(fun pending ->
                   within "join first real wake" (fun () -> Eio.Promise.await_exn pending)
                   |> ignore);
                 Option.iter !second ~f:(fun pending ->
                   within "join cancelled real wake" (fun () -> join_cancelled pending));
                 Option.iter !closing ~f:(fun pending ->
                   within "join actual source close" (fun () ->
                     Eio.Promise.await_exn pending))))
           (fun () ->
              phase := "park actual subscription schedulers after Held ordinary Turn";
              within "actual future scheduler sleeps and dispatch join" (fun () ->
                Scheduler_parking.park parking fixture);
              assert_no_job_producer fixture.ordinary;
              assert_no_job_producer (state child);
              (* Parking stops background progress after the terminal Turn. Settle
                 its retained facts before testing the next source reservation. *)
              within "actual Held inference facts settled" (fun () ->
                Actor.drain_inference_usage child.actor |> protocol_ok);
              within "actual Held source lane joined" source_idle;
              let before_root = Direct.branch parent scope reference in
              let root_account = Goal.accounting (state parent) in
              let root_usage = root_total parent in
              phase := "fresh passive Resume before actual Reserve";
              let enabled =
                within "passive Waiting Resume" (fun () ->
                  resume_same_wait fixture "570-boundary-enable-reserve")
              in
              let marks = Direct.marks child in
              reserve.target
              <- Some
                   ( (state parent).identity.session_id
                   , scope
                   , Barrier.Kind.Reserve
                       (GD.session (Option.value_exn enabled.goal_descendants)) );
              let pending =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Factory.deliver_goal_wake
                    (Daemon.factory daemon)
                    child
                    ~wake:fixture.wake)
              in
              first := Some pending;
              phase := "local Hold while real ancestor Reserve waits";
              within "ancestor Host_turn Reserve" (fun () -> await_gate reserve pending);
              (* Parent Actor owns its suspended writer; only the child is read. *)
              let selected = Resume.checkpoint child in
              assert (Option.is_none selected.active_operation);
              assert (
                P.Run_wake.equal
                  fixture.wake
                  (Subscription.waiting selected fixture.admission));
              let reserved_operation =
                match
                  A.Goal_descendant_occurrence.kind (Option.value_exn reserve.occurrence)
                with
                | Host_turn { operation_id } -> operation_id
                | Attempt _ -> failwith "subscription Reserve selected an Attempt"
              in
              assert (
                Option.is_none
                  (L.find_turn_handle
                     selected.inference_ledger
                     ~operation_id:reserved_operation
                     ~generation:selected.identity.generation));
              [%test_eq: int] 3 !(fixture.child_calls);
              Goal_admitted_guard_tests.command
                daemon
                child
                ~key:"570-held-during-reserve"
                (Hold (Goal.goal_id, Operator_pause))
              |> protocol_ok
              |> ignore;
              let held = Resume.checkpoint child in
              assert (
                G.State.equal (Open (Held Operator_pause)) (G.state (Goal.goal held)));
              assert (Option.is_none (Eio.Promise.peek reserve.resume));
              Barrier.release reserve;
              (match
                 within "Held actual source response" (fun () ->
                   Eio.Promise.await_exn pending)
               with
               | Error failure ->
                 (* Hold changes the captured State while root reservation waits.
                    The host's exact-before check rejects before local policy or
                    the child writer can run. *)
                 [%test_eq: P.Error.code] Conflict failure.code;
                 [%test_eq: string]
                   "child preparation actual before projection changed"
                   failure.message
               | Ok _ -> failwith "Held subscription source reached child writer");
              within "actual no-writer root close" (fun () ->
                Actor.drain_descendant_cleanup child.actor |> protocol_ok);
              within "Held source cleanup joined" source_idle;
              let branch = Direct.branch parent scope reference in
              assert (Option.is_none (GD.Branch.pending branch));
              [%test_eq: int64]
                (GD.Branch.confirmed_prefix before_root)
                (GD.Branch.confirmed_prefix branch);
              [%test_eq: int64]
                (Int64.succ (GD.Branch.last_nonce before_root))
                (GD.Branch.last_nonce branch);
              (match GD.Branch.last branch with
               | Some (No_writer_closed actual) ->
                 assert (
                   A.Goal_descendant_occurrence.equal
                     (A.Goal_descendant_pending.occurrence actual)
                     (Option.value_exn reserve.occurrence))
               | None | Some (Included _) ->
                 failwith "Held source lost exact no-writer seal");
              Existing.assert_same held child;
              [%test_eq: int] 3 !(fixture.child_calls);
              let no_writer_marks = Direct.marks child in
              [%test_eq: int64] marks.turn no_writer_marks.turn;
              [%test_eq: int64] marks.attempt no_writer_marks.attempt;
              assert (
                A.Goal_accounting.equal root_account (Goal.accounting (state parent)));
              let after_root = root_total parent in
              [%test_eq: int64] root_usage.turn after_root.turn;
              [%test_eq: int64] root_usage.attempt after_root.attempt;
              phase := "fresh SAME Wait Resume and actual cold wake preparation";
              within "re-enable actual unconsumed Wait" (fun () ->
                resume_same_wait fixture "570-boundary-enable-import")
              |> ignore;
              within "resumed source lane joined" source_idle;
              Owner.unload_and_wait child.runtime |> protocol_ok;
              assert (not (Owner.is_loaded child.runtime));
              let cold = Resume.checkpoint child in
              assert (
                P.Run_wake.equal
                  fixture.wake
                  (Subscription.waiting cold fixture.admission));
              let before_branch = Direct.branch parent scope reference in
              let before_local = Goal.accounting cold in
              let before_turns =
                L.summary cold.inference_ledger |> P.Inference_query.Summary.turns
              in
              imported.target
              <- Some
                   ( (state parent).identity.session_id
                   , scope
                   , Barrier.Kind.Import reference );
              let pending =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Eio.Cancel.sub (fun context ->
                    active_caller := Some context;
                    Exn.protect
                      ~finally:(fun () -> active_caller := None)
                      ~f:(fun () ->
                        Eio.Promise.resolve signal_caller context;
                        Factory.deliver_goal_wake
                          (Daemon.factory daemon)
                          child
                          ~wake:fixture.wake)))
              in
              second := Some pending;
              phase := "both native Goal ACKs and common child ACK before Import";
              within "actual parent Host_turn Import" (fun () ->
                await_gate imported pending);
              let accepted = Resume.checkpoint child in
              let operation = Option.value_exn accepted.active_operation in
              [%test_eq: P.Operation.kind] (Turn Idle_followup) operation.kind;
              (match operation.state with
               | Starting -> ()
               | Running | Cancelling | Completed | Failed _ | Cancelled | Interrupted _
                 ->
                 failwith "native subscription worker started before parent confirmation");
              Subscription.assert_operation_owned accepted fixture.admission operation;
              Resume.assert_episode cold accepted fixture.admission;
              let handle =
                L.find_turn_handle
                  accepted.inference_ledger
                  ~operation_id:operation.id
                  ~generation:operation.generation
                |> Option.value_exn
              in
              assert (
                P.Id.Operation.equal operation.id (L.Turn_handle.operation_id handle));
              [%test_eq: int] operation.generation (L.Turn_handle.generation handle);
              let included =
                Option.value_exn accepted.goal_descendants
                |> GD.local_included
                |> List.find_exn ~f:(fun included ->
                  A.Goal_descendant_occurrence.equal
                    (A.Goal_descendant_pending.occurrence
                       (A.Goal_descendant_included.pending included))
                    (Option.value_exn imported.occurrence))
              in
              let occurrence =
                A.Goal_descendant_included.pending included
                |> A.Goal_descendant_pending.occurrence
              in
              (match A.Goal_descendant_occurrence.kind occurrence with
               | Host_turn { operation_id } ->
                 assert (P.Id.Operation.equal operation.id operation_id)
               | Attempt _ -> failwith "native ACK Import selected an Attempt");
              [%test_eq: int64]
                (Int64.succ (A.Goal_accounting.admitted_turns before_local))
                (A.Goal_accounting.admitted_turns (Goal.accounting accepted));
              [%test_eq: int64]
                (A.Goal_accounting.admitted_attempts before_local)
                (A.Goal_accounting.admitted_attempts (Goal.accounting accepted));
              assert_same_attempts cold accepted;
              let acknowledged_marks = Direct.marks child in
              [%test_eq: int64] (Int64.succ marks.turn) acknowledged_marks.turn;
              [%test_eq: int64] marks.attempt acknowledged_marks.attempt;
              [%test_eq: int] 3 !(fixture.child_calls);
              assert (Owner.is_loaded child.runtime);
              assert (not (Owner.source_lane_idle child.runtime));
              assert (Option.is_none (Eio.Promise.peek pending));
              (* No loaded-runtime borrow here: its genuine ancestry guard would
               read the parent Actor whose writer this barrier suspends. Native
               State ACKs above and actual close/cache evidence below are separate. *)
              cache_directory
              := Some
                   (Agent_store.Session_store.Handle.cache_directory
                      (Option.value_exn child.store_handle));
              phase := "actual winning caller cancellation retains native ACK";
              Eio.Cancel.cancel (Eio.Promise.await caller) cancelled;
              within "cancelled real source caller" (fun () -> join_cancelled pending);
              let terminal =
                within "same actual native operation terminal ACK" (fun () ->
                  let rec await () =
                    raise_failure ();
                    let current = Resume.checkpoint child in
                    if Option.is_none current.active_operation
                    then current
                    else (
                      Eio.Fiber.yield ();
                      await ())
                  in
                  await ())
              in
              let terminal_run = Resume.run terminal fixture.admission in
              let proofs =
                List.filter terminal_run.terminal_work ~f:(fun proof ->
                  match proof.P.Run_work.Terminal.work.key with
                  | Operation id -> P.Id.Operation.equal operation.id id
                  | Retained _ -> false)
              in
              let proof =
                Resume.one_actual "same native operation terminal proof" proofs
              in
              [%test_eq: P.Run_work.Terminal.outcome] Failed proof.outcome;
              let failed_turns =
                L.summary terminal.inference_ledger |> P.Inference_query.Summary.turns
              in
              [%test_eq: int64] (Int64.succ before_turns.failed) failed_turns.failed;
              [%test_eq: int64] before_turns.completed failed_turns.completed;
              [%test_eq: int64] before_turns.cancelled failed_turns.cancelled;
              [%test_eq: int64] before_turns.interrupted failed_turns.interrupted;
              [%test_eq: int64] 0L failed_turns.pending;
              [%test_eq: int] 0 !cache_closes;
              assert (Owner.is_loaded child.runtime);
              (* Plain unload is a finite real Actor fence, so this also proves
               factual custody is still pending without reading the parent. *)
              (match Owner.unload child.runtime with
               | Error failure ->
                 [%test_eq: P.Error.code] Conflict failure.code;
                 [%test_eq: string]
                   "cannot unload runtime with outstanding descendant admission cleanup"
                   failure.message
               | Ok () ->
                 failwith "unconfirmed native ACK lost its runtime before strong close");
              let close =
                Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait child.runtime)
              in
              closing := Some close;
              (* Request close, then release; a pending fork is not evidence that
               close has already installed its negative. *)
              Barrier.release imported;
              phase := "join actual strong source close and root confirmation";
              within "strong native subscription source close" (fun () ->
                Eio.Promise.await_exn close);
              within "retained native/common ACK facts" (fun () ->
                Actor.drain_descendant_cleanup child.actor |> protocol_ok);
              assert (not (Owner.is_loaded child.runtime));
              (match
                 Owner.with_loaded_runtime child.runtime (fun _ ->
                   failwith "closed source ran callback")
               with
               | Error failure ->
                 [%test_eq: P.Error.code] Server_shutting_down failure.code
               | Ok _ -> failwith "strongly closed native source admitted a runtime");
              [%test_eq: int] 1 !cache_closes;
              let final = Resume.checkpoint child in
              let confirmed =
                Direct.confirmed
                  parent
                  child
                  scope
                  reference
                  ~turns:acknowledged_marks.turn
                  ~attempts:acknowledged_marks.attempt
              in
              assert (A.Goal_descendant_included.equal included confirmed);
              let final_branch = Direct.branch parent scope reference in
              assert (Option.is_none (GD.Branch.pending final_branch));
              [%test_eq: int64]
                (Int64.succ (GD.Branch.confirmed_prefix before_branch))
                (GD.Branch.confirmed_prefix final_branch);
              [%test_eq: int64]
                (Int64.succ (GD.Branch.last_nonce before_branch))
                (GD.Branch.last_nonce final_branch);
              let snapshot =
                A.Child_contributions.snapshot
                  (Option.value_exn (state parent).child_contributions)
                  ~reference
                |> protocol_ok
                |> Option.value_exn
              in
              assert (Delegations.Reference.equal reference (S.reference snapshot));
              let scopes = S.goal_scopes snapshot |> Option.value_exn in
              [%test_eq: int] 1 (List.length scopes);
              assert (Origin.Scope.equal scope (S.Goal_scope.scope (List.hd_exn scopes)));
              let coverage = S.descendants snapshot |> S.Descendants.coverage in
              assert (not (S.Coverage.historical_children_unknown coverage));
              [%test_eq: int64] 0L (S.Coverage.unobserved_children coverage);
              Resume.assert_episode accepted final fixture.admission;
              assert (
                P.Id.Operation.equal
                  operation.id
                  (Subscription.accepted final "545-subscription"));
              assert_same_attempts cold final;
              assert (
                A.Goal_accounting.equal (Goal.accounting accepted) (Goal.accounting final));
              assert (
                A.Goal_accounting.equal root_account (Goal.accounting (state parent)));
              let final_root = root_total parent in
              [%test_eq: int64] (Int64.succ root_usage.turn) final_root.turn;
              [%test_eq: int64] root_usage.attempt final_root.attempt;
              [%test_eq: int] 3 !(fixture.child_calls);
              let final_counts = !(fixture.counters.providers) in
              [%test_eq: int] (fixture.providers_before + 3) final_counts;
              Owner.close_and_wait child.runtime;
              [%test_eq: int] 1 !cache_closes;
              Existing.assert_same final child;
              [%test_eq: int] final_counts !(fixture.counters.providers);
              raise_failure ();
              print_endline
                "real local Hold closes exact no-writer reservation; cancelled \
                 native/common ACK keeps one charged Turn and closes resources once")));
  [%expect
    {| real local Hold closes exact no-writer reservation; cancelled native/common ACK keeps one charged Turn and closes resources once |}]
;;
