open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Resume = Descendant_goal_resume_tests
module History = Descendant_history_adapter_tests
module Goal = Goal_service_tests
module Probe = Birth.Provider_probe
module GD = A.Goal_descendants
module O = Agent_store.Delegation_goal_origin
module D = Agent_store.Delegation_store
module M = Chat_response.Moderator_manager
module E = P.Moderator_execution
module I = P.Invocation
module L = A.Inference_ledger
module S = A.Child_usage_snapshot

let source =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>Actual inherited idle follow-up fixture.</developer>
<tool type="inherited" name="read_file"/>
<script id="idle_followup" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let read_gate () = Tool.call("read_file", `Object([
  {key = "root"; value = `String("data")},
  {key = "file"; value = `String("569-idle-gate.txt")}
]))
let on_event ctx state event = match event with
| `Session_start ->
  Task.bind(Runtime.emit(`String("seed")), fun ignored -> Task.pure(state))
| `Session_resume ->
  Task.bind(Runtime.emit(`String("pending")), fun ignored -> Task.pure(state))
| `Internal_event(payload) ->
  Task.bind(read_gate(), fun result ->
  match result with
  | `Error(message) -> Task.fail(message)
  | `Ok(_) ->
    match payload with
    | `String("seed") -> Task.pure(state + 1)
    | `String("pending") ->
      Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state + 1))
    | _ -> Task.fail("unexpected fixture event"))
| `Tool_observed(p) ->
  (match p.parent_event with
   | `None -> Task.fail("native event observation lost its actual parent")
   | `Some(_) ->
     if state == 1 then
       Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state + 1))
     else Task.pure(state + 1))
| _ -> Task.pure(state)
</script>
|}
;;

let checkpoint child =
  Actor.with_moderator_checkpoint child.Registry.actor (fun () -> Actor.state child.actor)
  |> protocol_ok
;;

let snapshot child =
  A.Moderator_checkpoint.decode (checkpoint child).moderator
  |> protocol_ok
  |> Option.value_exn
;;

let history child =
  (state child).conversation.canonical_history
  |> A.History_codec.all_of_protocol
  |> protocol_ok
;;

(* The real scheduler is parked. One actual Factory drain executes the pending
   lifecycle and returns before dispatching its newly emitted queue head. *)
let start_child daemon child ~phase =
  Factory.prepare_session_start (Daemon.factory daemon) child |> protocol_ok;
  Owner.ensure_loaded_authenticated child.Registry.runtime ~actor:original |> protocol_ok;
  let writer, _ =
    Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
    (fun () ->
       Actor.start child.actor ~actor:original ~attachment_id:writer.id
       |> protocol_ok
       |> ignore;
       let activation =
         Owner.with_loaded_runtime child.runtime (fun runtime ->
           Result.of_option
             runtime.A.Runtime_builder.moderator_activation
             ~error:(P.Error.invalid_request "actual idle source has no activation"))
         |> protocol_ok
       in
       assert (activation.pending ());
       let before = (state child).moderator_executions in
       assert (Factory.drain_idle_moderator (Daemon.factory daemon) child |> protocol_ok);
       assert (not (activation.pending ()));
       let current = checkpoint child in
       assert (Option.is_none current.active_operation);
       (match current.lifecycle.observed with
        | Idle -> ()
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> failwith "actual idle source did not become Idle");
       let added =
         List.filter current.moderator_executions ~f:(fun (receipt : E.t) ->
           not
             (List.exists before ~f:(fun (old : E.t) ->
                P.Id.Moderator_execution.equal old.context.id receipt.context.id)))
       in
       match added with
       | [ (receipt : E.t) ] ->
         assert (E.equal_phase receipt.context.phase phase);
         assert (Option.is_none receipt.context.operation_id);
         assert (Option.is_none receipt.context.job);
         assert (Option.is_none receipt.requests);
         (match receipt.status with
          | Completed _ -> ()
          | Running | Failed _ | Interrupted _ ->
            failwith "actual idle source lifecycle did not complete")
       | _ -> failwith "actual startup did not produce exactly one lifecycle receipt")
;;

(* The exact installed Owner lease, manager, script bridge and Actor claim used by
   Runtime_owner.drain_loaded_queued_events; no callback/receipt is seeded. *)
let run_event child =
  Owner.with_loaded_runtime child.Registry.runtime (fun runtime ->
    let manager = Option.value_exn runtime.A.Runtime_builder.moderator_manager in
    let entries = ref [] in
    let claim ~snapshot handle =
      Actor.with_current_idle_queued_moderator_event_tools
        child.actor
        ~snapshot
        (fun
            ~executing
             ~policy_caller
             ~model_jobs
             ~allocate_inference
             ~retirement_reason
             ~event
             ~execute
             ~commit
           ->
           entries := history child;
           handle
             ~executing
             ~policy_caller
             ~model_jobs
             ~allocate_inference
             ~retirement_reason
             ~event
             ~execute
             ~commit)
    in
    A.Moderator_event.run_queued_idle
      ~model_recipes:runtime.model_recipes
      ~claim
      ~run_actions:(Actor.run_actions child.actor)
      ?script_tools:runtime.moderator_script_tools
      ~manager
      ~history:(fun () -> !entries)
      ~available_tools:runtime.moderator_tools
      ~session_meta:`Null
      ~now:runtime.now
      ())
  |> protocol_ok
  |> Option.value_exn
;;

let observe child =
  Owner.with_loaded_runtime child.Registry.runtime (fun runtime ->
    let manager = Option.value_exn runtime.A.Runtime_builder.moderator_manager in
    let definition = M.extension_definition manager |> Option.value_exn in
    let observer = M.invocation_observer manager |> Option.value_exn in
    let script_tools = Option.value_exn runtime.moderator_script_tools in
    let entries = ref [] in
    A.Moderator_observation.drain_idle_with_tools
      ~model_recipes:runtime.model_recipes
      ~max_observations:1
      ~script_tools
      ~definition
      ~claim:(fun handle ->
        Actor.with_idle_moderator_observation_tools
          child.actor
          ~observer
          (fun ~observing ~capabilities ~execute ~commit ->
             entries := history child;
             handle ~observing ~capabilities ~execute ~commit))
      ~manager
      ~history:(fun () -> !entries)
      ~available_tools:runtime.moderator_tools
      ~session_meta:`Null
      ~now:runtime.now
      ())
  |> protocol_ok
;;

let event_after child before =
  (state child).moderator_executions
  |> List.filter ~f:(fun receipt ->
    E.equal_phase receipt.E.context.phase Internal_event
    && not
         (List.exists before ~f:(fun old ->
            P.Id.Moderator_execution.equal old.E.context.id receipt.context.id)))
  |> function
  | [ (receipt : E.t) ] ->
    (match receipt.status with
     | Completed _ -> ()
     | Running | Failed _ | Interrupted _ -> failwith "actual idle event did not complete");
    assert (Option.is_none receipt.context.operation_id);
    assert (Option.is_none receipt.context.job);
    receipt
  | found ->
    raise_s
      [%sexp
        "one actual queue head did not produce one execution", (List.length found : int)]
;;

let read_for child (receipt : E.t) =
  (state child).invocations
  |> List.filter ~f:(fun invocation ->
    String.equal invocation.I.context.tool_name "read_file"
    && Option.equal
         P.Id.Moderator_execution.equal
         invocation.parent_event
         (Some receipt.E.context.id))
  |> function
  | [ (invocation : I.t) ] ->
    assert (
      P.Id.Session.equal invocation.context.session_id (state child).identity.session_id);
    [%test_eq: int] (state child).identity.generation invocation.context.generation;
    let observation = Option.value_exn invocation.observation in
    assert (I.equal_observer observation.observer receipt.context.source);
    invocation
  | _ -> failwith "actual event did not own exactly one inherited read"
;;

let assert_request requests ~turn =
  [%test_eq: bool] turn requests.I.request_turn;
  assert (not requests.request_compaction);
  assert (Option.is_none requests.end_session)
;;

let branch parent scope reference =
  GD.branches (Option.value_exn (state parent).goal_descendants) ~goal_id:Goal.goal_id
  |> List.find_exn ~f:(fun branch ->
    O.Scope.equal (GD.Branch.scope branch) scope
    && D.Reference.equal (GD.Branch.reference branch) reference)
;;

let assert_counts parent child scope reference ~turns ~attempts =
  let actual =
    A.Child_accounting.snapshot (state child) |> protocol_ok |> Option.value_exn
  in
  let check partition =
    let marks = S.Partition.marks partition in
    [%test_eq: int64] turns marks.turn;
    [%test_eq: int64] attempts marks.attempt;
    [%test_eq: int64] 0L marks.untracked_turns;
    [%test_eq: int64] 0L marks.untracked_attempts
  in
  check (S.own actual);
  let scoped =
    S.goal_scopes actual
    |> Option.value_exn
    |> List.find_exn ~f:(fun entry -> O.Scope.equal (S.Goal_scope.scope entry) scope)
  in
  check (S.Goal_scope.own scoped);
  let branch = branch parent scope reference in
  assert (Option.is_none (GD.Branch.pending branch));
  [%test_eq: int64] Int64.(turns + attempts) (GD.Branch.confirmed_prefix branch)
;;

let%expect_test
    "genuine inherited event observation and pending user own idle follow-up Turns once"
  =
  let pause = ref ignore in
  let resume = ref ignore in
  let clock_now = ref (fun () -> failwith "actual scheduler clock absent") in
  let mono_now = ref (fun () -> failwith "actual scheduler monotonic clock absent") in
  let real_env = ref None in
  let gate_armed = ref false in
  let gate_path = ref None in
  let gate_hits = ref 0 in
  let gate_entered, signal_gate = Eio.Promise.create () in
  let gate_resume, release_gate = Eio.Promise.create () in
  let release_read () =
    gate_armed := false;
    if Option.is_none (Eio.Promise.peek gate_resume)
    then Eio.Promise.resolve release_gate ()
  in
  let roots = [ History.Root_barrier.create (); History.Root_barrier.create () ] in
  let active = ref None in
  let expected_input = ref None in
  let expected_operation = ref None in
  let provider_calls = ref 0 in
  let failure = ref None in
  let budget =
    P.Goal.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 5L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ~timeout:120.
    ~budget
    ~inherited_tools:[ "read_file", Resume.parent_reader ]
    ~child_sources:[ "child.chatmd", source ]
    ~wrap_env:(fun env ->
      real_env := Some env;
      let env =
        Goal_background_runtime_tests.with_controlled_polling
          env
          ~pause
          ~resume
          ~clock_now
      in
      let mono_clock, pause_mono, resume_mono, _ =
        controlled_monotonic_clock env#mono_clock
      in
      let pause_wall = !pause in
      let resume_wall = !resume in
      (pause
       := fun () ->
            pause_wall ();
            pause_mono ());
      (resume
       := fun () ->
            resume_mono ();
            resume_wall ());
      (mono_now := fun () -> Eio.Time.Mono.now mono_clock);
      let env : Eio_unix.Stdenv.base =
        object
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
      in
      let env =
        Crash_fault_io.wrap
          env
          ~boundary:Before_open_in
          ~matches:(fun path ->
            !gate_armed && Option.exists !gate_path ~f:(String.equal path))
          ~reached:(fun _ ->
            (* The real reader checks binary content and then opens the file
               again. Intercept only its first actual open in this window. *)
            gate_armed := false;
            Int.incr gate_hits;
            assert (Int.equal !gate_hits 1);
            Eio.Promise.resolve signal_gate ();
            Eio.Promise.await gate_resume)
      in
      List.fold roots ~init:env ~f:(fun env barrier ->
        History.Root_barrier.wrap barrier env))
    ~post_creation_stream:(fun inputs ->
      Option.iter !active ~f:(fun probe ->
        try
          Probe.before_submit probe;
          let operation = Option.value_exn (state probe.Probe.child).active_operation in
          let expected_id, reason = Option.value_exn !expected_operation in
          assert (P.Id.Operation.equal operation.id expected_id);
          (match operation.kind, reason with
           | Turn Moderator_request, `Moderator | Turn User_submit, `User -> ()
           | _ -> failwith "actual follow-up selected wrong Turn reason");
          Option.iter !expected_input ~f:(fun entry ->
            let current = state probe.Probe.child in
            let actual =
              List.filter current.conversation.canonical_history ~f:(fun actual ->
                P.History.Id.equal actual.P.History.id entry.P.History.id)
            in
            assert (List.equal P.History.equal_entry actual [ entry ]);
            let dispositions =
              List.filter_map
                current.conversation.pending_dispositions
                ~f:(fun document ->
                  let disposition = A.Pending_disposition_document.value document in
                  if
                    P.History.Id.equal
                      (A.Pending_disposition.history_id disposition)
                      entry.id
                  then Some disposition
                  else None)
            in
            (match dispositions with
             | [ disposition ] ->
               (match A.Pending_disposition.outcome disposition with
                | Adopted revision ->
                  assert (P.History.Content_revision.equal revision entry.content_revision)
                | Cancelled | Retired _ -> failwith "actual user input was not adopted")
             | _ -> failwith "actual user input lacks one adoption outcome");
            assert (List.is_empty current.conversation.deferred_user_entries);
            let text = "569 actual pending authenticated user" in
            [%test_eq: int]
              1
              (List.count inputs ~f:(fun input ->
                 let json = Openai.Responses.Item.jsonaf_of_t input in
                 Option.equal
                   Jsonaf.exactly_equal
                   (Jsonaf.member "role" json)
                   (Some (`String "user"))
                 && String.is_substring (Jsonaf.to_string json) ~substring:text)));
          Int.incr provider_calls
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          failure := Some (exn, backtrace);
          Exn.raise_with_original_backtrace exn backtrace);
      completed_response "569 actual inherited idle provider suffix")
    (fun env sw daemon parent child scope reference counters ->
       let clock = Eio.Stdenv.clock (Option.value_exn !real_env) in
       let within phase f =
         try Eio.Time.with_timeout_exn clock 30. f with
         | Eio.Time.Timeout -> failwith ("569 follow-up timed out: " ^ phase)
       in
       let raise_failure () =
         Option.iter !failure ~f:(fun (exn, backtrace) ->
           Exn.raise_with_original_backtrace exn backtrace)
       in
       Eio.Time.with_timeout_exn clock 90. (fun () ->
         protect_cleanup
           ~finally:(fun () ->
             release_read ();
             List.iter roots ~f:History.Root_barrier.release;
             !resume ())
           (fun () ->
              !pause ();
              within "real schedulers parked" (fun () ->
                let rec parked () =
                  match
                    ( Daemon.For_testing.job_scheduler_wait_deadline daemon
                    , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
                  with
                  | Some wall, Some mono
                    when Float.(wall > !clock_now ())
                         && Mtime.compare mono (!mono_now ()) > 0 -> ()
                  | _ ->
                    Eio.Fiber.yield ();
                    parked ()
                in
                parked ());
              (* The loop is parked after a complete pass. Join its actual
                 dispatched callbacks before starting the stopped child. *)
              within "real schedule callbacks joined" (fun () ->
                Daemon.For_testing.await_schedule_scheduler_idle daemon);
              let workspace =
                Option.value_exn (state parent).spec.workspace_instance.configured_root
              in
              let path = Eio.Path.(Eio.Stdenv.fs env / workspace / "569-idle-gate.txt") in
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                path
                "569 genuine inherited reader\n";
              (* Functions.scoped_resolver opens fs/canonical_path, rather than
                 the configured path (which may contain an OS symlink). *)
              gate_path
              := Some
                   (Eio.Path.native_exn
                      Eio.Path.(
                        Eio.Stdenv.fs env
                        / (state parent).spec.workspace_instance.canonical_root
                            .native_path
                        / "569-idle-gate.txt"));
              within "actual child startup" (fun () ->
                start_child daemon child ~phase:Session_start);
              Resume.assert_inherited_reader env daemon parent child;
              let writer, _ =
                Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () ->
                  within "detach actual writer" (fun () ->
                    Actor.detach child.actor writer.id |> protocol_ok))
                (fun () ->
                   let initial = snapshot child in
                   [%test_eq: int] 1 (List.length initial.queued_internal_events);
                   let parent_attempts_before =
                     A.Goal_accounting.admitted_attempts (Goal.accounting (state parent))
                   in
                   [%test_eq: int64] 2L parent_attempts_before;
                   let initial_providers = !(counters.providers) in
                   let initial_events = (state child).moderator_executions in
                   within "real seed event callback" (fun () -> run_event child) |> ignore;
                   let seed = event_after child initial_events in
                   assert (Option.is_none seed.requests);
                   let native = read_for child seed in
                   (match (Option.value_exn native.observation).status with
                    | Awaiting -> ()
                    | Observing | Observed | Observation_failed _ ->
                      failwith "real read was not awaiting observation");
                   assert (Option.is_none (state child).active_operation);
                   assert (List.is_empty (snapshot child).queued_internal_events);
                   assert_counts parent child scope reference ~turns:0L ~attempts:0L;
                   assert (
                     List.is_empty
                       (GD.local_included
                          (Option.value_exn (state child).goal_descendants)));
                   [%test_eq: int] initial_providers !(counters.providers);
                   within "inert actual event checkpoint" (fun () ->
                     Actor.apply_moderator_follow_up child.actor |> protocol_ok)
                   |> ignore;
                   assert (not (Actor.apply_moderator_follow_up child.actor |> protocol_ok));
                   assert_counts parent child scope reference ~turns:0L ~attempts:0L;
                   let observed =
                     within "genuine native Tool_observed callback" (fun () ->
                       observe child)
                   in
                   [%test_eq: int] 1 (List.length observed.outcomes);
                   let native =
                     List.find_exn (state child).invocations ~f:(fun current ->
                       P.Id.Invocation.equal current.I.context.id native.context.id)
                   in
                   (match Option.value_exn native.observation with
                    | { status = Observed
                      ; follow_up = Some (Pending_follow_up requests)
                      ; _
                      } -> assert_request requests ~turn:true
                    | _ -> failwith "actual Tool_observed did not retain its Turn intent");
                   let launch barrier ~turns ~reason apply =
                     assert (Option.is_none !active);
                     let probe =
                       Probe.create_with_counts
                         ~turns
                         ~attempts:turns
                         ~attempt_ceiling:5L
                         ~parent
                         ~child
                         ~scope
                         ~reference
                         ~parent_attempts_before
                     in
                     active := Some probe;
                     barrier.History.Root_barrier.target
                     <- Some ((state parent).identity.session_id, scope, Import reference);
                     let pending = Eio.Fiber.fork_promise ~sw apply in
                     protect_cleanup
                       ~finally:(fun () ->
                         History.Root_barrier.release barrier;
                         within "join actual follow-up caller" (fun () ->
                           Eio.Promise.await_exn pending)
                         |> ignore)
                       (fun () ->
                          within "actual Host_turn import" (fun () ->
                            let rec await () =
                              match Eio.Promise.peek barrier.entered with
                              | Some () -> ()
                              | None ->
                                (match Eio.Promise.peek pending with
                                 | Some (Ok (Error failure)) ->
                                   raise_s [%sexp (failure : P.Error.t)]
                                 | Some (Error exn) -> raise exn
                                 | Some (Ok (Ok _)) ->
                                   failwith
                                     "follow-up returned before real Host_turn import"
                                 | None ->
                                   Eio.Fiber.yield ();
                                   await ())
                            in
                            await ());
                          let occurrence = Option.value_exn barrier.occurrence in
                          let operation_id =
                            match A.Goal_descendant_occurrence.kind occurrence with
                            | Host_turn { operation_id } -> operation_id
                            | Attempt _ -> failwith "Host_turn barrier selected Attempt"
                          in
                          expected_operation := Some (operation_id, reason);
                          History.Root_barrier.release barrier;
                          assert (
                            within "actual alias result" (fun () ->
                              Eio.Promise.await_exn pending)
                            |> protocol_ok);
                          within "actual inherited provider terminal" (fun () ->
                            let rec quiet () =
                              raise_failure ();
                              let current = checkpoint child in
                              let completed =
                                P.Inference_query.Summary.turns
                                  (L.summary current.inference_ledger)
                              in
                              if
                                Option.is_none current.active_operation
                                && Int64.equal completed.completed turns
                              then ()
                              else (
                                Eio.Fiber.yield ();
                                quiet ())
                            in
                            quiet ());
                          raise_failure ();
                          Probe.after_terminal probe;
                          let _, handle = Option.value_exn probe.Probe.submitted in
                          assert (
                            Option.equal
                              P.Id.Operation.equal
                              (L.Handle.operation_id handle)
                              (Some operation_id));
                          active := None;
                          assert_counts
                            parent
                            child
                            scope
                            reference
                            ~turns
                            ~attempts:turns;
                          assert (
                            not
                              (Actor.apply_observation_follow_up child.actor
                               |> protocol_ok));
                          assert (
                            not
                              (Actor.apply_moderator_follow_up child.actor |> protocol_ok)))
                   in
                   launch (List.nth_exn roots 0) ~turns:1L ~reason:`Moderator (fun () ->
                     Actor.apply_observation_follow_up child.actor);
                   let applied =
                     List.find_exn (state child).invocations ~f:(fun current ->
                       P.Id.Invocation.equal current.I.context.id native.context.id)
                   in
                   (match (Option.value_exn applied.observation).follow_up with
                    | Some (Applied_follow_up requests) ->
                      assert_request requests ~turn:true
                    | None
                    | Some
                        ( Pending_follow_up _
                        | Compaction_accepted_follow_up _
                        | Discarded_follow_up _ ) ->
                      failwith "same real observed Turn intent was not applied");
                   [%test_eq: int] 1 !provider_calls;
                   [%test_eq: int]
                     1
                     (Option.value_exn (state child).automatic_turn_budget).followup_turns;
                   (* Foreground safe points drain genuine queued events. Emit
                      the pending head only in the later real cold lifecycle. *)
                   assert (List.is_empty (snapshot child).queued_internal_events);
                   let before_cold = checkpoint child in
                   let before_origin =
                     GD.origin (Option.value_exn before_cold.goal_descendants)
                   in
                   let before_source =
                     A.Runtime_builder.moderator_snapshot_observer before_cold.moderator
                     |> protocol_ok
                   in
                   let previous_worker =
                     Owner.with_loaded_runtime child.runtime (fun runtime ->
                       Ok runtime.A.Runtime_builder.worker)
                     |> protocol_ok
                   in
                   within "actual child stop and unload" (fun () ->
                     Actor.stop child.actor ~attachment_id:writer.id ~mode:Graceful
                     |> protocol_ok
                     |> ignore;
                     Owner.unload_and_wait child.runtime |> protocol_ok);
                   assert (not (Owner.is_loaded child.runtime));
                   within "actual child cold resume" (fun () ->
                     start_child daemon child ~phase:Session_resume);
                   Resume.assert_inherited_reader env daemon parent child;
                   let resumed = checkpoint child in
                   assert (
                     P.Id.Prompt_revision.equal
                       before_cold.spec.prompt_revision_id
                       resumed.spec.prompt_revision_id);
                   assert (
                     Option.equal
                       D.Reference.equal
                       before_cold.spec.delegation
                       resumed.spec.delegation);
                   assert (
                     Option.equal
                       O.equal
                       before_origin
                       (GD.origin (Option.value_exn resumed.goal_descendants)));
                   assert (
                     Option.equal
                       I.equal_observer
                       before_source
                       (A.Runtime_builder.moderator_snapshot_observer resumed.moderator
                        |> protocol_ok));
                   Owner.with_loaded_runtime child.runtime (fun runtime ->
                     assert (
                       not (phys_equal previous_worker runtime.A.Runtime_builder.worker));
                     Ok ())
                   |> protocol_ok;
                   assert_counts parent child scope reference ~turns:1L ~attempts:1L;
                   [%test_eq: int] 1 !provider_calls;
                   [%test_eq: int] 1 (List.length (snapshot child).queued_internal_events);
                   let initial_events = (state child).moderator_executions in
                   gate_armed := true;
                   let callback =
                     Eio.Fiber.fork_promise ~sw (fun () -> run_event child)
                   in
                   protect_cleanup
                     ~finally:(fun () ->
                       release_read ();
                       within "join real pending callback" (fun () ->
                         Eio.Promise.await_exn callback)
                       |> ignore)
                     (fun () ->
                        within "actual inherited read gate" (fun () ->
                          let rec await () =
                            match Eio.Promise.peek gate_entered with
                            | Some () -> ()
                            | None ->
                              (match Eio.Promise.peek callback with
                               | Some (Error exn) -> raise exn
                               | Some (Ok _) ->
                                 failwith
                                   "real pending callback returned before inherited read \
                                    gate"
                               | None ->
                                 Eio.Fiber.yield ();
                                 await ())
                          in
                          await ());
                        let before_user = state child in
                        assert (Option.is_none before_user.active_operation);
                        let id =
                          A.History_id_source.allocate child.history_ids |> protocol_ok
                        in
                        let entry =
                          Owner.parse_user_content
                            child.runtime
                            ~id
                            { kind = Plain_text
                            ; text = "569 actual pending authenticated user"
                            ; attachments = []
                            }
                          |> protocol_ok
                          |> A.History_codec.to_protocol
                        in
                        assert (
                          not
                            (List.exists
                               before_user.conversation.canonical_history
                               ~f:(fun actual -> P.History.Id.equal actual.id entry.id)));
                        let submitted =
                          Actor.submit_message_authenticated
                            child.actor
                            ~actor:original
                            ~attachment_id:writer.id
                            entry
                          |> protocol_ok
                        in
                        [%test_eq: P.History.Id.t] entry.id submitted.history_id;
                        (match submitted.disposition with
                         | Deferred -> ()
                         | _ -> failwith "genuine event borrow did not defer user input");
                        assert (Option.is_none submitted.operation_id);
                        let pending = (state child).conversation.deferred_user_entries in
                        assert (
                          List.equal
                            P.History.equal_entry
                            (List.map pending ~f:A.Pending_input_document.entry)
                            [ entry ]);
                        assert (
                          List.for_all pending ~f:(fun pending ->
                            A.Pending_input_document.Owner.equal
                              (A.Pending_input_document.owner pending)
                              (Submitting_principal
                                 (Operator_authorization.principal original).id)));
                        expected_input := Some entry;
                        release_read ();
                        within "actual pending event commit" (fun () ->
                          Eio.Promise.await_exn callback)
                        |> ignore);
                   let pending_event = event_after child initial_events in
                   assert_request (Option.value_exn pending_event.requests) ~turn:true;
                   read_for child pending_event |> ignore;
                   launch (List.nth_exn roots 1) ~turns:2L ~reason:`User (fun () ->
                     Actor.apply_moderator_follow_up child.actor);
                   let applied =
                     List.find_exn (state child).moderator_executions ~f:(fun current ->
                       P.Id.Moderator_execution.equal
                         current.E.context.id
                         pending_event.context.id)
                   in
                   (match applied.intent with
                    | Some Applied -> ()
                    | None | Some (Pending | Waiting_compaction _ | Discarded _) ->
                      failwith "same actual pending event intent was not applied");
                   [%test_eq: int] 2 !provider_calls;
                   [%test_eq: int] (initial_providers + 2) !(counters.providers);
                   [%test_eq: int] 1 !gate_hits;
                   [%test_eq: int]
                     0
                     (Option.value_exn (state child).automatic_turn_budget).followup_turns;
                   [%test_eq: int]
                     2
                     (List.count (state child).invocations ~f:(fun invocation ->
                        String.equal invocation.I.context.tool_name "read_file"));
                   assert (List.is_empty (state child).jobs);
                   assert (List.is_empty (state child).conversation.compaction_archives);
                   assert (List.is_empty (snapshot child).queued_internal_events);
                   print_endline
                     "real event checkpoint stays uncharged; observed follow-up and \
                      original pending user own two confirmed Turns once"))));
  [%expect
    {| real event checkpoint stays uncharged; observed follow-up and original pending user own two confirmed Turns once |}]
;;
