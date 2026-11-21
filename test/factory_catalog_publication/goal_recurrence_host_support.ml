open! Core
open Agent_server_test_support
module Portable_path = Source.Portable_path
open Retry_support
module Native = Goal_native_start_tests
module G = P.Goal
module Exec = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference
module Descendants = A.Goal_descendants
module Delegations = Agent_store.Delegation_store
module Scope = Agent_store.Delegation_goal_origin.Scope
module Ledger = A.Inference_ledger
module Totals = A.Goal_policy_totals
module Driver = Agent_server.Recurrence_dispatcher
module Client = Agent_client
module Lifecycle = P.Session_lifecycle

let recurrence_id = P.Id.Recurrence.of_string "rec_goal_review" |> protocol_ok
let goal_id = P.Id.Goal.of_string "goal_recurring_review" |> protocol_ok
let input_text = "Prepare the handoff for two release-note reviews."
let example = "docs-src/examples/learning/goal-recurrence/"
let guidance_root = example ^ "guidance"
let guidance_members = [ "AGENTS.md"; "release-review/SKILL.md" ]

let installed_package inventory resource member =
  let module Sources = Capability_sources in
  let checked result =
    Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
  in
  let resource =
    Sources.find inventory (Sources.Id.of_string resource |> checked) |> checked
  in
  let path = Portable_path.create member |> Or_error.ok_exn in
  Sources.Resource.member resource path |> checked |> Sources.Member.contents
;;

let startup : Native.Startup_fixture.t =
  let packages =
    [ "goal-requests", "goal_requests", "goal-requests"
    ; "recurrence-requests", "recurrence_requests", "recurrence-requests"
    ; "instruction-requests", "instruction_requests", "instruction-selection"
    ; "lifecycle", "lifecycle", "lifecycle"
    ; "lifecycle-run", "lifecycle_run", "lifecycle"
    ]
    |> List.map ~f:(fun (directory, source, installed) ->
      "examples/libraries/" ^ directory ^ "/", source, installed)
  in
  let workspace_prompt = example ^ "agent.chatmd" in
  let prepare_sources env root =
    let install member contents =
      let path = Eio.Path.(Eio.Stdenv.fs env / root / member) in
      Eio.Path.mkdirs
        ~exists_ok:true
        ~perm:0o700
        (Eio.Path.split path |> Option.value_exn |> fst);
      Eio.Path.save ~create:(`Exclusive 0o600) path contents
    in
    let copy member =
      install member (In_channel.read_all (Filename.concat "../.." member))
    in
    List.iter
      [ "agent.chatmd"; "chatml.project.json"; "moderator.chatml"; "worker.chatmd" ]
      ~f:(fun member -> copy (example ^ member));
    List.iter guidance_members ~f:(fun member ->
      copy (Filename.concat guidance_root member));
    let inventory =
      let module Sources = Capability_sources in
      Sources.installed ~limits:Sources.Limits.default ~checkpoint:ignore
      |> Result.map_error ~f:Sources.Error.message
      |> Result.ok_or_failwith
    in
    List.iter packages ~f:(fun (directory, source, resource) ->
      List.iter
        [ directory ^ "chatml.project.json"
        ; directory ^ "src/" ^ source ^ ".chatml"
        ; directory ^ "src/" ^ source ^ ".chatmli"
        ]
        ~f:(fun member ->
          install ("docs-src/" ^ member) (installed_package inventory resource member)))
  in
  { workspace_prompt = Some workspace_prompt
  ; workspace_project_roots =
      List.map packages ~f:(fun (directory, _, _) -> "docs-src/" ^ directory)
  ; prepare_sources
  ; goal_id
  ; create_key = "recurring-review-create"
  ; start_key = "recurring-review-start"
  ; input_text
  }
;;

let configure_guidance root (config : Agent_server.Config.t) =
  let root = Filename.concat root guidance_root in
  let project_sources = Option.value_exn config.server.project_sources in
  { config with
    server =
      { config.server with
        project_sources =
          Some
            { project_sources with
              external_roots = root :: project_sources.external_roots
            }
      ; authoring_project_access =
          Some
            { read_grants =
                [ { root
                  ; principal_id = principal.id
                  ; authentication_kind = principal.authentication_kind
                  }
                ]
            ; selections = []
            }
      }
  }
;;

let await ?(on_timeout = fun () -> []) ~clock ~phase f =
  try
    Eio.Time.with_timeout_exn clock 30. (fun () ->
      let rec loop () =
        match f () with
        | Some value -> value
        | None ->
          Eio.Time.sleep clock 0.025;
          loop ()
      in
      loop ())
  with
  | Eio.Time.Timeout ->
    raise_s [%sexp ("Timed out while " ^ phase : string), (on_timeout () : Sexp.t list)]
;;

let validate_healthy (current : A.Session_state.t) =
  Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
  Native.completed_callbacks current;
  assert (not current.halted);
  current
;;

let healthy entry = validate_healthy (state entry)
let goal = Native.Startup_fixture.goal startup

let recurrence (current : A.Session_state.t) =
  Catalog.find (Option.value_exn current.recurrence_catalog) recurrence_id
  |> Option.value_exn
;;

let frontier current = Entry.frontier (recurrence current) |> protocol_ok

let with_driver env sw daemon (parent : Registry.entry) f =
  let session_id = (state parent).identity.session_id in
  let module C = Agent_server.Recurrence_execution_configuration in
  let grant =
    C.Controller.create ~session_id ~existing_targets:[] ~captured_children:true
    |> protocol_ok
  in
  let configuration =
    C.create
      ~principal_id:(P.Id.Principal.of_string "pri_goal_review_executor" |> protocol_ok)
      ~controllers:[ grant ]
      ~expires_at:None
    |> protocol_ok
  in
  with_planner env sw daemon parent (fun () ->
    Recurrence_target_acquisition_tests.with_execution_host
      daemon
      sw
      configuration
      (fun host ->
         let driver =
           Driver.create
             ~factory:(Daemon.factory daemon)
             ~registry:(Daemon.registry daemon)
             ~host
         in
         protect_cleanup ~finally:(fun () -> Driver.close driver) (fun () -> f driver)))
;;

let drive operation =
  match operation with
  | Ok () | Error { P.Error.code = Conflict; retryable = true; _ } -> ()
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let run current run_id =
  A.Run_state.find (Option.value_exn current.A.Session_state.run_state) run_id
  |> Option.value_exn
;;

let one = Goal_resume_runtime_tests.one_actual

let operator_connection daemon principal =
  let active = Atomic.make true in
  let actor =
    Operator_authorization.guarded ~principal ~is_current:(fun () -> Atomic.get active)
  in
  let notifications = Eio.Stream.create 256 in
  let context =
    Agent_server.Connection_context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Attachment.create () |> P.Id.Attachment.to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  Client.In_memory.create
    ~request:(fun command ->
      Agent_server.Dispatcher.dispatch_command (Daemon.dispatcher daemon) ~context command)
    ~notifications
    ~close:(fun () ->
      Atomic.set active false;
      Daemon.close_connection daemon context)
;;

let with_operator ?(principal = principal) daemon f =
  let connection = operator_connection daemon principal in
  protect_cleanup
    ~finally:(fun () -> Client.Connection.close connection)
    (fun () ->
       initialize connection;
       f connection)
;;

let with_attachment env sw connection ~session_id ~mode f =
  let handle =
    Client.Session_handle.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~session_id
      ~mode
      ~subscribe:false
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Client.Session_handle.close handle)
    (fun () -> f handle)
;;

let with_writer env sw connection ~session_id f =
  with_attachment env sw connection ~session_id ~mode:Read_write f
;;

module Guidance = struct
  module Wire = P.Instruction_control
  module Controls = Client.Instruction_controls

  let root controls =
    Controls.refresh controls ~selection:None
    |> protocol_ok
    |> Wire.Discovery.roots
    |> one "granted guidance root"
  ;;

  let manifests controls =
    let root = root controls in
    [ Wire.Selection.Project { root; scope = None }; Skills [ root ] ]
    |> List.map ~f:(fun selection ->
      let observed =
        Controls.refresh controls ~selection:(Some selection) |> protocol_ok
      in
      selection, Wire.Discovery.members observed)
  ;;

  let select env sw daemon (entry : Registry.entry) =
    with_operator daemon (fun connection ->
      let session_id = (state entry).identity.session_id in
      with_writer env sw connection ~session_id (fun handle ->
        let controls = Client.Session_handle.instructions handle in
        List.iteri (manifests controls) ~f:(fun index (selection, members) ->
          let member = one "explicitly selected instruction" members in
          [%test_eq: string]
            (List.nth_exn guidance_members index)
            (Portable_path.to_string member.member);
          let before = Controls.inspect controls |> protocol_ok in
          [%test_eq: int64] (Int64.of_int index) before.summary.revision;
          let source = Wire.Source_request.create ~selection ~members |> protocol_ok in
          let capture = Controls.capture controls source |> protocol_ok in
          let key =
            P.Idempotency_key.of_string ("choose-review-guidance-" ^ Int.to_string index)
            |> protocol_ok
          in
          let receipt =
            Controls.activate
              controls
              ~expected_generation:before.generation
              ~expected_revision:before.summary.revision
              ~key
              capture
            |> protocol_ok
          in
          [%test_eq: int64] (Int64.succ before.summary.revision) receipt.revision)))
  ;;

  let assert_provider_inputs inputs =
    let guidance =
      List.filter_map inputs ~f:(fun input ->
        let json = Res.Item.jsonaf_of_t input in
        let text = Jsonaf.to_string json in
        match Jsonaf.member "role" json with
        | Some (`String "user")
          when String.is_substring
                 text
                 ~substring:"OChat session guidance from explicitly selected files." ->
          Some text
        | _ -> None)
      |> one "selected guidance request item"
    in
    List.iter
      [ "Release-note handoff guidance"; "Review handoff checklist" ]
      ~f:(fun text -> assert (String.is_substring guidance ~substring:text));
    assert (
      not (String.is_substring guidance ~substring:"Revised guidance must stay inactive"))
  ;;
end

let lifecycle connection session_id =
  let snapshot = Client.Admin.get_session connection session_id |> protocol_ok in
  (P.Public.Snapshot.fields snapshot).lifecycle |> Option.value_exn
;;

let lifecycle_request observation key =
  Lifecycle.Request.create
    ~expected:(Lifecycle.Observation.expected observation)
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
;;

let assert_no_child daemon current =
  let frontier = frontier current in
  assert (List.is_empty (Frontier.protected_children frontier));
  assert (List.is_empty (Frontier.child_associations frontier));
  assert (List.is_empty (Frontier.executions frontier));
  let store = Daemon.store daemon in
  let empty_ledger =
    Delegations.with_records
      (Agent_store.Session_store.delegations store)
      ~max_records:16
      ~max_bytes:1_048_576
      ~f:(fun records -> Ok (List.is_empty records))
    |> Source_preparation_tests.store_ok
  in
  assert empty_ledger;
  let indexed =
    Agent_store.Session_index.list_checked (Agent_store.Session_store.session_index store)
    |> Source_preparation_tests.store_ok
    |> one "controller is the only indexed session"
  in
  assert (
    P.Id.Session.equal indexed.session.id current.A.Session_state.identity.session_id)
;;

let execution current ~slot =
  let rows = Frontier.executions (frontier current) in
  List.find rows ~f:(fun execution ->
    let nominal = Intent.nominal (Exec.intent execution) in
    Int64.equal
      slot
      (A.Recurrence_identity.Occurrence.slot (A.Recurrence_nominal.key nominal)))
;;

let assert_capture current ~session ~episode =
  let entry = recurrence current in
  assert (Entry.equal_control (Entry.control entry) Enabled);
  let definition = Entry.current entry in
  (match A.Recurrence_definition.goal_binding definition with
   | Unbound -> failwith "authored recurrence lost its actual Goal"
   | Controller_goal binding ->
     let module Capture = A.Recurrence_definition.Goal_binding.Capture in
     assert (P.Session_ref.equal (Capture.owner binding) session);
     assert (P.Id.Goal.equal (Capture.goal_id binding) goal_id);
     assert (P.Goal_episode.equal (Capture.creator_episode binding) episode));
  match A.Recurrence_definition.schedule definition with
  | Calendar _ -> failwith "authored fixed-delay definition changed its schedule"
  | Interval { rule; first_due } ->
    assert (
      Recurrence_rules.Rule.Interval.equal_mode
        (Recurrence_rules.Rule.Interval.mode rule)
        Fixed_delay);
    assert (
      Time_ns.equal
        first_due
        (P.Timestamp.to_time_ns (P.Goal_episode.admission episode).committed_at))
;;

type occurrence =
  { origin : Origin.t
  ; terminal : Terminal.t
  ; reference : Delegations.Reference.t
  }

let advance_schedule_and_join ~clock daemon (started : Native.accepted_start) seconds =
  let await_idle () =
    await ~clock ~phase:"joining the daemon schedule pass" (fun () ->
      match Daemon.For_testing.schedule_scheduler_wait_deadline daemon with
      | Some deadline when Mtime.compare deadline (started.scheduler_monotonic_now ()) > 0
        ->
        Daemon.For_testing.await_schedule_scheduler_idle daemon;
        Some ()
      | Some _ | None -> None)
  in
  (* Join the preceding pass before advancing the paused clock, then wait for
     the released pass to return to a future sleep and finish its workers. *)
  await_idle ();
  started.advance_scheduler seconds;
  await_idle ()
;;

let join_occurrence ~clock ~tick daemon driver (parent : Registry.entry) ~slot =
  let selected, origin =
    await
      ~clock
      ~phase:("admitting captured occurrence " ^ Int64.to_string slot)
      (fun () ->
         match execution (healthy parent) ~slot with
         | Some selected
           when Option.is_some (Exec.Attempt.origin (Exec.current_attempt selected)) ->
           Some
             ( selected
             , Option.value_exn (Exec.Attempt.origin (Exec.current_attempt selected)) )
         | Some _ | None ->
           drive (Driver.process_entry driver parent);
           None)
  in
  [%test_eq: int] 1 (List.length (Exec.attempts selected));
  let association = Option.value_exn (Intent.child (Exec.intent selected)) in
  let reference = A.Recurrence_child_association.reference association in
  assert (
    P.Id.Session.equal
      reference.child_session_id
      (P.Session_ref.session_id (Origin.session origin)));
  let child =
    Registry.find (Daemon.registry daemon) reference.child_session_id |> Option.value_exn
  in
  let remaining_ticks = ref 10 in
  let finished =
    await
      ~clock
      ~phase:("joining captured worker " ^ Int64.to_string slot)
      (fun () ->
         let current = healthy child in
         match (run current (Origin.run_id origin)).lifecycle with
         | Terminal (Completed _) when Option.is_none current.active_operation ->
           Some current
         | Terminal (Completed _) | Admitted | Active | Waiting _ ->
           if !remaining_ticks > 0
           then (
             Int.decr remaining_ticks;
             tick ());
           None
         | Terminal _ ->
           raise_s
             [%sexp
               "captured worker did not complete"
             , (run current (Origin.run_id origin) : P.Run.t)])
  in
  let terminal =
    await
      ~clock
      ~phase:("importing child terminal " ^ Int64.to_string slot)
      (fun () ->
         let selected = execution (healthy parent) ~slot |> Option.value_exn in
         match Exec.Attempt.terminal (Exec.current_attempt selected) with
         | Some terminal -> Some terminal
         | None ->
           drive (Driver.process_entry driver parent);
           None)
  in
  let child_run = run finished (Origin.run_id origin) in
  assert (P.Run.Mode.equal child_run.mode Workflow);
  let retained_origin =
    A.Run_state.recurrence_origin (Option.value_exn finished.run_state) child_run.id
    |> Option.value_exn
  in
  assert (Origin.equal retained_origin origin);
  Origin.validate_run retained_origin ~run:child_run ~admission:(Origin.admission origin)
  |> protocol_ok;
  Terminal.validate terminal ~origin ~run:child_run ~receipt:(Terminal.receipt terminal)
  |> protocol_ok;
  assert (Terminal.Outcome.equal (Terminal.outcome terminal) Completed);
  List.filter finished.moderator_executions ~f:(fun execution ->
    P.Moderator_execution.equal_phase execution.context.phase Session_start)
  |> one "captured startup"
  |> ignore;
  { origin; terminal; reference }, finished
;;

let assert_descendant daemon current ~session ~episode occurrence child =
  let record =
    Delegations.resolve
      (Agent_store.Session_store.delegations (Daemon.store daemon))
      occurrence.reference
    |> Source_preparation_tests.store_ok
  in
  assert (Delegations.equal_stage record.stage Linked);
  let origin = Option.value_exn record.admission.goal_origin in
  let inherited =
    Descendants.origin (Option.value_exn child.A.Session_state.goal_descendants)
  in
  assert (Option.equal Agent_store.Delegation_goal_origin.equal inherited (Some origin));
  let scope =
    Agent_store.Delegation_goal_origin.scopes origin |> one "inherited Goal scope"
  in
  assert (P.Session_ref.equal (Scope.owner scope) session);
  assert (P.Id.Goal.equal (Scope.goal_id scope) goal_id);
  assert (P.Goal_episode.equal (Scope.creator_episode scope) episode);
  let branch =
    Descendants.branches
      (Option.value_exn current.A.Session_state.goal_descendants)
      ~goal_id
    |> List.find_exn ~f:(fun branch ->
      Delegations.Reference.equal
        occurrence.reference
        (Descendants.Branch.reference branch))
  in
  assert (Scope.equal (Descendants.Branch.scope branch) scope);
  assert (Option.is_none (Descendants.Branch.pending branch));
  [%test_eq: int64] 2L (Descendants.Branch.confirmed_prefix branch);
  let row = Ledger.rows child.inference_ledger |> one "captured child provider attempt" in
  let handle = Ledger.Row.handle row in
  assert (P.Id.Session.equal child.identity.session_id (Ledger.Handle.session_id handle));
  let record = Ledger.Row.record row in
  (match Inference.Observation.Attempt_record.state record with
   | Terminal terminal ->
     assert (
       Inference.Event.Terminal.equal_outcome
         Completed
         (Inference.Event.Terminal.outcome terminal))
   | Prepared | Running | Interrupted _ ->
     failwith "captured provider Attempt did not settle");
  let module Observation = Inference.Observation in
  let usage =
    Observation.Attempt_record.observations record
    |> List.filter_map ~f:(fun row ->
      match Observation.payload row with
      | Usage usage -> Some usage
      | Context_estimate _ | Configuration _ | Transport_selection _ | Diagnostic _ ->
        None)
    |> one "actual child usage record"
  in
  (match Observation.Count.view (Observation.Usage.count usage Output) with
   | Unknown Not_reported -> ()
   | Unknown _ | Actual _ | Estimated _ -> failwith "omitted child token count changed");
  handle
;;

let assert_totals current ~session ~children =
  let view =
    A.Session_state.goal_policy_context current ~owner:session
    |> protocol_ok
    |> Totals.capture ~goal_id
    |> protocol_ok
  in
  [%test_eq: int64]
    (Int64.of_int (children + 1))
    (Totals.View.committed view ~dimension:Attempt);
  [%test_eq: int64]
    (Int64.of_int (children + 1))
    (Totals.View.committed view ~dimension:Host_turn);
  [%test_eq: int64] 0L (Totals.View.pending view ~dimension:Attempt);
  [%test_eq: int64] 0L (Totals.View.pending view ~dimension:Host_turn);
  [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.local_usage view));
  [%test_eq: int64]
    (Int64.of_int children)
    (A.Goal_usage.attempts (Totals.View.physical_usage view));
  [%test_eq: int64] 0L (A.Goal_usage.attempts (Totals.View.causal_usage view));
  let unknown_output usage =
    A.Goal_usage.component usage Output
    |> A.Goal_usage.Metric.unknown
    |> fun unknown -> A.Goal_usage.Unknown.count unknown Not_reported
  in
  [%test_eq: int64] 1L (unknown_output (Totals.View.local_usage view));
  let physical = A.Goal_usage.component (Totals.View.physical_usage view) Output in
  [%test_eq: int64] 0L (A.Goal_usage.Metric.actual_attempts physical);
  [%test_eq: int64] 0L (A.Goal_usage.Metric.estimated_attempts physical);
  let unknown = A.Goal_usage.Metric.unknown physical in
  let unavailable =
    List.fold
      [ Inference.Observation.Count.Not_reported
      ; Explicit_null
      ; Interrupted
      ; Not_submitted
      ; Before_tracking
      ]
      ~init:(A.Goal_usage.Metric.untracked_attempts physical)
      ~f:(fun n reason -> Int64.(n + A.Goal_usage.Unknown.count unknown reason))
  in
  [%test_eq: int64] (Int64.of_int children) unavailable
;;
