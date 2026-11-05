open! Core
open Agent_server_test_support
open Retry_support
module Configuration = Agent_server.Recurrence_execution_configuration
module Host = Agent_server.Recurrence_execution_host
module Public = Recurrence_service_tests

module Selection = struct
  type t =
    | Self
    | Distinct
  [@@deriving sexp]
end

let denied = function
  | Error (error : P.Error.t) ->
    [%test_eq: P.Error.code] Permission_denied error.code;
    assert (not error.retryable)
  | Ok _ -> failwith "unconfigured or closed execution host issued authority"
;;

let configured controller target ~expires_at =
  let grant =
    Configuration.Controller.create
      ~session_id:(state controller).identity.session_id
      ~existing_targets:[ (state target).identity.session_id ]
      ~captured_children:false
    |> protocol_ok
  in
  Configuration.create
    ~principal_id:(P.Id.Principal.of_string "pri_541_actual_execution" |> protocol_ok)
    ~controllers:[ grant ]
    ~expires_at
  |> protocol_ok
;;

let with_entries selection f =
  with_fixture
    ~max_root_agents:2
    (fun env sw daemon controller controller_runtime _ counters ->
       Source.with_loaded_entry
         daemon
         (fun ~entry:other ~runtime:other_runtime ~revision:_ ->
            let target, target_runtime, ungranted =
              match selection with
              | Selection.Self -> controller, controller_runtime, other
              | Distinct -> other, other_runtime, controller
            in
            with_planner env sw daemon controller (fun () ->
              f env sw daemon controller target target_runtime other ungranted counters)))
;;

let assert_retained controller occurrence admission = function
  | Agent_server.Recurrence_run_service.Outcome.Retained retained ->
    assert (P.Run_receipt.equal admission retained.admission);
    let imported = retained.reconciliation |> protocol_ok in
    let actual = Public.imported_row controller occurrence ~changed:false imported in
    assert (Intent.equal occurrence.intent (Execution.intent actual))
  | Attempted _ -> failwith "host replay attempted another activation"
;;

let exercise selection =
  with_entries
    selection
    (fun env sw daemon controller target target_runtime other ungranted counters ->
       (* Planning has a real separate owner. Neither it nor the stored creator
       makes the omitted daemon execution configuration present. *)
       assert (Option.is_none (Daemon.recurrence_execution_host daemon));
       let now = ref (P.Timestamp.of_string "2050-01-01T00:00:00Z" |> protocol_ok) in
       let expiry = P.Timestamp.add_ms !now 1 |> protocol_ok in
       let make expires_at =
         Host.create
           (configured controller target ~expires_at)
           ~factory:(Daemon.factory daemon)
           ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
           ~sw
           ~now:(fun () -> !now)
         |> protocol_ok
       in
       let host = make None in
       let expiring = make (Some expiry) in
       let sibling = make None in
       protect_cleanup
         ~finally:(fun () -> List.iter [ host; expiring; sibling ] ~f:Host.close)
         (fun () ->
            let controller_before = state controller in
            let other_before = state other in
            let controller_frames = Existing.frames env controller in
            let other_frames = Existing.frames env other in
            let counts = !(counters.inference_ports), !(counters.providers) in
            let unchanged () =
              Existing.assert_same controller_before controller;
              Existing.assert_same other_before other;
              [%test_eq: string list] controller_frames (Existing.frames env controller);
              [%test_eq: string list] other_frames (Existing.frames env other);
              [%test_eq: int * int]
                counts
                (!(counters.inference_ports), !(counters.providers));
              Owner.with_loaded_runtime target.runtime (fun actual ->
                assert (phys_equal target_runtime actual);
                Ok ())
              |> protocol_ok
            in
            let selected_controller = Host.controller host controller |> protocol_ok in
            let actor = Host.Controller.actor selected_controller in
            assert (Operator_authorization.is_current actor);
            let principal = Operator_authorization.principal actor in
            assert (not (P.Id.Principal.equal principal.id Retry_support.principal.id));
            assert (
              P.Scope.Set.equal
                principal.scopes
                (P.Scope.Set.of_list [ Send_messages; View_session_transcript ]));
            Host.Controller.authorize selected_controller (state controller)
            |> protocol_ok;
            (match Host.Controller.authorize selected_controller (state other) with
             | Error error ->
               [%test_eq: P.Error.code] Conflict error.code;
               assert (not error.retryable)
             | Ok () -> failwith "controller policy accepted another actual session");
            Host.controller host other |> denied;
            Host.existing_target selected_controller ~target:ungranted |> denied;
            let selected =
              Host.existing_target selected_controller ~target |> protocol_ok
            in
            Host.Target.authorize selected (state target) |> protocol_ok;
            let target_actor = Host.Target.actor selected in
            assert (Operator_authorization.is_current target_actor);
            assert (
              P.Scope.Set.equal
                (Operator_authorization.principal target_actor).scopes
                (P.Scope.Set.of_list [ Send_messages; View_session_transcript ]));
            unchanged ();
            let occurrence = create_occurrence daemon controller target in
            (* Freeze the first request from the genuine configured original, not
           the fixture operator who authored the administrative recurrence. *)
            let previous =
              Attempt.create
                ~intent:occurrence.intent
                ~number:0
                ~principal_id:(Operator_authorization.principal target_actor).id
                ~expected_revision:(state target).counters.revision
                ~key:(Attempt.key occurrence.previous)
              |> protocol_ok
            in
            let occurrence = { occurrence with previous } in
            let allowed =
              Host.Controller.automatic_policy
                selected_controller
                (state controller)
                (Entry.current (recurrence controller))
              |> protocol_ok
            in
            assert allowed;
            let execution =
              Host.Target.execution selected ~intent:occurrence.intent |> protocol_ok
            in
            Registry.with_loaded_read
              (Daemon.registry daemon)
              (state target).identity.session_id
              (fun actual ->
                 assert (phys_equal actual target);
                 assert (Operator_authorization.is_current actor);
                 assert (Operator_authorization.is_current target_actor);
                 Host.Controller.authorize selected_controller (state controller)
                 |> protocol_ok;
                 Host.Target.authorize selected (state target) |> protocol_ok;
                 (match Host.Target.execution selected ~intent:occurrence.intent with
                  | Error error ->
                    [%test_eq: P.Error.code] Conflict error.code;
                    assert (not error.retryable)
                  | Ok _ -> failwith "new execution ignored its read reservation");
                 Ok ())
            |> protocol_ok;
            let bind host =
              let controller = Host.controller host controller |> protocol_ok in
              let target = Host.existing_target controller ~target |> protocol_ok in
              ( target
              , Host.Target.execution target ~intent:occurrence.intent |> protocol_ok )
            in
            let expires, expired_execution = bind expiring in
            let sibling_target, sibling_execution = bind sibling in
            let admission =
              Host.Execution.activate execution ~attempt:previous
              |> protocol_ok
              |> Public.activated
            in
            let final, index, selected_run = Public.await_completed target admission in
            [%test_eq: int] 1 !(counters.providers);
            assert (Option.is_none final.active_operation);
            assert (
              String.is_substring
                (List.hd_exn !(counters.transcripts))
                ~substring:"539 actual retry input");
            let origin =
              A.Run_state.recurrence_origin index selected_run.id |> Option.value_exn
            in
            A.Recurrence_run_origin.validate
              origin
              ~intent:occurrence.intent
              ~attempt:previous
            |> protocol_ok;
            A.Recurrence_run_origin.validate_run origin ~run:selected_run ~admission
            |> protocol_ok;
            assert (P.Id.Principal.equal selected_run.principal_id principal.id);
            (match
               Host.Execution.reconcile sibling_execution ~attempt:previous |> protocol_ok
             with
             | Retained retained ->
               assert (P.Run_receipt.equal admission retained.admission);
               retained.reconciliation
               |> protocol_ok
               |> Public.imported_row controller occurrence ~changed:true
               |> ignore
             | Absent -> failwith "configured host lost its actual completed Run");
            let before_controller = state controller in
            let before_target = state target in
            let controller_frames = Existing.frames env controller in
            let target_frames = Existing.frames env target in
            let counts = !(counters.inference_ports), !(counters.providers) in
            let no_effects () =
              Existing.assert_same before_controller controller;
              Existing.assert_same before_target target;
              [%test_eq: string list] controller_frames (Existing.frames env controller);
              [%test_eq: string list] target_frames (Existing.frames env target);
              [%test_eq: int * int]
                counts
                (!(counters.inference_ports), !(counters.providers))
            in
            now := expiry;
            assert (not (Operator_authorization.is_current (Host.Target.actor expires)));
            Host.Execution.activate expired_execution ~attempt:previous |> denied;
            Host.controller expiring controller |> denied;
            no_effects ();
            Host.close host;
            assert (not (Operator_authorization.is_current actor));
            assert (not (Operator_authorization.is_current target_actor));
            Host.Execution.activate execution ~attempt:previous |> denied;
            Host.Target.authorize selected (state target) |> denied;
            no_effects ();
            assert (Operator_authorization.is_current (Host.Target.actor sibling_target));
            Host.Execution.activate sibling_execution ~attempt:previous
            |> protocol_ok
            |> assert_retained controller occurrence admission;
            no_effects ();
            print_s [%sexp (selection : Selection.t)]))
;;

let%expect_test
    "configured originals drive real self and distinct activation without cross-host \
     authority"
  =
  List.iter Selection.[ Self; Distinct ] ~f:exercise;
  print_endline
    "exact real entries and configured principals; original expiry and close deny \
     without writes or effects";
  print_endline
    "an unrelated live host retains its own proof and the exact acknowledged admission";
  [%expect
    {|
    Self
    Distinct
    exact real entries and configured principals; original expiry and close deny without writes or effects
    an unrelated live host retains its own proof and the exact acknowledged admission
    |}]
;;
