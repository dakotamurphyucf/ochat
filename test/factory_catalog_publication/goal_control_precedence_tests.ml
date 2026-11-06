open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Staging = Goal_resume_tests
module Native = Goal_control_completion_tests
module Service = Agent_server.Goal_run_service
module G = P.Goal
module Control = Actor.Goal_callback_control

(* Reuse the actual cold lifecycle fixture with real selected native tools.
   Finish uses the projected Goal revision after Hold and the original actual
   callback context's evidence/action. *)
let prefix =
  {|let* before_control = call("goal_get", get_input) in
      let* held = call("goal_hold", control(before_control, "hold")) in
      |}
;;

let prompt decision =
  let base =
    Resume.prompt Resume.Surface.V1 decision
    |> Native.replace_prompt
         ~pattern:"<script id=\"goal_resume\""
         ~with_:(Native.native_tools ^ "\n<script id=\"goal_resume\"")
    |> Native.replace_prompt
         ~pattern:"let initial_state = 0\n"
         ~with_:("let initial_state = 0\n" ^ Native.native_functions ^ "\n")
  in
  match decision with
  | Resume.Decision.Wait ->
    base
    |> Native.replace_prompt
         ~pattern:"Task.bind(Run.wait(wake),"
         ~with_:(prefix ^ "Task.bind(Run.wait(wake),")
  | Finish ->
    base
    |> Native.replace_prompt
         ~pattern:"Task.bind(Goal.finish("
         ~with_:
           (prefix
            ^ {|let* after_hold = call("goal_get", get_input) in
      let original_intent = field(context, "finish") in
      let intent = `Object([
        {key = "expected_goal_revision"; value = field(row(after_hold), "revision")},
        {key = "evidence"; value = field(original_intent, "evidence")},
        {key = "action"; value = field(original_intent, "action")}
      ]) in
      Task.bind(Goal.finish(|}
           )
    |> Native.replace_prompt ~pattern:"field(context, \"finish\"))," ~with_:"intent),"
  | Continue ->
    base
    |> Native.replace_prompt
         ~pattern:"    let context = Json.parse(lines[2]) in\n    Task.pure(state + 1)"
         ~with_:
           ("    let context = Json.parse(lines[2]) in\n    "
            ^ prefix
            ^ {|let* after_hold = call("goal_get", get_input) in
      let* progressed = call("goal_progress", progress(after_hold)) in
      Task.pure(state + 1)|}
           )
;;

let%expect_test
    "actual resumed control preserves Progress and passive Wait, then yields to terminal \
     settlement"
  =
  List.iter
    Resume.Decision.[ Continue; Wait; Finish ]
    ~f:(fun decision ->
      Resume.with_cold_episode
        ~prompt_text:(prompt decision)
        V1
        decision
        (fun
            env
             daemon
             entry
             _old_runtime
             counters
             _probe
             session
             admission
             _initial
             stopped
             actor
             request
           ->
           let result =
             Service.resume_episode (Resume.public daemon) ~entry ~actor ~session ~request
             |> protocol_ok
           in
           let current = Resume.settled_snapshot entry in
           Resume.assert_episode stopped current admission;
           let callback = Resume.completed_resume current admission in
           Resume.assert_native current admission callback;
           let control_receipt =
             A.Goal_catalog.receipts (Option.value_exn current.goal_catalog)
             |> List.find_exn ~f:(fun receipt ->
               String.equal
                 Native.control_key
                 (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))
           in
           let receipt =
             match decision, result with
             | ( Continue
               , Actor.Goal_resume_result.Resumed
                   { receipt; continuation = Controlled control } ) ->
               Native.assert_control Native.Decision.Hold_progress current control;
               assert (not (Owner.is_loaded entry.runtime));
               assert (
                 A.Session_state.Runtime_initialization.equal
                   Ready
                   current.runtime_initialization);
               (match (Resume.run current admission).lifecycle with
                | Admitted | Active -> ()
                | Waiting _ | Terminal _ -> failwith "control manufactured a Run outcome");
               receipt
             | ( Wait
               , Actor.Goal_resume_result.Resumed
                   { receipt; continuation = Controlled control } ) ->
               assert (
                 A.Goal_command_receipt.equal control_receipt (Control.receipt control));
               assert (
                 Control.Disposition.equal
                   (Held Native.reason)
                   (Control.disposition control));
               let wake = Option.value_exn (Control.waiting control) in
               (match (Resume.run current admission).lifecycle with
                | Waiting actual -> assert (P.Run_wake.equal wake actual)
                | Admitted | Active | Terminal _ ->
                  failwith "control discarded actual passive Wait");
               assert (
                 G.State.equal
                   (Open (Held (Needs_input Native.reason)))
                   (G.state (Goal.goal current)));
               let schedule = List.hd_exn current.schedules in
               [%test_eq: int] 1 (List.length current.schedules);
               [%test_eq: int] 0 schedule.delivery_count;
               (match schedule.status with
                | Scheduled -> ()
                | _ -> failwith "passive producer ran before due clock");
               receipt
             | Finish, Resumed { receipt; continuation = Settled settlement } ->
               let episode = G.episode (Goal.goal stopped) |> Option.value_exn in
               assert (
                 P.Goal_episode.equal
                   episode
                   (P.Goal_episode.Settlement.episode settlement));
               P.Goal_episode.validate_terminal
                 episode
                 ~run:(Resume.run current admission)
                 ~receipt:(P.Goal_episode.Settlement.receipt settlement)
               |> protocol_ok;
               (match
                  (Resume.run current admission).lifecycle, G.state (Goal.goal current)
                with
                | Terminal (Completed _), Closed (Achieved _) -> ()
                | _ ->
                  failwith "settlement precedence had no actual Completed terminal proof");
               receipt
             | (Wait | Finish | Continue), _ ->
               failwith "callback control/settlement precedence changed"
           in
           Staging.assert_receipt request stopped receipt;
           assert (Option.is_none current.active_operation);
           assert (
             List.equal
               P.Run_work.equal
               (Resume.operation_work (Resume.run stopped admission))
               (Resume.operation_work (Resume.run current admission)));
           assert (
             A.Goal_accounting.equal (Goal.accounting stopped) (Goal.accounting current));
           [%test_eq: int] 1 !(counters.providers);
           Resume.repeat daemon entry session actor request receipt counters env));
  print_endline
    "actual installed resume returns exact Hold->Progress or passive Wait control; \
     validated Finish settlement wins; no Administrative turn";
  [%expect
    {| actual installed resume returns exact Hold->Progress or passive Wait control; validated Finish settlement wins; no Administrative turn |}]
;;
