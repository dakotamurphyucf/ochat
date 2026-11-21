open! Core
module A = Agent_session
module P = Agent_protocol
module H = A.Recovery_rollover_hooks
module R = A.Recovery_rollover
module E = Recovery_checkpoint_scenario
module Stock = Recovery_capacity_scenario
module Provider = Support.Tui_stream_provider
module Fixture = Crash_recovery_fixture
module Scope = P.Recovery_window.Scope

type policy =
  | Veto
  | Fail_after

let on_event =
  {|
let initial_state = 0
let on_event : context -> int -> moderator_event -> int task =
  fun ctx state event -> match event with
  | `Session_start -> Task.pure(state)
  | `Session_resume -> Task.pure(state)
  | `Turn_start -> Task.pure(state)
  | `Item_appended(_) -> Task.pure(state)
  | `Pre_tool_call(_) -> Task.pure(state)
  | `Post_tool_response(_) -> Task.pure(state)
  | `Turn_end -> Task.pure(state)
  | `Internal_event(_) -> Task.pure(state)
  | `Tool_invoked(_) -> Task.pure(state)
  | `Tool_observed(_) -> Task.pure(state)
  | `Job_completed(_) -> Task.pure(state)
  | `Subscription_expired(_) -> Task.pure(state)
|}
;;

let before = function
  | Veto -> "Task.pure((|state, `Before(`Veto)|))"
  | Fail_after ->
    {|Task.pure((|state, `Before(`Proceed({hints = `Some([]); continuation = `None}))|))|}
;;

let after = function
  | Veto -> "Task.fail(\"veto must not enter after\")"
  | Fail_after ->
    {|let* ignored = Tool.call("recovery_note_write", `Object([])) in
      Task.fail("post-cut effects are unavailable")|}
;;

let write_prompt env fixture policy =
  Fixture.write
    env
    (Support.Config_fixture.prompt_path fixture)
    (String.concat
       [ {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Use the selected local context tools only.</developer>
<tool name="recovery_note_write"/><tool name="context_checkpoint"/><tool name="context_new_window"/>
<script id="rollover-owner" language="chatml" kind="moderator" api="extensibility-v1">
|}
       ; on_event
       ; {|
let on_context_rollover :
  context -> int -> context_rollover_event -> (int * context_rollover_decision) task =
  fun ctx state event -> match event with
  | `Before_context_rollover(offer) -> |}
       ; before policy
       ; "\n  | `After_context_rollover(committed) -> "
       ; after policy
       ; "\n</script>\n"
       ])
;;

let wait_for env read =
  let clock = Eio.Stdenv.clock env in
  let rec next () =
    match read () with
    | Some value -> value
    | None ->
      Eio.Time.sleep clock 0.01;
      next ()
  in
  Eio.Time.with_timeout_exn clock 15. next
;;

let recorded_state env fixture summary attachment_id =
  Support.Background_fixture.checkpoint env fixture { summary; attachment_id }
;;

let durable_active state =
  Option.map state.A.Session_state.recovery_windows ~f:(fun ledger ->
    A.Recovery_window_ledger.active ledger |> A.Recovery_window_record.scope)
;;

let wait_until_idle env fixture summary attachment =
  wait_for env (fun () ->
    Option.filter (recorded_state env fixture summary attachment) ~f:(fun state ->
      match
        state.runtime_initialization, state.lifecycle.desired, state.lifecycle.observed
      with
      | Ready, Running, Idle -> Option.is_none state.active_operation
      | _ -> false))
;;

let successor env fixture summary attachment previous =
  Option.bind (recorded_state env fixture summary attachment) ~f:durable_active
  |> Option.filter ~f:(fun active -> not (Scope.equal active previous))
;;

let assert_waiting env fixture session attachment scope =
  wait_for env (fun () ->
    let open Option.Let_syntax in
    let%bind ledger = E.retained_rollover env fixture session attachment in
    let%bind schedule = R.foreground ledger ~scope in
    let%bind () =
      match schedule.phase with
      | Waiting -> Some ()
      | _ -> None
    in
    let%bind receipt = R.find ledger ~id:schedule.owner in
    match receipt.status, receipt.hooks with
    | Terminal (Committed { successor; _ }), Some hooks when Scope.equal successor scope
      -> Some (receipt, hooks)
    | _ -> None)
;;

let assert_failed hooks =
  match H.before hooks, H.after hooks with
  | Completed (_, Proceed { hints = Some selection; _ }), Stopped (_, Failed)
    when List.is_empty (A.Recovery_rollover_hint_ref.Selection.references selection) -> ()
  | _ ->
    Fixture.fail
      "post-cut effect did not leave a durable failure and explicit empty selection"
;;

let run env environment =
  let fixture name =
    let fixture = Fixture.fixture env environment ("authored-rollover-hook-" ^ name) in
    let config_path =
      String.substr_replace_first
        Stock.policy
        ~pattern:"(mode automatic)"
        ~with_:"(mode manual)"
      |> Stock.configuration fixture
      |> Support.Config_fixture.write_configuration
           fixture
           ~name:"authored-rollover-hook.sexp"
    in
    fixture, config_path
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let request = Provider.await_request provider env in
    let foreground policy start name check =
      let fixture, config_path = fixture name in
      write_prompt env fixture policy;
      Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
        let session, attachment, previous =
          E.enabled_session client connection ~server_id name
        in
        ignore (wait_until_idle env fixture session attachment : A.Session_state.t);
        let submitted =
          E.submit
            client
            session
            attachment
            ~timing:Safe_boundary
            ~key:(name ^ "-send")
            "select recovery and roll over"
        in
        (match submitted.disposition, submitted.operation_id with
         | Started, Some _ | Deferred, None -> ()
         | _ -> Fixture.fail "inconsistent user message admission");
        let acknowledged =
          E.write_and_register request ~start previous ~id:(name ^ "-checkpoint")
        in
        E.request_transition acknowledged previous ~id:name;
        check fixture config_path session attachment previous)
    in
    foreground Veto 0 "vetoed" (fun fixture _config_path session attachment previous ->
      let next = request 3 in
      let id =
        match E.result next "vetoed" with
        | Complete body ->
          P.Id.Operation.of_json (Stock.member body "id") |> Fixture.protocol_ok
        | _ -> Fixture.fail "veto test did not admit its real foreground rollover"
      in
      Provider.reply_summary next "The old window is still active.";
      let settled = wait_until_idle env fixture session attachment in
      Fixture.require
        (Option.exists (durable_active settled) ~f:(Scope.equal previous))
        "veto changed the active window";
      match Option.bind settled.recovery_rollover ~f:(R.find ~id) with
      | Some { status = Terminal (Superseded Hook_vetoed); hooks = Some hooks; _ } ->
        (match H.before hooks, H.after hooks with
         | Completed (_, Veto), Pending -> ()
         | _ -> Fixture.fail "veto entered an after-hook")
      | _ -> Fixture.fail "veto disposition did not survive the journal");
    let fixture, config_path, session, scope, receipt =
      foreground
        Fail_after
        4
        "failed-after"
        (fun fixture config_path session attachment previous ->
           let scope =
             wait_for env (fun () -> successor env fixture session attachment previous)
           in
           let receipt, hooks = assert_waiting env fixture session attachment scope in
           assert_failed hooks;
           ignore (wait_until_idle env fixture session attachment : A.Session_state.t);
           Fixture.require
             (Provider.request_count provider = 7)
             "failed after-hook dispatched a follow-up";
           fixture, config_path, session, scope, receipt)
    in
    Stock.with_stock env fixture ~port ~config_path (fun client _ _ ->
      let attachment = Stock.reattach client session "reopen-hook-disposition" in
      Stock.start client session attachment "reopen-hook-start";
      let observed, hooks = assert_waiting env fixture session attachment scope in
      assert_failed hooks;
      Fixture.require
        (R.Receipt.equal observed receipt)
        "reopen replaced an authored phase disposition";
      Fixture.require
        (Provider.request_count provider = 7)
        "reopen repeated the authored follow-up"))
;;
