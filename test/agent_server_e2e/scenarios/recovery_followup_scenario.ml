open! Core
module P = Agent_protocol
module F = P.Recovery_followup
module Fixture = Crash_recovery_fixture
module Stock = Recovery_capacity_scenario
module Operator = Recovery_operator_rollover_scenario
module Client = Agent_client.Recovery_windows
module Config = Support.Config_fixture
module Provider = Support.Tui_stream_provider

let ok = Fixture.protocol_ok
let require = Fixture.require

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Record the sample, then wait for an operator at rollover.</developer>
<script id="followup" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
let on_context_rollover ctx state event = match event with
| `Before_context_rollover(_) -> Task.pure((|state, `Before(`Proceed({hints = `None; continuation = `None}))|))
| `After_context_rollover(_) -> Task.pure((|state, `After(`Wait)|))
</script>|}
;;

let exercise env temporary ~cancel =
  let fixture =
    Fixture.fixture
      env
      temporary
      (if cancel then "managed-cancel" else "managed-continue")
  in
  Fixture.write env (Config.prompt_path fixture) prompt;
  let config_path =
    Stock.configuration fixture Stock.policy
    |> Config.write_configuration fixture ~name:"recovery.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let connect ?(drop = false) f =
      Operator.with_client
        ~sw
        env
        fixture
        ~token:(Config.admin_token fixture)
        ~extra_features:[ F.feature ]
        ~lose_followup_reply:drop
        f
    in
    Stock.with_stock env fixture ~port ~config_path (fun _ _ _ ->
      let session, run_id, waiting =
        connect ~drop:cancel (fun http connection initialized ->
          let session, writer =
            Support.Data_integrity_http.create ~mode:Owner_read_write http "waiting"
          in
          let reference =
            P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
          in
          let follow selection =
            Client.followup connection { attachment_id = writer.id; selection }
          in
          let initial =
            Stock.enable connection ~server_id:initialized.server_id session writer.id
          in
          Stock.start http session writer.id "start";
          Recovery_checkpoint_scenario.await_idle env http session;
          let before = follow (Inspect reference) |> ok in
          require
            (F.Reply.equal_phase before.phase Idle
             && P.Recovery_window.Scope.equal before.basis.scope initial)
            "new session already had a follow-up";
          Recovery_checkpoint_scenario.send
            http
            session
            writer.id
            ~key:"seed"
            "Record the sample.";
          let archive =
            List.init 1050 ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
            |> String.concat
          in
          Provider.reply_summary (Provider.await_request provider env 0) archive;
          Recovery_checkpoint_scenario.await_idle env http session;
          let snapshot = Fixture.get http session.id in
          let request =
            P.Run_start.create
              ~session_id:session.id
              ~attachment_id:writer.id
              ~generation:session.generation
              ~expected_revision:snapshot.revision
              ~mode:Single_turn
              ~input:
                (User_submission
                   { kind = Plain_text; text = "Continue the sample."; attachments = [] })
              ~key:(Fixture.key "managed-cut")
            |> ok
          in
          let admitted =
            match Fixture.request http (Session_run_start request) with
            | Session_run_start receipt -> receipt
            | _ -> failwith "managed submission was not admitted"
          in
          let waiting =
            Support.Background_fixture.await
              env
              "authored Wait after physical cut"
              (fun () ->
                 match follow (Inspect reference) with
                 | Error { code = Conflict; _ } -> None
                 | result ->
                   let status = ok result in
                   Option.some_if (F.Reply.equal_phase status.phase Waiting) status)
          in
          require
            (not (P.Recovery_window.Scope.equal initial waiting.basis.scope))
            "Wait did not belong to the successor";
          connect (fun _ foreign _ ->
            Operator.expect_error
              (Client.followup
                 foreign
                 { attachment_id = writer.id; selection = Inspect reference })
              Permission_denied);
          Operator.expect_error (follow (Apply (before.basis, Continue))) Conflict;
          if cancel
          then Operator.expect_error (follow (Apply (waiting.basis, Cancel))) Interrupted
          else (
            (match follow (Apply (waiting.basis, Continue)) with
             | Ok _ | Error { code = Interrupted; _ } -> ()
             | Error failure ->
               raise_s [%sexp "follow-up was rejected", (failure : P.Error.t)]);
            let request = Provider.await_request provider env 1 in
            require
              (Recovery_reminder_scenario.has_entry
                 request
                 ~role:"user"
                 ~substring:"Continue the sample.")
              "continued Run lost its original input";
            Provider.reply_summary request "Continued on operator approval.";
            Recovery_checkpoint_scenario.await_idle env http session);
          session, admitted.run_id, waiting)
      in
      connect (fun http connection initialized ->
        let writer = Stock.reattach http session "reconnected" in
        let reference =
          P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
        in
        let follow selection =
          Client.followup connection { attachment_id = writer; selection }
        in
        let current = follow (Inspect reference) |> ok in
        let expected_phase = if cancel then F.Reply.Cancelled else Admitted in
        require
          (F.Reply.equal_phase current.phase expected_phase
           && P.Recovery_window.Scope.equal waiting.basis.scope current.basis.scope)
          "the selected follow-up outcome did not survive reconnect";
        Operator.expect_error (follow (Apply (waiting.basis, Continue))) Conflict;
        let state =
          Recovery_managed_rollover_scenario.durable_state env fixture session.id
        in
        let index = Option.value_exn state.run_state in
        let run = Agent_session.Run_state.find index run_id |> Option.value_exn in
        require
          (match run.lifecycle with
           | Terminal Cancelled -> cancel
           | Terminal (Completed _) -> not cancel
           | _ -> false)
          "original managed Run did not reach the selected outcome";
        require
          (Int.equal (Provider.request_count provider) (if cancel then 1 else 2))
          "reconnecting repeated model admission")))
;;

let run env temporary =
  exercise env temporary ~cancel:true;
  exercise env temporary ~cancel:false
;;
