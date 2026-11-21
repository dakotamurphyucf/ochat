open! Core
module P = Agent_protocol
module Q = P.Recovery_query
module Scope = P.Recovery_window.Scope
module Checkpoint = Recovery_checkpoint_scenario
module Stock = Recovery_capacity_scenario
module Config = Support.Config_fixture
module Provider = Support.Tui_stream_provider
module Fixture = Crash_recovery_fixture

let successor connection previous ~windows =
  let all = Checkpoint.list_windows connection previous in
  Fixture.require (List.length all = windows) "unexpected number of retained windows";
  Fixture.require
    (List.exists all ~f:(fun row ->
       Scope.equal row.scope previous && Q.State.equal row.state Sealed))
    "provider entered before the predecessor was archived";
  match List.filter all ~f:(fun row -> Q.State.equal row.state Open) with
  | [ row ] -> row.scope
  | _ -> failwith "expected exactly one open successor"
;;

let run env environment =
  let fixture = Fixture.fixture env environment "automatic-capacity" in
  Fixture.write
    env
    (Config.prompt_path fixture)
    {|<config model="gpt-4.1" max_tokens="100"/><developer>Answer the current question.</developer>|};
  let config_path =
    Stock.configuration fixture Stock.policy
    |> Config.write_configuration fixture ~name:"automatic-capacity.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let request = Provider.await_request provider env in
    let archived_answer =
      List.init 1050 ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
      |> String.concat
    in
    let last_window =
      Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
        let session, attachment_id, original =
          Checkpoint.enabled_session client connection ~server_id "automatic-boundaries"
        in
        let send key message =
          Checkpoint.send client session attachment_id ~key message
        in
        send "seed-answer" "Record the diagnostic sample.";
        Provider.reply_summary (request 0) archived_answer;
        Checkpoint.await_idle env client session;
        send "after-sample" "Continue the investigation.";
        let first = request 1 in
        let next = successor connection original ~windows:2 in
        Fixture.require
          (not
             (String.is_substring
                (Jsonaf.to_string (Provider.body first))
                ~substring:archived_answer))
          "the hard-limit predecessor was submitted";
        Provider.reply_summary first "Recovered.";
        Checkpoint.await_idle env client session;
        send "rejected-input" "Check the new measurement.";
        Provider.reject_context (request 2);
        let second = request 3 in
        let last = successor connection next ~windows:3 in
        Provider.reject_context second;
        let terminal =
          Support.Tui_fixture.await env (fun () ->
            let current = (Fixture.get client session.id).session in
            Option.some_if (Option.is_none current.active_operation) current)
        in
        let state =
          Support.Background_fixture.checkpoint
            env
            fixture
            { summary = terminal; attachment_id }
          |> Option.value_exn
        in
        Fixture.require
          (List.length (Agent_session.Inference_ledger.rows state.inference_ledger) = 4)
          "a withheld predecessor or repeated rejection allocated another attempt";
        Fixture.require
          (Scope.equal (successor connection next ~windows:3) last)
          "the rejected first successor started a recursive rollover";
        last)
    in
    Stock.with_stock env fixture ~port ~config_path (fun _ connection _ ->
      let windows = Checkpoint.list_windows connection last_window in
      Fixture.require
        (List.length windows = 3
         && List.exists windows ~f:(fun row ->
           Scope.equal row.scope last_window && Q.State.equal row.state Open)
         && Provider.request_count provider = 4)
        "restart replayed an automatic transition or inferred from a failed successor"))
;;
