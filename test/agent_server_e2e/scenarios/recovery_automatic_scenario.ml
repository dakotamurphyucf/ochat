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

let run_soft env environment =
  let module Soft = Recovery_soft_checkpoint_scenario in
  let module Notes = Recovery_note_provider_scenario in
  let module Catalog = Recovery_notes.Catalog in
  let module R = Agent_session.Recovery_rollover in
  let fixture = Fixture.fixture env environment "automatic-soft-checkpoint" in
  Fixture.write env (Config.prompt_path fixture) Soft.prompt;
  let policy =
    Stock.policy
    |> String.substr_replace_first
         ~pattern:"(checkpoint_reserve 512)"
         ~with_:"(checkpoint_reserve 5800)"
    |> String.substr_replace_first
         ~pattern:"(warning_lead 256)"
         ~with_:"(warning_lead 32)"
  in
  let config_path =
    Stock.configuration fixture policy
    |> Config.write_configuration fixture ~name:"automatic-soft-checkpoint.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
      let session, attachment_id, original =
        Checkpoint.enabled_session
          client
          connection
          ~server_id
          "automatic-soft-checkpoint"
      in
      Checkpoint.send client session attachment_id ~key:"soft-checkpoint" Soft.required;
      let selected = Provider.await_request provider env 0 in
      Fixture.require
        (Set.equal
           (Checkpoint.tools selected)
           (String.Set.singleton "recovery_note_write"))
        "first automatic provider submission was not the bounded checkpoint recipe";
      let expected_path =
        P.Id.Context_window.to_string (Scope.window original)
        |> Chatml_source.Digest.of_contents ~checkpoint:ignore
        |> Chatml_source.Digest.to_hex
        |> fun digest -> "context-checkpoints/" ^ String.prefix digest 48
      in
      Fixture.require
        (Recovery_reminder_scenario.has_entry
           selected
           ~role:"user"
           ~substring:expected_path)
        "selected provider request did not describe its exact Create path";
      Notes.function_call
        selected
        ~id:"stock-soft-note"
        ~name:"recovery_note_write"
        (`Object
            [ "kind", `String "create"
            ; "path", `String expected_path
            ; "expected_catalog_revision", `String "0"
            ; "expected", `Null
            ; "content", `String Soft.private_body
            ]);
      let fresh = Provider.await_request provider env 1 in
      let next = successor connection original ~windows:2 in
      Fixture.require
        (Set.mem (Checkpoint.tools fresh) "context_remaining"
         && not
              (Recovery_reminder_scenario.has_entry
                 fresh
                 ~role:"user"
                 ~substring:Soft.guidance))
        "fresh successor retained checkpoint-only capability or guidance";
      let summary = (Fixture.get client session.id).session in
      let state =
        Support.Background_fixture.checkpoint env fixture { summary; attachment_id }
        |> Option.value_exn
      in
      Fixture.require
        (Scope.equal (Soft.active_scope state) next)
        "strict readback lost the successor presented by its actual write";
      (match Soft.committed_checkpoint state ~predecessor:original with
       | Checkpointed witness ->
         (match R.Checkpoint.recipe witness, R.Checkpoint.notes witness with
          | Authored { soft_note = Some note; _ }, [ selected ] ->
            Fixture.require
              (Catalog.equal_selection selected note.created)
              "stock soft cut did not use the exact immutable note creation"
          | _ -> Fixture.fail "stock rollover lost the authored checkpoint proof")
       | Archive_only _ -> Fixture.fail "stock soft gate did not commit a checkpoint");
      Provider.reply_summary fresh "Done.";
      Checkpoint.await_idle env client session;
      Fixture.require
        (Int.equal (Provider.request_count provider) 2)
        "stock soft checkpoint or successor was submitted twice"))
;;
