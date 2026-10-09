open! Core
module F = Support.Background_fixture
module T = Support.Temporary_environment
module C = Support.Config_fixture
module Report = Support.Load_report
module D = Agent_server.Daemon
module A = Agent_session.Session_actor
module P = Agent_protocol
module L = Support.Load_fixture

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "storage measurement failed", (error : Agent_store.Store_error.t)]
;;

let integer value = `Number (Int.to_string value)
let int64 value = `Number (Int64.to_string value)
let seconds value = Jsonaf.Export.jsonaf_of_float value

let state daemon session =
  Agent_server.Session_registry.find (D.registry daemon) session.F.summary.id
  |> Option.value_exn
  |> fun (entry : Agent_server.Session_registry.entry) ->
  A.state entry.actor |> F.protocol_ok
;;

let duration clock f =
  let started = Eio.Time.Mono.now clock in
  let result = f () in
  result, Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now clock)) /. 1e9
;;

let snapshot_directory fixture session =
  Filename.concat
    (C.data_dir fixture)
    ("sessions/" ^ P.Id.Session.to_string session.F.summary.id ^ "/snapshot")
;;

let current_marker env directory =
  let path = Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT") in
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> None
  | `Regular_file -> Some (Eio.Path.load path)
  | _ -> failwith "unexpected fixture CURRENT kind"
;;

let function_call serial arguments =
  let open Openai.Responses.Response_stream in
  let call_id = sprintf "storage-call-%d" serial in
  let item_id = sprintf "storage-item-%d" serial in
  let item ~arguments ~status : Item.t =
    Function_call
      { name = "run_chatml"
      ; arguments
      ; call_id
      ; _type = "function_call"
      ; id = Some item_id
      ; status
      }
  in
  [ Output_item_added
      { item = item ~arguments:"" ~status:None
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments = Jsonaf.to_string arguments
      ; item_id
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = item ~arguments:(Jsonaf.to_string arguments) ~status:(Some "completed")
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
  |> Stdlib.List.to_seq
;;

let schedule client session name =
  let module V = Chatml.Chatml_value_codec in
  let payload =
    Chatml.Chatml_lang.VVariant ("Internal_event", [ V.jsonaf_to_value (`String "Wake") ])
    |> V.Snapshot.of_value
    |> Result.ok_or_failwith
    |> V.Snapshot.to_jsonaf
  in
  match
    F.request
      client
      (Schedule_create
         { session_id = session.F.summary.id
         ; attachment_id = session.attachment_id
         ; payload
         ; due = After_ms 0
         ; misfire = Deliver_once_immediately
         ; idempotency_key = F.key name
         })
  with
  | Schedule_create result -> result.schedule
  | _ -> failwith "unexpected storage schedule result"
;;

let await_delivery env daemon session count =
  L.wait env "storage notification delivery" (fun () ->
    let current = state daemon session in
    F.require (Option.is_none current.failure) "storage moderator failed";
    let delivered =
      List.fold current.schedules ~init:0 ~f:(fun count schedule ->
        match schedule.P.Schedule.status with
        | Delivered ->
          F.require (schedule.delivery_count = 1) "storage notification repeated";
          count + 1
        | Failed error -> raise_s [%sexp "storage schedule failed", (error : P.Error.t)]
        | Cancelled -> failwith "storage schedule unexpectedly cancelled"
        | Scheduled | Delivering -> count)
    in
    delivered = count && Int.of_string (F.moderator_state current) = count)
;;

let run env report =
  T.with_ ~scenario:"storage-defaults" ~env (fun temporary ->
    let fixture =
      C.create temporary ~name:"storage-defaults" ~http_port:(F.reserve_port env)
    in
    (* Other E2E cases deliberately checkpoint more often. This workload removes
       those fixture overrides so validation supplies the production defaults. *)
    let configuration =
      C.configuration fixture ()
      |> String.substr_replace_all ~pattern:"    (snapshot_every_events 10)\n" ~with_:""
      |> String.substr_replace_all ~pattern:"    (snapshot_every_ms 1000)" ~with_:""
      (* This isolated workload executes only the prompt's synthetic read_file
         and run_chatml tools; the shared E2E fixture otherwise denies all tools. *)
      |> String.substr_replace_all
           ~pattern:"(tool_default deny)"
           ~with_:"(tool_default allow)"
    in
    F.save fixture (C.config_path fixture) configuration;
    let loaded =
      match Agent_server.Config_parser.load ~env ~path:(C.config_path fixture) with
      | Error diagnostics ->
        raise_s [%sexp "storage config parse failed", (diagnostics : _ list)]
      | Ok raw ->
        (match Agent_server.Config_validator.validate ~env raw with
         | Error diagnostics ->
           raise_s [%sexp "storage config validation failed", (diagnostics : _ list)]
         | Ok config -> config)
    in
    let durability = loaded.server.durability in
    F.require
      (durability.snapshot_every_events = 100 && durability.snapshot_every_ms = 5000)
      "storage workload must use validated production checkpoint defaults";
    let commits = L.integer "OCHAT_E2E_STORAGE_COMMITS" 120 in
    let seed_messages = L.integer "OCHAT_E2E_STORAGE_SEED_MESSAGES" 8 in
    let repeats = L.integer "OCHAT_E2E_STORAGE_FRAGMENT_REPEATS" 2500 in
    F.require
      (commits <= 1000 && seed_messages <= 128 && repeats <= 2500)
      "storage workload exceeds its explicit harness bounds";
    let large = String.concat (List.init repeats ~f:(fun _ -> "📚\"\\\n")) in
    let large_path = Filename.concat (C.physical_workspace fixture) "large-result.txt" in
    F.save fixture large_path large;
    F.save
      fixture
      (C.prompt_path fixture)
      {|<developer>Fixed isolated storage workload.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool><tool name="run_chatml"/>
<script id="storage-counter" language="chatml" kind="moderator" api="extensibility-v1">
type state = int
let initial_state = 0
let on_event ctx state event =
  match event with | `Internal_event(_) -> Task.pure(state + 1) | _ -> Task.pure(state)
</script>|};
    let queued = ref None in
    let provider_calls = ref 0 in
    let serial = ref 0 in
    let provider ~sw:_ ~inputs:_ =
      Int.incr provider_calls;
      match !queued with
      | None -> Stdlib.Seq.empty
      | Some arguments ->
        queued := None;
        Int.incr serial;
        function_call !serial arguments
    in
    let options =
      { D.default_options with
        qualify_chatml_extensions = true
      ; inference_policy =
          Agent_server_test_support.inference_policy
            ~default_model:"storage-fixture"
            ~post_stream:provider
      }
    in
    let clock = Eio.Stdenv.mono_clock env in
    Report.record
      report
      env
      "workload"
      [ "commits", integer commits
      ; "seed_messages", integer seed_messages
      ; "seed_message_bytes", integer 1024
      ; "large_result_bytes", integer (String.length large)
      ; "fragment_repeats", integer repeats
      ; "checkpoint_events", integer durability.snapshot_every_events
      ; "checkpoint_ms", integer durability.snapshot_every_ms
      ; "retained_snapshots", integer 2
      ; "script_wall_seconds", seconds Chatml_execution.default_limits.wall_seconds
      ; "script_compile_seconds", seconds Chatml_compilation.default_limits.wall_seconds
      ; "native_watch_qualification", `String "separate helper qualification required"
      ; ( "checkpoint_prune_split"
        , `String "combined synchronous hook; no production tracing" )
      ];
    let retained = ref None in
    let canonical = ref None in
    let expected_invocations = ref [] in
    let saw_checkpoints = ref 0 in
    let measure daemon session label f =
      let directory = snapshot_directory fixture session in
      let marker = current_marker env directory in
      let before = state daemon session in
      let result, elapsed = duration clock f in
      let after = state daemon session in
      let installed =
        not (Option.equal String.equal marker (current_marker env directory))
      in
      if installed then Int.incr saw_checkpoints;
      Report.record
        report
        env
        label
        [ "duration_seconds", seconds elapsed
        ; ("checkpoint_installed", if installed then `True else `False)
        ; "event_before", int64 before.counters.event_sequence
        ; "event_after", int64 after.counters.event_sequence
        ; "transaction_before", int64 before.counters.transaction_sequence
        ; "transaction_after", int64 after.counters.transaction_sequence
        ; "provider_calls", integer !provider_calls
        ];
      result
    in
    let await_turn daemon session previous previous_attempts =
      L.wait env "storage turn completion" (fun () ->
        let current = state daemon session in
        let completed, pending =
          Agent_session.Inference_ledger.rows current.inference_ledger
          |> List.fold ~init:(false, false) ~f:(fun (completed, pending) row ->
            let ordinal =
              Agent_session.Inference_ledger.Row.handle row
              |> Agent_session.Inference_ledger.Handle.ordinal
            in
            if Set.mem previous_attempts ordinal
            then completed, pending
            else (
              match
                Agent_session.Inference_ledger.Row.record row
                |> Inference.Observation.Attempt_record.state
              with
              | Terminal terminal ->
                (match Inference.Event.Terminal.outcome terminal with
                 | Completed -> true, pending
                 | Refused | Incomplete _ | Failed _ ->
                   raise_s
                     [%sexp
                       "storage inference failed", (terminal : Inference.Event.Terminal.t)])
              | Interrupted _ -> failwith "storage inference interrupted"
              | Prepared | Running -> completed, true))
        in
        completed
        && (not pending)
        && Option.is_none current.active_operation
        && List.is_empty current.conversation.deferred_user_entries
        && List.length current.conversation.canonical_history > previous);
      let current = state daemon session in
      F.require
        (Option.is_none current.failure && not current.halted)
        "storage turn failed"
    in
    let send daemon client session index =
      let initial = state daemon session in
      let before = List.length initial.conversation.canonical_history in
      let previous_attempts =
        Agent_session.Inference_ledger.rows initial.inference_ledger
        |> List.map ~f:(fun row ->
          Agent_session.Inference_ledger.Row.handle row
          |> Agent_session.Inference_ledger.Handle.ordinal)
        |> Int64.Set.of_list
      in
      (match
         F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content =
                  { kind = Plain_text
                  ; text = sprintf "%d:%s" index (String.make 1024 'x')
                  ; attachments = []
                  }
              ; idempotency_key = F.key (sprintf "storage-send-%d" index)
              ; timing = Agent_protocol.Pending_input.Timing.Safe_boundary
              })
       with
       | Session_send_message _ -> ()
       | _ -> failwith "unexpected storage send result");
      await_turn daemon session before previous_attempts
    in
    Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
      F.with_client ~sw env fixture (fun client ->
        let session = F.create client "storage-session" in
        List.iter (List.range 0 seed_messages) ~f:(fun index ->
          ignore
            (measure daemon session "seed-turn" (fun () ->
               send daemon client session index)
             : unit));
        let arguments =
          `Object
            [ ( "source"
              , `String
                  {|let main input =
  let* result = Tool.call("read_file", input) in
  match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code)|}
              )
            ; ( "input"
              , `Object [ "file", `String "large-result.txt"; "root", `String "data" ] )
            ; "tools", `Array [ `String "read_file" ]
            ; "timeout_ms", integer 30000
            ]
        in
        let invocation_before = (state daemon session).invocations in
        queued := Some arguments;
        ignore
          (measure daemon session "nested-large-tool-turn" (fun () ->
             send daemon client session seed_messages)
           : unit);
        L.wait env "nested tool publication" (fun () ->
          List.exists (state daemon session).invocations ~f:(fun invocation ->
            String.equal invocation.context.tool_name "run_chatml"
            &&
            match invocation.status with
            | Published _ -> true
            | _ -> false));
        let fresh =
          List.filter (state daemon session).invocations ~f:(fun invocation ->
            not
              (List.exists invocation_before ~f:(fun previous ->
                 P.Id.Invocation.equal previous.context.id invocation.context.id)))
        in
        List.iter fresh ~f:(fun invocation ->
          match invocation.status with
          | Published (Complete _) | Resolved (Complete _) -> ()
          | _ ->
            raise_s
              [%sexp
                "nested storage tool failed"
              , (invocation.context.tool_name : string)
              , (invocation.status : P.Invocation.status)]);
        F.require (List.length fresh >= 2) "nested tool invocation missing";
        let read_output =
          List.find_exn fresh ~f:(fun invocation ->
            String.equal invocation.context.tool_name "read_file")
          |> fun (invocation : P.Invocation.t) ->
          match invocation.status with
          | Published (Complete value) | Resolved (Complete value) ->
            (match Openai.Responses.Tool_output.Output.t_of_jsonaf value with
             | Text text -> text
             | Content _ -> failwith "large read output was not text")
          | _ -> failwith "large read output missing"
        in
        let expected_output =
          sprintf
            "large-result.txt:1-%d:\n[total_lines=%d]\n%s"
            repeats
            repeats
            (String.chop_suffix_exn large ~suffix:"\n")
        in
        F.require
          (String.equal read_output expected_output)
          "large invocation changed UTF8, quotes, backslashes, newlines or read-file \
           metadata";
        Report.record
          report
          env
          "large-invocation-proof"
          [ "decoded_output_bytes", integer (String.length read_output)
          ; "retained_utf8_markers", integer repeats
          ];
        expected_invocations
        := List.map fresh ~f:(fun invocation -> invocation.context.id);
        List.iter (List.range 0 commits) ~f:(fun index ->
          let _, elapsed =
            duration clock (fun () ->
              ignore
                (measure daemon session "notification-commit" (fun () ->
                   schedule client session (sprintf "storage-wake-%d" index))
                 : P.Schedule.t);
              await_delivery env daemon session (index + 1))
          in
          Report.record
            report
            env
            "notification-completion"
            [ "duration_seconds", seconds elapsed ]);
        L.await_schedules env client session commits;
        let _, elapsed =
          duration clock (fun () ->
            L.wait env "notification delivery" (fun () ->
              F.moderator_state (state daemon session)
              |> Int.of_string
              |> fun count -> count >= commits))
        in
        Report.record
          report
          env
          "notification-drain"
          [ "duration_seconds", seconds elapsed ];
        Eio.Time.sleep (Eio.Stdenv.clock env) 5.01;
        ignore
          (measure daemon session "time-cadence-next-commit" (fun () ->
             schedule client session "storage-time-wake")
           : P.Schedule.t);
        await_delivery env daemon session (commits + 1);
        L.stop client session "storage-stop";
        let stopped = state daemon session in
        canonical
        := Some
             (List.map stopped.conversation.canonical_history ~f:P.History.entry_to_json);
        let _, elapsed =
          duration clock (fun () -> F.checkpoint env fixture session |> Option.value_exn)
        in
        Report.record
          report
          env
          "snapshot-and-journal-replay"
          [ "duration_seconds", seconds elapsed ];
        retained := Some session.summary));
    (* No live daemon remains: this is the sole snapshot/journal mutation owner.
       Measure validated no-delete pruning separately using the API present in
       both reference and candidate heads. *)
    let summary = Option.value_exn !retained in
    let directory =
      Filename.concat
        (C.data_dir fixture)
        ("sessions/" ^ P.Id.Session.to_string summary.id ^ "/snapshot")
    in
    let snapshots =
      Agent_store.Snapshot.retained_stored
        ~env
        ~directory
        ~max_payload_length:(64 * 1024 * 1024)
        ~allow_incomplete:false
      |> store_ok
    in
    F.require (!saw_checkpoints >= 3) "default checkpoints were not exercised";
    F.require (List.length snapshots = 2) "default retention pruning was not verified";
    Report.record
      report
      env
      "validated-retention"
      [ "observed_checkpoints", integer !saw_checkpoints
      ; "retained_snapshots", integer (List.length snapshots)
      ];
    let removed, prune_elapsed =
      duration clock (fun () ->
        Agent_store.Snapshot.prune_older
          ~env
          ~directory
          ~keep:2
          ~max_payload_length:(64 * 1024 * 1024)
        |> store_ok)
    in
    F.require (Int.equal removed 0) "default daemon did not already prune retention";
    Report.record
      report
      env
      "offline-validated-no-delete-prune"
      [ "duration_seconds", seconds prune_elapsed; "removed", integer removed ];
    let restart_started = Eio.Time.Mono.now clock in
    let _, restart_elapsed =
      duration clock (fun () ->
        Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
          Report.record
            report
            env
            "restart-startup-ready"
            [ ( "duration_seconds"
              , seconds
                  (Mtime.Span.to_float_ns
                     (Mtime.span restart_started (Eio.Time.Mono.now clock))
                   /. 1e9) )
            ];
          F.with_client ~sw env fixture (fun client ->
            let summary = Option.value_exn !retained in
            let session, _ = F.attach client summary "storage-restart" in
            let restored = state daemon session in
            let history =
              List.map restored.conversation.canonical_history ~f:P.History.entry_to_json
            in
            F.require
              (Option.value_exn !canonical |> List.equal Jsonaf.exactly_equal history)
              "restart changed canonical history";
            List.iter !expected_invocations ~f:(fun id ->
              F.require
                (List.exists restored.invocations ~f:(fun invocation ->
                   P.Id.Invocation.equal id invocation.context.id))
                "restart lost nested invocation");
            let before = !provider_calls in
            ignore
              (F.checkpoint env fixture session |> Option.value_exn
               : Agent_session.Session_state.t);
            F.require (Int.equal before !provider_calls) "replay resubmitted inference";
            Report.record
              report
              env
              "restart-loaded"
              [ "history_entries", integer (List.length history)
              ; "provider_replays", integer (!provider_calls - before)
              ])))
    in
    Report.record
      report
      env
      "restart-lifecycle"
      [ "duration_seconds", seconds restart_elapsed ])
;;
