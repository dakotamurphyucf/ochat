open! Core
module P = Agent_protocol
module Local = Recovery_headless_permission_scenario
module Provider = Support.Tui_stream_provider
module Process = Support.Process_manager
module Ledger = Agent_session.Recovery_window_ledger

let require = Support.Tui_fixture.require

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Record the diagnostic sample and answer each new question.</developer>
<script id="managed" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.map(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> state)
| _ -> Task.pure(state)
</script>|}
;;

let durable_state env fixture session =
  let directory = Crash_recovery_fixture.session_directory fixture session in
  Support.Background_fixture.checkpoint_from_paths
    env
    ~snapshot_directory:(Filename.concat directory "snapshot")
    ~journal_directory:(Filename.concat directory "journal")
  |> Option.value_exn
;;

let reported result ~prefix decode =
  String.split_lines result.Process.stderr.contents
  |> List.find_map_exn ~f:(fun line ->
    Option.bind (String.chop_prefix line ~prefix) ~f:(String.chop_suffix ~suffix:"]"))
  |> decode
  |> Local.ok
;;

let run env temporary =
  Local.with_local_provider
    env
    temporary
    ~name:"cold-managed-rollover"
    ~prompt
    (fun ~sw ~provider ~fixture ~environment ~cwd ~prompt_file ~store ->
       let policy =
         match Sexp.of_string Recovery_capacity_scenario.policy with
         | List [ Atom "recovery_capacity"; settings ] ->
           Sexp.to_string_mach settings
           |> String.substr_replace_first
                ~pattern:"(prompt smoke)"
                ~with_:"(prompt embedded.prompt)"
         | _ -> failwith "unexpected recovery capacity fixture"
       in
       let capacity = Eio.Path.(cwd / "recovery.sexp") in
       Eio.Path.save ~create:(`Or_truncate 0o600) capacity policy;
       let executable =
         Local.executable env ~variable:"OCHAT_E2E_MAIN_EXE" ~name:"main"
       in
       let invoke arguments =
         let command =
           [ executable
           ; "chat-completion"
           ; "-prompt-file"
           ; prompt_file
           ; "-storage-root"
           ; store
           ; "-recovery-capacity-file"
           ; Eio.Path.native_exn capacity
           ; "-output-file"
           ; Eio.Path.native_exn Eio.Path.(cwd / "transcript.chatmd")
           ]
           @ arguments
         in
         Process.spawn ~sw ~env ~cwd ~environment ~max_output_bytes:65536 command
       in
       let finish process =
         let outcome =
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
             Process.await process)
         in
         require (Process.equal_exit outcome.exit (Exited 0)) outcome.stderr.contents;
         outcome
       in
       let initial = finish (invoke [ "-enable-recovery"; "-run-mode"; "workflow" ]) in
       let session =
         reported initial ~prefix:"[managed conversation " P.Id.Session.of_string
       in
       let state () = durable_state env fixture session in
       let windows (state : Agent_session.Session_state.t) =
         Option.value_exn state.recovery_windows
       in
       let original = state () |> windows |> Ledger.active |> Ledger.Window.scope in
       let send text ~reply ~check =
         let index = Provider.request_count provider in
         let process =
           invoke [ "-session-id"; P.Id.Session.to_string session; "-message"; text ]
         in
         let request =
           Support.Background_fixture.await
             env
             ("stock managed input: " ^ text)
             (fun () ->
                Option.iter (Process.poll_result process) ~f:(fun result ->
                  failwith
                    ("managed command exited before inference: " ^ result.stderr.contents));
                Provider.request_at provider index)
         in
         let body = Provider.body request in
         require
           (Recovery_reminder_scenario.has_entry request ~role:"user" ~substring:text)
           "the selected user input did not reach the provider";
         check (Jsonaf.to_string (Jsonaf.member_exn "input" body));
         Provider.reply_summary request reply;
         reported (finish process) ~prefix:"[managed run " P.Id.Run.of_string
       in
       let archived =
         List.init 1050 ~f:(fun i -> Md5.digest_string (Int.to_string i) |> Md5.to_hex)
         |> String.concat
       in
       ignore
         (send "Record the diagnostic sample." ~reply:archived ~check:ignore : P.Id.Run.t);
       let answer = "Recovered on the successor." in
       let excludes_old input =
         require
           (not (String.is_substring input ~substring:archived))
           "the new-window provider submission reused the retired answer"
       in
       let recovered =
         send "Continue the investigation." ~reply:answer ~check:excludes_old
       in
       let terminal state =
         let index = Option.value_exn state.Agent_session.Session_state.run_state in
         let run = Agent_session.Run_state.find index recovered |> Option.value_exn in
         let receipts =
           Agent_session.Run_state.receipts index
           |> List.filter ~f:(fun receipt ->
             P.Id.Run.equal receipt.P.Run_receipt.run_id recovered)
         in
         (match
            run.lifecycle, run.owned_work, run.relinquished_work, run.terminal_work
          with
          | Terminal (Completed _), [ owned ], [ retired ], [ proof ] ->
            require
              ((not (P.Run_work.equal owned retired))
               && P.Run_work.equal owned proof.work
               && P.Run_work.Terminal.equal_outcome proof.outcome Succeeded)
              "the Run did not complete against its newly owned Operation"
          | _ -> failwith "the original managed Run has unexpected terminal work");
         List.iter [ P.Run_receipt.Kind.Admission; Terminal ] ~f:(fun kind ->
           require
             (List.count receipts ~f:(fun receipt ->
                P.Run_receipt.Kind.equal receipt.kind kind)
              = 1)
             "the original Run lost or duplicated an admission or terminal receipt");
         run
       in
       let after_state = state () in
       let completed = terminal after_state in
       let after = windows after_state in
       let successor = Ledger.active after |> Ledger.Window.scope in
       require
         (List.length (Ledger.windows after) = 2
          && not (P.Recovery_window.Scope.equal original successor))
         "the automatic cut did not retain two distinct windows";
       ignore
         (send "Use the retained successor." ~reply:"Confirmed." ~check:(fun input ->
            excludes_old input;
            require
              (String.is_substring input ~substring:answer)
              "cold reopen lost the successor answer")
          : P.Id.Run.t);
       let reopened = state () in
       require
         (P.Run.equal completed (terminal reopened))
         "cold reopen rewrote the original Run";
       let final = windows reopened in
       require
         (List.length (Ledger.windows final) = 2
          && P.Recovery_window.Scope.equal
               (Ledger.active final |> Ledger.Window.scope)
               successor
          && Provider.request_count provider = 3)
         "cold reopen duplicated rollover or model admission")
;;
