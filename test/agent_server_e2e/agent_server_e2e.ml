open Core
module Admin_scenario = Scenarios.Admin_scenario
module Conformance_scenario = Scenarios.Conformance_scenario
module Cleanup_scenario = Scenarios.Cleanup_scenario
module Crash_scenario = Scenarios.Crash_scenario
module Harness_scenario = Scenarios.Harness_scenario
module Http_scenario = Scenarios.Http_scenario
module Liveness_scenario = Scenarios.Liveness_scenario
module Matrix_scenario = Scenarios.Matrix_scenario
module Multi_client_scenario = Scenarios.Multi_client_scenario
module Permission_scenario = Scenarios.Permission_scenario
module Process_harness_scenario = Scenarios.Process_harness_scenario
module Prompt_scenario = Scenarios.Prompt_scenario
module Quota_scenario = Scenarios.Quota_scenario
module Replay_scenario = Scenarios.Replay_scenario
module Recovery_scenario = Scenarios.Recovery_scenario
module Shell_security_scenario = Scenarios.Shell_security_scenario
module Smoke_scenario = Scenarios.Smoke_scenario
module Stdio_scenario = Scenarios.Stdio_scenario
module Unix_scenario = Scenarios.Unix_scenario
module Workspace_scenario = Scenarios.Workspace_scenario

let run_scenario env ~case = function
  | "relay-framing-check" -> Scenarios.Relay_framing_scenario.run env
  | "stream-probe-check" ->
    Support.Stream_probe.self_check ();
    Eio.Flow.copy_string "stream probe self-check passed\n" (Eio.Stdenv.stdout env)
  | "live-openai" -> Scenarios.Live_provider_scenario.run env
  | "load" -> Scenarios.Load_scenario.run env ~case
  | "soak" -> Scenarios.Soak_scenario.run env ~case
  | "administration-idempotency" -> Admin_scenario.run env ~case
  | "cross-transport-conformance" -> Conformance_scenario.run env ~case
  | "workspace-cleanup" -> Cleanup_scenario.run env ~case
  | "crash-matrix" -> Crash_scenario.run env ~case
  | "harness-isolation" -> Harness_scenario.run env ~case
  | "http-transport" -> Http_scenario.run env ~case
  | "session-liveness" -> Liveness_scenario.run env ~case
  | "canonical-runtime" -> Matrix_scenario.run_runtime env ~case
  | "runtime-integrity" -> Scenarios.Runtime_integrity_scenario.run env ~case
  | "compaction-integrity" -> Scenarios.Compaction_integrity_scenario.run env ~case
  | "background-orchestration" -> Scenarios.Background_scenario.run env ~case
  | "data-integrity" -> Scenarios.Data_integrity_scenario.run env ~case
  | "auth-security" -> Scenarios.Auth_security_scenario.run env ~case
  | "jobs-orchestration" -> Matrix_scenario.run_background env ~case
  | "data-retention" -> Matrix_scenario.run_data env ~case
  | "graceful-restart" -> Matrix_scenario.run_persistence env ~case
  | "security-matrix" -> Matrix_scenario.run_security env ~case
  | "multi-client" -> Multi_client_scenario.run env ~case
  | "permission-reviewers" -> Permission_scenario.run env ~case
  | "process-harness" -> Process_harness_scenario.run env ~case
  | "prompt-lifecycle" -> Prompt_scenario.run env ~case
  | "quota-queues-leases" -> Quota_scenario.run env ~case
  | "replay-backpressure" -> Replay_scenario.run env ~case
  | "recovery-migration" -> Recovery_scenario.run env ~case
  | "shell-grants-redaction" -> Shell_security_scenario.run env ~case
  | "fork-children" -> Scenarios.Fork_children_scenario.run env ~case
  | "daemon-smoke" -> Smoke_scenario.run env ~case
  | "stdio-modes" -> Stdio_scenario.run env ~case
  | "unix-transport" -> Unix_scenario.run env ~case
  | "workspace-context" -> Workspace_scenario.run env ~case
  | "tui-parity" -> Scenarios.Tui_scenario.run env ~case
  | "tui-typeahead" -> Scenarios.Typeahead_scenario.run env ~case
  | "tui-manual" -> Scenarios.Tui_manual_scenario.run env ~case
  | "tui-manual-link" -> Scenarios.Tui_manual_link_scenario.run env ~case
  | "tui-manual-http-link" -> Scenarios.Tui_manual_http_link_scenario.run env ~case
  | "tui-stream-child" -> Scenarios.Tui_stream_scenario.child env (Option.value_exn case)
  | scenario -> raise_s [%sexp "unknown E2E scenario", (scenario : string)]
;;

let dispatch env ~scenario ~case ~child_behavior =
  match scenario, child_behavior with
  | Some scenario, None -> run_scenario env ~case scenario
  | None, Some behavior -> Process_harness_scenario.run_child env behavior
  | Some _, Some _ -> raise_s [%sexp "scenario and child behavior are mutually exclusive"]
  | None, None -> raise_s [%sexp "either --scenario or --child-behavior is required"]
;;

let command =
  Command.basic
    ~summary:"Run an isolated Ochat agent-server E2E scenario"
    (let%map_open.Command scenario =
       flag "--scenario" (optional string) ~doc:"NAME E2E scenario name"
     and case = flag "--case" (optional string) ~doc:"NAME Optional scenario subcase"
     and child_behavior =
       flag "--child-behavior" (optional string) ~doc:"NAME Internal fixture behavior"
     in
     fun () ->
       Eio_main.run (fun env ->
         Mirage_crypto_rng_unix.use_default ();
         dispatch env ~scenario ~case ~child_behavior))
;;

let () = Command_unix.run command
