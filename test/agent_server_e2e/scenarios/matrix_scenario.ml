open Core

type case =
  { name : string
  ; run : Eio_unix.Stdenv.base -> unit
  }

let invoke runner selected env = runner env ~case:(Some selected)

let runtime_cases =
  [ { name = "history-identities-adjacency-fifo"
    ; run =
        (fun env ->
          invoke Runtime_integrity_scenario.run "history.tool-pair-deferred-restart" env;
          invoke Multi_client_scenario.run "observer.same-event-order" env)
    }
  ; { name = "generated-model-settings"
    ; run =
        (fun env ->
          invoke Runtime_integrity_scenario.run "generated.provider-settings-restart" env)
    }
  ; { name = "safe-points-moderator-wakeup"
    ; run =
        (fun env ->
          invoke Runtime_integrity_scenario.run "moderator.boundaries-overlays-halt" env;
          invoke Runtime_integrity_scenario.run "moderator.wakeup-during-tool" env;
          invoke Runtime_integrity_scenario.run "moderator.self-trigger-budget" env)
    }
  ; { name = "cancellation-compaction"
    ; run =
        (fun env ->
          Compaction_integrity_scenario.run env ~case:None;
          invoke Permission_scenario.run "reviewer.cancelled" env)
    }
  ; { name = "replacement-reconnect-stability"
    ; run =
        (fun env ->
          invoke Replay_scenario.run "snapshot.replacement" env;
          invoke Replay_scenario.run "snapshot.live-boundary-race" env;
          invoke Replay_scenario.run "reconnect.repeated" env)
    }
  ]
;;

let background_cases =
  [ { name = "job-schedule-protocol"
    ; run =
        (fun env ->
          invoke Background_scenario.run "call.success" env;
          invoke Background_scenario.run "call.failure" env;
          invoke Background_scenario.run "spawn.failure" env;
          invoke Background_scenario.run "spawn.running-cancel" env;
          invoke Background_scenario.run "spawn.cancel-releases-blocked-capacity" env;
          invoke Background_scenario.run "call.cancel-blocked-provider" env;
          invoke Background_scenario.run "stop.cancel-blocked-jobs" env;
          invoke Background_scenario.run "retry.backoff-reopen-late-completion" env)
    }
  ; { name = "zero-client-orchestration"
    ; run = (fun env -> invoke Background_scenario.run "spawn.zero-client-success" env)
    }
  ; { name = "capacity-restart"
    ; run =
        (fun env ->
          invoke Background_scenario.run "spawn.capacity-queued-cancel" env;
          invoke Background_scenario.run "restart.running-interrupted-queued-resumed" env)
    }
  ; { name = "delivery-order-dedup"
    ; run =
        (fun env ->
          invoke Background_scenario.run "restart.completed-jobs-scheduled-wake" env;
          invoke Background_scenario.run "restart.schedule-overdue-policies" env;
          invoke Background_scenario.run "moderator.end-session-suppresses-timer" env)
    }
  ]
;;

let data_cases =
  [ { name = "blob-transfer-ownership"
    ; run =
        (fun env ->
          invoke
            Data_integrity_scenario.run
            "http.blob-bounds-digest-foreign-ownership"
            env;
          invoke Conformance_scenario.run "conformance.blob-read" env)
    }
  ; { name = "export-atomicity"
    ; run =
        (fun env ->
          invoke Data_integrity_scenario.run "http.export-atomic-success" env;
          invoke Data_integrity_scenario.run "http-peer.export-atomic-failures" env;
          invoke Data_integrity_scenario.run "http-peer.export-atomic-cancellation" env)
    }
  ; { name = "audit-attribution-visibility"
    ; run =
        (fun env ->
          invoke
            Data_integrity_scenario.run
            "http.audit-attribution-pagination-tamper-restart"
            env;
          invoke Conformance_scenario.run "conformance.visibility" env)
    }
  ; { name = "retention-and-path-safety"
    ; run =
        (fun env ->
          invoke Data_integrity_scenario.run "maintenance-fixture.retention" env;
          invoke
            Data_integrity_scenario.run
            "daemon-timer.retention-active-protection"
            env;
          invoke Replay_scenario.run "replay.expired-cursor" env;
          invoke Cleanup_scenario.run "cleanup.symlink-refusal" env;
          invoke Cleanup_scenario.run "cleanup.recoverable-reference-protection" env)
    }
  ]
;;

let persistence_cases =
  [ { name = "clean-sigterm-and-owner-grace"
    ; run =
        (fun env ->
          invoke Smoke_scenario.run "daemon.graceful-sigterm" env;
          invoke Liveness_scenario.run "owner.restart-during-grace" env)
    }
  ; { name = "queues-jobs-schedules"
    ; run =
        (fun env ->
          invoke Quota_scenario.run "lease.restart-recovery" env;
          invoke Background_scenario.run "restart.completed-jobs-scheduled-wake" env)
    }
  ; { name = "pending-permission"
    ; run = (fun env -> invoke Permission_scenario.run "reviewer.restart-claimed-job" env)
    }
  ; { name = "replay-export-audit"
    ; run =
        (fun env ->
          invoke Replay_scenario.run "reconnect.repeated" env;
          invoke Compaction_integrity_scenario.run "http.atomic-replacement-restart" env;
          invoke
            Data_integrity_scenario.run
            "http.audit-attribution-pagination-tamper-restart"
            env)
    }
  ; { name = "reload-transactionality"
    ; run =
        (fun env ->
          invoke Prompt_scenario.run "prompt.reload-revision-b" env;
          invoke Prompt_scenario.run "prompt.invalid-reload-rollback" env)
    }
  ]
;;

let security_cases =
  [ { name = "oauth-proxy-static-auth"
    ; run = (fun env -> Auth_security_scenario.run env ~case:None)
    }
  ; { name = "static-auth-http-parser"
    ; run =
        (fun env ->
          invoke Http_scenario.run "http.static-auth-matrix" env;
          invoke Http_scenario.run "http.malformed-body" env;
          invoke Http_scenario.run "http.oversized-body" env)
    }
  ; { name = "unix-peer-and-socket-policy"
    ; run =
        (fun env ->
          invoke Unix_scenario.run "unix.peer-principal-stability" env;
          invoke Smoke_scenario.run "config.insecure-socket-parent" env;
          invoke Smoke_scenario.run "daemon.live-socket-refusal" env;
          invoke Smoke_scenario.run "daemon.stale-socket-recovery" env)
    }
  ; { name = "scope-visibility-before-mutation"
    ; run =
        (fun env ->
          invoke Conformance_scenario.run "conformance.error-codes" env;
          invoke Conformance_scenario.run "conformance.visibility" env;
          invoke Multi_client_scenario.run "observer.mutations-rejected" env)
    }
  ; { name = "blob-path-cleanup-attacks"
    ; run =
        (fun env ->
          invoke Conformance_scenario.run "conformance.blob-read" env;
          invoke Cleanup_scenario.run "physical.never-delete" env;
          invoke Cleanup_scenario.run "cleanup.symlink-refusal" env;
          invoke Cleanup_scenario.run "cleanup.identity-change-refusal" env)
    }
  ; { name = "redaction-surfaces"
    ; run =
        (fun env ->
          invoke Shell_security_scenario.run "redaction.events-jobs-audit-health-logs" env;
          invoke Shell_security_scenario.run "redaction.live-split-deltas" env;
          invoke Shell_security_scenario.run "redaction.live-started" env;
          invoke Shell_security_scenario.run "redaction.live-trace" env)
    }
  ]
;;

let select cases selected =
  match selected with
  | None -> cases
  | Some name ->
    (match List.find cases ~f:(fun case -> String.equal case.name name) with
     | Some case -> [ case ]
     | None -> raise_s [%sexp "unknown consolidated E2E case", (name : string)])
;;

let run name cases env selected_case =
  let selected = select cases selected_case in
  List.iter selected ~f:(fun case -> case.run env);
  Eio.Flow.copy_string
    (Sexp.to_string_hum
       [%sexp
         { scenario = (name : string)
         ; selected_case : string option
         ; passed_cases = (List.map selected ~f:(fun case -> case.name) : string list)
         }]
     ^ "\n")
    (Eio.Stdenv.stdout env)
;;

let run_runtime env ~case = run "canonical-runtime" runtime_cases env case
let run_background env ~case = run "jobs-orchestration" background_cases env case
let run_data env ~case = run "data-retention" data_cases env case
let run_persistence env ~case = run "graceful-restart" persistence_cases env case
let run_security env ~case = run "security-matrix" security_cases env case
