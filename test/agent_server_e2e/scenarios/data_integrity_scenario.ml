open Core

let cases =
  [ ( "http.audit-attribution-pagination-tamper-restart"
    , Support.Data_integrity_http.test_audit )
  ; "http.blob-bounds-digest-foreign-ownership", Support.Data_integrity_http.test_blobs
  ; "daemon-recovery.asset-publication", Support.Data_integrity_http.test_recovery_assets
  ; "daemon-recovery.converter-adoption", Support.Recovery_asset_converter.run
  ; "daemon-recovery.configured-capacity", Recovery_capacity_scenario.run
  ; "daemon-recovery.automatic-capacity", Recovery_automatic_scenario.run
  ; "daemon-recovery.automatic-soft-checkpoint", Recovery_automatic_scenario.run_soft
  ; "daemon-recovery.ordinary-hard-limit", Recovery_successor_guard_scenario.ordinary_hard_limit
  ; "daemon-recovery.near-limit-reminder", Recovery_reminder_scenario.run_stock
  ; "daemon-recovery.near-limit-before-write", Recovery_reminder_scenario.run_cut
  ; "daemon-recovery.near-limit-required-input", Recovery_reminder_scenario.run_tight
  ; "daemon-recovery.successor-hint-guards", Recovery_successor_guard_scenario.run
  ; "daemon-recovery.selected-successor-sources", Recovery_successor_selection_scenario.run
  ; "daemon-recovery.authored-checkpoint", Recovery_checkpoint_scenario.run
  ; "daemon-recovery.soft-checkpoint", Recovery_soft_checkpoint_scenario.run
  ; "daemon-recovery.soft-checkpoint-lost-accept",
    Recovery_soft_checkpoint_scenario.run_lost_accept
  ; "daemon-recovery.authored-rollover-hooks", Recovery_rollover_hooks_scenario.run
  ; "daemon-recovery.installed-originals", Support.Installed_originals.run
  ; "daemon-recovery.installed-handoff", Support.Installed_originals.run_handoff
  ; "http.export-atomic-success", Support.Data_integrity_http.test_export
  ; "http-peer.export-atomic-failures", Support.Data_integrity_download.run
  ; ( "http-peer.export-atomic-cancellation"
    , Support.Data_integrity_download.run_cancellation )
  ; "maintenance-fixture.retention", Support.Data_integrity_retention.run
  ; "daemon-timer.retention-active-protection", Data_integrity_timer.run
  ]
;;

let select = function
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown data integrity case", (name : string)])
;;

let run env ~case =
  Support.Temporary_environment.with_ ~scenario:"data-integrity" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    let sexp =
      [%sexp
        { scenario = ("data-integrity" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }]
    in
    Eio.Flow.copy_string (Sexp.to_string_hum sexp ^ "\n") (Eio.Stdenv.stdout env))
;;
