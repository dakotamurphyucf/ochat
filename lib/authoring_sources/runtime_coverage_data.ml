(* Reviewed runtime transactions and durable work semantics. Source pins detect
   implementation drift, not automatically discovered features. Evidence refers
   to separately executed behavior suites. Surface mappings describe readable
   contracts, never an authority grant. *)
let shared_features =
  [ ( "lifecycle.history"
    , "Compaction retains work; generation reset retires live authority and preserves \
       receipt identity"
    , [ "lib/agent_session/administration.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/managed_submission_tracking.ml"
      ; "lib/agent_server/managed_output_cursor.ml"
      ; "lib/agent_session/notification_delivery.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/chatml_composition/notification_administration_tests.ml"
      ; "test/agent_session/notification_compaction_tests.ml"
      ; "test/agent_session/managed_submission_recovery_tests.ml"
      ; "test/agent_session/managed_output_cursor_tests.ml"
      ] )
  ; ( "lifecycle.source_replacement"
    , "Stopped revision admission, source-owned retirement and no automatic state or \
       event migration"
    , [ "lib/agent_server/command_handler.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/chat_response/background_delivery.ml"
      ; "lib/agent_session/script_schedule_service.ml"
      ; "lib/agent_session/external_ingress.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/chatml_composition/moderator_upgrade_tests.ml"
      ; "test/chatml_composition/background_upgrade_tests.ml"
      ; "test/chatml_composition/timer_upgrade_tests.ml"
      ] )
  ; ( "lifecycle.stop"
    , "Graceful and cancel dispositions, joined cleanup and independent child lifetimes"
    , [ "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/background_execution.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_server/delegation_lifecycle.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/agent_session/extension_stop_tests.ml"
      ; "test/agent_session/delegation_lifecycle_tests.ml"
      ; "test/chatml_composition/background_shell_tests.ml"
      ] )
  ; ( "compilation.policy"
    , "No-effect compiler domains, captured revisions, explicit policy and cooperative \
       cancellation"
    , [ "lib/chatml/chatml_compilation.ml"
      ; "lib/chatml/chatml_compilation_checkpoint.ml"
      ; "lib/chat_response/startup_compilation.ml"
      ; "lib/agent_session/runtime_script_preparation.ml"
      ; "lib/chat_response/script_contract_plan.ml"
      ; "lib/chat_response/restored_programs.ml"
      ]
    , "runtime.execution"
    , [ "test/chatml_compilation_test.ml"
      ; "lib/chatml/chatml_compilation_checkpoint_tests.ml"
      ; "test/chatml_captured_compilation/domain_tests.ml"
      ; "test/chatml_script_routing/startup_budget_tests.ml"
      ; "test/chatml_source_owner/runtime_preparation_tests.ml"
      ; "test/chatml_source_owner/script_tests.ml"
      ] )
  ; ( "tasks.array_traversal"
    , "Sequential deferred callbacks with per-interpretation snapshots, control and \
       resumable state"
    , [ "lib/chatml/chatml_host_runtime.ml"
      ; "lib/chatml/chatml_task_array.ml"
      ; "lib/chatml/chatml_task_array_mode.ml"
      ]
    , "chatml.task-effects"
    , [ "test/chatml_stdlib_traversal/traversal_tests.ml"
      ; "test/chatml_stdlib_traversal/limits_tests.ml"
      ; "test/chatml_stdlib_traversal/lifecycle_tests.ml"
      ; "task-effects.array-traversal"
      ] )
  ; ( "execution.ancestry"
    , "Owned shared counters, nested scope lifetimes and independent session budgets"
    , [ "lib/chatml/chatml_execution.ml" ]
    , "runtime.execution"
    , [ "test/chatml_execution_budget_test.ml"; "test/chatml_projection_budget_test.ml" ]
    )
  ; ( "execution.developer_command_wire"
    , "Pure bounded Developer command, receipt, approval and method-affine wire data; \
       Host resolution and transport handlers require separate installation"
    , [ "lib/agent_protocol/developer_command_id.ml"
      ; "lib/agent_protocol/developer_codec.ml"
      ; "lib/agent_protocol/developer_route.ml"
      ; "lib/agent_protocol/developer_fixture.ml"
      ; "lib/agent_protocol/developer_metadata.ml"
      ; "lib/agent_protocol/developer_request_data.ml"
      ; "lib/agent_protocol/developer_view.ml"
      ; "lib/agent_protocol/developer_receipt.ml"
      ; "lib/agent_protocol/developer_common.ml"
      ; "lib/agent_protocol/developer_approval.ml"
      ; "lib/agent_protocol/developer_wire.ml"
      ; "lib/agent_protocol/developer_repl_wire_common.ml"
      ; "lib/agent_protocol/developer_repl_wire_result.ml"
      ; "lib/agent_protocol/developer_repl_wire_task.ml"
      ; "lib/agent_protocol/developer_repl_wire.ml"
      ; "lib/agent_store/developer_command_receipt.ml"
      ]
    , "runtime.execution"
    , [ "test/developer_protocol/developer_protocol_tests.ml"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.developer_portable_endpoint"
    , "Explicit Host-backed Developer methods, current Authoring source admission and \
       exact-key historical retry; bounded joined lifecycle invalidations with \
       authoritative reobservation"
    , [ "lib/agent_protocol/developer_wire.ml"
      ; "lib/agent_protocol/developer_repl_wire.ml"
      ; "lib/agent_server/developer_command_wire_receipt.ml"
      ; "lib/agent_server/developer_endpoint.ml"
      ; "lib/agent_server/developer_repl_registry.ml"
      ; "lib/agent_server/developer_portable_admission.ml"
      ; "lib/agent_server/developer_change_hub.ml"
      ; "lib/agent_server/dispatcher.ml"
      ; "lib/agent_server/daemon.ml"
      ; "lib/agent_server/authoring_installation.ml"
      ; "lib/agent_server/connection_context.ml"
      ; "lib/agent_server/startup_cleanup.ml"
      ; "bin/ochat_agent_server.ml"
      ]
    , "runtime.execution"
    , [ "test/developer_root_owner/developer_host_tests.ml"
      ; "lib/agent_server/developer_change_hub_tests.ml"
      ; "test/agent_server_e2e/scenarios/developer_wire_scenario.ml"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.developer_shell_approval"
    , "Bounded once-only developer Shell review under real command, native and claimed \
       execution ownership; selected checks survive approval waits and cancellation"
    , [ "lib/shell_runtime/approval_broker.ml"
      ; "lib/shell_runtime/call_context.ml"
      ; "lib/agent_server/developer_approval_owner.ml"
      ; "lib/agent_server/developer_command_lifecycle.ml"
      ; "lib/agent_server/developer_native_executor.ml"
      ; "lib/agent_server/developer_background_job_runner.ml"
      ; "lib/agent_server/developer_model_job_runner.ml"
      ; "lib/agent_server/developer_child_agent_runner.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/agent_server/daemon.ml"
      ]
    , "runtime.execution"
    , [ "test/shell_runtime_phase1_test.ml"
      ; "test/developer_root_owner/developer_approval_tests.ml"
      ; "test/provider_runtime_host/developer_model_approval_tests.ml"
      ] )
  ; ( "execution.developer_native_profile"
    , "Opt-in selected Prompt native profile pinned to the Live catalog; exact runtime \
       bindings, reviewer currentness and independent Shell grant admission"
    , [ "lib/agent_server/developer_daemon_configuration.ml"
      ; "lib/agent_server/developer_native_permission.ml"
      ; "lib/agent_server/developer_live_resources.ml"
      ; "lib/agent_server/developer_child_agent_runner.ml"
      ; "lib/agent_server/developer_background_job_runner.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/agent_server/daemon.ml"
      ]
    , "runtime.execution"
    , [ "test/developer_root_owner/developer_host_tests.ml"
      ; "test/developer_root_owner/developer_approval_tests.ml"
      ; "test/provider_runtime_host/developer_agent_tests.ml"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.developer_configured_startup"
    , "Explicit finite daemon Developer selection bound to installed Authoring, source, \
       native and provider owners; shared Standard contracts and truthful opt-in"
    , [ "lib/agent_server/developer_startup_configuration.ml"
      ; "lib/agent_server/developer_startup_limits.ml"
      ; "lib/agent_server/developer_startup_policy.ml"
      ; "lib/agent_server/config_validator.ml"
      ; "lib/agent_server/authoring_installation.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/daemon.ml"
      ; "bin/inference_composition.ml"
      ]
    , "runtime.execution"
    , [ "lib/agent_server/developer_startup_tests.ml"
      ; "test/agent_server_e2e/scenarios/developer_wire_scenario.ml"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.developer_embedded_local_policy"
    , "Bounded local stdio policy binds exact Store-owned operator identity to existing \
       source, Developer and manifest grants without changing absent-policy behavior"
    , [ "lib/agent_server/embedded_local_template.ml"
      ; "lib/agent_server/config_validator.ml"
      ; "lib/agent_server/embedded.ml"
      ; "lib/agent_server/daemon.ml"
      ; "bin/ochat_agent_stdio.ml"
      ]
    , "runtime.execution"
    , [ "test/local_storage/embedded_local_policy_tests.ml"
      ; "test/local_storage/embedded_repl_shell_tests.ml"
      ; "test/agent_server_e2e/scenarios/stdio_scenario.ml"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.developer_repl_task"
    , "Retained REPL Task references and bounded frozen-type projection, explicit \
       durable Start and terminal receipt, current selected authority at new effects, \
       and joined cancellation/reset"
    , [ "lib/chatml_repl_execution/chatml_repl_execution.ml"
      ; "lib/agent_store/developer_command_receipt.ml"
      ; "lib/agent_store/developer_command_row.ml"
      ; "lib/agent_store/developer_run_store.ml"
      ; "lib/agent_server/developer_capture_host.ml"
      ; "lib/agent_server/developer_root_source.ml"
      ; "lib/agent_server/developer_repl_epoch.ml"
      ; "lib/agent_server/developer_repl_projection.ml"
      ; "lib/agent_server/developer_daemon_configuration.ml"
      ; "lib/agent_server/developer_repl_task_original.ml"
      ; "lib/agent_server/developer_repl_task_execution.ml"
      ; "lib/agent_server/developer_repl_task_receipt.ml"
      ; "lib/agent_server/developer_command_budgets.ml"
      ; "lib/agent_server/developer_command_lifecycle.ml"
      ; "lib/agent_server/developer_root_owner.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/agent_server/daemon.ml"
      ]
    , "runtime.execution"
    , [ "test/developer_root_owner/developer_host_tests.ml"
      ; "lib/agent_server/developer_host_pair_tests.ml"
      ; "test/developer_root_owner/command_lifecycle_tests.ml"
      ; "test/developer_run_store/developer_command_tests.ml"
      ; "test/developer_run_store/developer_run_store_tests.ml"
      ] )
  ; ( "execution.developer_repl_source_retention"
    , "Captured REPL source survives editor closure under a separately charged owner; \
       immutable Project contract comparison evidence avoids repeated encoding while \
       fresh actor, source and upload checks still deny revoked effects"
    , [ "lib/agent_server/authoring_source_host.ml"
      ; "lib/agent_server/authoring_context_owner.ml"
      ; "lib/agent_server/developer_capture_host.ml"
      ; "lib/agent_server/developer_repl_epoch.ml"
      ]
    , "runtime.execution"
    , [ "test/project_source_host/authoring_tests.ml"
      ; "test/authoring_context_service/service_tests.ml"
      ; "test/authoring_upload_store/publication_tests.ml"
      ; "docs-src/development/chatml-authoring-source-host.md"
      ; "docs-src/design/chatml-developer-runtime.md"
      ] )
  ; ( "execution.authoring_upload_selection"
    , "Configured upload publication reselects current actor and target policy; exact \
       immutable compiler identity avoids redundant encoding while distinct values \
       retain semantic comparison and parent/previous checks"
    , [ "lib/agent_server/authoring_configured_upload_policy.ml"
      ; "lib/agent_server/authoring_upload_validation.ml"
      ; "lib/agent_server/authoring_upload_host.ml"
      ]
    , "runtime.execution"
    , [ "test/authoring_upload_host/host_tests.ml"
      ; "test/authoring_installation/installation_tests.ml"
      ; "docs-src/design/chatml-authoring-services.md"
      ] )
  ; ( "execution.trace"
    , "Optional bounded source-owned event prefix, final outcome custody and unchanged \
       execution, effects and cancellation"
    , [ "lib/chatml_trace/chatml_trace.mli"
      ; "lib/chatml_trace/chatml_trace.ml"
      ; "lib/chatml/chatml_eval.ml"
      ; "lib/chatml/chatml_compilation.ml"
      ; "lib/chatml/chatml_host_runtime.ml"
      ; "lib/chatml_linked_runtime/chatml_linked_runtime.ml"
      ; "lib/chatml_linked_runtime/chatml_module_initializer.ml"
      ; "lib/chatml_linked_runtime/chatml_repl_modules.ml"
      ]
    , "runtime.execution"
    , [ "test/chatml_trace/trace_tests.ml"
      ; "docs-src/design/chatml-execution-tracing.md"
      ] )
  ; ( "execution.developer_trace_attachment"
    , "Bounded source-owned recording for Developer commands and retained REPL work; \
       joined terminal custody and authenticated historical source projection"
    , [ "lib/agent_server/developer_trace_retention.ml"
      ; "lib/agent_server/developer_historical_source.ml"
      ; "lib/agent_server/authoring_source_host.ml"
      ; "lib/agent_server/authoring_context_service_core.ml"
      ; "lib/agent_server/authoring_context_service.ml"
      ; "lib/agent_server/authoring_host_policy.ml"
      ; "lib/agent_server/authoring_upload_host.ml"
      ; "lib/agent_server/authoring_installation.ml"
      ; "lib/agent_server/developer_capture_host.ml"
      ; "lib/agent_store/authoring_upload_store.ml"
      ; "lib/agent_server/developer_command_lifecycle.ml"
      ; "lib/agent_server/developer_repl_epoch.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/chatml_trace/chatml_trace.ml"
      ]
    , "runtime.execution"
    , [ "lib/agent_server/developer_host_pair_tests.ml"
      ; "test/developer_root_owner/command_lifecycle_tests.ml"
      ; "test/chatml_trace/trace_tests.ml"
      ] )
  ; ( "execution.developer_trace_pages"
    , "Authenticated bounded pages over retained Developer Command, REPL statement and \
       Task traces; exact comparison cursors, historical source filtering and explicit \
       portable startup admission"
    , [ "lib/agent_protocol/developer_trace_wire.ml"
      ; "lib/agent_server/developer_trace_page.ml"
      ; "lib/agent_server/developer_trace_retention.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/agent_server/developer_endpoint.ml"
      ; "lib/agent_server/developer_portable_admission.ml"
      ; "lib/agent_server/developer_daemon_configuration.ml"
      ; "lib/agent_server/daemon.ml"
      ; "lib/agent_server/dispatcher.ml"
      ]
    , "runtime.execution"
    , [ "test/developer_protocol/developer_protocol_tests.ml"
      ; "lib/agent_server/developer_trace_page.ml"
      ; "lib/agent_server/developer_portable_admission_tests.ml"
      ; "test/agent_server_e2e/scenarios/developer_wire_scenario.ml"
      ] )
  ; ( "execution.developer_trace_client"
    , "Closed first-party trace reads on one initialized connection across embedded, \
       HTTP, Unix and stdio; exact page affinity and current Host authorization"
    , [ "lib/agent_client/developer_trace_connection.ml"
      ; "lib/agent_client/connection.ml"
      ; "lib/agent_client/transport.ml"
      ; "lib/agent_client/in_memory.ml"
      ; "lib/agent_transport_http/client.ml"
      ; "lib/agent_transport_ndjson/client.ml"
      ; "lib/agent_transport_stdio/gateway.ml"
      ; "lib/agent_server/embedded.ml"
      ]
    , "runtime.execution"
    , [ "test/agent_client_trace/trace_connection_tests.ml"
      ; "test/agent_server_e2e/scenarios/developer_trace_client_scenario.ml"
      ] )
  ; ( "execution.configured_modules"
    , "Contract and invocation preflight before effects, canonical once-only value \
       dependencies, execution-local state and shared controls"
    , [ "lib/chatml_linked_runtime/chatml_linked_runtime.ml"
      ; "lib/chatml_linked_runtime/chatml_module_initializer.ml"
      ; "lib/chatml_linked_runtime/chatml_repl_modules.ml"
      ; "lib/chatml/chatml_builtin_modules.ml"
      ; "lib/chatml/chatml_compiler_contract.ml"
      ; "lib/chatml/chatml_contract_encoding.ml"
      ; "lib/chatml/chatml_eval.ml"
      ; "lib/chatml/chatml_lang.ml"
      ; "lib/chatml/chatml_host_runtime.ml"
      ; "lib/chatml/chatml_execution.ml"
      ]
    , "runtime.execution"
    , [ "test/chatml_linked_runtime/runtime_tests.ml"
      ; "test/chatml_linked_runtime/control_tests.ml"
      ; "test/chatml_linked_runtime/order_model_tests.ml"
      ; "test/chatml_linked_runtime/invocation_tests.ml"
      ; "test/chatml_linked_runtime/repl_module_tests.ml"
      ] )
  ; ( "diagnostics.deferred_ownership"
    , "Deferred source sites, bounded diagnostics and unchanged task fuel/recovery \
       boundaries"
    , [ "lib/chatml/chatml_lang.ml"
      ; "lib/chatml/chatml_host_runtime.ml"
      ; "lib/chatml/chatml_execution.ml"
      ; "lib/chatml_sources/chatml_diagnostic.ml"
      ]
    , "runtime.execution"
    , [ "test/chatml_owned_diagnostics/task_tests.ml"
      ; "test/chatml_owned_diagnostics/execution_tests.ml"
      ; "test/chatml_owned_diagnostics/session_tests.ml"
      ; "test/chatml_diagnostics/diagnostic_invariants.ml"
      ] )
  ; ( "jobs.launch"
    , "Selected job starts stage intent until owner commit; reads and cancellation \
       retain live authority"
    , [ "lib/chat_response/background_job_operations.ml"
      ; "lib/chat_response/background_request.ml"
      ; "lib/agent_session/script_job_kernel.ml"
      ; "lib/agent_session/script_job_request.ml"
      ; "lib/agent_session/script_job_service.ml"
      ; "lib/agent_session/staged_jobs.ml"
      ; "lib/agent_server/developer_script_job_service.ml"
      ; "lib/agent_server/developer_host.ml"
      ; "lib/agent_server/developer_background_job_runner.ml"
      ; "lib/agent_server/developer_claim_deadline.ml"
      ; "lib/agent_server/developer_managed_tool_executor.ml"
      ; "lib/agent_server/developer_native_executor.ml"
      ; "lib/agent_server/developer_script_job_execution.ml"
      ]
    , "runtime.jobs.owned"
    , [ "test/chatml_composition/background_tests.ml"
      ; "test/agent_session/background_transaction_tests.ml"
      ; "test/agent_session/moderator_job_transaction_tests.ml"
      ; "test/script_job_kernel/kernel_tests.ml"
      ; "test/developer_root_owner/native_executor_tests.ml"
      ] )
  ; ( "jobs.results"
    , "Retained status, bounded completion projections and authorized artifact reads"
    , [ "lib/agent_protocol/job.ml"
      ; "lib/agent_protocol/stored_completion.ml"
      ; "lib/agent_session/script_job_service.ml"
      ; "lib/agent_session/background_execution.ml"
      ]
    , "runtime.work-values"
    , [ "test/chatml_composition/background_artifact_tests.ml"
      ; "test/agent_session/background_disclosure_tests.ml"
      ] )
  ; ( "jobs.recovery"
    , "Claimed attempts, cancellation, waiting dependencies and interruption without \
       blind replay"
    , [ "lib/agent_server/job_scheduler.ml"
      ; "lib/agent_server/job_scheduler_developer.ml"
      ; "lib/agent_session/background_execution.ml"
      ; "lib/agent_session/session_actor.ml"
      ]
    , "runtime.recovery.background"
    , [ "test/chatml_composition/background_recovery_tests.ml"
      ; "test/chatml_composition/background_pending_restart_tests.ml"
      ] )
  ; ( "jobs.saved_agent_inference"
    , "A physically selected saved-Agent callback and its parser-owned nested Content \
       children retain one Root, bounded source budget and developer inference tracking \
       without granting other async jobs model authority. Queued child starts retain \
       separate issuer captures; installed Tool and Script workers restore them inside \
       the physical Claim with saved pins, source-owned managed dependencies and joined \
       resource lifetimes"
    , [ "lib/agent_protocol/developer_agent_artifact.ml"
      ; "lib/chat_response/background_request.ml"
      ; "lib/agent_session/captured_agent_runner.ml"
      ; "lib/agent_session/claimed_child_runtime.ml"
      ; "lib/agent_session/developer_claimed_resource_owner.ml"
      ; "lib/agent_session/developer_child_preflight.ml"
      ; "lib/agent_session/selected_agent_depth.ml"
      ; "lib/agent_store/developer_run_document.ml"
      ; "lib/agent_store/developer_run_store.ml"
      ; "lib/agent_server/developer_root_owner.ml"
      ; "lib/agent_server/developer_background_job_runner.ml"
      ; "lib/agent_server/developer_child_agent_runner.ml"
      ; "lib/agent_server/developer_child_content_projection.ml"
      ; "lib/agent_server/developer_child_selection.ml"
      ; "lib/agent_server/developer_queued_issuer.ml"
      ; "lib/agent_server/developer_agent_source_runtime.ml"
      ; "lib/agent_server/developer_live_resources.ml"
      ; "lib/agent_server/developer_native_executor.ml"
      ; "lib/chat_response/converter.ml"
      ]
    , "runtime.jobs.owned"
    , [ "test/queued_source_foundation/foundation_tests.ml"
      ; "test/developer_run_store/developer_inference_tracking_tests.ml"
      ; "test/developer_root_owner/native_executor_tests.ml"
      ; "test/provider_runtime_host/developer_agent_tests.ml"
      ; "test/provider_runtime_host/runtime_host_tests.ml"
      ] )
  ]
;;

let moderator_features =
  [ ( "moderator.state_limits"
    , "Fresh event scopes, bounded data snapshots and validation before commit"
    , [ "lib/chatml/chatml_value_codec.ml"
      ; "lib/chat_response/moderator_invocation.ml"
      ; "lib/chat_response/moderator_manager.ml"
      ]
    , "runtime.execution"
    , [ "test/moderation/moderator_execution_budget_test.ml"
      ; "test/moderation/moderator_invocation_test.ml"
      ; "test/chatml_script_routing/routing_tests.ml"
      ] )
  ; ( "moderator.transactions"
    , "Serialized handler state, invocation resolution and local commit versus external \
       effects"
    , [ "lib/chat_response/moderator_manager.ml"
      ; "lib/chat_response/moderator_invocation.ml"
      ; "lib/agent_session/moderator_tool_dispatch.ml"
      ; "lib/agent_session/session_actor.ml"
      ]
    , "runtime.invocations.moderator"
    , [ "test/chatml_composition/moderator_pending_tests.ml"
      ; "test/agent_server_restart_test.ml"
      ; "test/chatml_script_routing/routing_tests.ml"
      ] )
  ; ( "moderator.control"
    , "Model and process effects, transactional runtime requests and bounded turn \
       continuation"
    , [ "lib/chatml/chatml_host_runtime.ml"
      ; "lib/chat_response/runtime_semantics.ml"
      ; "lib/chat_response/moderator_manager.ml"
      ; "lib/agent_protocol/model_spawn_request.ml"
      ; "lib/chat_response/ctx.ml"
      ; "lib/chat_response/driver.ml"
      ; "lib/chat_response/model_executor.ml"
      ; "lib/chat_response/model_completion_drain.ml"
      ]
    , "runtime.control"
    , [ "test/agent_docs/docs_chatml_control.ml"
      ; "test/chatml_composition/automatic_turn_budget_tests.ml"
      ; "test/moderation/moderator_execution_budget_test.ml"
      ; "test/moderation/chat_response_moderator_executor_test.ml"
      ; "test/model_spawn_request/model_spawn_request_tests.ml"
      ; "test/provider_runtime_host/developer_agent_tests.ml"
      ] )
  ; ( "jobs.delivery"
    , "Retained terminal attempts, source identity and acknowledgement before completion \
       handling"
    , [ "lib/chat_response/background_delivery.ml"
      ; "lib/agent_session/background_job_event.ml"
      ; "lib/agent_session/session_actor.ml"
      ]
    , "runtime.jobs.acknowledgement"
    , [ "test/agent_session/background_delivery_tests.ml"
      ; "test/chatml_composition/background_moderator_tests.ml"
      ] )
  ; ( "subscriptions.state"
    , "Owned provisional mutations, epochs, terminal winners, expiry and catch rollback"
    , [ "lib/chat_response/subscription_operations.ml"
      ; "lib/agent_session/script_subscription_service.ml"
      ; "lib/agent_session/staged_subscriptions.ml"
      ; "lib/agent_protocol/subscription.ml"
      ]
    , "runtime.jobs.subscriptions"
    , [ "test/chatml_composition/subscription_tests.ml"
      ; "test/chatml_composition/subscription_expiry_tests.ml"
      ; "test/chatml_script_routing/routing_tests.ml"
      ] )
  ; ( "timers.lifecycle"
    , "Source-owned one-shot timers, misfire policy and subscription dependency receipts"
    , [ "lib/chat_response/schedule_operations.ml"
      ; "lib/agent_session/script_schedule_service.ml"
      ; "lib/agent_server/schedule_scheduler.ml"
      ]
    , "runtime.jobs.timers"
    , [ "test/chatml_composition/timer_tests.ml"
      ; "test/chatml_composition/timer_upgrade_tests.ml"
      ] )
  ; ( "notifications.publication"
    , "Staged publication, disclosure ceilings, correlation and per-owner receipt \
       selection"
    , [ "lib/chat_response/notification_operations.ml"
      ; "lib/agent_session/script_notification_service.ml"
      ; "lib/agent_session/staged_notifications.ml"
      ; "lib/agent_protocol/delivery.ml"
      ]
    , "runtime.delivery.notifications"
    , [ "test/chatml_composition/notification_admission_tests.ml"
      ; "test/agent_session/notification_transaction_tests.ml"
      ] )
  ; ( "notifications.delivery"
    , "Acknowledgement ancestry, safe-point history insertion, wake coalescing and \
       stopped sessions"
    , [ "lib/agent_session/notification_readiness.ml"
      ; "lib/agent_session/notification_delivery.ml"
      ; "lib/agent_session/notification_history.ml"
      ; "lib/agent_session/session_actor.ml"
      ]
    , "runtime.delivery.notifications"
    , [ "test/chatml_composition/notification_ancestry_tests.ml"
      ; "test/chatml_composition/notification_wake_tests.ml"
      ; "test/agent_session/notification_history_tests.ml"
      ] )
  ; ( "ingress.authority"
    , "Registered producers, scoped data-only completion, schemas, epochs and replay \
       receipts"
    , [ "lib/chat_response/ingress_operations.ml"
      ; "lib/agent_session/script_ingress_service.ml"
      ; "lib/agent_session/external_ingress.ml"
      ]
    , "runtime.delivery.ingress"
    , [ "test/chatml_composition/ingress_protocol_tests.ml"
      ; "test/agent_session/external_ingress_tests.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/agent_client/developer_trace_connection.ml"
    , "a737c2bc921e42546ae3ae8165eae078a409861563acbb61782db870dcf4adf6" )
  ; ( "lib/agent_client/connection.ml"
    , "7971ebae32e686004739e1a6c0217509f2729cc8df9a5ce3232b3c5f226c8aa0" )
  ; ( "lib/agent_client/transport.ml"
    , "d39a1097e48c06192980ded2c44f4d268d7f93d333a3fe2227f30fb8ac0c6ab6" )
  ; ( "lib/agent_client/in_memory.ml"
    , "c2729b79245910c4b772535bb5bc71028380b9ce01f57181b8078283e9e4f204" )
  ; ( "lib/agent_transport_http/client.ml"
    , "663a0a0be532725d433ef1828e71b01928dcb9c37843b96d4938ab82082956c9" )
  ; ( "lib/agent_transport_ndjson/client.ml"
    , "d0455e5ec03f882f1dc249eee5d571df3cef7f916b0c6dfbb8447e825c2db0a4" )
  ; ( "lib/agent_transport_stdio/gateway.ml"
    , "552d07ffc9ddd233166ac927be974e6193bd8bd76f7e2928d2911b5481ce1029" )
  ; ( "lib/agent_server/developer_startup_configuration.ml"
    , "87e0f93fd9b82f319d26b1612407ce3543a0f0913d258b42f6739a09af2441f5" )
  ; ( "lib/agent_server/developer_startup_limits.ml"
    , "f3c3dc3b7bcb74a0eec7cde6455e7f950b97f8e84176a4ac7f5b9f2741d14f88" )
  ; ( "lib/agent_server/developer_startup_policy.ml"
    , "4f9bb9a387599ae65a60dff32f59e49bffd4e5a66dfa9181345ec344447699b8" )
  ; ( "lib/agent_server/config_validator.ml"
    , "7c0e553ab0d172a19cf38d7f7f30d1d42e43d06d794b11c621e4a0fd542c63d7" )
  ; ( "lib/agent_server/embedded_local_template.ml"
    , "93691f37b0b11d3b5ef3303d2d6f35d7bdffe2ea2eb360db9d007ab841210e3e" )
  ; ( "lib/agent_server/embedded.ml"
    , "f37e10701440a427900545871dd8433933ebaa12edd9d61e2b5c464de835f6d6" )
  ; ( "bin/ochat_agent_stdio.ml"
    , "8d134cd11c7df2a60e6b4b77a5ff7f9ea0c28a8a2674a3dd9f573adaa7623902" )
  ; ( "lib/agent_server/session_factory.ml"
    , "6d91a79fe221939cafec72f63e21a5ab8a81476defb7a63999a4fc97012a90ac" )
  ; ( "bin/inference_composition.ml"
    , "1f57451d5543a5fe8f2f23dc0e593374b64946d088cba164acd1c087a7cdce18" )
  ; ( "lib/agent_protocol/developer_command_id.ml"
    , "169796d1fcf9fd4003ab3c0985220b3c29e937a3e0893fbfde4736f378e0750e" )
  ; ( "lib/agent_protocol/developer_codec.ml"
    , "7b3ce2ecabdaeab666cd622b139e17e05d0aee107fd55bc8e39cf74568a48690" )
  ; ( "lib/agent_protocol/developer_route.ml"
    , "6b06658547f3114aefad22a8f7dd73f57e1ddb6c7e464d5370abe3020b6a78d9" )
  ; ( "lib/agent_protocol/developer_fixture.ml"
    , "5d731984676192165c6f317aaea60aea0fefcf700456870a26568d5ed2c64d3e" )
  ; ( "lib/agent_protocol/developer_metadata.ml"
    , "3c1dc02a49191c8bc22f3690fac56064b8a8d02b5bcddf373a8f27444e0ef8f3" )
  ; ( "lib/agent_protocol/developer_request_data.ml"
    , "f470183b3cfa04fb9970a8dff0ffd3a88eba3768130c06319ce58b2cfea3b3df" )
  ; ( "lib/agent_protocol/developer_view.ml"
    , "b4ce96438f22d25730b016fdc940ae0c7e65ccc6166de834ceea7e4165490571" )
  ; ( "lib/agent_protocol/developer_receipt.ml"
    , "f8121cb5159f58b9dc06c5403ff80c67a32f6fff5037e0c60d36e1a3d03c844c" )
  ; ( "lib/agent_protocol/developer_common.ml"
    , "5c3846d01b34cf7c514e8f5f4eb491ba7bb9a7578e928549479cd63d58710bdf" )
  ; ( "lib/agent_protocol/developer_approval.ml"
    , "53faaa0df245f4e1ca8194589e8abd190580ff81f433dcfc8fe1d039e94527a5" )
  ; ( "lib/agent_protocol/developer_wire.ml"
    , "7455a8a86e065aa4e6c4d04a8ee82eef9a0f0231eaae34576137effb2282a093" )
  ; ( "lib/agent_protocol/developer_repl_wire_common.ml"
    , "73b6d7e0b82fb0136f32a24b74b70b711ccd57010c797af925896c4c1eccdee4" )
  ; ( "lib/agent_protocol/developer_repl_wire_result.ml"
    , "d6b1c58696a2ca6c4d386ebadc87740c1377bb93808954e77bfffef4e1e5b795" )
  ; ( "lib/agent_protocol/developer_repl_wire_task.ml"
    , "d0cf80397174e6b8487b1b139c7e116640bcb331d8f176bf0979c621a2bbcf7c" )
  ; ( "lib/agent_protocol/developer_repl_wire.ml"
    , "2926bde94430e0348d63ff0b11918080077163ea5646688f76bc8b81894d1f94" )
  ; ( "lib/agent_protocol/developer_trace_wire.ml"
    , "962b2f2869c7d7148f5331b584bc5d44558380fd0c99441d3a1f4fc8eb9cf98b" )
  ; ( "lib/agent_server/developer_trace_page.ml"
    , "824dda3ea7b8d177f9916e2ed99a52328ee7f84e7a40c189b4b8a66ce2f50c6e" )
  ; ( "lib/agent_server/developer_command_wire_receipt.ml"
    , "5d30a8c37696290d8b1a1201855f763fdd39a7506a265232c11089179d625c1b" )
  ; ( "lib/agent_server/developer_endpoint.ml"
    , "20969b9490ecad9ed049513a566a95ca26137a25fd63689cda13337b6a8f2dd0" )
  ; ( "lib/agent_server/developer_repl_registry.ml"
    , "98b4d64dfb527512faa03099a2971a8f0bac733f7eb9eb6d6c704e335673e6f0" )
  ; ( "lib/agent_server/developer_portable_admission.ml"
    , "ff4357ffeec4ff2088d8d06f209eb330d70a628bd16e15784e45391ffac3f265" )
  ; ( "lib/agent_server/developer_change_hub.ml"
    , "c55785240bc6028fb86a9be3d0b0e15ce8322c15b293bb175e953b6faf0a0d12" )
  ; ( "lib/agent_server/dispatcher.ml"
    , "e02a0e78a8f874c8e81f5cb02e83f3bddc4409996e88b79b23a5d86e30412d6d" )
  ; ( "lib/agent_server/authoring_installation.ml"
    , "cffa9affee87aae27b9b0f1d308194fcb45ab84d0da74bf0af5482c61a2ca784" )
  ; ( "lib/agent_server/connection_context.ml"
    , "6efbe0038e4d033c7fd06b441e88e0f37194f444a8a79cf54c8c78def67c1584" )
  ; ( "lib/agent_server/startup_cleanup.ml"
    , "bcbd487cccc3783d039e2a31af0e4c836813fa4a6ef417305e4adb58a3f250fc" )
  ; ( "bin/ochat_agent_server.ml"
    , "92de01b5a80a959e0ef77853af9113b8455afa7f7cde2beafb7ce4689da5b9fe" )
  ; ( "lib/agent_store/developer_command_receipt.ml"
    , "e5dc9cc0cdcd487287fc443b97ff996a7b1ea9319aa7c0c98c920ffcd4abe129" )
  ; ( "lib/shell_runtime/approval_broker.ml"
    , "e66598888f26cca06cdb4ae70078fd2b89aea5518adfa64d7ab5958fe8f540fc" )
  ; ( "lib/shell_runtime/call_context.ml"
    , "2a7f5fc4025ec16659aed443679e997d8828262475909815f5507a7276f06de8" )
  ; ( "lib/agent_server/developer_approval_owner.ml"
    , "b35fec39738beb88b66c4f7cd50e1fb4e432f8188b72b58057a683f17c81d657" )
  ; ( "lib/agent_server/developer_trace_retention.ml"
    , "f95116a80f0d5aed07f8773212ed585a0b97bdbb0eb1df575fd8b17f163fbc4c" )
  ; ( "lib/agent_server/developer_command_lifecycle.ml"
    , "2b45b46ab1242a9bb41404cccd65f0fbed2bcd9b204fdbf0a18b5dec97e80ac4" )
  ; ( "lib/agent_server/developer_daemon_configuration.ml"
    , "7ef6ad1812644916c0505731cb26cc008a684f7141ce341891b0376b666b77ee" )
  ; ( "lib/agent_server/developer_native_permission.ml"
    , "fee57b82d1415cee8cf1a5e82cf4f6612d8de53484ed638d7c51c33fdefecccd" )
  ; ( "lib/agent_server/developer_model_job_runner.ml"
    , "7604ff6140540e3b53720515526242e7bc1014c97d6895aacadbfb2909bda718" )
  ; ( "lib/agent_server/daemon.ml"
    , "5346930892fcd88a090ad80015ce3fc4895decc8f43e63d845ab98597ce31188" )
  ; ( "lib/agent_session/developer_claimed_resource_owner.ml"
    , "fb251c6e022f9880820b15807018adc5cf240500050a0424321d7507d3e2bc41" )
  ; ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_session/claimed_child_runtime.ml"
    , "f5ab8751939aa924659c11e022719444ee8457d818631db8151abf0dc7ab22e6" )
  ; ( "lib/agent_protocol/model_spawn_request.ml"
    , "258c00854df5cd7a65eb28d8fccc4ab1e1d1880ccbbc15999306fd448322d160" )
  ; ( "lib/chat_response/ctx.ml"
    , "7cc0abfa6a5091d99c4d6877d6495ceb6c516c2950902c0ca6d0cd9c5844f99e" )
  ; ( "lib/chat_response/driver.ml"
    , "5bc8bf3d7831648af0f2ce63ff144a1c9d40f1f09f303b850354abdaedc14dd0" )
  ; ( "lib/chat_response/model_executor.ml"
    , "587b7c855a3b9e47a5a055ae29dbe2f2966c4a293bf5b15776c7e98a858c9113" )
  ; ( "lib/chat_response/model_completion_drain.ml"
    , "e635d3475610f909725ddab11af873e4ff1d2b55f06a59bfe24f9669c781f8c4" )
  ; ( "lib/chatml/chatml_task_array.ml"
    , "b5f9b9636fbcdc894d1c60de57da053abb2c3dfe1994989433dcc3ccd31c4523" )
  ; ( "lib/chatml/chatml_task_array_mode.ml"
    , "8b580d138d8664634a4cfee9b106bef686317ddccc6fa4a5df8dfbd265f4e3cf" )
  ; ( "lib/chatml_linked_runtime/chatml_linked_runtime.ml"
    , "45ccf948bf8c8185d3eff317bc2f6c9ee845585e681888d7665d08f8923ba2f1" )
  ; ( "lib/chatml_linked_runtime/chatml_module_initializer.ml"
    , "fccec1e6cb793c9caf9448db7474cbc2a40e6a42412e34a3b0967f256ebbf47a" )
  ; ( "lib/chatml_linked_runtime/chatml_repl_modules.ml"
    , "42fe1312dbbe7c725bc8979f49f4397721d6bd61ab7049c9c9357ffd35ae1bc7" )
  ; ( "lib/chatml/chatml_builtin_modules.ml"
    , "4d005f60b55cee3263e7925bae6d1c81129ca383cf6218b3cc2dc86270def236" )
  ; ( "lib/chatml/chatml_compiler_contract.ml"
    , "85a8d9efb0d8eee24819512c0fc4afaa62c49456d3e0d19e112f78852dcb86a6" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "fd6017ab5a829ce8ae18056cb13da322c7ddae323815cf803bfe5fd804e21444" )
  ; ( "lib/chatml_trace/chatml_trace.ml"
    , "b2fa111ff3d9c269b82b4931e8bbcebc58a393c1700bf24573ca1851cb6aecc4" )
  ; ( "lib/chatml_trace/chatml_trace.mli"
    , "8ebf49b2e1858f86daadf6fb687cce0436edec79bdb46935fd7ae2647e1e8f7b" )
  ; ( "lib/agent_session/administration.ml"
    , "00ae2dcdef80bd0552e6a7dded2e3872ed547b82b2ee85297ca5eb7d3ccd9f8a" )
  ; ( "lib/agent_server/command_handler.ml"
    , "895cc7c1e6cc706adc39f6b98c71e1c994aab20814cd86f642d244bb17a06a59" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "7431f8254c524a641f3ce86633cd812481b938ba3b6e35bf1e31d01059cd7d66" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/chatml/chatml_compilation.ml"
    , "a8179fdcf91abed22840028394cadf7032162c45f76f686c11e6ed6a12edb721" )
  ; ( "lib/chatml/chatml_compilation_checkpoint.ml"
    , "14e7da378d7d2cab859bf8d2c7357f88521d11cdec8851cc83df37cd2bee6224" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "7a69997aff6b6b2e15ead628049febc51fe95a36ac2e5b63b8e5f5277f27e4cb" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/agent_protocol/delivery.ml"
    , "63ae0f4d478302e84a1e49c84f406abf090794db4c231aa76181ad02072581d9" )
  ; ( "lib/agent_protocol/developer_agent_artifact.ml"
    , "9fa006b7103cab22b7de2f290c808ed9deb3f9ed968cf7a036767444d7531de2" )
  ; ( "lib/agent_protocol/job.ml"
    , "9f7ce5c6116a85816df7e489d7a6ba0c54114e9248592b2e7b38f0b0097a01bb" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "a5447544cbe1cd55302cd70a3c50bf7f6b0ec07ad3af8ce1f70b7b90dfbd07bc" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "879dba4811cd93c1d65da4267ac80e701dfc236a81c14c62c8e794866407d32e" )
  ; ( "lib/agent_server/job_scheduler_developer.ml"
    , "18062ce3137c3b1782fb4015657ca304a591341a33e1d1da47d362b137b7fd19" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "9450ea610c424f91480b70ca733226137fa0b18325f07bf789da3e582e8d1876" )
  ; ( "lib/agent_server/developer_child_agent_runner.ml"
    , "379e6ee1a98e25875c780023d5acf5b6062589ec1c3eb717b3ff60ca4dbb7204" )
  ; ( "lib/agent_server/developer_child_content_projection.ml"
    , "17dbb6bc0891ad5bb5c3879a2b1e946c05a01d13db32a15d220faa531cb82ba8" )
  ; ( "lib/agent_server/developer_child_selection.ml"
    , "615ce2d2ece542ac27f37f994fa6b727c7010ea467819bcf7e652d31b1e360d1" )
  ; ( "lib/agent_server/developer_queued_issuer.ml"
    , "d7a52e5cd46ff8da9f8c7143b9cd0609a97ed199671756d5a8b4c4014e06bce8" )
  ; ( "lib/agent_server/developer_agent_source_runtime.ml"
    , "9a4e50dd8f52dee6f5b9171f416d01078f79c257e563e66d1ad1623493b6f7e3" )
  ; ( "lib/agent_server/developer_live_resources.ml"
    , "7dcdb1c031b7fb173a5e55ea7590645725b3904550d6aac056599b580a9b6315" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "e7931b4f548e321069c442029d71c6cca47db54af468d7d30957ae6cdbd4a909" )
  ; ( "lib/agent_server/developer_native_executor.ml"
    , "db60849c0b914c023a522a86160f0b7029b3ce3b51cdb589c13514d1770bd55e" )
  ; ( "lib/agent_server/developer_root_owner.ml"
    , "c3db34e908d7379648d5d902207978e28b8f8ada828585b613f55a2dd1092d5b" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_server/developer_script_job_service.ml"
    , "79ab45f6fd273e4f55060607c7635b52e19e724b8ff0bf0356d4c328024db6d6" )
  ; ( "lib/agent_server/developer_host.ml"
    , "adfaea98f5408ec991c4bcb4788444fcbef39556f95da37c35319b2821354c70" )
  ; ( "lib/agent_server/schedule_scheduler.ml"
    , "37cf371632d8ad86693c162e0593d56866f76df577dceb06ddd2ac5fe5bfdae9" )
  ; ( "lib/agent_session/background_execution.ml"
    , "2149c979ce81c6c851c8d8cbe749c622334f3c43d05b72b17ae89873e5d563c3" )
  ; ( "lib/agent_session/captured_agent_runner.ml"
    , "de47f87c561f5470e4a5c9fd49632228f91d3e005d138f2c6c71a1e0eee4b0c4" )
  ; ( "lib/agent_session/selected_agent_depth.ml"
    , "a5c52ff1391956f08af2cdb1651db530ab66abea394d7a7599885bcfc778c412" )
  ; ( "lib/agent_session/developer_child_preflight.ml"
    , "09658d83545bb026fb6eb3d3d30dc8f7b111fccac24960c8d0ca8a29c8b7266a" )
  ; ( "lib/chat_response/converter.ml"
    , "c6cdff248b4433655daa72064ce42e90d9522a7e2a632b78f555f86961bc0b0e" )
  ; ( "lib/agent_store/developer_run_document.ml"
    , "7274f5aa4214a9fe6fd16614e6f9163d2aeb36b72e12160adb4389f45566fcf7" )
  ; ( "lib/chatml_repl_execution/chatml_repl_execution.ml"
    , "f37aaf35200ba627a2f23867059723555afec06f11e632bf101ffd913cc497da" )
  ; ( "lib/agent_store/developer_command_row.ml"
    , "c7fe85fb222c80450fdc512732c9ad9fc2fd8ab8e12e65eeb64302e4904a5809" )
  ; ( "lib/agent_server/authoring_source_host.ml"
    , "eb5183c8659d25c59a480875548ef2e1b038126786266b58b5ad88b241e86035" )
  ; ( "lib/agent_server/authoring_context_owner.ml"
    , "ddd01bf4bbeb53c562bce7f6935acaf990ca1031c62818d277c9282a674e0df7" )
  ; ( "lib/agent_server/developer_capture_host.ml"
    , "7212e5fb9fd8025df3fda921a2e1ba28c266ffbe53c614b8bf6a873847b7c0f6" )
  ; ( "lib/agent_server/developer_root_source.ml"
    , "052785a90fef793261bdcdabcf5f5067269ae321d65e1fb5169952d4974fd16a" )
  ; ( "lib/agent_server/developer_repl_epoch.ml"
    , "c15bc93776420d6a965e159dab3014f962dbf396eb3ca37f48b079e3786efb2f" )
  ; ( "lib/agent_server/developer_repl_projection.ml"
    , "d8240d92b432b2d40c4401e08c53b1c16c3445cbadd0f005a1cf59ae2da2657b" )
  ; ( "lib/agent_server/developer_repl_task_original.ml"
    , "97033f759efa9952eaea486c795d7e2092a985818c424cebe36a03d219ac9089" )
  ; ( "lib/agent_server/developer_repl_task_execution.ml"
    , "846762edbf1c35652dc6aeca24899f3fa5219aa3000b7673087588522c815133" )
  ; ( "lib/agent_server/developer_repl_task_receipt.ml"
    , "209e79b8ba0f3ca9a10a8cdbc86186761aa45fb1f42a35ca1c5af47fde08b655" )
  ; ( "lib/agent_server/developer_command_budgets.ml"
    , "870e5dde56c8a23d0da7ef2ec24d8f223efe7883f217d113b482a24b4e9cf6ab" )
  ; ( "lib/agent_store/developer_run_store.ml"
    , "2f9c2cfbb4ff17ae7ea0b38cf51014279738640ba86af788ae9752dab7940af0" )
  ; ( "lib/agent_session/background_job_event.ml"
    , "5f5a20065fb9652005e4aadeaea85d01f271aff9674a71a318bc9cd6c34e0692" )
  ; ( "lib/agent_session/external_ingress.ml"
    , "a86eff0d9e1389bd1b5bc5df9bd6688965b18fd49432463133e6782d3d5e8e1f" )
  ; ( "lib/agent_session/moderator_tool_dispatch.ml"
    , "0a5bcab978a0040322f288676a8e784d7c5ea95253b2dfe38ce8121abfae27f0" )
  ; ( "lib/agent_session/notification_delivery.ml"
    , "9f18c6eb7561b0afda29cd706214ddf2a376c54b9f28980ce536079ae5b7b45e" )
  ; ( "lib/agent_session/notification_history.ml"
    , "d15c7a52b7b90b283e9beae7f3be36201c41bcbb4ca0402699b450787e2c3403" )
  ; ( "lib/agent_session/notification_readiness.ml"
    , "bd4c4f73cb75daa367a43532591993c92877b159eb2dfe392b751afa281c483d" )
  ; ( "lib/agent_session/script_ingress_service.ml"
    , "0aaa0f00f9fe546c442cc7dcf1bf513d87797e9beea516222f08d17ef2f7a097" )
  ; ( "lib/agent_session/script_job_kernel.ml"
    , "a9818d737cced5b3f62b213aad988617813acd6c8b030499d74b0e08c199f804" )
  ; ( "lib/agent_session/script_job_request.ml"
    , "9fdedd1ec520d69cb2bd7d5d32639bcc84d6c0d2dad1835513d7c7feaaf0d9d7" )
  ; ( "lib/agent_session/script_job_service.ml"
    , "c716bf64a8834746b818afa567fbf515cbd66947d7172494287fcc5f0b339d79" )
  ; ( "lib/agent_session/script_notification_service.ml"
    , "ca17dedcd13940b10e7b530343d61d2cfc8b681cee2d8b03e02498c68644a781" )
  ; ( "lib/agent_session/script_schedule_service.ml"
    , "9af44ed67062da4b8fa546c35d57a9aa627c24775f4d763946aed8ae84e7b58b" )
  ; ( "lib/agent_session/script_subscription_service.ml"
    , "54c775836b551733687e907ff46a2a7a0cc26bd46ff915cc6fd5f0cf5116f069" )
  ; ( "lib/agent_session/session_actor.ml"
    , "714a16a81a32a77e8f52614509b8ff82b20ccdbce524317859d5b5ab1b30c880" )
  ; ( "lib/agent_session/staged_jobs.ml"
    , "7731a0fc9f021188e3856041fe493f3044556bc7a15364815c47299fe968603b" )
  ; ( "lib/agent_session/staged_notifications.ml"
    , "8cd8537e4aa24fe231cab7d2b454e67fbf0cde3a4a20d392206f32cf1f8db14a" )
  ; ( "lib/agent_session/staged_subscriptions.ml"
    , "9c48fbd27c5c8889bff5094106e49a77c0f3cf328023370c9e52877421216abc" )
  ; ( "lib/chat_response/background_delivery.ml"
    , "057c3b5ab886ae06b403dbec7f3c03c057e414504d74c7e419216809cc69de25" )
  ; ( "lib/chat_response/background_job_operations.ml"
    , "86e16c6df7d1eafcb41cb8b558318f7f5b310f9b356afc2412f0e0a32055fcb5" )
  ; ( "lib/chat_response/background_request.ml"
    , "4c1a46dd36de612779ed0328774d6888d47bbab58495a80da253ccac7bbb83c5" )
  ; ( "lib/chat_response/ingress_operations.ml"
    , "d8e2813f75f67ca4c4d3ee6fc264a9731889538987a53d1f80ff27c2c399c825" )
  ; ( "lib/chat_response/moderator_invocation.ml"
    , "445f7e4a6597b27d3af34905fee56441bcf82285fd52eb8d9f0c400379b28deb" )
  ; ( "lib/chat_response/moderator_manager.ml"
    , "85bdbac427071ad2ed53220b8436ee7612a61cd91b2692808e6e8b720a81d63a" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "ed072efe4420b0301cb7e3e9c42230f67bfa09ab42e9e067fa26d1320fec901b" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "f8d63210438cadd286834a77bb62eaa371cc4f94aea08751aa84a87bc49333f0" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ; ( "lib/chat_response/startup_compilation.ml"
    , "0f60a1f036408809e5618f3310fbc2213e9f7fe99788fb785951bd31c7b96bb1" )
  ; ( "lib/agent_session/runtime_script_preparation.ml"
    , "27d9b620bf6cfa39a7aa42ca700b4095125bf596bd63b06de79f4f161c8edba4" )
  ; ( "lib/chat_response/script_contract_plan.ml"
    , "05eccbbde26eb960d6d6b6ce89a3a5b4cad786e3734c7ff9526c25fb9d059dcd" )
  ; ( "lib/chat_response/restored_programs.ml"
    , "329f2747454cc915a6ac3713fbffadc46d2d71b925167a3be68c0aefa9617fdb" )
  ; ( "lib/agent_server/developer_historical_source.ml"
    , "57397ec74cd606e528c2c5954fe150decc65834cee02504bd3e3b67e3ccce0c5" )
  ; ( "lib/agent_server/authoring_context_service_core.ml"
    , "7c58f58bdcd48344d17a1209a0431d7d80616c4688db6f9ec42411b6d7bc9061" )
  ; ( "lib/agent_server/authoring_context_service.ml"
    , "514a11a719d6156fcda7bb5c71348a17b45c3863ab5f966baa5053c93d29213c" )
  ; ( "lib/agent_server/authoring_host_policy.ml"
    , "b48f6f6c86b47207bf134b535edf388a44fa4d9a7bbf59953b4b91a7f1bde30c" )
  ; ( "lib/agent_server/authoring_upload_host.ml"
    , "8774074a3192360392904b79b9e9101d05a6f1da03a63653be7bb8aeefa61a57" )
  ; ( "lib/agent_server/authoring_configured_upload_policy.ml"
    , "eba2c3c6d6bdf9e52b907e0eecee174aaa3a33a3c47e3bb8dfbb89ad7986d2b1" )
  ; ( "lib/agent_server/authoring_upload_validation.ml"
    , "e493028079cd44dc8513f5f03656da3fb3dc0f3bba5c22b51334b49bb7f7c6c0" )
  ; ( "lib/agent_store/authoring_upload_store.ml"
    , "c71ff246dcfcc411df0419b86de3b127599a80e20397a9915fbce54b97f6fa4b" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.task-effects"
    , "4a4f9617b016c491dfd397876bbb32d01e95f727064471e2a4432dca0478a17e" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "26ab23b3bcf104bdd0c5e34e6123c552f4bc1adf38a16bfca7cf9696c5077d02" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "b7f328cc4186ccde448fc23cc6158c198e81fd55cf493efa8c812889a51ee0c3" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "d12925247f47d7caef5cf2abe69229efe2625413de4beb44e2f20783a17c890a" )
  ; ( "one_off_v1"
    , "runtime.lifecycle"
    , "aa5c35f1df416a412336ed3955111b0473ee852ec5b38a3478e5e11114967ec2" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "825fb66844cb93487475aa7cd09f5b62450ff0e91f05fb40fe6bb94679dd77bb" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "23048f4a5d502a0dc68d1f03e54cf6836f46c0dfb8ad7e9c562abd38dc55cb1e" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "84501aa3a0ff67f82c25b8f0e76c32748a23296e0af09641b68f375a4c7ea7d0" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "2d8acea7732297ef2dc50ffd7eebd90ee3ddc01d3c1835b677a93b50f90b6d87" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "29fd463ae32f984f54ee9f7544cb0228e115ec3b5c8e50b1530d228045609d34" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "32cda996387de074947768209c753e163bbc385696d95a6d0feb5c4572746ca2" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "956c182bcd21fd795755593e140838b7f8a53d2a981b6dddabeb61a3c6c9469d" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "6bd3425b03a3b00a0006738da0d0ba3c889cc7c5c2a7be64da471a055df090cd" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "30caaff44d38ee236ad0fd599fd9d5f74ba8bea81963284d6d3ed0570152c1ee" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "3f0225a862b0a3aa8b58f91c19c47b6803708a3465789ccc68b97db1ed233166" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "fd10abbf78b538aae89594155a079e855abcb385ac0c56a5e1a12c9084905a96" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "6b4461dce7279150ab140fc6fb822fed23d371bf3677cfd5dc329b19a0c1bdd6" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "c7d6da737ca0946d186e4509b68d8087fe38b9a4aedb5b31856c994ffc8ebd87" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "e9499f66de65bda3c77c4b4571580b0324b9c71e02b0cf279da62e5aa860ed03" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "08ded5d15bbbd337b58a852482b212424aa0308182d67332d2bcd0df84bc8efe" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "0a4fe41bc282fe0904a77ccc1713f088ea6c38b8f2077ff7058bcec9a242e1c5" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "7865cc67eeed4a87a5d88a45668ac0f01023a1472d210ec9a2ff36dabc7edd05" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "0d8a24607b79cbebf7d8d1f04a55f97e4bce640bd5a59cfeca0e712cf8e991f3" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "8eaacf1eb39f069ad75f6a391bc3a32cefc0de89c728a0501501fb601232e58f" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "82014d0ba666ca8a8bcdafea5e82c5ba494bd22c75e418748894be56178d1fd9" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "41ef06703225472b5df73e7a5a706d50bba873d6270c514138d9ed2d9a4a5eb0" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "8cf1f82c57131e8d10a5d7335de68a76802320c07395a414b243e372c68d1658" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "52b0187bad6552d64b6ac3a84b573a6d2615bf23adef72240d38a638af7f015f" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "f28c477076da3747b6e3b738c17c8032fa536172fb73f15404cc7537693eb0c5" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "2ea0ccbeb5b8e8cf74e577eea85bc1893a30e65eeb0d1de9f6a12fd4de69ec82" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "1de47c230a2d1f578a6ef1d076094e56a8cf9364f93797250a6bd748920e3386" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "9e61a30bba535ce86428f99a0fe6e6e11b50d82ecd3dc2216042145c0d79a263" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "20c4d086321e1718dce841522c58f89ddf59d7f2b7104eb083a31d039a00920c" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "055fc7697f6373a87bbc781087b8a7952297b2cdf4dff753d4c050ab8a0d685f" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "275a9f78a16234c4d78c974304802cf223ec120352975adaf7f4f9317e1e3682" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "b048fe21c82a2f1c3d22ddd97663314c318e1aade7a285e3d4ab9ac979a7a56c" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "59307a6598c75266bb6f0709bbc0fb444d0a4a267f1fb9956ab1f33e73172f8d" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "cae5e7b9ef9cfc91a0c85eb9fdf85eaaf5ab452521cb15bb40a52e1c19c3ae3f" )
  ]
;;
