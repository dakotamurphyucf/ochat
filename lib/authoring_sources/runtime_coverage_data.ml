(* Reviewed runtime transactions and durable work semantics. Source pins detect
   implementation drift, not automatically discovered features. Evidence refers
   to separately executed behavior suites. Surface mappings describe readable
   contracts, never an authority grant. *)
let shared_features =
  [ ( "instructions.request_context"
    , "Request-only User guidance under the exact agent owner; authored higher-priority \
       instructions and canonical history remain unchanged"
    , [ "lib/chat_response/agent_guidance.ml"
      ; "lib/agent_session/instruction_guidance_text.ml"
      ; "lib/chat_response/ctx.ml"
      ; "lib/chat_response/driver.ml"
      ; "lib/chat_response/model_executor.ml"
      ; "lib/chat_response/in_memory_stream.ml"
      ; "lib/agent_session/runtime_builder.ml"
      ; "lib/agent_session/captured_agent_runner.ml"
      ; "lib/agent_session/script_tool_calls.ml"
      ]
    , "instructions.sources"
    , [ "test/chatml_composition/instruction_runtime_tests.ml"
      ; "test/inference/guidance_tests.ml"
      ] )
  ; ( "instructions.continuation_and_clear"
    , "ACK-owned lifecycle and completion handoffs, explicit compactor guidance \
       reservation and actual writer-profile Clear admission"
    , [ "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/execution_custody.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/context_compaction/compactor.ml"
      ; "lib/agent_session/instruction_clear_capacity.ml"
      ; "lib/agent_session/session_transition.ml"
      ; "lib/agent_session/session_terminal_journal.ml"
      ]
    , "instructions.sources"
    , [ "test/agent_session/agent_authority_tests.ml"
      ; "test/agent_session/instruction_selection_persistence_tests.ml"
      ; "test/agent_session/instruction_clear_capacity_tests.ml"
      ; "test/chatml_composition/instruction_runtime_tests.ml"
      ] )
  ; ( "lifecycle.history"
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
  ; ( "lifecycle.legacy_checkpoint_rebuild"
    , "Exact captured Single identity recognition, unloaded administrative recovery and \
       explicit archived rebuild; migration-blocked queued work is not claimed"
    , [ "lib/agent_session/legacy_moderator_checkpoint.ml"
      ; "lib/agent_session/session_state.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/job_scheduler.ml"
      ; "lib/chat_response/moderator_manager.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/legacy_source_identity/classifier_tests.ml"
      ; "test/agent_administration/legacy_source_recovery_tests.ml"
      ] )
  ; ( "lifecycle.stop"
    , "Graceful and cancel dispositions, joined cleanup and independent child lifetimes"
    , [ "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/actor_reply.ml"
      ; "lib/agent_session/background_execution.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_server/delegation_lifecycle.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/agent_session/owner_termination_tests.ml"
      ; "test/agent_session/extension_stop_tests.ml"
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
      ; "lib/agent_protocol/developer_repl_wire_completion.ml"
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
      ; "lib/chatml_repl_execution/value_preview.ml"
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
      ; "lib/agent_session/job_custody.ml"
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
      ; "lib/agent_session/legacy_callback_runtime.ml"
      ; "lib/agent_session/constructor_permission_checkpoint.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/execution_custody.ml"
      ; "lib/agent_session/legacy_queue_custody.ml"
      ; "lib/agent_session/legacy_queue_checkpoint.ml"
      ; "lib/agent_session/legacy_schedule_custody.ml"
      ; "lib/agent_session/moderator_source_installation.ml"
      ]
    , "runtime.invocations.moderator"
    , [ "test/agent_session/event_delivery_tests.ml"
      ; "test/chatml_composition/moderator_pending_tests.ml"
      ; "test/factory_catalog_publication/creation_tests.ml"
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
  [ ( "lib/agent_session/actor_reply.ml"
    , "00e0abeb3120d71ddd4fc0dc109b0d584da3ba8b771f6d0c02aad525df984fb4" )
  ; ( "lib/agent_session/execution_custody.ml"
    , "8d64b47d0385c80013963ffdf886519fe04c3058d543a79b6d9934f4573d5133" )
  ; ( "lib/agent_session/legacy_queue_custody.ml"
    , "f1d7d4892f9ee3e237d6bdc924c231f1664fa176d63158377644f4ab5fd3c48f" )
  ; ( "lib/agent_session/legacy_queue_checkpoint.ml"
    , "1dd6ea7f4bb9cd85ec569493a6f53c92484c680890da475eff0ea592d5da2922" )
  ; ( "lib/agent_session/legacy_schedule_custody.ml"
    , "f9c7f73ae0f24cc5c3cb3097813a7a76fd77d7c9eb87f80a1a8fa64d5d6a148f" )
  ; ( "lib/agent_session/moderator_source_installation.ml"
    , "1e1595ffe97a6e23fd8456e5e891c256fb606d1b2c0b68940e9a2ac465d9483d" )
  ; ( "lib/agent_session/legacy_moderator_checkpoint.ml"
    , "e541ee7532b2a3e96d7c06c539cd1637c0ddaa1c55d7f1c51cec24b6d9c4f399" )
  ; ( "lib/agent_session/session_state.ml"
    , "4c3007f2766732f41925ebb62c26011d3174d46cf192c53bf0b25b72f49babe5" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "1ef19969a6cdf75a8c91cd16d3b1a88aadc222b51886cadc514e8ff23731dc9f" )
  ; ( "lib/chat_response/agent_guidance.ml"
    , "5f00cdc8f90b5819dd8e079c8c201ec7ca790af59045b82a4c874315ad97be4b" )
  ; ( "lib/agent_session/instruction_guidance_text.ml"
    , "f479e72bd27d60883647524052eb04770198f07603b2d738633130ece31c4861" )
  ; ( "lib/chat_response/in_memory_stream.ml"
    , "e246a224d598a61fc78d93197cdf3e933fa41539bb37bc1d68471eb55573f145" )
  ; ( "lib/agent_session/constructor_permission_checkpoint.ml"
    , "f1d765c7b5bd1a488e360d2b27cc93e58503478e69d9e7f1a0d61e00751ebe5f" )
  ; ( "lib/agent_session/legacy_callback_runtime.ml"
    , "44c5f795ade0b2ea0b0c4fb8fe91a9b8ab2f70084c0ab8410782fd52660753c8" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "17d9dc85183044ab552c05c54f1d45e572358a114d76e16ca3da659fd5cc6db3" )
  ; ( "lib/context_compaction/compactor.ml"
    , "7b11eae5ce4d929bf1ab4702111d03f70fe97cfe8543135c73782c2b92391ff0" )
  ; ( "lib/agent_session/session_transition.ml"
    , "4ecbf130526447ffbb4e36604f436a8bc3d3bd2d3623fd7eac0c5b7f08e26540" )
  ; ( "lib/agent_session/instruction_clear_capacity.ml"
    , "baa952a408f74f0795059df9e7f6bb3c677957c1866c6d2b20cb83fc7a475e0e" )
  ; ( "lib/agent_session/session_terminal_journal.ml"
    , "df1c2f60f413a5fe01dfe04a20eb91a869e6d157becfee8ec0171dc8a6573f33" )
  ; ( "lib/agent_client/developer_trace_connection.ml"
    , "a737c2bc921e42546ae3ae8165eae078a409861563acbb61782db870dcf4adf6" )
  ; ( "lib/agent_client/connection.ml"
    , "33b6ec7ea16d65b4d5d44580b423276008065be644b810968ab81174d11c492e" )
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
    , "bf10291391f6d9b16cc04f78ae8d8e976be19fe16607469c406174839a683bd4" )
  ; ( "lib/agent_server/embedded_local_template.ml"
    , "93691f37b0b11d3b5ef3303d2d6f35d7bdffe2ea2eb360db9d007ab841210e3e" )
  ; ( "lib/agent_server/embedded.ml"
    , "70a039a5ba5ef1e19158d117ce3ffc2392051934f5a82d6e9633fcfa9319ed59" )
  ; ( "bin/ochat_agent_stdio.ml"
    , "8d134cd11c7df2a60e6b4b77a5ff7f9ea0c28a8a2674a3dd9f573adaa7623902" )
  ; ( "lib/agent_server/session_factory.ml"
    , "951eb03a5f45839105aa789c19b06f39c8bbe59614c206d410a62a323d45f479" )
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
    , "cdab9fc4fe9da8ce81986c5b5842f1bea65d2205c054fbdb8057d5c244045577" )
  ; ( "lib/agent_protocol/developer_repl_wire_result.ml"
    , "d6b1c58696a2ca6c4d386ebadc87740c1377bb93808954e77bfffef4e1e5b795" )
  ; ( "lib/agent_protocol/developer_repl_wire_task.ml"
    , "d0cf80397174e6b8487b1b139c7e116640bcb331d8f176bf0979c621a2bbcf7c" )
  ; ( "lib/agent_protocol/developer_repl_wire.ml"
    , "291081bead9338010a064cb01431997e709c532c2f3b4d467e5991b6f1ee6682" )
  ; ( "lib/agent_protocol/developer_repl_wire_completion.ml"
    , "1ecd8d646ce259785b4af397230be73e0c7afed5dbfec166fe7804151f63295e" )
  ; ( "lib/agent_protocol/developer_trace_wire.ml"
    , "962b2f2869c7d7148f5331b584bc5d44558380fd0c99441d3a1f4fc8eb9cf98b" )
  ; ( "lib/agent_server/developer_trace_page.ml"
    , "824dda3ea7b8d177f9916e2ed99a52328ee7f84e7a40c189b4b8a66ce2f50c6e" )
  ; ( "lib/agent_server/developer_command_wire_receipt.ml"
    , "5d30a8c37696290d8b1a1201855f763fdd39a7506a265232c11089179d625c1b" )
  ; ( "lib/agent_server/developer_endpoint.ml"
    , "1738ec6aafa29777797a562e45a0545e7117195272fbc5e58e5cb6265a698087" )
  ; ( "lib/agent_server/developer_repl_registry.ml"
    , "98b4d64dfb527512faa03099a2971a8f0bac733f7eb9eb6d6c704e335673e6f0" )
  ; ( "lib/agent_server/developer_portable_admission.ml"
    , "ff4357ffeec4ff2088d8d06f209eb330d70a628bd16e15784e45391ffac3f265" )
  ; ( "lib/agent_server/developer_change_hub.ml"
    , "6c034e4b3e9c6010aa9596d468b753f28cee092b01393bea645668d1984f58fc" )
  ; ( "lib/agent_server/dispatcher.ml"
    , "e4602c092077367f6fb0c95d176ec9eef22f0ee2cbc24b5bbf536b41975daa86" )
  ; ( "lib/agent_server/authoring_installation.ml"
    , "c153b07c85dae5395d4a5a4e30a0d430dcf922c23d29cc3f302a70c490b4a94b" )
  ; ( "lib/agent_server/connection_context.ml"
    , "772410fd0d32bc57ae9ee1f303f3d2a1b293143251fe419c99e141e8921dabba" )
  ; ( "lib/agent_server/startup_cleanup.ml"
    , "97e4fb957609ddc6a32c93efce60769f379227b9708555599042de5153e02ad7" )
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
    , "db1c4ff75f729a9cd83618fe5fb8de7dc6ea57d70a47a8b8ed3a7a7ffe19bb3d" )
  ; ( "lib/agent_session/developer_claimed_resource_owner.ml"
    , "fb251c6e022f9880820b15807018adc5cf240500050a0424321d7507d3e2bc41" )
  ; ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_session/claimed_child_runtime.ml"
    , "f5ab8751939aa924659c11e022719444ee8457d818631db8151abf0dc7ab22e6" )
  ; ( "lib/agent_protocol/model_spawn_request.ml"
    , "258c00854df5cd7a65eb28d8fccc4ab1e1d1880ccbbc15999306fd448322d160" )
  ; ( "lib/chat_response/ctx.ml"
    , "eded87978f564f53f88c9f2831f58c6eb502eda5fd62f980753b37c8bf99a52f" )
  ; ( "lib/chat_response/driver.ml"
    , "e2aeffb6a2d299213ff3ead090663a714ae33b2db8d09c14e04803c514fb6930" )
  ; ( "lib/chat_response/model_executor.ml"
    , "c7d62b3b14d60a2d398ab0171e525245987b78fe48db25caa819e500b5d71cbe" )
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
    , "798836e32d154f322a0fdd0760d1348956515c3887b65e4b94b5c83c61bdb7e3" )
  ; ( "lib/agent_server/command_handler.ml"
    , "9c0c8f4815a4ae12c2879e291b9b8aeead2c88bc51fa44c987c319f4143160fb" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "d5f9ad2f17a76b1708fa2bb25f501db04e853d1cdc680e44a1e93823d163be19" )
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
    , "7c2757e15bf13ca1dcc082d3a4a63add2238e4e6bc59b7617529ee646abdfa55" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "a5447544cbe1cd55302cd70a3c50bf7f6b0ec07ad3af8ce1f70b7b90dfbd07bc" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "fef8098605682e45b5eea681178cd65227e88521c2b95f811cc5e5193d0c4302" )
  ; ( "lib/agent_server/job_scheduler_developer.ml"
    , "18062ce3137c3b1782fb4015657ca304a591341a33e1d1da47d362b137b7fd19" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "9450ea610c424f91480b70ca733226137fa0b18325f07bf789da3e582e8d1876" )
  ; ( "lib/agent_server/developer_child_agent_runner.ml"
    , "09bb4a77257188b362bcf474d367e17bf44fc786e0761cd894c2cb5c72158573" )
  ; ( "lib/agent_server/developer_child_content_projection.ml"
    , "17dbb6bc0891ad5bb5c3879a2b1e946c05a01d13db32a15d220faa531cb82ba8" )
  ; ( "lib/agent_server/developer_child_selection.ml"
    , "615ce2d2ece542ac27f37f994fa6b727c7010ea467819bcf7e652d31b1e360d1" )
  ; ( "lib/agent_server/developer_queued_issuer.ml"
    , "d7a52e5cd46ff8da9f8c7143b9cd0609a97ed199671756d5a8b4c4014e06bce8" )
  ; ( "lib/agent_server/developer_agent_source_runtime.ml"
    , "9a4e50dd8f52dee6f5b9171f416d01078f79c257e563e66d1ad1623493b6f7e3" )
  ; ( "lib/agent_server/developer_live_resources.ml"
    , "cb88d63f805aaff22b8c4aec2310b66ef8c85ecce3d2017309f35de1d42584b6" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "e7931b4f548e321069c442029d71c6cca47db54af468d7d30957ae6cdbd4a909" )
  ; ( "lib/agent_server/developer_native_executor.ml"
    , "db60849c0b914c023a522a86160f0b7029b3ce3b51cdb589c13514d1770bd55e" )
  ; ( "lib/agent_server/developer_root_owner.ml"
    , "a976f19949607a693fcf119c3e8cad98669bb2f3016f67864de903af9e62b722" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_server/developer_script_job_service.ml"
    , "79ab45f6fd273e4f55060607c7635b52e19e724b8ff0bf0356d4c328024db6d6" )
  ; ( "lib/agent_server/developer_host.ml"
    , "45be3610abdb3090ee3f0929b5317dc978af1020ae4cffdca2268b66fb395bf1" )
  ; ( "lib/agent_server/schedule_scheduler.ml"
    , "f6c3ed44cccf5a39f71b5eef052ee5e185f4fc066f7d53239033a915f28fff5a" )
  ; ( "lib/agent_session/background_execution.ml"
    , "33996f6b8a1165fc7e992c5c019a2db478b67b1fe6ddb3fed34fa8b731454254" )
  ; ( "lib/agent_session/captured_agent_runner.ml"
    , "02221a990c09bc62411c110600970c92382370ca59b68141927e35a111976d13" )
  ; ( "lib/agent_session/selected_agent_depth.ml"
    , "a5c52ff1391956f08af2cdb1651db530ab66abea394d7a7599885bcfc778c412" )
  ; ( "lib/agent_session/developer_child_preflight.ml"
    , "09658d83545bb026fb6eb3d3d30dc8f7b111fccac24960c8d0ca8a29c8b7266a" )
  ; ( "lib/chat_response/converter.ml"
    , "c6cdff248b4433655daa72064ce42e90d9522a7e2a632b78f555f86961bc0b0e" )
  ; ( "lib/agent_store/developer_run_document.ml"
    , "7274f5aa4214a9fe6fd16614e6f9163d2aeb36b72e12160adb4389f45566fcf7" )
  ; ( "lib/chatml_repl_execution/chatml_repl_execution.ml"
    , "74028e61ebe6af6583c707a97eae3bd553516113db1d6c66661bf621c1ece864" )
  ; ( "lib/chatml_repl_execution/value_preview.ml"
    , "d5965d27fec2766036cb245e47be35bf31fa20c06985e78b9418b0403399297c" )
  ; ( "lib/agent_store/developer_command_row.ml"
    , "c7fe85fb222c80450fdc512732c9ad9fc2fd8ab8e12e65eeb64302e4904a5809" )
  ; ( "lib/agent_server/authoring_source_host.ml"
    , "52567ed5c8b6cdb56119581f8bebb943dffcc28475cf54d86376edbad9ab0f41" )
  ; ( "lib/agent_server/authoring_context_owner.ml"
    , "ddd01bf4bbeb53c562bce7f6935acaf990ca1031c62818d277c9282a674e0df7" )
  ; ( "lib/agent_server/developer_capture_host.ml"
    , "7212e5fb9fd8025df3fda921a2e1ba28c266ffbe53c614b8bf6a873847b7c0f6" )
  ; ( "lib/agent_server/developer_root_source.ml"
    , "052785a90fef793261bdcdabcf5f5067269ae321d65e1fb5169952d4974fd16a" )
  ; ( "lib/agent_server/developer_repl_epoch.ml"
    , "bd164d1feb03e20a37174c40be4a92ad161f6196af55ab2e9c9fa316c308236d" )
  ; ( "lib/agent_server/developer_repl_projection.ml"
    , "dd2b6a5002d1fd535a67fd869dd1cf2c46947b1c884ef733178a619abcc407ff" )
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
    , "a214385ca0285384bc8447ac819b4e49ee373faddaafafd6446e457480cb421b" )
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
    , "42901e69f8855fcd3767b493f13993d57567b4d5e6ca5a17a947770d962e431e" )
  ; ( "lib/agent_session/job_custody.ml"
    , "3f3a132b6fd32f11eba332ec3ae9f6ff4dbfb771dd542ab12cab5a1316e8f3b3" )
  ; ( "lib/agent_session/staged_jobs.ml"
    , "5acc00996e4588a634811ede92cfc32187877bf1c938aaa8e3bf8bdd163c5833" )
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
    , "d303444ce708c366136a1f0d1d26c9ffb960b00f90c3160b32aaba2441811fb9" )
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
    , "a78731095144187a36d482b808ac48c62cfdbb226715ff0ce9858e911ff33534" )
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
    , "instructions.sources"
    , "6c01d31beec1423681006f2320554f8f09c4163b27a8633bd2d8c2fa602a4852" )
  ; ( "tool_v1"
    , "instructions.sources"
    , "66d91bb62409d861fb5a355506c12db41be038092317e625c57c608da95533f0" )
  ; ( "moderator_v1"
    , "instructions.sources"
    , "552553a84efd975cfe41652872c94ea6c06db57593d050795fc8a31f6a33a070" )
  ; ( "delegated_moderator_v1"
    , "instructions.sources"
    , "854af913368900b0355a1dec3b19e230a1a917fd1224b201ce096f85f471efa8" )
  ; ( "one_off_v1"
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
    , "72164c5c3adfc5b77901d5dd10903c952ffdc32a1bba4e50f7a689b01a48cb69" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "12c40c04cb8f0dbf7b54c44e0194109f2852bf277764440ac8819143df9a4773" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "9cbc54b85c8bf9c8bc0bac6f9a7b0df2e6698eab0002802967cc8f7e05c6bdee" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "6a8c4afc635969a4a6de9e497120cc65bb35b24bec10063d4a514e50461b9088" )
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
