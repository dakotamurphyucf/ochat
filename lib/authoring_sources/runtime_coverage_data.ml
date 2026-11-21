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
      ; "lib/agent_session/writer_attachment.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_session/initial_lifecycle_startup.ml"
      ; "lib/agent_session/execution_custody.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ; "lib/context_compaction/compactor.ml"
      ; "lib/agent_session/instruction_clear_capacity.ml"
      ; "lib/agent_session/session_transition.ml"
      ; "lib/agent_session/session_terminal_journal.ml"
      ]
    , "instructions.sources"
    , [ "test/agent_session/agent_authority_tests.ml"
      ; "test/factory_catalog_publication/goal_native_start_tests.ml"
      ; "test/agent_session/instruction_selection_persistence_tests.ml"
      ; "test/agent_session/instruction_clear_capacity_tests.ml"
      ; "test/chatml_composition/instruction_runtime_tests.ml"
      ] )
  ; ( "lifecycle.history"
    , "Compaction retains work; generation reset retires live authority and preserves \
       receipt identity"
    , [ "lib/agent_session/administration.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_session/turn_admission_staging.ml"
      ; "lib/agent_session/managed_submission_tracking.ml"
      ; "lib/agent_server/managed_output_cursor.ml"
      ; "lib/agent_session/notification_delivery.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/chatml_composition/notification_administration_tests.ml"
      ; "test/agent_session/notification_compaction_tests.ml"
      ; "test/agent_session/managed_submission_recovery_tests.ml"
      ; "test/agent_session/managed_output_cursor_tests.ml"
      ; "test/agent_session/turn_admission_staging_tests.ml"
      ] )
  ; ( "lifecycle.source_replacement"
    , "Stopped revision admission, source-owned retirement and no automatic state or \
       event migration"
    , [ "lib/agent_server/command_handler.ml"
      ; "lib/chat_response/extension_compiler.ml"
      ; "lib/chat_response/generated_admission.ml"
      ; "lib/agent_session/generated_definition.ml"
      ; "lib/agent_session/runtime_source_preparation.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_server/session_registry.ml"
      ; "lib/agent_session/runtime_unmoderated_source.ml"
      ; "lib/agent_session/ordinary_runtime_basis.ml"
      ; "lib/agent_server/session_policy_resource.ml"
      ; "lib/agent_server/constructor_inference_graph.ml"
      ; "lib/agent_server/runtime_native_scope.ml"
      ; "lib/agent_server/native_cleanup_observation.ml"
      ; "lib/agent_server/native_cleanup_observation.mli"
      ; "lib/agent_server/native_resource_lifetime.ml"
      ; "lib/agent_server/runtime_resource_testing.ml"
      ; "lib/agent_server/runtime_resource_testing.mli"
      ; "lib/agent_server/independent_cleanup.ml"
      ; "lib/agent_server/independent_resources.ml"
      ; "lib/agent_server/independent_eligibility.ml"
      ; "lib/agent_server/independent_eligibility.mli"
      ; "lib/agent_session/runtime_close.ml"
      ; "lib/agent_session/runtime_close.mli"
      ; "lib/agent_server/ancestry_cleanup.ml"
      ; "lib/agent_server/ancestry_cleanup.mli"
      ; "lib/agent_session/preparation_tracking.ml"
      ; "lib/agent_session/delegated_runtime_load.ml"
      ; "lib/chat_response/background_delivery.ml"
      ; "lib/agent_session/script_schedule_service.ml"
      ; "lib/agent_session/external_ingress.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/chatml_composition/moderator_upgrade_tests.ml"
      ; "test/chatml_composition/background_upgrade_tests.ml"
      ; "test/chatml_composition/timer_upgrade_tests.ml"
      ; "test/factory_catalog_publication/delegated_load_tests.ml"
      ; "test/factory_catalog_publication/delegated_warm_resource_tests.ml"
      ; "test/factory_catalog_publication/goal_resume_runtime_tests.ml"
      ; "test/factory_catalog_publication/policy_resource_construction_fault_tests.ml"
      ; "test/agent_session/runtime_builder_tests.ml"
      ; "test/agent_session/generated_definition_tests.ml"
      ; "test/agent_server_inherited_standalone_test.ml"
      ; "lib/agent_server/native_resource_lifetime_tests.ml"
      ; "test/agent_session/delegation_resource_tests.ml"
      ] )
  ; ( "lifecycle.legacy_checkpoint_rebuild"
    , "Exact captured Single identity recognition, unloaded administrative recovery and \
       explicit archived rebuild; migration-blocked queued work is not claimed"
    , [ "lib/agent_session/legacy_moderator_checkpoint.ml"
      ; "lib/agent_session/session_state.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_session/recurrence_activation_custody.ml"
      ; "lib/agent_session/actor_reply.ml"
      ; "lib/agent_session/background_execution.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_server/delegation_lifecycle.ml"
      ; "lib/agent_server/delegated_runtime.ml"
      ]
    , "runtime.lifecycle"
    , [ "test/agent_session/owner_termination_tests.ml"
      ; "test/agent_session/extension_stop_tests.ml"
      ; "test/agent_session/delegation_lifecycle_tests.ml"
      ; "test/agent_session/delegated_runtime_tests.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_session/extension_invariants.ml"
      ; "lib/agent_session/job_dependency.ml"
      ; "lib/agent_session/job_launch.ml"
      ; "lib/agent_session/standalone_completion_contract.ml"
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
      ; "lib/agent_store/job_result_store.ml"
      ; "lib/agent_session/script_job_service.ml"
      ; "lib/agent_session/background_execution.ml"
      ]
    , "runtime.work-values"
    , [ "test/chatml_composition/background_artifact_tests.ml"
      ; "test/agent_session/background_disclosure_tests.ml"
      ; "test/agent_session/job_artifact_retention_tests.ml"
      ] )
  ; ( "jobs.recovery"
    , "Claimed attempts, cancellation, waiting dependencies and interruption without \
       blind replay"
    , [ "lib/agent_server/job_scheduler.ml"
      ; "lib/agent_server/job_scheduler_developer.ml"
      ; "lib/agent_session/background_execution.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
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
      ; "lib/agent_protocol/inline_image.ml"
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
  [ ( "context.authored_rollover_hooks"
    , "Optional installed moderator rollover hook, exact claims and durable follow-up \
       disposition"
    , [ "lib/chatml/chatml_context_rollover.ml"
      ; "lib/chat_response/context_rollover_hook.ml"
      ; "lib/agent_session/recovery_rollover.ml"
      ; "lib/agent_session/recovery_rollover_hooks.ml"
      ; "lib/agent_session/recovery_rollover_hook_policy.ml"
      ; "lib/agent_session/recovery_rollover_hook_snapshot.ml"
      ; "lib/chat_response/moderator_manager.ml"
      ; "lib/agent_session/session_actor.ml"
      ]
    , "runtime.context_windows"
    , [ "test/agent_server_e2e/scenarios/recovery_rollover_hooks_scenario.ml"
      ; "test/agent_session/recovery_rollover_hooks_tests.ml"
      ] )
  ; ( "moderator.state_limits"
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
    , "Serialized handler state, invocation resolution, versioned Model completion \
       projection and local commit versus external effects"
    , [ "lib/agent_session/model_job_event.ml"
      ; "lib/agent_session/policy_restore_basis.ml"
      ; "lib/agent_session/policy_restore_basis.mli"
      ; "lib/agent_server/session_policy_resource.ml"
      ; "lib/agent_session/moderator_lifecycle.ml"
      ; "lib/agent_session/moderator_lifecycle.mli"
      ; "lib/agent_session/run_state.ml"
      ; "lib/agent_session/run_state.mli"
      ; "lib/agent_session/session_state.mli"
      ; "lib/agent_session/session_state.ml"
      ; "lib/agent_session/model_job_event.mli"
      ; "lib/agent_session/runtime_builder.ml"
      ; "lib/chat_response/moderator_manager.ml"
      ; "lib/chat_response/moderator_invocation.ml"
      ; "lib/agent_session/moderator_tool_dispatch.ml"
      ; "lib/agent_session/standalone_tool_dispatch.ml"
      ; "lib/agent_session/managed_moderator_dispatch.ml"
      ; "lib/agent_session/operation_worker.ml"
      ; "lib/agent_session/callback_cleanup.ml"
      ; "lib/agent_session/callback_cleanup.mli"
      ; "lib/agent_session/callback_cleanup_data.ml"
      ; "lib/agent_session/callback_cleanup_data.mli"
      ; "lib/agent_session/product_callback_data.ml"
      ; "lib/agent_session/product_callback_data.mli"
      ; "lib/agent_server/initialization_capture.ml"
      ; "lib/agent_server/initialization_capture.mli"
      ; "lib/agent_server/initialization_capture_data.ml"
      ; "lib/agent_server/initialization_capture_data.mli"
      ; "lib/agent_server/disposal_report_data.ml"
      ; "lib/agent_server/disposal_report_data.mli"
      ; "lib/agent_session/cleanup_diagnostic.ml"
      ; "lib/agent_session/cleanup_diagnostic.mli"
      ; "lib/agent_session/close_observation.ml"
      ; "lib/agent_session/close_observation.mli"
      ; "lib/agent_session/runtime_builder.mli"
      ; "lib/agent_server/session_policy_resource.mli"
      ; "lib/agent_server/original_cleanup_report.ml"
      ; "lib/agent_server/original_cleanup_report.mli"
      ; "lib/agent_server/cleanup_original_view.ml"
      ; "lib/agent_server/cleanup_original_view.mli"
      ; "lib/agent_server/session_disposal_report.ml"
      ; "lib/agent_server/session_disposal_report.mli"
      ; "lib/agent_server/callback_disposal.ml"
      ; "lib/agent_server/callback_disposal.mli"
      ; "lib/agent_server/runtime_callback_data.ml"
      ; "lib/agent_server/runtime_callback_data.mli"
      ; "lib/agent_session/delegated_policy_caller.ml"
      ; "lib/agent_session/delegated_policy_caller_custody.ml"
      ; "lib/agent_session/delegated_policy_governance_data.ml"
      ; "lib/agent_session/delegated_policy_seed.ml"
      ; "lib/agent_session/delegated_policy_claim_state.ml"
      ; "lib/agent_session/invocation_capabilities.ml"
      ; "lib/agent_session/moderator_event.ml"
      ; "lib/agent_session/moderator_observation.ml"
      ; "lib/agent_session/observation_continuation.ml"
      ; "lib/agent_session/legacy_callback_runtime.ml"
      ; "lib/agent_session/constructor_permission_checkpoint.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/inference_graph_owner.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/descendant_accounting_host.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ; "lib/agent_session/idle_completion_write.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_server/graph_tracking.ml"
      ; "lib/agent_session/runtime_unmoderated_source.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_session/execution_custody.ml"
      ; "lib/agent_session/legacy_queue_custody.ml"
      ; "lib/agent_session/legacy_queue_checkpoint.ml"
      ; "lib/agent_session/external_delivery_checkpoint.ml"
      ; "lib/agent_protocol/session_storage_semantics.ml"
      ; "lib/agent_session/session_storage_semantics.ml"
      ; "lib/agent_session/session_state_document.ml"
      ; "lib/agent_session/session_state_document.mli"
      ; "lib/agent_session/session_state_decode_evidence.ml"
      ; "lib/agent_session/session_state_decode_evidence.mli"
      ; "lib/agent_session/session_delta_document.ml"
      ; "lib/agent_session/queued_moderator_event.ml"
      ; "lib/agent_session/queue_provenance_validation.ml"
      ; "lib/agent_session/retained_job_producer.ml"
      ; "lib/agent_session/queue_producer.ml"
      ; "lib/agent_protocol/moderator_execution.ml"
      ; "lib/agent_protocol/queue_producer.ml"
      ; "lib/agent_session/session_record_shapes.ml"
      ; "lib/agent_session/legacy_schedule_custody.ml"
      ; "lib/agent_session/ingress_creator_custody.ml"
      ; "lib/agent_session/staged_mutations.ml"
      ; "lib/agent_session/staged_ingress.ml"
      ; "lib/agent_session/moderator_source_installation.ml"
      ; "lib/agent_session/pending_turn.ml"
      ; "lib/agent_session/run_input_handoff.ml"
      ; "lib/agent_session/recurrence_activation_custody.ml"
      ; "lib/agent_session/policy_caller_source.ml"
      ; "lib/agent_session/policy_cause_leaf.ml"
      ; "lib/agent_session/policy_forwarding_frame.ml"
      ; "lib/agent_session/policy_forwarding_route.ml"
      ; "lib/agent_session/policy_forwarding.ml"
      ; "lib/agent_session/policy_cause_data.ml"
      ; "lib/agent_session/policy_native_ancestry.ml"
      ; "lib/agent_session/policy_canonical_usage_source.ml"
      ; "lib/agent_session/policy_standalone_source.ml"
      ; "lib/agent_session/policy_canonical_usage_data.ml"
      ; "lib/agent_session/policy_canonical_usage_carrier.ml"
      ; "lib/agent_session/policy_canonical_usage_selection.ml"
      ; "lib/agent_session/policy_canonical_usage_update.ml"
      ; "lib/agent_session/policy_canonical_usage_projection.ml"
      ; "lib/agent_session/policy_canonical_usage_forwarding.ml"
      ; "lib/agent_session/policy_canonical_usage_ordered.ml"
      ; "lib/agent_session/policy_canonical_usage.ml"
      ; "lib/agent_session/constructor_model_producer.ml"
      ; "lib/agent_session/model_job_admission.ml"
      ; "lib/agent_session/session_commit_attempt.ml"
      ; "lib/agent_session/goal_causal_admission_custody.ml"
      ; "lib/agent_session/actor_reply.ml"
      ; "lib/agent_session/goal_causal_root_route.ml"
      ; "lib/agent_session/goal_descendant_root_route.ml"
      ; "lib/agent_session/session_commit_observation.ml"
      ; "lib/agent_session/canonical_backend_binding_data.ml"
      ; "lib/agent_session/canonical_backend_binding_data.mli"
      ; "lib/agent_session/failed_startup_data.ml"
      ; "lib/agent_session/failed_startup_data.mli"
      ; "lib/agent_session/failed_startup_release_data.ml"
      ; "lib/agent_session/failed_startup_release_data.mli"
      ; "lib/agent_session/session_actor.mli"
      ; "lib/agent_session/session_persistence.mli"
      ; "lib/agent_session/session_persistence.ml"
      ; "lib/agent_session/session_recovery_cpu.ml"
      ; "lib/agent_session/session_recovery_cpu.mli"
      ; "lib/agent_server/session_retention.ml"
      ; "lib/agent_server/session_retention.mli"
      ; "lib/agent_server/session_recovery_owner.ml"
      ; "lib/agent_server/session_recovery_owner.mli"
      ; "lib/agent_session/goal_policy_receipt.ml"
      ; "lib/agent_session/goal_policy_contributions_data.ml"
      ; "lib/agent_session/goal_policy_contributions_capacity.ml"
      ; "lib/agent_session/goal_policy_contributions_carrier.ml"
      ; "lib/agent_session/goal_policy_contributions_update.ml"
      ; "lib/agent_session/goal_policy_contributions.ml"
      ; "lib/agent_session/goal_policy_totals.ml"
      ; "lib/agent_session/goal_policy_ordered.ml"
      ; "lib/agent_session/session_goal_policy_projection.ml"
      ; "lib/agent_session/goal_charge_admission.ml"
      ; "lib/agent_session/goal_activation_policy.ml"
      ; "lib/agent_session/goal_root_policy.ml"
      ; "lib/agent_session/goal_resume_admission.ml"
      ; "lib/agent_session/goal_continuation_phase.ml"
      ; "lib/agent_session/goal_command_admission.ml"
      ; "lib/agent_session/canonical_usage_update.ml"
      ; "lib/agent_session/session_delta.ml"
      ; "lib/agent_session/session_document_transition.ml"
      ; "lib/agent_session/host_turn_tracking.ml"
      ; "lib/agent_session/session_state_presence.ml"
      ; "lib/agent_store/snapshot.ml"
      ; "lib/agent_store/transaction.ml"
      ; "lib/agent_session/history_retirement.ml"
      ; "lib/agent_session/pending_archive_transition.ml"
      ; "lib/agent_server/session_seed_receipt.ml"
      ]
    , "runtime.invocations.moderator"
    , [ "test/agent_session/session_recovery_cpu_tests.ml"
      ; "test/agent_session/moderator_lifecycle_evidence_tests.ml"
      ; "test/agent_session/policy_restore_basis_tests.ml"
      ; "test/factory_catalog_publication/cold_policy_resource_tests.ml"
      ; "test/factory_catalog_publication/cold_policy_negative_tests.ml"
      ; "test/factory_catalog_publication/cold_policy_foreign_actor_tests.ml"
      ; "test/factory_catalog_publication/cold_policy_custody_tests.ml"
      ; "test/agent_session/goal_policy_transition_tests.ml"
      ; "test/agent_session/pending_document_tests.ml"
      ; "test/agent_session/decode_evidence_tests.ml"
      ; "test/agent_session/document_adoption_tests.ml"
      ; "lib/agent_server/session_retention_tests.ml"
      ; "test/agent_session/runtime_lease_tests.ml"
      ; "test/agent_session/model_job_event_tests.ml"
      ; "test/factory_catalog_publication/caused_async_retry_tests.ml"
      ; "test/agent_session/event_delivery_tests.ml"
      ; "test/agent_session/creator_custody_tests.ml"
      ; "test/factory_catalog_publication/descendant_idle_follow_up_tests.ml"
      ; "test/chatml_composition/moderator_pending_tests.ml"
      ; "test/factory_catalog_publication/creation_tests.ml"
      ; "test/recurrence_creation/handoff_tests.ml"
      ; "test/recurrence_child/input_tests.ml"
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
      ; "lib/agent_session/run_job_completion.ml"
      ; "lib/agent_session/run_transition.ml"
      ; "lib/agent_session/run_work_settlement.ml"
      ; "lib/agent_session/run_producer_settlement.ml"
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
      ; "lib/agent_session/goal_wake_selection.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ; "lib/agent_session/run_job_completion.ml"
      ; "lib/agent_session/run_transition.ml"
      ; "lib/agent_session/run_work_settlement.ml"
      ; "lib/agent_session/run_producer_settlement.ml"
      ]
    , "runtime.jobs.acknowledgement"
    , [ "test/agent_session/run_schedule_settlement_tests.ml"
      ; "test/agent_session/run_producer_settlement_tests.ml"
      ; "test/factory_catalog_publication/goal_subscription_runtime_tests.ml"
      ; "test/factory_catalog_publication/goal_wake_runtime_tests.ml"
      ; "test/agent_session/background_delivery_tests.ml"
      ; "test/chatml_composition/background_moderator_tests.ml"
      ] )
  ; ( "subscriptions.state"
    , "Owned provisional mutations, epochs, terminal winners, expiry and catch rollback"
    , [ "lib/chat_response/subscription_operations.ml"
      ; "lib/agent_session/script_subscription_service.ml"
      ; "lib/agent_session/staged_subscriptions.ml"
      ; "lib/agent_protocol/subscription.ml"
      ; "lib/agent_session/run_subscription_evidence.ml"
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
      ; "test/agent_session/creator_custody_tests.ml"
      ; "test/chatml_composition/timer_upgrade_tests.ml"
      ] )
  ; ( "notifications.publication"
    , "Staged publication, disclosure ceilings, correlation and per-owner receipt \
       selection"
    , [ "lib/chat_response/notification_operations.ml"
      ; "lib/agent_session/script_notification_service.ml"
      ; "lib/agent_session/staged_notifications.ml"
      ; "lib/agent_protocol/delivery.ml"
      ; "lib/agent_session/run_delivery_evidence.ml"
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
      ; "lib/agent_session/notification_wake_selection.ml"
      ; "lib/agent_session/goal_wake_selection.ml"
      ; "lib/agent_session/session_actor.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/descendant_accounting_host.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ; "lib/agent_server/runtime_owner.ml"
      ; "lib/agent_session/goal_descendant_admission_custody.ml"
      ]
    , "runtime.delivery.notifications"
    , [ "test/chatml_composition/notification_ancestry_tests.ml"
      ; "test/chatml_composition/notification_wake_tests.ml"
      ; "test/agent_session/notification_history_tests.ml"
      ; "test/factory_catalog_publication/goal_subscription_runtime_tests.ml"
      ; "test/factory_catalog_publication/descendant_idle_notification_tests.ml"
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
      ; "test/agent_session/creator_custody_tests.ml"
      ; "test/agent_session/external_ingress_tests.ml"
      ; "test/agent_session/moderator_queue_persistence_tests.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/chatml/chatml_context_rollover.ml"
    , "5b882911f6969cbd7f20cc905e81b543c2b73bcba1a9275f96a3a48066fe8d97" )
  ; ( "lib/chat_response/context_rollover_hook.ml"
    , "cf083e4d6400e36ec5997397f183305fe9bbe84687a6996528518bbb15ba5c8f" )
  ; ( "lib/agent_session/recovery_rollover.ml"
    , "30bed975a3af6e2deb24daef296834a69b5a23728a33f0fe8e2b2193d69f3111" )
  ; ( "lib/agent_session/recovery_rollover_hooks.ml"
    , "80588d7f09e6569d58ede58aab4b145971a813de7d31ce1c938ebf1a385e8125" )
  ; ( "lib/agent_session/recovery_rollover_hook_policy.ml"
    , "339753605344ac58e0a5d7f522b715b930bb9b11111015a9ed6f1403b44cacc5" )
  ; ( "lib/agent_session/recovery_rollover_hook_snapshot.ml"
    , "f788d06316f5e0d2512fffaf2bad5aa4d919d3e679cdb738f5eb6acb51b4b20f" )
  ; ( "lib/agent_session/writer_attachment.ml"
    , "290d2de6ab34333ac91f4635138cc59ed0ae3640921b1bf38860dabd95126745" )
  ; ( "lib/agent_server/runtime_resource_testing.ml"
    , "36017898365372e19e30bd2ef1abece86fe252679fdccc0d690f2c079c8ccf78" )
  ; ( "lib/agent_server/runtime_resource_testing.mli"
    , "97be915e88a747277818f62fa7af42b6f4a3922bd3c92328b843b44ffb887912" )
  ; ( "lib/agent_session/runtime_close.ml"
    , "5e99b62f03edb34f720917ff0c925f33768da995a9537ef12fdf114598eebd91" )
  ; ( "lib/agent_session/runtime_close.mli"
    , "f824f75182decd6f73b81e9ce4290cdf7a34d1d93a67d2ec17764d7e631ab9a0" )
  ; ( "lib/agent_server/ancestry_cleanup.ml"
    , "b4ffc2938929c691b19c7cdd5337af63e95f2b6392b6b3848b3afb5635e57458" )
  ; ( "lib/agent_server/ancestry_cleanup.mli"
    , "dd689a2f3189aa9bcb26389e51fdcf312ba2f9ad5b05269ad93d3d16ae7cf645" )
  ; ( "lib/agent_session/failed_startup_release_data.ml"
    , "054d9e60654be79f45de98d568b78cd00e27d10e54217ad5f25ae463fe816b21" )
  ; ( "lib/agent_session/failed_startup_release_data.mli"
    , "50f7d685f6140e4087d5dea03f759f03b08d204e461ef77c44789b74b98703eb" )
  ; ( "lib/agent_session/cleanup_diagnostic.ml"
    , "65fc7443dbc0fcd753cb13b0933f4aacc22434560e7cd15bc097ee351d72c6ec" )
  ; ( "lib/agent_session/cleanup_diagnostic.mli"
    , "f09969a0b483c3c5a09e44e12d5219f80e62556470d8579997e21bd850af6d60" )
  ; ( "lib/agent_session/close_observation.ml"
    , "006d302082acb1ea7c62ef87bb1cf3bfd41392faabf138fe2a16048c38c4f090" )
  ; ( "lib/agent_session/close_observation.mli"
    , "ec4ed234561b3337766d1172057e7daddde5fb54a2d2d2dcbd4cfbd25c566ac5" )
  ; ( "lib/agent_session/runtime_builder.mli"
    , "3d51c9ad4b281be800ede6040c9e4ca903a7e6290e8c1221bd3023c938d5b7a9" )
  ; ( "lib/agent_server/session_policy_resource.mli"
    , "c744590e865c71a18c6e265ba830ec481771063421eaec587e9bb8eccf59e9dc" )
  ; ( "lib/agent_session/canonical_backend_binding_data.ml"
    , "c1ae57bb1ada69c57caec8793cda4bde5175ab37e1dc2c87f72865ca2b43533e" )
  ; ( "lib/agent_session/canonical_backend_binding_data.mli"
    , "ba1956174073a3f6b52b6e5182ef9c7097559cd2f0547fe634af56ec1835c156" )
  ; ( "lib/agent_session/failed_startup_data.ml"
    , "a689293adb107002de12bd511cc5a6f5ce335705dcdcd9c9592324651e9fa1af" )
  ; ( "lib/agent_session/failed_startup_data.mli"
    , "30a8caafe8f5ffb683f9a00a934110545ef2457ad6829383473738f5b379683f" )
  ; ( "lib/agent_session/session_actor.mli"
    , "8ab9b64479f7659df9e1ba0eed8f17e69867b45d6ea64887ac844f0ec965d3e4" )
  ; ( "lib/agent_session/session_persistence.mli"
    , "b5a69b807ca80235776abc20fbac71af706625ac2a480a52955e072516a7c905" )
  ; ( "lib/agent_session/policy_restore_basis.ml"
    , "a5eb1344b40d62feb062a5384403b187e434637483e4a35e8c71b70b89c93ba1" )
  ; ( "lib/agent_session/policy_restore_basis.mli"
    , "4da158b59adf513c7a16142aff1127be3bd3841a9375391f558b1673d9d41a69" )
  ; ( "lib/agent_session/moderator_lifecycle.ml"
    , "d12ce82590427da98b3890b1c96de50061bca9a7c7c3e35b77cc8d2f9172f4a2" )
  ; ( "lib/agent_session/moderator_lifecycle.mli"
    , "bd2034d5326b4a81c06748c38453814c809ded2e7554dddf517ceb58f457ffed" )
  ; ( "lib/agent_session/run_state.ml"
    , "9cb96b4da7fb354e46985f2bcd45f7c7c5eea6f0317d51e21d3d6fa9ff519f98" )
  ; ( "lib/agent_session/run_state.mli"
    , "6293ace6a1162a861309252c911a59b8cc52d4b904e3b9001fb2b4b17008b915" )
  ; ( "lib/agent_session/session_state.mli"
    , "e20f22f12245dc2add032ae04dc36bb6a26c296ba416703f6889a7e1ed044a5c" )
  ; ( "lib/agent_server/session_retention.ml"
    , "0a8fa0865eb44be8e2a01be44d219ef0e01bc9c3d4f879712dc06e829ae9c828" )
  ; ( "lib/agent_server/session_retention.mli"
    , "8d2ec15328c278754e25b31f0735b45d40fbde760294bdba0ff126689e908476" )
  ; ( "lib/agent_server/session_recovery_owner.ml"
    , "8f50aa24b5fc61bfed94430645e0f8ed1a598389eda58224f03240dd6addf343" )
  ; ( "lib/agent_server/session_recovery_owner.mli"
    , "1700662ae9d1e3258e77aa754063c16ae75829d5f69bbda978ff288a9305ca58" )
  ; ( "lib/chat_response/extension_compiler.ml"
    , "a91376f2fc29922fc978ae0cc18f86d97df3c5cc47cf30d3f6bc7cf7028db77e" )
  ; ( "lib/chat_response/generated_admission.ml"
    , "137adaa4beca5c2d5f0d6f36c5c18e25ffe3370b5086376720b0df1a3cb9bc17" )
  ; ( "lib/agent_session/generated_definition.ml"
    , "ed31e81ef5b6a8353ddbf74307857756455316a924dcbdbb84c2a7f6f13c7f9c" )
  ; ( "lib/agent_session/runtime_source_preparation.ml"
    , "fb8d995e3299a8b1de7a36c2f8b3c98e5c9fcb485e0dccad7c6cff979f371187" )
  ; ( "lib/agent_session/standalone_tool_dispatch.ml"
    , "781368110a55e792fd6204f3505fa432a26b5c7205e708f28a3562561be141d3" )
  ; ( "lib/agent_session/delegated_policy_caller.ml"
    , "9526f697bed7ed5b341465ba0262446ab8c2a3f15b0ee402694a68558dc24ffa" )
  ; ( "lib/agent_session/delegated_policy_caller_custody.ml"
    , "01ab7a4baaeb8ce326e3d0f6e50ed0044dd6aed5fad7f399f2c68b3ee438cfef" )
  ; ( "lib/agent_session/delegated_policy_governance_data.ml"
    , "9c8ed21cffc141d258b4da1b57221138b2050f0815324b78780654e57b04044f" )
  ; ( "lib/agent_session/delegated_policy_seed.ml"
    , "afce4ea6ce97dd2474421d80f49b4d92279808631c9472a4ebc8a606689db2f9" )
  ; ( "lib/agent_session/delegated_policy_claim_state.ml"
    , "04cb30ecb3983eece2a35dbc7ba36fc857ab8332e6786eb3bb3f5c69c3515bc7" )
  ; ( "lib/agent_session/invocation_capabilities.ml"
    , "7e0f58616974ad956937380f2afe45a2fa83e69d583327b261a87aa0a4d67c49" )
  ; ( "lib/agent_session/moderator_event.ml"
    , "126075a20b6a8fbd9bdcb2ff6e995a40006c827562ad2ed5c58d498335566775" )
  ; ( "lib/agent_session/moderator_observation.ml"
    , "fdca709906191f8076c98928cbe911a251e925a675dc790746a5cb88f6f2218b" )
  ; ( "lib/agent_session/ordinary_runtime_basis.ml"
    , "d3cfc0e59de5e403d2cb96d008de8e97afa40b1c4a0730b70a0537c729220387" )
  ; ( "lib/agent_session/goal_activation_policy.ml"
    , "2b4f668498e6862b564a61fc448683cf8a05385c36c234b45aec7eee670b72a0" )
  ; ( "lib/agent_session/goal_root_policy.ml"
    , "fb857c15a04a7a32cc552badd40cee11a736929f9ffe6a586e470fa79da0e7e4" )
  ; ( "lib/agent_session/goal_resume_admission.ml"
    , "9ba313cc353a2415b74840666e39519117a390700fb77807670fcf903387ff42" )
  ; ( "lib/agent_session/goal_policy_ordered.ml"
    , "d29b78c64befd4a2f0112eb1badb9fed9e5971fd28908df5b048d0c24e302eeb" )
  ; ( "lib/agent_session/session_goal_policy_projection.ml"
    , "1c4abd330c981d16b98a6bac75142e360ba83c658e38d69e74f02493f93e2ba5" )
  ; ( "lib/agent_session/goal_charge_admission.ml"
    , "1e327916612608ccf42c3c43cb765bdb3c28f85b91961d47c8f967b6be7921f7" )
  ; ( "lib/agent_session/goal_command_admission.ml"
    , "46bc8cd5e9df9c8ccbf937de50073077df3aad44b44636a67a74e1093dc28b8f" )
  ; ( "lib/agent_session/model_job_event.ml"
    , "d7f6906631b3691ed6d3e42a38485c3278aad4952a6d432018c28eb47dd29889" )
  ; ( "lib/agent_session/model_job_event.mli"
    , "c5fd69c57b30d2c25e0fb55e4c54a78b420f8f48c5059480307d6da832f26bbd" )
  ; ( "lib/agent_session/model_job_admission.ml"
    , "c16b3fb470372825216f76bd2a87822d39ab6460188faa1da52c60e4645a33ae" )
  ; ( "lib/agent_session/constructor_model_producer.ml"
    , "c7e65b258f4613fdd8b9b0d9af1b41546bb58b232c8d1f0be6889ab6a8c67e61" )
  ; ( "lib/agent_session/goal_causal_root_route.ml"
    , "fbe6ddf82ed8e66a8a16911e4d6b7c891ae068f5d6fbd79ebff330089d135ac8" )
  ; ( "lib/agent_session/goal_descendant_root_route.ml"
    , "9c57f99c5776d359511d51dae1e468b3817d3e1aba40ece722bb2e7e1295b637" )
  ; ( "lib/agent_session/goal_causal_admission_custody.ml"
    , "f6334316eaccaa4247234daf7aa11415e80b34a6d7b61e9a20813212b9182164" )
  ; ( "lib/agent_session/session_commit_attempt.ml"
    , "514a471821ec61306711e26d07b3dd293b7d21dcd7d3b34e9297f0314f034ad3" )
  ; ( "lib/agent_session/session_commit_observation.ml"
    , "babbe9ec3ff397949cbcdf453f7805afeb71e7e90db2c9924c8a2cf9eaa2155a" )
  ; ( "lib/agent_session/session_persistence.ml"
    , "4ddf0fc46a4c6a6b564668fe5edb707846cf00ce1c59b9af10c013cbde10aaeb" )
  ; ( "lib/agent_session/session_recovery_cpu.ml"
    , "9756e9a7e8680f8b8f0a76fc747697dd3272a40a079ddec802ee0394b700b40e" )
  ; ( "lib/agent_session/session_recovery_cpu.mli"
    , "81b5696c56eb3073a84bc13aafaca2244f2b7e06987280bfdbf71c5782cf65de" )
  ; ( "lib/agent_session/goal_policy_receipt.ml"
    , "58ee982e6a4f8390dd73a2d07c2a8ad6686f1e6d416d0aaefdb5a0af121beb63" )
  ; ( "lib/agent_session/goal_policy_contributions_data.ml"
    , "a461d53e6bb7b3e1c20b302c08212b8d9a0fdf8d4f15030f1750683ed0177aaa" )
  ; ( "lib/agent_session/goal_policy_contributions_capacity.ml"
    , "b963f75a74d4c6233ddea01e300797d1bbdc9ce8c73e1c31757897c04ad99084" )
  ; ( "lib/agent_session/goal_policy_contributions_carrier.ml"
    , "e43e5f2381776d29a558ecf268650ab3ff25f2441ed953febea4e317140427cb" )
  ; ( "lib/agent_session/goal_policy_contributions_update.ml"
    , "3226deff1ccf25a6557faa930a5a50ea60a256e8c19e96cb30726bb8bff98046" )
  ; ( "lib/agent_session/goal_policy_contributions.ml"
    , "554ab10025081eefa30b21b9cc4a077878fca58eb28ca46828839b133c419072" )
  ; ( "lib/agent_session/goal_policy_totals.ml"
    , "72502a856c422b0e035768a7a066ee6fd8a7d303efb9c0f5768825417cec90b0" )
  ; ( "lib/agent_session/ingress_creator_custody.ml"
    , "973b8dbf94270cf2be8235b8f5bbad91d75a5f8db12a517ec17d963306bc8836" )
  ; ( "lib/agent_server/delegated_runtime.ml"
    , "acedc14b103f77c9f54a7f659daae9198cc36fa9ef82af5cb4f28af55c32017f" )
  ; ( "lib/agent_server/independent_cleanup.ml"
    , "9d944bf7ef95e0088c79a017e071ac8ddf88b46f4cb0e2eb609ec79312a65b95" )
  ; ( "lib/agent_server/independent_resources.ml"
    , "b97e321436e0e53016f83db11d08ae00fdd77a727f515563cedd9336fb23d919" )
  ; ( "lib/agent_server/independent_eligibility.ml"
    , "fe38e0740f5e4d4eedbe70979d42337e6a8687c925a12378bc91574666f61d12" )
  ; ( "lib/agent_server/independent_eligibility.mli"
    , "452688b083b9f25f7c9fb9be9f89dc438bf773f4ce06b3738a5800423add8a2f" )
  ; ( "lib/agent_server/native_resource_lifetime.ml"
    , "5ab25176afeddee94d98cf77cf46b6c1a83d8f2a5a2711de7676894f6f65d794" )
  ; ( "lib/agent_server/native_cleanup_observation.ml"
    , "bc12426cae7c1146d17fc881d8ea40b9663b8aa5e71fae646750b0e30963aa72" )
  ; ( "lib/agent_server/native_cleanup_observation.mli"
    , "68f862bffc927aac06c23e7373c4b3f6557a9d738be4c43d69f819ac867b6377" )
  ; ( "lib/agent_server/runtime_native_scope.ml"
    , "e5ceee5143f341934404eab807269225232ae013d56dd553441c6ce8672a3e68" )
  ; ( "lib/agent_server/constructor_inference_graph.ml"
    , "163623d4cfbfeffef74c1340355222e56e0c2f653226f8bc8e66b2d14ddbe2d3" )
  ; ( "lib/agent_server/session_policy_resource.ml"
    , "b70bea88fcb92da9e62deea493d74063170248404d0e26698c994bf9f8a62840" )
  ; ( "lib/agent_session/staged_mutations.ml"
    , "be527f8c3537789d715605166e79096a33ba87e55985085e61575b4702d5bc49" )
  ; ( "lib/agent_session/staged_ingress.ml"
    , "27e596e1b90c67bbf7aa24ae7b0c916ceccdda0bb00c7580f19e6c09cb12f8da" )
  ; ( "lib/agent_session/session_storage_semantics.ml"
    , "3bdda9cd25f848dc815889d6883b433fdd4cb50a4adea4dfc67b81474890f0ab" )
  ; ( "lib/agent_session/session_state_document.mli"
    , "450131747211ca900ea4f26787f72881f39f78b3f69cfea6feb68d7dee59e152" )
  ; ( "lib/agent_session/session_state_decode_evidence.ml"
    , "6c3a130b7c590090e3732ac75178ffd84aefc38b343b336b5a1648345e77a00f" )
  ; ( "lib/agent_session/session_state_decode_evidence.mli"
    , "c7f2a5c52c54801bcff4f743f7f6d9ae0f870499b3e2e9c9c43f68c93d1d201d" )
  ; ( "lib/agent_session/session_state_document.ml"
    , "3ec088449f5ef9b0f29bc0504fe2915b81d9a890393679d5d08090ffd1221e3d" )
  ; ( "lib/agent_session/session_delta_document.ml"
    , "972e80f4ed33ba5b3cc7fef4e509726b21e686408d815541e9e636ecb8e6cf5b" )
  ; ( "lib/agent_protocol/session_storage_semantics.ml"
    , "4f9cebf86a4561279e27e8dde5a3a419bbf2b1e91f238abbed791e1c8b1e6b30" )
  ; ( "lib/agent_session/pending_turn.ml"
    , "0cacc56da4b2a17f822d25dd25501096b2ac7184888f9ef3b09093023192c178" )
  ; ( "lib/agent_session/run_input_handoff.ml"
    , "b13a379fdd240a7f764043cf69e3e9c3888a28611a4f2e5c22d742b876427d96" )
  ; ( "lib/agent_session/recurrence_activation_custody.ml"
    , "a91e3ff910020a422ef3f23a29262ea36029b7a21a96536c4e6a83c2b99ef99a" )
  ; ( "lib/agent_session/actor_reply.ml"
    , "00e0abeb3120d71ddd4fc0dc109b0d584da3ba8b771f6d0c02aad525df984fb4" )
  ; ( "lib/agent_session/observation_continuation.ml"
    , "72a753c4cd54295d42203799db41fffbd5c5f8c4675bf9ee9b261472986ba281" )
  ; ( "lib/agent_session/execution_custody.ml"
    , "8d64b47d0385c80013963ffdf886519fe04c3058d543a79b6d9934f4573d5133" )
  ; ( "lib/agent_session/legacy_queue_custody.ml"
    , "f068a7b5badd8bf9f2c34aad0aada6d17265dff5580688d96c1b978482f367fa" )
  ; ( "lib/agent_session/legacy_queue_checkpoint.ml"
    , "5e44f72b7804ac3f8432684f0455865ba560e4479799f75d3886ce1340ed49ca" )
  ; ( "lib/agent_session/external_delivery_checkpoint.ml"
    , "5c55a05e548f34f7e7739e0a465ff39449ce9f24ff393eba83b988d0bc67a4ac" )
  ; ( "lib/agent_session/queued_moderator_event.ml"
    , "37a8d3ce9aba797dccc67a73f3ddf2028955b078e39a3e07cf3a60e576ea7147" )
  ; ( "lib/agent_protocol/moderator_execution.ml"
    , "41362edf7371a04d20fb0d1bb7741236cbe499287b68ff7994aaf42c5c741908" )
  ; ( "lib/agent_protocol/queue_producer.ml"
    , "1a62ecc6250a302c3d9c73919d8a59e33a61b8026932c4cd8e0d8ce3e99f3123" )
  ; ( "lib/agent_session/session_record_shapes.ml"
    , "1816e1f982856f428365ff07e96fcab805ced1b8f53c4c1dd82412bfdfd3ce9a" )
  ; ( "lib/agent_session/queue_producer.ml"
    , "148f5655fffdacdcbb79535a2a31af4e4aa55eb07a5771167017ab768c0b1c33" )
  ; ( "lib/agent_session/retained_job_producer.ml"
    , "dd066605e67fbe7dcb05c60e730e172de2ab06905c204cc10858f6478a2ab154" )
  ; ( "lib/agent_session/queue_provenance_validation.ml"
    , "8b571e6f016c13e3db5d3e74a529a9381e43815cb346af6213b617de1a0b1a73" )
  ; ( "lib/agent_session/legacy_schedule_custody.ml"
    , "059b3a836171db66b10ab72fa50d2d3ef6e4392d2a921cda2da5c9d327e5638d" )
  ; ( "lib/agent_session/moderator_source_installation.ml"
    , "72186a7a5ce773070c324536310c27fab827452e375db9bbea5e304084d7b39f" )
  ; ( "lib/agent_session/legacy_moderator_checkpoint.ml"
    , "e541ee7532b2a3e96d7c06c539cd1637c0ddaa1c55d7f1c51cec24b6d9c4f399" )
  ; ( "lib/agent_session/session_state.ml"
    , "b5dbb3c9b34077677c9a4788f675c3eb1c88f42aa398107b501ba6a03f077347" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "a242925f1b617445e36748161773678f0e0adad74268cbc006988a116c0d92e4" )
  ; ( "lib/chat_response/agent_guidance.ml"
    , "d122b6dba173bc153ad96564a59e03f9b3cad51e8a64923b21af7765fbd860c6" )
  ; ( "lib/agent_session/instruction_guidance_text.ml"
    , "f479e72bd27d60883647524052eb04770198f07603b2d738633130ece31c4861" )
  ; ( "lib/chat_response/in_memory_stream.ml"
    , "ca8569f8437721a1bd4adcc2040bc84bc67aeac6a118a2d787f9e36d30e60d7c" )
  ; ( "lib/agent_session/run_job_completion.ml"
    , "f8d8d2b2523ba7de9df158362e4cf25a67f9334c58b293462451a53d208a788e" )
  ; ( "lib/agent_session/run_transition.ml"
    , "a409d91056f413a6a4f694e8381d04633cd168f27fbb1da8c3fd0602f9c18ba3" )
  ; ( "lib/agent_session/run_subscription_evidence.ml"
    , "cd6f05412544bcf1154be210f332f737b58b66943b3ea47086550ec395e11cad" )
  ; ( "lib/agent_session/run_delivery_evidence.ml"
    , "c2b6b1b87d3070e96d4d1444e83b7054e89225546a11d5395472bb86dbe55de5" )
  ; ( "lib/agent_session/run_producer_settlement.ml"
    , "c619b5ee028ae63319da3c9166ab23aefd638775d725f75d3f9344a85fb57035" )
  ; ( "lib/agent_session/run_work_settlement.ml"
    , "3e4a12c5d3e9bd013110524c40a5590424cfed82b3088735b2516ac95ed512aa" )
  ; ( "lib/agent_session/constructor_permission_checkpoint.ml"
    , "f1d765c7b5bd1a488e360d2b27cc93e58503478e69d9e7f1a0d61e00751ebe5f" )
  ; ( "lib/agent_session/legacy_callback_runtime.ml"
    , "98e620d02390bdeb0a8483cf04565fe055e83bcc825f5fbb31c8943e46860b82" )
  ; ( "lib/agent_session/runtime_unmoderated_source.ml"
    , "18779ed765c2ecf7ac429a1376f8e2708a9b419f779350652342f3362513a6b6" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "6eb216aede723c89d1285ac551ff33a0ef1837f97f321102d924199c6fbc4ce1" )
  ; ( "lib/context_compaction/compactor.ml"
    , "7b11eae5ce4d929bf1ab4702111d03f70fe97cfe8543135c73782c2b92391ff0" )
  ; ( "lib/agent_session/session_transition.ml"
    , "77c2a3e91cae24f7994866f4cdb5b6f38b5cffb49224ba7ff927c2f921e5bc3a" )
  ; ( "lib/agent_session/instruction_clear_capacity.ml"
    , "baa952a408f74f0795059df9e7f6bb3c677957c1866c6d2b20cb83fc7a475e0e" )
  ; ( "lib/agent_session/session_terminal_journal.ml"
    , "df1c2f60f413a5fe01dfe04a20eb91a869e6d157becfee8ec0171dc8a6573f33" )
  ; ( "lib/agent_client/developer_trace_connection.ml"
    , "a737c2bc921e42546ae3ae8165eae078a409861563acbb61782db870dcf4adf6" )
  ; ( "lib/agent_client/connection.ml"
    , "ae0b0643050203375da9d4be3df75cd2a5ef18e98bbda95695f9e68046390119" )
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
    , "1f9635dd9a39f0cc651a827fb767ef7886435b5590578c48c2e360d2f85e5cf5" )
  ; ( "lib/agent_server/embedded_local_template.ml"
    , "93691f37b0b11d3b5ef3303d2d6f35d7bdffe2ea2eb360db9d007ab841210e3e" )
  ; ( "lib/agent_server/embedded.ml"
    , "fa6a1f71ed55e2ee0e968184dc1980704b0a04cdb998d661fe32287140294726" )
  ; ( "bin/ochat_agent_stdio.ml"
    , "8d134cd11c7df2a60e6b4b77a5ff7f9ea0c28a8a2674a3dd9f573adaa7623902" )
  ; ( "lib/agent_server/native_resource_authority.ml"
    , "6c1d600bb41fa15279a82bc66b479c850778eda7ba40dac7911a4819df9b991b" )
  ; ( "lib/agent_server/delegated_policy_call.ml"
    , "1527c1a798b49bb509cea4ab3eba4c1dbcbec82396d72f094f430b0d53c19fc0" )
  ; ( "lib/agent_server/delegated_policy_vector.ml"
    , "6004f0ff15cf25b2282aa7b510eced951f4ecdabf223285799d6f7956263500b" )
  ; ( "lib/agent_server/descendant_accounting_host.ml"
    , "3baff14cf9c606e899c08f95805cc3ca9f070827b46ec531e1c230314820b0fd" )
  ; ( "lib/agent_server/session_factory.ml"
    , "d3ad739c0ce56f8d08005e07eddc6211ac5ae3d4eae79bbe366c3820a1dda998" )
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
    , "6ec0d1a08bf028806f4382bfd454706efa65b108038351336942cb6fd71e90a4" )
  ; ( "lib/agent_server/authoring_installation.ml"
    , "c153b07c85dae5395d4a5a4e30a0d430dcf922c23d29cc3f302a70c490b4a94b" )
  ; ( "lib/agent_server/connection_context.ml"
    , "f9a0481a7151da1864ff8680c8d75fb0381e5a4889fa09d0269d349d993ee052" )
  ; ( "lib/agent_server/startup_cleanup.ml"
    , "856d23d016d852c26dd8290d006fa0553ea3040aea31e6566cf4ff7528f337a1" )
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
    , "27c017c2106f7a03d0b65f8c221c1c5c8a0d7863b1f7e8290fefb82fbb43fbd9" )
  ; ( "lib/agent_session/developer_claimed_resource_owner.ml"
    , "fb251c6e022f9880820b15807018adc5cf240500050a0424321d7507d3e2bc41" )
  ; ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_session/claimed_child_runtime.ml"
    , "f5ab8751939aa924659c11e022719444ee8457d818631db8151abf0dc7ab22e6" )
  ; ( "lib/agent_protocol/model_spawn_request.ml"
    , "258c00854df5cd7a65eb28d8fccc4ab1e1d1880ccbbc15999306fd448322d160" )
  ; ( "lib/chat_response/ctx.ml"
    , "30a3b6a6497f0af9075513f83d63bc953b332a74d6013805af55233a57e810e9" )
  ; ( "lib/chat_response/driver.ml"
    , "091a398d90aa8c1f556902a782dc97338ce89d964f3e3ab39c756b308e2e6737" )
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
    , "7e67f112f6484dea487bba037bb04e114655ed1d030a2ce56672ef068f02def0" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "fd6017ab5a829ce8ae18056cb13da322c7ddae323815cf803bfe5fd804e21444" )
  ; ( "lib/chatml_trace/chatml_trace.ml"
    , "b2fa111ff3d9c269b82b4931e8bbcebc58a393c1700bf24573ca1851cb6aecc4" )
  ; ( "lib/chatml_trace/chatml_trace.mli"
    , "8ebf49b2e1858f86daadf6fb687cce0436edec79bdb46935fd7ae2647e1e8f7b" )
  ; ( "lib/agent_session/administration.ml"
    , "aa07d835e69d5d8af13af5c8920801be52a3893cd2866cd7ed74a477f35aea02" )
  ; ( "lib/agent_server/command_handler.ml"
    , "799053701aefa3510fafaa30f94e3562615c10273d6b9b44344cce5a188a343d" )
  ; ( "lib/agent_session/preparation_tracking.ml"
    , "101b922ff06365f9592e1b168a7e4267ddc74f769ae48d4a26ca204865a13732" )
  ; ( "lib/agent_session/turn_admission_staging.ml"
    , "bcb737947d1aa72a814bb69002fef5960be1db2d2ed398ce9979b4778961f68b" )
  ; ( "lib/agent_session/delegated_runtime_load.ml"
    , "c202fb7653c63d810965bf80d65b60bc91067f544ffb886a4539fc67e0d0e859" )
  ; ( "lib/agent_server/graph_tracking.ml"
    , "166ef8eeecb21c2e592b866532f00f1d0c3daefbf392f4a51421cb92092733fe" )
  ; ( "lib/agent_server/session_registry.ml"
    , "f5107716d5e8f97953fa3e3a74b6c39a73f12b9c866bfe751fe93a02649d0d95" )
  ; ( "lib/agent_server/cleanup_original_view.ml"
    , "8dd7c37b5561b6daa2ba6a033a88660cea08a738e027969240d9d03a20883896" )
  ; ( "lib/agent_server/cleanup_original_view.mli"
    , "a2f23b476ef86053ff97e19cf577fc72ec17c438728e4087cd9e56db93863d2f" )
  ; ( "lib/agent_server/runtime_callback_data.ml"
    , "20f0b30376b5f527e57b6e3065b724b1fcb138cd4a56843ad50a8d59bcba7566" )
  ; ( "lib/agent_server/runtime_callback_data.mli"
    , "03e05233b9f160fd0b137489be33fcf2e5eabb20ec943f819cd6d2fdd79629c0" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "a74883be707523ac25e4706a756ecb50f2b25e2a099ffe690cb4acfcb6c347c6" )
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
    , "4becd5383fc52ac74b21eccc2a838a7902ef92cbcc7118dfc86d441500d5da47" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "a5447544cbe1cd55302cd70a3c50bf7f6b0ec07ad3af8ce1f70b7b90dfbd07bc" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "4d6592566d285a4ae2b75e3ad64c904294880592e8c55657be9db04625f03774" )
  ; ( "lib/agent_server/job_scheduler_developer.ml"
    , "18062ce3137c3b1782fb4015657ca304a591341a33e1d1da47d362b137b7fd19" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "89b0cd7a7d04842c81b3bd632a1bf1f8563b9e31af9e83b571795f521da2b720" )
  ; ( "lib/agent_server/developer_child_agent_runner.ml"
    , "09bb4a77257188b362bcf474d367e17bf44fc786e0761cd894c2cb5c72158573" )
  ; ( "lib/agent_protocol/inline_image.ml"
    , "9ea13e23b7e0a4316027cb2658bbe314b6ba468206c7242c82d88ceebbdb9b5c" )
  ; ( "lib/agent_server/developer_child_content_projection.ml"
    , "64e0c3cd685afdb3a639ec6ceaf4667741dca93f8ae2636258531fad07cf2adb" )
  ; ( "lib/agent_server/developer_child_selection.ml"
    , "615ce2d2ece542ac27f37f994fa6b727c7010ea467819bcf7e652d31b1e360d1" )
  ; ( "lib/agent_server/developer_queued_issuer.ml"
    , "d7a52e5cd46ff8da9f8c7143b9cd0609a97ed199671756d5a8b4c4014e06bce8" )
  ; ( "lib/agent_server/developer_agent_source_runtime.ml"
    , "9a4e50dd8f52dee6f5b9171f416d01078f79c257e563e66d1ad1623493b6f7e3" )
  ; ( "lib/agent_server/developer_live_resources.ml"
    , "cb88d63f805aaff22b8c4aec2310b66ef8c85ecce3d2017309f35de1d42584b6" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "7037fa9099d655af26c9596eaf901caffa2d02edd5d14b6e99b38bb4d737f1ec" )
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
    , "be5a2e29e0871138160e1291b513ab5dd177ab3d533291757e46c81c64215ffd" )
  ; ( "lib/agent_session/background_execution.ml"
    , "33996f6b8a1165fc7e992c5c019a2db478b67b1fe6ddb3fed34fa8b731454254" )
  ; ( "lib/agent_session/captured_agent_runner.ml"
    , "02221a990c09bc62411c110600970c92382370ca59b68141927e35a111976d13" )
  ; ( "lib/agent_session/selected_agent_depth.ml"
    , "a5c52ff1391956f08af2cdb1651db530ab66abea394d7a7599885bcfc778c412" )
  ; ( "lib/agent_session/developer_child_preflight.ml"
    , "09658d83545bb026fb6eb3d3d30dc8f7b111fccac24960c8d0ca8a29c8b7266a" )
  ; ( "lib/chat_response/converter.ml"
    , "c4617afa8e0a8d28283289bd5cfd5c170eb04d30071fee1389e4f28faef4490e" )
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
    , "c72ef1787b03d22c8775d4ec8e24b967a6b2517c428f4f05e87f82804591f5a0" )
  ; ( "lib/agent_session/external_ingress.ml"
    , "2d9a49a577cce4279c6da7a1808675fe3bd002131cb535aab03e2c691073dc6b" )
  ; ( "lib/agent_session/moderator_tool_dispatch.ml"
    , "2103f409294e44c40e75df9794673008ff362b90233f05262cabe02656709b3b" )
  ; ( "lib/agent_session/managed_moderator_dispatch.ml"
    , "b1e6b6fe948e6cfecec617144117b5a4ee683591965c6085cfb8560deacf006f" )
  ; ( "lib/agent_server/original_cleanup_report.ml"
    , "cde104f1bb514c9564565ac85cb8ed3404c7d24662046060aac2e3b2a8cfd41d" )
  ; ( "lib/agent_server/original_cleanup_report.mli"
    , "fdee91507254f2de0d5b94de3fdf1f43a78f8880f3f743c00e84178ac819fdc0" )
  ; ( "lib/agent_server/disposal_report_data.ml"
    , "8a52430dce954a51e0709d5e9cdb037d08582bffed0867ed8eaf2c3d2def91bc" )
  ; ( "lib/agent_server/disposal_report_data.mli"
    , "0bca97fddb3f0b4a4e1c88813f0bd3dd6ad01734b61bf92480f50f21854b0129" )
  ; ( "lib/agent_server/session_disposal_report.ml"
    , "0e6b5d62a7447a6d52bade233c6a3dc534cf6cdee02c1548883ea3b5e17e5b1d" )
  ; ( "lib/agent_server/session_disposal_report.mli"
    , "b4ab8e7570c469ed81b4fc9fcf6c86f760a363b4849daf0b0ceb56db655f055f" )
  ; ( "lib/agent_server/callback_disposal.ml"
    , "a17caaf21e6f75f9b46173e220c341fd0115b6621a76106e7cf3ae2b7af60a74" )
  ; ( "lib/agent_server/callback_disposal.mli"
    , "55c0d8a9464e6d0f18d838e329c5b2e8498ee7bed3e29bc46fa56f0091cb2362" )
  ; ( "lib/agent_server/initialization_capture.ml"
    , "1ba9c845e78f433ca9b719dcd6d75c4d682c77026b9c5773577b0b515d2029a9" )
  ; ( "lib/agent_server/initialization_capture.mli"
    , "4ea548f0e3deef2cf4d6a38b001815183c612c758fc5f272ddd1119bc82523c4" )
  ; ( "lib/agent_server/initialization_capture_data.ml"
    , "f1e5ffec58c8dddd931e87a4867b7d6d6f9115f04f96ad4d87eaf7557fcedb5c" )
  ; ( "lib/agent_server/initialization_capture_data.mli"
    , "d926c3a6042048c9310a2477a26ac5c87ee40307034a73a10bf1daec6bd61abe" )
  ; ( "lib/agent_session/callback_cleanup.ml"
    , "42b6a4746a65318a55b12f5911f0ad57d0a2d24ffd7f5380ab2f1b56f757298d" )
  ; ( "lib/agent_session/callback_cleanup.mli"
    , "72869936689842cb3751e2d366c7fa34a4bd82bf089b1e5a37560bae5c561d51" )
  ; ( "lib/agent_session/callback_cleanup_data.ml"
    , "2bf6d5da1075f751473c1df9b54cde2bf7c2c1cbc5f584c5e02652fa9d03ec94" )
  ; ( "lib/agent_session/callback_cleanup_data.mli"
    , "671fd01b47fd36c8575810ee157e329adef4d73bdfb4c135e318e77290454ebd" )
  ; ( "lib/agent_session/product_callback_data.ml"
    , "f59d9cee2f0300c7cc1762b2795e06ba4d8dc8c93688594a904ed2767eeccd2f" )
  ; ( "lib/agent_session/product_callback_data.mli"
    , "b981c51b31151bcfe467e03d818142c75ca4d3cf2a1ed5e5536eaf3dbcce3b76" )
  ; ( "lib/agent_session/operation_worker.ml"
    , "b1116aee053b5b69e00bce6aeb85ec558e9795fc3e8f72374b436831250a0dfe" )
  ; ( "lib/agent_session/notification_delivery.ml"
    , "9f18c6eb7561b0afda29cd706214ddf2a376c54b9f28980ce536079ae5b7b45e" )
  ; ( "lib/agent_session/notification_history.ml"
    , "d15c7a52b7b90b283e9beae7f3be36201c41bcbb4ca0402699b450787e2c3403" )
  ; ( "lib/agent_session/notification_readiness.ml"
    , "a92bdff0c1cbe6e50dd3a812c30ebee171d111a2e2f72596e1b0e4e6c9e2b760" )
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
  ; ( "lib/agent_session/notification_wake_selection.ml"
    , "001872ff3729d6a4a7cd861cd6ec4ebbf75296637bd5c5cdaf3ede82c6f9b6ba" )
  ; ( "lib/agent_session/initial_lifecycle_startup.ml"
    , "34befd9d623fc41ffc0e0013960277ff03d4cde6d0f2febf6f0149dd91f074eb" )
  ; ( "lib/agent_session/inference_graph_owner.ml"
    , "50e6bf5e41f6f3ce3e40551569b8a99b5e4ab0ec00ca7346aceb047600782f3a" )
  ; ( "lib/agent_session/goal_wake_selection.ml"
    , "800d7f82dd88851acbc56ff6b90b528d9e355673c68f5f825c0a2f555ca91ea8" )
  ; ( "lib/agent_session/goal_continuation_phase.ml"
    , "e24a210492bfcc9e94c7bf472603531eb053eb7654de7c6100083464da9b1b51" )
  ; ( "lib/agent_session/session_actor.ml"
    , "c6c0f60dd3424f43c5f5fc86fb1ecd0c2d226ded2753dc98067cd3815a61fea3" )
  ; ( "lib/agent_session/idle_completion_write.ml"
    , "d427fa80c4346b64f3f9bf4d5025ad2218403278816ade76ff17ba1adaa426d9" )
  ; ( "lib/agent_session/goal_descendant_admission_custody.ml"
    , "4ce662b70134370984bb202f0f50bf8df65576eff655eaf57902edf7c50d6dc7" )
  ; ( "lib/agent_session/job_custody.ml"
    , "927e0267c6bd7b1f95fd5cad662959583347a7db8cbbc9fe211276ee513a9291" )
  ; ( "lib/agent_session/extension_invariants.ml"
    , "528a11326b61b95f52e739f561af5f6a020702e93adbe7c235754954d0963acb" )
  ; ( "lib/agent_session/job_dependency.ml"
    , "4caa00f2bee4227bfaa58a2c9c29ebe7d5bb97f60055b2ed3428aa82a3f97466" )
  ; ( "lib/agent_session/job_launch.ml"
    , "13100688e99707d7e19244ff616367f47555e2c1d46f6fb874d8b2e9f2fb92bf" )
  ; ( "lib/agent_session/policy_canonical_usage_source.ml"
    , "31b58e561e3ca88f38234951b269f5eff16d052c070859868cbf7d0196b28e7a" )
  ; ( "lib/agent_session/policy_standalone_source.ml"
    , "a65a46355b4b9ea3feef61282335213ba7bfbbdfe5ac3b3414af09f9b785c8f5" )
  ; ( "lib/agent_session/policy_native_ancestry.ml"
    , "9ffc613339d8997771d1bb8caed6ed1af753731d36518e0d56d8ade8aba432a3" )
  ; ( "lib/agent_session/standalone_completion_contract.ml"
    , "15ac1b3ace97fb724704a9c4273e6250f77d31d351e7eea287286d8b9af72ded" )
  ; ( "lib/agent_session/staged_jobs.ml"
    , "895feeff14aee6ef29c556c87ffa347d2e28856317bc56bfa8d6284fcb2911d9" )
  ; ( "lib/agent_session/staged_notifications.ml"
    , "8cd8537e4aa24fe231cab7d2b454e67fbf0cde3a4a20d392206f32cf1f8db14a" )
  ; ( "lib/agent_session/staged_subscriptions.ml"
    , "9c48fbd27c5c8889bff5094106e49a77c0f3cf328023370c9e52877421216abc" )
  ; ( "lib/chat_response/background_delivery.ml"
    , "057c3b5ab886ae06b403dbec7f3c03c057e414504d74c7e419216809cc69de25" )
  ; ( "lib/chat_response/background_job_operations.ml"
    , "86e16c6df7d1eafcb41cb8b558318f7f5b310f9b356afc2412f0e0a32055fcb5" )
  ; ( "lib/chat_response/background_request.ml"
    , "5c986dac06102d1c99074b2e0cb70cd808d6a09ff44dbc148c975e1c4ff6383c" )
  ; ( "lib/chat_response/ingress_operations.ml"
    , "d8e2813f75f67ca4c4d3ee6fc264a9731889538987a53d1f80ff27c2c399c825" )
  ; ( "lib/chat_response/moderator_invocation.ml"
    , "445f7e4a6597b27d3af34905fee56441bcf82285fd52eb8d9f0c400379b28deb" )
  ; ( "lib/chat_response/moderator_manager.ml"
    , "3ba1323d09cafa21feb684edfaab8b0ad7286b676c1e14ad13a951a5f36a01a3" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "b36da091bfad8de042dbb28c7d1cb9e4f5c2d60c220d2a554dc46b246832bee1" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "f8d63210438cadd286834a77bb62eaa371cc4f94aea08751aa84a87bc49333f0" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ; ( "lib/chat_response/startup_compilation.ml"
    , "471f85d761957a20d98f00116f40c650cfb7a8102ea73e51f03162ecee11a46d" )
  ; ( "lib/agent_session/runtime_script_preparation.ml"
    , "27d9b620bf6cfa39a7aa42ca700b4095125bf596bd63b06de79f4f161c8edba4" )
  ; ( "lib/chat_response/script_contract_plan.ml"
    , "61891b7cb0ba9bd8d4f5e4872464abde29f7b69bd6028db5232577bb352cae56" )
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
  ; ( "lib/agent_session/policy_caller_source.ml"
    , "4f3e52c4be7bc658a3bd1eef9322fdd74a8fd54dae6b1423da7ff9995be5be36" )
  ; ( "lib/agent_session/policy_cause_leaf.ml"
    , "c29b812d227fe8747ec121f355dea527af444098f3b7480896afa0c61fcab316" )
  ; ( "lib/agent_session/policy_forwarding_frame.ml"
    , "d01f5e45c489fc1d6fa4174a15f7cdb669a1cf104898610ee4cfa18615a56cef" )
  ; ( "lib/agent_session/policy_forwarding_route.ml"
    , "e37c906d6b6ad3ecb438ca4bc7451dbf9aed91d0ead07b2b3f4c3f3f43e3fb16" )
  ; ( "lib/agent_session/policy_forwarding.ml"
    , "442f2b94f248a1cd52702fe4a0af693c9c4e36776f4c491097bc189bb48d50ae" )
  ; ( "lib/agent_session/policy_cause_data.ml"
    , "09761a1dca50b42896cfd85ab7213d6da4c58fbfe931c9125b0141222ee5faed" )
  ; ( "lib/agent_session/policy_canonical_usage_data.ml"
    , "32c1555c6377f3fa6ba74313dd36026a634cc47511134c2abfb50cc0e6b9eab0" )
  ; ( "lib/agent_session/policy_canonical_usage_carrier.ml"
    , "a3880c08e9f959e29e21c1e4329491bdb7cdd318f84c69a9ac7e2275754214a2" )
  ; ( "lib/agent_session/policy_canonical_usage_selection.ml"
    , "2b6bd4eb41333bee74baa5de3b72aca88b384550b19f9abd075033b1fdfd8da1" )
  ; ( "lib/agent_session/policy_canonical_usage_update.ml"
    , "deb08c1fa8cd3c4fd8c056c4bdbe751ed680f3658052933b52696dff6ef80874" )
  ; ( "lib/agent_session/policy_canonical_usage_projection.ml"
    , "23bef8a7d84162868c0df6ca3c5be6ed9dd322bda797b6e5ecb9031707f27923" )
  ; ( "lib/agent_session/policy_canonical_usage_forwarding.ml"
    , "a838b036b7e632dc759c4a31af08682d3472d6ed4a23b5e0c9fc7c7a33832642" )
  ; ( "lib/agent_session/policy_canonical_usage_ordered.ml"
    , "c0ab240efdb1a6e072f5ad411326dd4990bf67023b210f747725f27d20e7e2c5" )
  ; ( "lib/agent_session/policy_canonical_usage.ml"
    , "e7497778f82c370895b7e7cbf794f0817c6e6b4655dcac628507dec1348be261" )
  ; ( "lib/agent_session/canonical_usage_update.ml"
    , "599bf11fce6d08aa522ce49c1df4ade47c640045591442fd67dcb47ec6084ea2" )
  ; ( "lib/agent_session/session_delta.ml"
    , "ad401f4b7f3cd704e21af74ff1f8775e989542519fe2343f513be445a6cc26d8" )
  ; ( "lib/agent_session/session_document_transition.ml"
    , "0e81f867cc6cd1cd5819a48a16401c4d4a542bfb532bebd7ed200d9d9f1cdfa9" )
  ; ( "lib/agent_session/host_turn_tracking.ml"
    , "e9f94b5e59d6097c288c2b49c613e2749da1245afcef4716dac3caa6fc91e9e0" )
  ; ( "lib/agent_session/session_state_presence.ml"
    , "fdd948fdf6ecd1be932db7c2a97894b90bfd9775bfc3068b3a63efab9eed234e" )
  ; ( "lib/agent_store/job_result_store.ml"
    , "b5e2e582e5f9a26b4383370c56ea88edbe63afc468f7c8af5f75a6c4b701e095" )
  ; ( "lib/agent_store/snapshot.ml"
    , "9713ebc879d28698bf04894999b3d64a2b7182b287012db0834d2fd7de1ba372" )
  ; ( "lib/agent_store/transaction.ml"
    , "46068f350c328279e63f25c6e14eebf1c5b3edac2a31a07e33c9f8a8977f0935" )
  ; ( "lib/agent_session/history_retirement.ml"
    , "7e5884b9a212427ec2aedf8e3634a0219962de635d8a5643a4740e16ba7223da" )
  ; ( "lib/agent_session/pending_archive_transition.ml"
    , "beb50b1eec7e8907977290c82e9f24efca4e647dd01a9a11103f6a6dd32a510e" )
  ; ( "lib/agent_server/session_seed_receipt.ml"
    , "34e9481bc822ee6097712d6cb5b6e9f3363eb305a4d4486a3196d31099d12d94" )
  ]
;;

let topic_contracts =
  [ ( "moderator_v1"
    , "runtime.context_windows"
    , "ab826a126a647f94866e13475ab21632d0cb7ccdb38bf503d093770a6fe46980" )
  ; ( "delegated_moderator_v1"
    , "runtime.context_windows"
    , "c70cb9ce2a3df3adf6d7271d9cda4b9bd84ba15b7c6db62224f46f776809d18e" )
  ; ( "one_off_v1"
    , "instructions.sources"
    , "4cbb224a500d3e811fe54e5cacfd8c8f91612e6fa2236c79f42f4b0c7655b2e3" )
  ; ( "tool_v1"
    , "instructions.sources"
    , "e492f0d45dec5c0235390596e998941a1b6ecdd1f1343f41f620cdc6c9ea4067" )
  ; ( "moderator_v1"
    , "instructions.sources"
    , "a8a1474fa78ef8f1638bf2b1fa22e638d6f2262256db336c8ac4ac34f802d271" )
  ; ( "delegated_moderator_v1"
    , "instructions.sources"
    , "673480da622c4484163c03d0f00beaab7465e6d5fa2425a44a7e5bd818aa48bb" )
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
    , "8c61ee5e60fc9d81c1e24f9b9962351a7caef51bc2e30492f0d1bf63c34bbb26" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "6db49070342dbe7066f1c58565f751f582ecc64ecb10eff69d449080f9a32a07" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "a474bc3dd538d0d05830335a83d566274986c77a106f3292e677a3a5b943dbe6" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "2cb5164165c274f41f3b8ada57182307d7ac3c61f6c10d01189171e0c9d81c75" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "18893e906072e01f0d9628d6c4ba052a6930b9abd42586f444f6a06c0a841b5e" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "46248cda3205d472696f0b4b24bc832b45e4133e7ca74b9c148734a4168757a7" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "796bdd317e02d2b393e4889a3cbf5a2ee8a4a2998badaaf3df60f0497d7384d3" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "15663cf25ea60b1e5a727d847988df9ee66a34206917b1aec936479ceda543ad" )
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
