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
      ; "lib/chat_response/startup_compilation.ml"
      ; "lib/agent_session/runtime_script_preparation.ml"
      ; "lib/chat_response/script_contract_plan.ml"
      ; "lib/chat_response/restored_programs.ml"
      ]
    , "runtime.execution"
    , [ "test/chatml_compilation_test.ml"
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
      ; "lib/agent_server/developer_background_job_runner.ml"
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
      ]
    , "runtime.control"
    , [ "test/agent_docs/docs_chatml_control.ml"
      ; "test/chatml_composition/automatic_turn_budget_tests.ml"
      ; "test/moderation/moderator_execution_budget_test.ml"
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
  [ ( "lib/chatml/chatml_task_array.ml"
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
    , "d571ed9ac9584d52b151bca47b07f44caa343ae99401e32d8cc5b460caac76da" )
  ; ( "lib/chatml_trace/chatml_trace.ml"
    , "6952bb534057fdaefcc42aac25b313122ea39a0e71b30bf20d656047b0c5bd65" )
  ; ( "lib/chatml_trace/chatml_trace.mli"
    , "a5362f26f1c83fe3223298ba09d523728f35d0002ad74d3857378fbc21b1effa" )
  ; ( "lib/agent_session/administration.ml"
    , "00ae2dcdef80bd0552e6a7dded2e3872ed547b82b2ee85297ca5eb7d3ccd9f8a" )
  ; ( "lib/agent_server/command_handler.ml"
    , "45c89b173ac6c33ab5036bee6c460a1118703abc207a4563bbb7d0ea6d10118e" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "7431f8254c524a641f3ce86633cd812481b938ba3b6e35bf1e31d01059cd7d66" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/chatml/chatml_compilation.ml"
    , "ed6f6dd7667e66cebe709f4e59f4def09e756edaeb34a4dbadff2134f863f4d7" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "d1f41fe885082993a1b9e375d5bce4f2a1adf17b2d9a846c6fdf8e1a6544358d" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/agent_protocol/delivery.ml"
    , "63ae0f4d478302e84a1e49c84f406abf090794db4c231aa76181ad02072581d9" )
  ; ( "lib/agent_protocol/job.ml"
    , "9f7ce5c6116a85816df7e489d7a6ba0c54114e9248592b2e7b38f0b0097a01bb" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "a5447544cbe1cd55302cd70a3c50bf7f6b0ec07ad3af8ce1f70b7b90dfbd07bc" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "004c098df00746429c2bdfd2f6a8dd90b3204d2f3f9c42f9aa8bcdf2bd6d93de" )
  ; ( "lib/agent_server/job_scheduler_developer.ml"
    , "b47829fd4e88d77ff6fbd9c75a375f8b99b145dec7f7e5d117c67018d1d37890" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "a31560b47a4e49201f55fb7c97605d181cfa96f3e9b0a9511e3c9df5e4e76eaa" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_server/developer_script_job_service.ml"
    , "bfec4516694d97df4e43a793d7fec44a74f45cb6b5947f4f94812eae2bd08395" )
  ; ( "lib/agent_server/schedule_scheduler.ml"
    , "37cf371632d8ad86693c162e0593d56866f76df577dceb06ddd2ac5fe5bfdae9" )
  ; ( "lib/agent_session/background_execution.ml"
    , "2149c979ce81c6c851c8d8cbe749c622334f3c43d05b72b17ae89873e5d563c3" )
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
    , "04c1cfd48c30ab3982640f3f6f57d5d1e54df8306bf9de7253b131ac8795d9cb" )
  ; ( "lib/agent_session/script_job_request.ml"
    , "9fdedd1ec520d69cb2bd7d5d32639bcc84d6c0d2dad1835513d7c7feaaf0d9d7" )
  ; ( "lib/agent_session/script_job_service.ml"
    , "2fee8a9394aae0cc978588ad16f5fe938388c2d2cc23fcb0588ed7f19e4491d8" )
  ; ( "lib/agent_session/script_notification_service.ml"
    , "ca17dedcd13940b10e7b530343d61d2cfc8b681cee2d8b03e02498c68644a781" )
  ; ( "lib/agent_session/script_schedule_service.ml"
    , "9af44ed67062da4b8fa546c35d57a9aa627c24775f4d763946aed8ae84e7b58b" )
  ; ( "lib/agent_session/script_subscription_service.ml"
    , "54c775836b551733687e907ff46a2a7a0cc26bd46ff915cc6fd5f0cf5116f069" )
  ; ( "lib/agent_session/session_actor.ml"
    , "aa91cd40798f283b07f17485ddf766d88784d960b5174ccb40ef90d8e5119bab" )
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
    , "104350ca788bb88e626045dc5269080d8d5f5005b88898c3faa69d9377793788" )
  ; ( "lib/chat_response/ingress_operations.ml"
    , "d8e2813f75f67ca4c4d3ee6fc264a9731889538987a53d1f80ff27c2c399c825" )
  ; ( "lib/chat_response/moderator_invocation.ml"
    , "485cab4625c91fe5efe73e10be2e97b2e4b065dafebcca5c46580ce5965be9dd" )
  ; ( "lib/chat_response/moderator_manager.ml"
    , "6bae9c06304bdcbfefc7979d759a9c012540cd46d98bc1b055da9b8a154259a6" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "560ef648e5245806a48f2d5823ed34b690b51cfda83cc312d94be8862efe9d6e" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "f8d63210438cadd286834a77bb62eaa371cc4f94aea08751aa84a87bc49333f0" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ; ( "lib/chat_response/startup_compilation.ml"
    , "0f60a1f036408809e5618f3310fbc2213e9f7fe99788fb785951bd31c7b96bb1" )
  ; ( "lib/agent_session/runtime_script_preparation.ml"
    , "2546f0cb0301ce1abf0e101c44903f506c5d318413826986d3bc4e743c323779" )
  ; ( "lib/chat_response/script_contract_plan.ml"
    , "05eccbbde26eb960d6d6b6ce89a3a5b4cad786e3734c7ff9526c25fb9d059dcd" )
  ; ( "lib/chat_response/restored_programs.ml"
    , "329f2747454cc915a6ac3713fbffadc46d2d71b925167a3be68c0aefa9617fdb" )
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
    , "60883042ef1f87c20cc4a40a8d661223998d3b7275b7221f2a4594cbe3a8b9aa" )
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
    , "38177c8e806fd8bbc08b83a019fc8b7c21c37b0ff757e62c7718405f9a708f5b" )
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
