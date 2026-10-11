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
  ; ( "execution.configured_modules"
    , "Contract and invocation preflight before effects, canonical once-only value \
       dependencies, execution-local state and shared controls"
    , [ "lib/chatml_linked_runtime/chatml_linked_runtime.ml"
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
      ; "lib/agent_session/script_job_service.ml"
      ; "lib/agent_session/staged_jobs.ml"
      ]
    , "runtime.jobs.owned"
    , [ "test/chatml_composition/background_tests.ml"
      ; "test/agent_session/background_transaction_tests.ml"
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
    , "6a818d806bd09eeb3970e3603f5444de154d8b39f8131dc237f21e94a4d9493f" )
  ; ( "lib/chatml/chatml_builtin_modules.ml"
    , "4d005f60b55cee3263e7925bae6d1c81129ca383cf6218b3cc2dc86270def236" )
  ; ( "lib/chatml/chatml_compiler_contract.ml"
    , "172cd116b1680c9ad2bdf0a94debf38f196090c984668e1bcb6d235f0a3331b4" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "2ed1cb2abca1e856a4aedf4a764fa4e72ff6b6d2466e30a79fa8754c95f4441b" )
  ; ( "lib/agent_session/administration.ml"
    , "294e4f4885be914c82f8fd9b327b9dac9a311ba6c49f3c424cdd34f7c9a9f54b" )
  ; ( "lib/agent_server/command_handler.ml"
    , "cbd0c66588ef9aa56523f1ac1d250ee7fe6affdd85adfdeeda989a3fbb6ffdd6" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "7431f8254c524a641f3ce86633cd812481b938ba3b6e35bf1e31d01059cd7d66" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/chatml/chatml_compilation.ml"
    , "28b7183fd9cece84beae3a54f241291f70725dfab4a44081b7ca8163e9ccc4d0" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "4a4bf26d29069eec620086008fa9883b20102ea425753489ed2f72a3abb1afdd" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/agent_protocol/delivery.ml"
    , "63ae0f4d478302e84a1e49c84f406abf090794db4c231aa76181ad02072581d9" )
  ; ( "lib/agent_protocol/job.ml"
    , "7c470ee027c3ecd23a63a2111227c98030726b58eb370f414c3b6bb85b5ba21e" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "a5447544cbe1cd55302cd70a3c50bf7f6b0ec07ad3af8ce1f70b7b90dfbd07bc" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "c678a807a835e4c91c9afe90d9c8a875934dd05be41a72482f1ea0a9f8215b23" )
  ; ( "lib/agent_server/schedule_scheduler.ml"
    , "37cf371632d8ad86693c162e0593d56866f76df577dceb06ddd2ac5fe5bfdae9" )
  ; ( "lib/agent_session/background_execution.ml"
    , "fc23c982b4655372427f5c81c46959194561beb110e8a6ce848468c460ccaea7" )
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
  ; ( "lib/agent_session/script_job_service.ml"
    , "06ba7b937d1ba64442d886509cbf560c5b80fcb4b0edc8aaa54266b81bca3482" )
  ; ( "lib/agent_session/script_notification_service.ml"
    , "ca17dedcd13940b10e7b530343d61d2cfc8b681cee2d8b03e02498c68644a781" )
  ; ( "lib/agent_session/script_schedule_service.ml"
    , "9af44ed67062da4b8fa546c35d57a9aa627c24775f4d763946aed8ae84e7b58b" )
  ; ( "lib/agent_session/script_subscription_service.ml"
    , "54c775836b551733687e907ff46a2a7a0cc26bd46ff915cc6fd5f0cf5116f069" )
  ; ( "lib/agent_session/session_actor.ml"
    , "31a99121c40ddf3208aa91df1a99ba79e033742d50f92d522cc1ec6b2f6a8746" )
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
    , "f4acfd8251274b5b4044e57959ed7887012f0b7f643d51f81fa4003eaa8dcdc9" )
  ; ( "lib/chat_response/ingress_operations.ml"
    , "d8e2813f75f67ca4c4d3ee6fc264a9731889538987a53d1f80ff27c2c399c825" )
  ; ( "lib/chat_response/moderator_invocation.ml"
    , "485cab4625c91fe5efe73e10be2e97b2e4b065dafebcca5c46580ce5965be9dd" )
  ; ( "lib/chat_response/moderator_manager.ml"
    , "871de11cfb01d4b73f24999ca459c081e629bc00dff942a0742f2100a8cd6b38" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "d68a1162707b73e0788382266af6906303cace72cb0f3c7d54c75162af5f13ba" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "f8d63210438cadd286834a77bb62eaa371cc4f94aea08751aa84a87bc49333f0" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ; ( "lib/chat_response/startup_compilation.ml"
    , "0f60a1f036408809e5618f3310fbc2213e9f7fe99788fb785951bd31c7b96bb1" )
  ; ( "lib/agent_session/runtime_script_preparation.ml"
    , "50d4b2009b20b355e3e1ec64626ad70492e795ff8fcca4b7efd2af7e0283f330" )
  ; ( "lib/chat_response/script_contract_plan.ml"
    , "e87fc20498aa81be0b10adb6c1a05eb36d71e39ad44bba43540f9fd2af829e52" )
  ; ( "lib/chat_response/restored_programs.ml"
    , "329f2747454cc915a6ac3713fbffadc46d2d71b925167a3be68c0aefa9617fdb" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.task-effects"
    , "c1cf4990803e28d10498ed66666235a14bbea0004d2a18f5db4e0f26d05b05b5" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "88f2928bb7f73a55208abca6c16ac9f79a3a0dcbedbff5f4f71f35e202b4baab" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "daf3a2479e16d3753c0ed15f7fd3a412a4d899a796f5e3ba703878a722ae5517" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "a5855463fc7c1128ab285f7ecdca5f7e42881aee4999f520ff5c46f90ed0d5ea" )
  ; ( "one_off_v1"
    , "runtime.lifecycle"
    , "a67ce849e551d2fa0cb0e8e48b68ea785491c387bd9e5f72233a254f51dd2df0" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "80324e905d70c12830e0f248409b82e8bcf37167cb23c8d3199fe260d8676d46" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "24a069cda2f722ad624bb601d83730370f941f27c2d230aaed6d5619f737e19a" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "2695de70fc838d575dc80dc2519ac3e9294d5064b6b29487254dd74ab85f0441" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "340b985a3812eab6a5c7d4216d2938d7b64634e5daa4bad649f3393d0f425824" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "10a26428101d4b8d9fc7153ad1f4c6d71f447bb793f16eb350faac37abe3253e" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "c3a3be3c526186a752fba220aef782fee85e6fd5b98606ff1eaf855c37c78d6d" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "db2e344c7fdcccc4d7d2770064549e9e49f917c7e47a2d9c216956d7a058d6b7" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "3f1c4db718bfdc3e03027580f9b0a170a4f3d594d7bab931428ab007b24dc550" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "68252d027369d9a66dfe40d8497bf19a04eb55ccb54c531824517a470e02031b" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "ad26648802cf6b51b616931972306d2a432d859e513e085cefc47a5513e63832" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "5c50925ebd661746c9ba4f9756b4da654a33d811734c3a947bf217f94d20018b" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "51beaf6d43c70fb00bcd5eac7cc654cde21eda4777d3e6a7e94c843fb8359c87" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "8ca93f0e1f0ae9673bd9a3f7c457e01462bada2cd3b10cbe7b63a7728acd2f83" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "a8db44a78098b4a546610c8dbf94eed685dcf677fb8e7f5a638073c9e428fac1" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "e0a7aa27478b30258850e726ff0925ae6cbdc371c055ca1b8dc9524acc68c1dd" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "ab3f504ae2511561c3cad27299fc240a3afb7330fd8efcbc4b813222c9f32b58" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "65f557e4f4aada0975ab37699ece149315369fdf3691405431176da763d8b2fc" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "26d9db7b2d54ff0e2fd2868bdca00c7727004cdf4133272a805d06bd78344667" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "af07c07d4a20786fd574b1cc9705aac67fcc7117b5357340a3fbfcf5a84c697c" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "b47fc3352107a41cb159d488d24ec80f3e921ad44ba524492dfd8afb7b8a6fb7" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "65aa1e3643a89e716f1fe5b67f594705d46b1d97189950395fbf9141c62531da" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "6020e3af95186192d03cc1285cbe6c9d17814dbd345af3e7694c7a3f91dc22f8" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "e271b2f193850906ebf99d4a2a627397013f0a6cf6c2e893c170ff5dc09aff78" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "da84be79ee1a326c0fda257e70f2ead1ba46f8d238f9cfb3a6c1d3cbecf54351" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "3b90b54e724c2cb60fbe4858883b79d7097ca701be53c7567bc47997a49d4dc1" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "3ff85262aeded3aadd288fcb8fd4380c19e50fd7a6b693a4f6406778ea228fd3" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "ca0501b384f586fa4b964516788be6011699ba9a0713dc905d99823a902f093e" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "761e23e11157c804a6826cc2b3b1840822ead762f29e83bc83d42071e697a2f4" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "de0425923496cc60a4ae9154dbd1ce46b3fc49e410aff416da274050bd03ffec" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "bcc353c462d5202fcdf09a4e044bc522b0e2bf2cb5adf67dc8790be01e9c27c6" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "fdf1d2138045e66e76fda591b803865e296cd3784bc04a90a28567edde1057f1" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "29db55509e5dd7c3125f1b396e72df66501cb0a111c46445d3162f11b5c19710" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "458f6ecb7fb530a78717de9fe2570262941033956f8cea2e4c45e2bdd5318387" )
  ]
;;
