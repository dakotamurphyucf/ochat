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
    , [ "lib/chatml/chatml_compilation.ml" ]
    , "runtime.execution"
    , [ "test/chatml_compilation_test.ml"
      ; "test/chatml_captured_compilation/domain_tests.ml"
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
    , "057abd58ea8c9513c836e36dd87ad664a01bf76d0c264ccb3b61249e2a9c2763" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "26e9c0d0374407252f65f8fb8f7439652ed2e181db63a4d938af8f7d74eb82d4" )
  ; ( "lib/agent_session/administration.ml"
    , "294e4f4885be914c82f8fd9b327b9dac9a311ba6c49f3c424cdd34f7c9a9f54b" )
  ; ( "lib/agent_server/command_handler.ml"
    , "5f3054bc7a800289757ef23127c329766fb3f951ef0fa36c2db225f35e926477" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "7431f8254c524a641f3ce86633cd812481b938ba3b6e35bf1e31d01059cd7d66" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/chatml/chatml_compilation.ml"
    , "f15466510b48217410039497a1d57ed9d83030c445d5045deb1a1638ee7f23dc" )
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
    , "d37f70ede0b3d14d3b55e41ffd50c1dcf27e5a295d5fe207e36054c0ca75ecb9" )
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
    , "3c9e62b0f11666b6e5916b21c5b3ee1c8a2a3184ec9ffc4066927dee3457f44e" )
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
    , "c67622bcd7c0da613b5c8a6aca7b8841be9ada1dd9283d6e83e14c70b3d16d05" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "81dbf7214fbe3019736f5d2befa068416665ecf53a08258135672b7aec2d67d2" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "bfe0c6b1ae8712e412bd761057b536aba3c6d954226fc39a709cc9c4e715d9e4" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.task-effects"
    , "873b9f154db650e5f77d473666fcb7da18a81da176549fd52eb2cde3a44d31f1" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "b631c5850592a190e151230ca8d5f66ca9b3ba26e03133b773d76460e9460922" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "bac7beee30b97ac2ad6f3e370019b195355525483055bdb17641ea5cda46886a" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "3e31eeb00d477c0334d066301d9f028f48f27158629d0e6ddb3e563da9cc13cd" )
  ; ( "one_off_v1"
    , "runtime.lifecycle"
    , "1b66af82a7d01ef4151095336a69f64006f91a258e8faea37d3bb06744e9ae4d" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "503b540ba086ec81acaceedc7ef3039450128b7d499b5c71fd3e369b96c7e86d" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "2d42a71494d85c61b720d5b6906768502b26b7c3fd6fde56fabf5b0d055674aa" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "0b82cad371c32cbb496293d6959a73dc274783bbb3a07b447826b53dc9a75f7f" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "af733859961c7c7ed26917bd07ddf75718931a68c7a95f3a0b01198685ad0c1e" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "3bede757fde116dcafa7a5553cc719f58594f9875540c4befa7a4a399563f538" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "4f6248ef309d76e43b1852f3a22bd4e41291e1ceb1ac3486b34e268db1bbe899" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "77db0dccacab07a6ef766625b325a00a9a3669d684af7d6ef4445033ea14f5d1" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "6a63223ee7ca630bb446d2bfbb8d546ee6e3cc7129b0ad45ae8c652a136312c9" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "b3b290903287bcb8744dbf884851148efbacac31194c6e884ebb42d9e3d875e0" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "7efdf848ebff5d1ec1e60082e711bb7753e831cdcb841839b2da2e856074f146" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "18d5a331f97cb694a68817d7ab1f7e8abafeadcf12125fe3980fb0dbfdfc721c" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "e487025eacee4350bd5950edb733cc5d66f48578fb418380a98ba339e5433ade" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "c3863153425462788cb45ad7e91a5714d13faade7654d273baced41f632b2186" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "8e881cf173793f560f97242317d8dd71013b4e36b005c7a165315eebdfa68a09" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "3d8e23c8979e76406882f01e5a6962ab547af2d565725652c3ce86e1abca30d2" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "48d639f9278365da6cd6247d4ffbb54a3c073ac2750fe93cb799e3f433a87aae" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "b092943b64d89f61f8c63d9ecf17e6c1263746932fac188b306dacba22fb0605" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "7d4949c806d2d70eb45864625b67a1fd2b7415ae79101ee8aaeaa4a6aa2822ae" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "b6dc301edac745333d9d6a74220b6f1ac705f7841b713583df41c9bdf188dc39" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "e148a7b0663855c25344a50f272c572fb0bc92281332e25d6e11fc9374e06ff3" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "358ca8e869d14d96f7781486c087ec6a0a7642018698cfda1a2749e668699fef" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "563221957cf4435a3e2ec32a969ab6b335767d89d10411eacbe7a6f3e9b829bc" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "1d7dac24b1f14b518d3fdbb559830c0bb6ca5b1213adde2bd3e64fcfb6cc4c2a" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "9808375f654f660d542bcf30bea4a774a6a059fdccc048ca16368bf651ed205d" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "7841e8fe97c81820f9d7da8b3f58c51c3b46065fe1bf90c06a603610a71a3db7" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "a1979493b8d961590bbe92633d5389f35fa8059fbc97963b9a4ee0e378447d33" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "3aae7d8d502126fda8c0270a53c8b2a550144666900fb08f78cc20128916afb8" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "728d9784d6bb4ac1eaf1dfba8cb92e72d9ecb1ece3d8a5236ea7341ad6c802d7" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "44f44dfdf8ac00e5c635e3330df8edc58ee9a47e8a0fab098ea9beabee38adc8" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "72f36b078824174c7e1d26fbcc2fff7161a03760465f29745ee7245286746543" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "2d295c1416a8e300b697f3c475f74861572b481367c6fffb933e112682e6a11d" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "9b13892a4dad1b69bbcebacbae85eb40ed439270ff34c00844f204da888f201b" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "58eefe48d7131f17be5fb7fb5de68ae233c712d8c9bb7366f221f752151bc0d1" )
  ]
;;
