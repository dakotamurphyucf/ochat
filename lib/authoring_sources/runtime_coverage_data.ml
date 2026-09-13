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
    , "No-effect compiler domains, explicit policy and cooperative cancellation"
    , [ "lib/chatml/chatml_compilation.ml" ]
    , "runtime.execution"
    , [ "test/chatml_compilation_test.ml" ] )
  ; ( "execution.ancestry"
    , "Owned shared counters, nested scope lifetimes and independent session budgets"
    , [ "lib/chatml/chatml_execution.ml" ]
    , "runtime.execution"
    , [ "test/chatml_execution_budget_test.ml"; "test/chatml_projection_budget_test.ml" ]
    )
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
  [ ( "lib/agent_session/administration.ml"
    , "d63ea3ff1e6e3afa5aa13a85069252dc417aec8a1a5e8fbd7b6aab9666b409d0" )
  ; ( "lib/agent_server/command_handler.ml"
    , "b5d00899cfa47677fe24eeaf280a14c3870e22bbed5484a2b8187b541bebe618" )
  ; ( "lib/agent_server/runtime_owner.ml"
    , "3bcaa009e3edb797d59489a087f7d4010ad22f79efcf40707f4c10f0ace6becf" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "395c05cb876b720af5c416d65202281e2878c3fd8732cebf56263d4f1edde0e4" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/chatml/chatml_compilation.ml"
    , "5b5f2a27e8615fc45c8050c7015fc36b33d9f4314379496ff7f1c5683d2efa3b" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "e2272786e56f379f14bd6fba149ca2e822c74a05ef512a87185bdbb518018afe" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ; ( "lib/agent_protocol/delivery.ml"
    , "777e3e1c569d328959909f4e174052ab9b53009fd3d5495628fd0a3508777b0d" )
  ; ( "lib/agent_protocol/job.ml"
    , "e4797978de6b8fd8caa68c09944d9b41687cefdf81c8689fa10b83ae351d29b2" )
  ; ( "lib/agent_protocol/stored_completion.ml"
    , "b9ff461aa95893621570bb499e1b676d2cebe9828c6082a53c4ae22de15cd761" )
  ; ( "lib/agent_protocol/subscription.ml"
    , "831e619d9c757f4b2e6de7e3c8c0c04c423cb289b8bfb4748dac69fd5909718a" )
  ; ( "lib/agent_server/job_scheduler.ml"
    , "f04a2acf1d396d9263f6c5e68c2d89b7ff436315394710da9ebfd5f75319494a" )
  ; ( "lib/agent_server/schedule_scheduler.ml"
    , "707e2c688bb5dec7be852fa5654237f9c512d1569b3513eb849bcff628f8bb31" )
  ; ( "lib/agent_session/background_execution.ml"
    , "fc23c982b4655372427f5c81c46959194561beb110e8a6ce848468c460ccaea7" )
  ; ( "lib/agent_session/background_job_event.ml"
    , "5f5a20065fb9652005e4aadeaea85d01f271aff9674a71a318bc9cd6c34e0692" )
  ; ( "lib/agent_session/external_ingress.ml"
    , "a2a5982bd2eb65846d97c46b56c706018b6c63293adf01fafd494bf0eeecfe0b" )
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
    , "09c0101eeb19f6c8bf0adc29f2c0ba952affb3bc3eef01e45c25cd1e5ba7f976" )
  ; ( "lib/agent_session/staged_jobs.ml"
    , "7731a0fc9f021188e3856041fe493f3044556bc7a15364815c47299fe968603b" )
  ; ( "lib/agent_session/staged_notifications.ml"
    , "8cd8537e4aa24fe231cab7d2b454e67fbf0cde3a4a20d392206f32cf1f8db14a" )
  ; ( "lib/agent_session/staged_subscriptions.ml"
    , "9c48fbd27c5c8889bff5094106e49a77c0f3cf328023370c9e52877421216abc" )
  ; ( "lib/chat_response/background_delivery.ml"
    , "ad8536c2f2e126c03e5c6a6f782b9dbaa3b7420874e26236531abf1d2eafb1d6" )
  ; ( "lib/chat_response/background_job_operations.ml"
    , "86e16c6df7d1eafcb41cb8b558318f7f5b310f9b356afc2412f0e0a32055fcb5" )
  ; ( "lib/chat_response/background_request.ml"
    , "f4acfd8251274b5b4044e57959ed7887012f0b7f643d51f81fa4003eaa8dcdc9" )
  ; ( "lib/chat_response/ingress_operations.ml"
    , "d8e2813f75f67ca4c4d3ee6fc264a9731889538987a53d1f80ff27c2c399c825" )
  ; ( "lib/chat_response/moderator_invocation.ml"
    , "2acb0161a22fcc03eeaa7b92660266b0010b1733d7fa595d16f79dfe3fa2c52b" )
  ; ( "lib/chat_response/moderator_manager.ml"
    , "60bb964853a7318838190d8aafe61c8a5611b4441c8e46e881de544068e4b6e2" )
  ; ( "lib/chat_response/notification_operations.ml"
    , "1e67db1e69921f11f0c28798e340e7e23dc5436eb492bb79a12a0125b0f69e29" )
  ; ( "lib/chat_response/runtime_semantics.ml"
    , "f56c964f2c885a683f0c87cbf97a8dc638e37ed9ef3dcb0b8515efc0b235a83a" )
  ; ( "lib/chat_response/schedule_operations.ml"
    , "2fd61378d934a8ca08127a0522f8bad4db9940d5d740cc4b29ba78be2bbfceca" )
  ; ( "lib/chat_response/subscription_operations.ml"
    , "88aa4ac9bd7c2100a9614f8b30639c6fd30e7ae0c11e86e423a57badd69c0603" )
  ; ( "lib/chatml/chatml_host_runtime.ml"
    , "9bce55b94e4733d606d1196bb893382580a4d87c85b717cee96c8b3a613dbc5f" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.lifecycle"
    , "cccd4f8959e399e2323c3cb03610b89b624d7a6f770f70d665c78bf56ca0fe6a" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "91bc98a2a3610925b335c235c5bf195b14ba45037381327b8b892d14671bff02" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "9b23dbef29415d385ff4cbada5df11b0ef8267976aa5b6b71cb447f339ef2b38" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "54df141bb77ffb7b1088dad7d2221496a3020ac02e7fb6291c3b998521b2238b" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "ee959d78220fa58d10fd56ce306f87828f5ca4c83a6dc0ad5203e43231717956" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "658c847f5e0404f5dcc33466d454167d371450b1273a2587717a23fce425851f" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "d0b2e26af0736a391c85b091971d3380710e14c3b126f609a17f39e391063f4e" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "7fd227a7455c28d6a847dc454f32df38563643aefdd0e039e2f48d1e16ed1618" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "61cc6553ecb8b4278b6d8aab419a43be25209835632f90051581b3d29f0274aa" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "14122ef8915f76e77b6172a07fff41ed399a384869197f64cade9f3429bbec8c" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "991622524c3de3e9dd41b11b44fd7670e2875318b12fb6d81a6f16df73cdbbc4" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "6510cab72c9e521d4f031ea1627ea7c6ee660b9a384473ad0cacd74526fb1128" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "2879183f3da7e9586e6f835fc14e526be1522123cd9da755fe9f43935f9a04b7" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "561d5e610ae95867a83f689048195b56e8530a8dfb98763bc8d75cea7c0aed7b" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "d9e2b0a9de443d20a9ea7757971fb171cb580a6358dbc7b6320a7d2816d1db45" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "71f95fab1303f5bb4c6fb5209455699087a62c2eab7484ad277aad5284a0e3ad" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "b1b417f643cf2af063a0ff046f900d066b5c3790ea4d3adf90d409780760a6e8" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "3490ac1d20006770a62f5ffcb001a255e438761072b01e3a18e2d1d14d7b8322" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "6449bc4e139f0a07c8fa0cbbecfe32da19b9f2698e8112115e63aa5c35b2f192" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "36441fde8b5cd0bf7cc3130d14f1c7a74c39afc011032e5b18223a29c3143384" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "d73bcfc93b161f370116191d92d0d46e7bb5b6f7e696f90f11e5641db7825183" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "3429d29cc6694035ae131c370a7435ce246a725d3ff98d6e1cab3b3a0e87ac5d" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "e8fd6f16a399063a821599cc2aace314f00866393bbdac5fe56d1718f71e7476" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "ebc730dd34dfd29de8cd9b6a0f4bca837b44ed75fce80479c5b33eb6a72f1f05" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "a67520428232b7c5253c323384c7934ae9d336541a5e26ad49d46e14251c2020" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "ac7b68c8ad7a7372ba7228e0af20701d2384b4b7aaf3e4e7f6dacc6a7aeb0275" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "179752ab98cbce17b4fd47ee673b18aeb1facd6e26b4743fe9b3eea70770d5d2" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "c98e93260bbb581fab57d13b8b807130290a65bc99fd50df3445693de635f74d" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "d72e04972d0d87d19f13099bc6fdf9fbcdbc47bd6d6333d4648d9e7781a1db1f" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "4a7c91f7adef4de7f7456e33f26ff1ddf0df14147ff204e5097ac4683b8e28a6" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "fa11ed1573f77ab9428932c107372e5bb9d5899739092368797d73c2d0640470" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "16b1181647213172b0ed5f590c154165ec91ed48dc7134341f4a9c209d05bf7b" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "3ce94476b699d3d11a87d35f6cc1d35d6ceea851166264d83c0968809ca5d1eb" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "36899670e3267a9171d32bc71833962a59ca6af5c7fdf73d05a378027dbb5d4b" )
  ]
;;
