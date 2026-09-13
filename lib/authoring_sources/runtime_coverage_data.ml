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
    , "96371496d05b722218b62cb70070c258aa0062f29386b5a6e1853844098992dc" )
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
    , "28c9ca77c1c5a84e70a5044cd91db19c401af1f3c1e93954af0f441cc50b5469" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "0447be9adc54fc764ed2b6b28769c3547c6ad29df40ed878f2bc920121d5d505" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "77e0c76fdd7912a68f67765f7700580dd26cb19a409e35eb9b5ac1bcf1ffadee" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "d0f64c8130b5d8735d68906116248b35455013211731345f37bbd32434a8403b" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "c080dc6cef32933bd0f105bc7911980cc4895d71a7d3281ea0d8ebe6a0abe84e" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "e07808a17df0589a70587d5979545088357114ce4b068fd9eb03c9f18ec423c4" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "3b8eea643ba19cc6b70f668f5a1217454465f7903c7c10cbfc3c7529e42cc97a" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "46eb7fc4b53765709194d2946484c98b926c5a725829ed4f12873c8bbf04708e" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "163f750b9404d6fb55a3b56bc21ecf318fca980596ac9a421f52ae15b49faee0" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "9980c2c4df7a1879a068c79789cfc0531c171b3ee0938dbc96396ca0d5b6f86c" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "28aea58d29d29f552632102a0097facf4c498e8bf22bb820baf10beae049df51" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "3804e5f48ae7d52b1c2fc0db45b5e8d15d3c5143f50a5578d4601b765e8d75b8" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "49245fb3cba48145f0a2ab0a7eafc6a65b0097096b1bccf5172e10ad0c7c4960" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "c99e4f0b59e3c95109bd055247044caf37a8c296eca65ca87e4f48590d1bd02d" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "7847600a60bb5b0f78b2c80b2a4d1e48a364da77a60560124f1781d4dc6b01dc" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "9ec826196e72c5fca73c9735fda0904e448489666a5da48724a3de974c212b6c" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "bf21b1ced680870f95eb8046c08541055068c74784173637db133b41701f0b18" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "e1385090f56225696e8944317d1a75898ba23b87dd57107efbb870b4a5ea30a0" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "4a5cdc079db35fb863b8689a801f6c742bdb3b006cbe0c0cbd8fa44d44fc584d" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "3cd05cd112afb5c64cd86d1efb31167202af37f35be60880db6f9b73b1927b7e" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "c3a779e51410e6c0c2e085c57ce792984cfac015f3fd601bc2718f2bf24bcdb3" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "65b01d26e7233ff3610980bc8b2d348f41e081576c959c3365f01a261481894a" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "41fe23335a1a9727150741cca858542fdba6ba4f884d96f8fe3fbcb80d10f7d7" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "d842f09b3b98bd00126143cd3d89287fafcad065544cd3a4d97034cba630835c" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "17b99c5afbe6e899ac7aca02dd743edbd4e011e05d4b976a9f432bc0033c5119" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "834e9efc677b3b5d74fce2ffa53e34299d197b3acb76a9c4c7d168c8f71ebf7e" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "dd3e87d4e686048930300367946dee952cf68ff7627188d443f4b2e1394b8d7c" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "1a863bafaaf33b494ff9c81ea7579547161cf25f36efe6236210fde2b500abd6" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "541702031a6ec43eebebb44f217059adc077ee816d465483d911ccebe8ec6ba6" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "ef129ab96ef53eb7c576fbceac1361f5147d2fc3de2183c6115ea87f0b66f7cd" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "acce3c2d1203c353dc71c18e8c414d3861cc39c8b70266e8d74cd8b562ff3b80" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "99182edeeb56301989299ba60c705cb21682f0134261fca7c7ab25f424122819" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "d33fa4c93eb0b57d246a9157813e5412774466b579c2215e619437328da669a8" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "9e147b21688d986838a19dc29d4e8fe567e5948e0a73b2209f73659d775a4228" )
  ]
;;
