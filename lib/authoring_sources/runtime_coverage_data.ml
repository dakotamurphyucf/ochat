(* Reviewed runtime transactions and durable work semantics. Source pins detect
   implementation drift, not automatically discovered features. Evidence refers
   to separately executed behavior suites. Surface mappings describe readable
   contracts, never an authority grant. *)
let shared_features =
  [ ( "compilation.policy"
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
  [ ( "lib/chatml/chatml_compilation.ml"
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
    , "bfcf6b0c9470e38554972b171a8b00599f34881279e609ca6e7bfeb447e10780" )
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
    , "4ea163a5134f7200511d85e6162846fd5dcdf199ff96d61fd6e22da9f352d350" )
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
    , "86061e050c890becd2801c700ba10346398734b0936b16e70c615051b12a57ed" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "d62ab1673edd8f856884ede9c2ca6b56a7a96a9de9b36cb429ff76813d2e4da4" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "d3fa55e108719ff46ead80152891ce7993191e48182ddc77f9b7711b413bce4b" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "bdcb21f6852f20a75c8ba64d64b3fc5970bcd92d0388fa2e8ab0886568128ff1" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "0a80668bcb9bde57cbe21ff576c1022c4640b256fbcbc8d804e3862a7859ce35" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "c1e091b3e849fd8c55e5f23ddf010bcb441c614ff2cb12d463b879cc763d200e" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "f92f12bba346da69ea95a5e3d39eac9eb8df856b9b5c1c56dc64bb6b862247ac" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "67bcbdc0686a7cb9e787a40f92ef5b8136ae17bb54d6ceee8bf2c47edfb208bc" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "9152ce93d805fbb6f03503a7c5d09cbbc4c3580635c747ffb17e126b0143f5d2" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "f58992cc9c62f7bfec65e3381309595e527c2e94c008f0b62a79180082230071" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "d6f287a69874947af3d68ec27d36c8ffa619e7abcb8c56dc3df49a64618e05da" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "55026df9771388448fd1bb1948cee8296419ecf876368532a914bdf3f1e8e241" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "2552bf47c7171a787634b2e27074373824c9884c3bff77388db129bddcfc3c8c" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "94e0ef57f9aa7579622e3eb855349806de28eaff7086002ad738255885a03e9c" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "f4bc2cbe7ac81aeea08efa11372902d763f1828b9937aa3b5644de4f3bdf4b97" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "f19d1ac3d3ed57d7b17fe8ed873043e9a586864d9f14febb5ea7d24a770c7ec8" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "b2bae731da421a9c11317ec5c0667bf0f193239a968ca4c4c6d3fcd0c3e21d51" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "6197677950ce8a9f4ad4424a2bda814dc2f7d6ff992ae4f0f910b06f91b8c034" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "920a8ba68d2fa4e24064e8178c99c9c9577e77e11629648b01181e178151d632" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "4f6e6c33e84bef5b5f8a01906c9c87d51f6a80df28e40181666545c3f1dd4257" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "526dc77652a80915d0cd9a15868146c084fe1e0a8c17590ef6afbeb81d154f3d" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "2f32e9d1e1bd961af032f80f667f681d86949689051289bff57a142dccef7470" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "06f932c4ea6b293bbaa185b3bb7b87d331bb7651ff91f502f34d4818153a5bb5" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "572f2d1fe92e8e07295b0a71a6b9e0d5938e0ec9facd753a5b27d54adb229e4e" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "d5393b298e9abb128d7230c9f062855117f739dfdba314587b5ec6eed5a1476a" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "4bde4b8e9c886cf561b7d46cc37ea94a092d602cedec0ee73ccde04f2760bf60" )
  ]
;;
