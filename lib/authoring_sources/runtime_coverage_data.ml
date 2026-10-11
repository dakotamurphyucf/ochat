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
    , [ "test/chatml_execution_budget_test.ml"
      ; "test/chatml_projection_budget_test.ml"
      ; "test/chatml_rendering_budget_test.ml"
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
    , "5b5f2a27e8615fc45c8050c7015fc36b33d9f4314379496ff7f1c5683d2efa3b" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "11a8eb8746bd224a805ca361bffb90aa63cd0f743b5150a0f847619063c248de" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
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
    , "2acb0161a22fcc03eeaa7b92660266b0010b1733d7fa595d16f79dfe3fa2c52b" )
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
    , "cf554656aeb65312d8779fa37ecfc2c537c48f5c45ff73c14c5f13ce6a61ef7d" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.task-effects"
    , "9d6300803ab1436dc7966292efe9fd480da9f88ba16b3b6628f277844ad1b1d4" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "404d44401798111f957a89c1b75df83a5c0f3d947dc1c1beb306d6563fed8cc6" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "62a241cc37f085faea82ba0410f98e1c47fcfd419a71e42569e0059de1a03d00" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "d1bcd0d98459506595cc334c65bd2978114c072cdea2da7f540b0b3ca90dee05" )
  ; ( "one_off_v1"
    , "runtime.lifecycle"
    , "41d3443fbd96b41b095ef64b9cc8f25632d2121e2118010d609b162ed9b546f6" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "130d884a14914e2704006085a96db39655e6c5a9fc03ace4c576a8c1d026b314" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "05a861fef4c04d35e7bb066220568e72b08a560690a096c67233886f6fb07b78" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "f7b2196ab90536420ba60538a9ade7742cc61dc0146450f26ca636c38e23a1d7" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "a2065201517e55098fb6430f6a18a0c15faac40407717626577abda52b2ca9ad" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "dc936fcdf39bb4dba8221ae7a48195c50681ca909e9e8893874a117137d6131d" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "d7bda3040f6381a498f05078fbc7943135845cead81ed02fd5ae1a2829114f9c" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "d6f80254ba98557310cea1b7d82e5d3704cb220a9974fcdbc7287bf9587d03a4" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "5418386c8ec04d365e6123f7bd4ec9db7ca3b816b60c7cf04f0fff9a6437d4f1" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "7a5d1b961c8efe88e1dad373b23529d58d39a9a462b6ef0f6d7c6424fa939701" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "3f97daca57a5b098664f55b2624b6a479500dcfeaf6a53aca1cd589c2bc5a489" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "db4685324606fdded7a6b9532885d4ea562ac1b7831e934278972c8038fbac4f" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "7ec0419fd132071e9eb8606ce1bf0f642037a27c55f9a082bf49154008abe3a8" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "cd3aea92757cebb6280598922ce2ee9475e1d88983c54ba935717aad2d02cc77" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "e1eb8092e8e018b70ccd1d35f0d3c9ec0f5cc90e33dcd4e884e9e6f62be17ec4" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "606175091a8871879944453b036a53374fba48e631b9a30bc12041e410bac95d" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "6f819c2df845d58f485b91e7bb6792d2bd6ba136b336cde0117f8268f8f36828" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "3490ac1d20006770a62f5ffcb001a255e438761072b01e3a18e2d1d14d7b8322" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "75566ac1f967696a4c44581d9fe1bc17d7af85ccaa69ac2bfdc551473844dd0a" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "d64ac559dea3a059adcb8fd7b87b13a3c58d219c9f051eb2bb497e8df8043510" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "1067915488f784b2084325ee253a880caa77d89ab5243ce9be7cf1f9a48a1f31" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "a1b695c26e30e7ecf89441f4e6f2a03feabe4422d2d354589d9965f688dc1be3" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "f90cc9b2d071447c0f4d0cd7f5027d96787b0b282b3d92f73b8ffa8b29bb2428" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "c4c9dbafad4c9aa774d24a0014b9ba778d4d5be7d923c0dfd551d1812d2924d3" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "7a492dec3a736b3f0cf9a49d218048c113cabe11770d3b16ccbc88917a2c72b2" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "81fe46e969816764d9a871fd1a4e6c9c8622046266e34be26e4db32a1dafda21" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "7e41919db44ad262844d76e8a4e342bc53cb36c480421863404b975aabb511a1" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "c98e93260bbb581fab57d13b8b807130290a65bc99fd50df3445693de635f74d" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "cf3dc6b2c991799efb8784f46b96e2a5e547e667443ac605f5daeeb6dea1ecdc" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "de69682bba4809f1f56b045208ae605674ceb1d8bf67add895ef070b73f1c6bc" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "e1166b34d9a448e53a14db265af487c7b26af098642230af24e12b94ec6b21e6" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "07ef274c1cd8a7aeb726f2ae5806f8660a33a091527231f990e1a862f35f0fee" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "7e430090ad26d60d1ec008893ed5b41cd67d862087001f08422709eff2539468" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "fe2d6c7afeb52e0601aa8333b3e7b56ddbd70443930c4a6ce1656fa869de70db" )
  ]
;;
