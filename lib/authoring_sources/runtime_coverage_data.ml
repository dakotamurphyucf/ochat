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
  [ ( "lib/agent_session/administration.ml"
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
    , "41b98477df614703ba52f1b6d8e7241514c7f8942dea609565c14ff2ac71f1b7" )
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
    , "9bce55b94e4733d606d1196bb893382580a4d87c85b717cee96c8b3a613dbc5f" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.lifecycle"
    , "bb93b0151cbda17d755f5bc6ac99072e344933627c6c4da4e001483c3efef590" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "f6b0d1db70396315c043c929eaac3cf0f98f98aac16bcf193711e1c2eed9ca86" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "8579b35f090ee78775bb29c4b1d569c1eadcb181adf6cfc5aaa9943a393d6cc9" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "573027719559881c1e897db2298616ca29dca285d8c74287860fd067f5fcdfa2" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "46ba1dea2f78d70fde7799491184e5e8de0b9ffd9afd69c223915b95bafcca31" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "524417b9e374eee831d93c13e3d4384084113b38168a0437ba7e0fd86a943a1d" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "525053bcfeae498cbfb74209deefd6a0dd71a4891f97c1ce4d307c54b8d365be" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "ca6cfe90e20ca626135bddabdd7dfb6421c01f1be28f794deb927c1abadabe91" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "17c97f4545f72112b99e7436182a3723b0a6d49092a0c02055fd00a887e97a8b" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "5146f7248cec485d8765690cb254af5462e44df20f69cc34ad846c728ca88df7" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "01dccf574fb12f2f3342a168894b3b771ac9ce03ba0eb640ebc18774b4352145" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "6a1cdb94a910bab75cb4cb9cf2f974268b70f238ab12defff5bda1de61ec7834" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "a229ce6b33d7758a2f458e73baeb9ed79c7b9c1d7c47bdd1ad3fb1dddd0da376" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "9377ef435e8f32eb876b4f9f08e91f6b7f1ae5cb3579443e1ef45294d9add09a" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "92d2892a3e24240286564e4634b90b0eba84c66fbc73f5f21838bf75282e1bbb" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "71c4a02f5379f2926a2c1749b9397c927090ca3579f918e8cbd3be29a2facb39" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "2e73e2e7d6cbc78ce66625a8d9bf0a869d264ec12e8e6ed7065ba0495d39f2ba" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "3490ac1d20006770a62f5ffcb001a255e438761072b01e3a18e2d1d14d7b8322" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "c73c0ba4509f2117d3d6aebeb34b260b3faa9320973e167a4576d0d648b1960b" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "d267afabc798e06e8eeea137b9452ed25a74a03f2983c4fa34f29bd6f20d867e" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "e5333ca3da23faf87ed9169937970dba70c2438ebc1a4da94d5abf1164ae3004" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "44c177491d84b8d4478bc9898389e7030fbb917c4eb338361d1a4013ec65e91e" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "8dcc480770a08026ab8d1a333bcedececee1ecffda33a7b9d531120d6312985c" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "f18db14ba1f5a0dc2e0dede93b3aca827a15b7b25458f0cab4b81f639229b030" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "ff0c4b1e5d91b5c228014d3f5b5770de8b66d45438d4b7f83557322044d2c8cb" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "4192fbd3d8d9f71079fe444251613e789cb73b141a81b556b554c582fc6414fc" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "975bb72a649de8fbb3464eef4dfe853d306a151401bc175e5c6bdf7822c1ee7b" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "c98e93260bbb581fab57d13b8b807130290a65bc99fd50df3445693de635f74d" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "a38da25b4f21e9caa3eb52d1c36ea9c82e0291746b448217ce796f366de2ea38" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "12198b1031bff86e608083e55b95ad6d4b0b5f37a641188844b2ad85f867054d" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "c3ff0496b7cb3aad967899af73b833c6418b3dd4ce9f415a629afe7790dc5d19" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "4668f58c23837e412e714cb75263b4d6c347471559eadffe81e7721d7de89581" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "ffc0c8d054cd87829a1388392fef50561bd6fb75be3ce775b76f766bb0bda081" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "1c69f63e64392eb221ef39abdd55f2c0c3ebc82c8071f972da2614759b86b1b0" )
  ]
;;
