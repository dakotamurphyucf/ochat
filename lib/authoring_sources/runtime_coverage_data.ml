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
    , "e2272786e56f379f14bd6fba149ca2e822c74a05ef512a87185bdbb518018afe" )
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
    , "1673ac405b3a883fc997080789b72adc0e029c917288b30fda3704f29711a59a" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "7b3111dbb31d0fa06ebf96b20eb22f8b871b9e6b7a07eee411b860b52c9fc11f" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "cedb4c4ca8fac959ac3d20fec261a69af5407e839f4bf567b2f6473948784f96" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "c9f77ff92432829ec4048ccbacd2c6a420404ce6a5c96bd4a7ec9db23856b11a" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "b1021c09f10b36db6a9c6481e33a8bc79f7059023a88089010d145e8e01c1da7" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "44d7dd48addc67cb918294fd3e16f92b1405e83d61e78350418138dd6cc327e5" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "b17693767d96094e4fa776d595c810e174505dcaa7ccb1e42ccdb2cbc3f15ee0" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "f817b1ce1948e6ab3bc0eed668ef1a5b9c281e537ac66dc915d282bf3827a29f" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "d6feb31943b7192430adcaa4e424814779c03853de173375be89eeb8c00dae8f" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "bb669223ca2a23fb3e1ef60c79a05793b11413cad3ccfef05d4ef13839768561" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "ea4f1af3e2e4bb11d90228c55065b9f8b7c3eb9a2c1ece857d815aab62bbaf27" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "37a2b81a42bbc5d34d38133283cfa217a698a26103ab9de8964a0d84a6bc12c2" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "5a4a8e60aa81b0a5f2f69d17390f72b9d077d191ec8a6a6e43bf3af3245a90b1" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "4ba9bb3a372cc5d2b1c301f784e420b53254f18b9f7c6b07d5003c6e58e571d5" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "8b2e5e14d195cb27401cf311b2e14e56bae95a7fda82b64da0ffbd95a13d0e30" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "04eebd5fc10d2a1fd4f82a7a486b3a2b3cd10f20f0bf6f3c5c57b74dfead9906" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "c464b7231a9869fef4d9fef4cc6ed0785d9048e9d9466baf30edfb8907c8428a" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "7eb9efca34f69ba54899283dce95dac9c3e0808c52ee6ad0dab6a9a423c56e95" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "b0a0b4c23609925e2dac684e9ca1ea1cf401b93ef7dea1afa476642f3410e9a3" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "c7c4d6a01f885d54566e4903a7672a63dae6cac35226346ea3206b9105ab1eb5" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "f894423f030a09b8f7a63aa352ce0df75c56cb7c212683f33f006550949aeced" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "4c858b798f55e16e6e8075ed9816abac55dfd5973ceed8c06da5be5d1c2868d0" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "255429f42f29169e4a8a79851185cf092740d808980bb09f7e6bcec886a062c9" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "b9970167f74482aeae47139f66747ae24a13f01526a73554671fec3513464c71" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "418d731c5949f42cea99741ccb2c5ee806f836156047da219062231cefc3e45a" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "c874280f5d6d56e4991d2154b8b648be45a732195abd6aafa5d559c25900bfba" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "c1554a4d4197458c449bbbc75b653e76a581dea0b988fd026793a963712ac014" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "851ff325eba8f3ecfec0014f66de26c907bc54ceaa085834a204f5845f26f790" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "e23e0305e4e31603aa53e8dd00a20455d1dbeb17d87f992a44808a47eecc8392" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "39b2aa11087c5f2434e9706c5d23344d36425a6b9edd8c5f9a4e1b24a92992ec" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "86a241ff31ae451212db4c24b53783b8dcbb3b1a1d4d1372c38d174a132a63e0" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "95f9e80902593ee4a5432acc11d301f9a6c9c81451b9078c49f9a51c299b3557" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "10f77c0579111f8a658dae2d424a36dd77a9d6be33fc896055c923d28786da33" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "85526da4ad08196c16867154045eebacf426f2dcd90a1d3bda2685b779b0d770" )
  ]
;;
