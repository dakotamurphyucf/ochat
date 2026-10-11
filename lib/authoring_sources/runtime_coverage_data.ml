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
    , "43d5a835e68b1886aabbda856fe4277554df96ba60918957728bfe3647855fe2" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "ecb14bccf8bcd9bc2553223084615522d0eaf157117a703d57c735d4ff6f1ee4" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "528847b1d259988113aacc124f1aeae71a51e0e46c2c2e40b8d4eaa30429a90b" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "1e5514e9d0b586f8989aacc73d81167630bd16c643afd242f9f030607fa8c695" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "f66a8ef7b345df1b4a0e84d3308edab4cddc2dadde64dad7e907359ad7e8b92a" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "44099dd3dd90b45536f936e4ecd86e47d050d81938a9ce52c5395740a984edd4" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "204413ccec758d8d92c48688715acf29f577747add223475f3ef3b40a32c8a76" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "0f2abeb1b11b982735a6d6bd6fdf10c7ee19a0f2676dfb729d6560567c8c7252" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "edffe1af2315159719f4e35af23ec2f3d9f93caa22b177a2e21d10e52ae472bc" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "e8a8c652bac673fa16993cd2e0e28a44335d6944c3a01e3fa9535115d14719aa" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "53a815fc6bf284e1e7cb211757212c871b36a812eb061ca9c491a9c9d3685e56" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "1ead6fcb578c7a261950af17461014d871b5c1284ba4ba2106b8fc44c9459440" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "2cb45710642a1bb860e06d0790951ed98fb8d0793ca3ef162715d54e625dbb54" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "a6c339e1c017e621a16bcf9523032d87370b9e2f17f6d3d087f1e9d63ea8741e" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "cde4a67e0a9eaf5581d05247b6551d4eddace6760466455c54a35050c20b581c" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "514d13ae4a14bab88daca89c8771c7287e8c5fc82667983feb6604737e869454" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "ddad80f420d08a51a59ab694b1236cc483403f3816f3cd457a3b8c7db6699966" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "7eb9efca34f69ba54899283dce95dac9c3e0808c52ee6ad0dab6a9a423c56e95" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "46b19a4447d657e5984f2e7a5a96198c9af357d9617f10000b9221b52254486e" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "f2522b045a69b48f5f39373dd500b68ee0087882bef7ecf643a54860433cfab6" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "87a524231194ecc2a59f6166868ac0d07466eedf4f13ecbbca6fd5199242364a" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "407a7579761da2932b9edff59225351efdf1d2ead0e591e9cd6f820b105196da" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "2e58951888f7c80ebfdf7d6ebaad9aea371cfd24364814f5974ad435e5c4af7d" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "477dfbbe1bca357e7ca6eefe13efbdcb3e020fded967a4773a1c2dc8e3cb3990" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "f004c875ae31faa841e92f19fecab1d22ddb603000de71264b5c6d2260251d9f" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "782c86118d8304d03bbbfcaaec7a66186113db5fccb6c8d53be7b3706b0b1043" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "03f5865e530f23962a285093636d76717e9ebb268d37b621333ba86828e81da6" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "851ff325eba8f3ecfec0014f66de26c907bc54ceaa085834a204f5845f26f790" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "6b0a9fbaf6b42d07a29af0e24b8c1fca07fc2a4dc16a5260417196ae2cc2ddf7" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "3b3bf447889ee0ffab22553b167814532be2e919ef664e7887480ea57b34d1e4" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "8df586caa9a48927f3f73324073d5b741605a8377e851b314916e4291c934590" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "80889328055d012de7a751b16469329b249b91a779f0e2fba1303b84f8b6c9cc" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "ccad9fb4aa894764be670307ba6500cfee90a7d47f96e064d72a352236725e3c" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "cff23a9b42dda20207080127abe770842c188e27b3fccfaee617f940a9697d88" )
  ]
;;
