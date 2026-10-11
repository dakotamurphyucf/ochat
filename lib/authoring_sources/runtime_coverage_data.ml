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
  ; ( "execution.ancestry"
    , "Owned shared counters, nested scope lifetimes and independent session budgets"
    , [ "lib/chatml/chatml_execution.ml" ]
    , "runtime.execution"
    , [ "test/chatml_execution_budget_test.ml"; "test/chatml_projection_budget_test.ml" ]
    )
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
    , "c468c9538044e219520562ffa39265cfcb61630b142522910285bc5ed395a5c0" )
  ; ( "lib/chatml/chatml_execution.ml"
    , "3a67924cbea7d014cc7c26932d7b9ec613ea4e49b3dfcd775fe53c4616006fdf" )
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
    , "be7c0621bb402557f0f8972ed5f6fd9f3bb10373f24a5b0a56f59b0fbe9439b8" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "ab1d916075b51bfd2fa2d3bc85649f43f467274135f754ddcff46506c9ae59a5" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.lifecycle"
    , "db1c69cd2b82e1dc651ebb4d6bd3f251e713e5f41d46f38e2ae2f0735226e62d" )
  ; ( "tool_v1"
    , "runtime.lifecycle"
    , "35bc43e1b9ea56031cfbfbc394089103f39657c990f012614dc4dd21114d3448" )
  ; ( "moderator_v1"
    , "runtime.lifecycle"
    , "9d9927e629beb190acca81abb269b149fec62a98895a44df19c4496ba0055ead" )
  ; ( "delegated_moderator_v1"
    , "runtime.lifecycle"
    , "e1c3aaa7ed7aedb2e13188a16276413e547488619cc322b596704abf4bd5d9b7" )
  ; ( "one_off_v1"
    , "runtime.execution"
    , "dc2555a17c458ebf6d44f809706dc66451bd3ea9ef11e4a8f6ebe63efadc973c" )
  ; ( "tool_v1"
    , "runtime.execution"
    , "235ea35bb84dc59d2d56fc4a7856e00a27a8b1356d16ad2788a4e6618e943162" )
  ; ( "moderator_v1"
    , "runtime.execution"
    , "e27c1caf8ef793c038745124742d1afce9ade2ff4615ff0a6897364e681d690f" )
  ; ( "delegated_moderator_v1"
    , "runtime.execution"
    , "6d920b501feae7e5d13ea2185a3b80e1d9cea87df3a7ccd7b8a90dd4cfb24229" )
  ; ( "one_off_v1"
    , "runtime.jobs.owned"
    , "b9ae2125f3db7caf8088dbeed3d435aa99340fc98d427f73107d278b437f98ce" )
  ; ( "one_off_v1"
    , "runtime.work-values"
    , "37e48b6a9659f75cfee317a7c4bb1f0ec5c141dbaca31030faa28efbc93d94bd" )
  ; ( "one_off_v1"
    , "runtime.recovery.background"
    , "b057f64c26b41219e91b158f8853db7df7cd9141986a8f0566f51ed4792d4353" )
  ; ( "tool_v1"
    , "runtime.jobs.owned"
    , "9a17b34fc0682025ea544ec1925cfa3a71416feefc20d40241fcf23484be875c" )
  ; ( "tool_v1"
    , "runtime.work-values"
    , "d3a500c62e2ea7ea686d8d843062392834c2ac33f6270be900463476e3d9ecf6" )
  ; ( "tool_v1"
    , "runtime.recovery.background"
    , "374ec946a35b7188a36a062a1c3ad70ef743dbf2e9a8bcad3e622b5a0e3e0fd9" )
  ; ( "moderator_v1"
    , "runtime.jobs.owned"
    , "d6ec077b9497845d2b13d13e273fdc129dc8f1abe842e6c677232fcb59c92432" )
  ; ( "moderator_v1"
    , "runtime.work-values"
    , "a9b7f295675109af5e8cb8c52812b4f8f325b907ea81d5d61d9230e047e38cfc" )
  ; ( "moderator_v1"
    , "runtime.recovery.background"
    , "95df99736f75455a45279dbe8cd11a0be0bccf6fc7a79df8042ca441faeef711" )
  ; ( "moderator_v1"
    , "runtime.invocations.moderator"
    , "b092943b64d89f61f8c63d9ecf17e6c1263746932fac188b306dacba22fb0605" )
  ; ( "moderator_v1"
    , "runtime.control"
    , "f0255a5f97a7e3b4c2fe0a9d99218dc770b13e5b6c10a2c3cd0d395b9bc33b63" )
  ; ( "moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "ad55ac1ce31e2b8bbb06ecc9071d8853fe23d72f5f4213ab205d7b88ce4c588d" )
  ; ( "moderator_v1"
    , "runtime.jobs.subscriptions"
    , "4de2d5869468ba6a8458b385e138e0f93608de00ec34ebb052af5f40e9ddda52" )
  ; ( "moderator_v1"
    , "runtime.jobs.timers"
    , "bfc67b8977be05d2eb2dc6c36f8efae34b385beb6cdd402d6a1e0676d6d9b883" )
  ; ( "moderator_v1"
    , "runtime.delivery.notifications"
    , "0b6f7ebd14c60d2c0d154e1e2060df410732d5213b1c4a57bcc408f8693e9a54" )
  ; ( "moderator_v1"
    , "runtime.delivery.ingress"
    , "c61eb24bbd16e1ffbe2f0138575c136bc8df4c495b2a70c5dd2542d30a216aff" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.owned"
    , "c594002c509f614224d366577cb75a34dcacfc43fafeb496c7b8e1cff06d0267" )
  ; ( "delegated_moderator_v1"
    , "runtime.work-values"
    , "49e5a3dde66985c8dbae8d5bad19f6ef454ce7d0a3e918010f54d0b59c7e26a6" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery.background"
    , "87f7cbe9b2c1e14f17f1c10fb60d76a0d12854d8f6d1f09fa118db5cb4d9f3e8" )
  ; ( "delegated_moderator_v1"
    , "runtime.invocations.moderator"
    , "3aae7d8d502126fda8c0270a53c8b2a550144666900fb08f78cc20128916afb8" )
  ; ( "delegated_moderator_v1"
    , "runtime.control"
    , "d6bd94d70deeefec2e931f7bde1d4a903b48034779e36c23876b560edfeecd19" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.acknowledgement"
    , "380b21273ffb3593075e8f26a29c18c792801cc94ab1ae1634d1a81e464ad095" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.subscriptions"
    , "0d95970189ec40358e76029a0de32fa062f2e0ba9e90a2187b2a7bb3533cf747" )
  ; ( "delegated_moderator_v1"
    , "runtime.jobs.timers"
    , "592d3f51ba270dc80a4cb3688b3220afb10633ec08d741644a6d50f7490ec316" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.notifications"
    , "dcccbf5da02b04f5f78fa550eb18ce12f2755c59ce521afa7a96cde7fe4bfd44" )
  ; ( "delegated_moderator_v1"
    , "runtime.delivery.ingress"
    , "31e69c8a360ce8b70e4890adb55305183dfd6183ee1242988cffff640a4e1de5" )
  ]
;;
