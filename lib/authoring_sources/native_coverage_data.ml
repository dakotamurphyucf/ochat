(* Reviewed native operation semantics. This taxonomy complements the runtime
   catalog's descriptor snapshots; source hashes detect drift, not new features.
   References name behavior suites, which must be run separately. *)
let features =
  [ ( "computation.request"
    , "Explicit source/input/tool selection and lowering-only limits"
    , [ "lib/chat_response/one_off_request.ml"; "lib/chat_response/one_off_script.ml" ]
    , "runtime.native.requests"
    , [ "test/chatml_composition/one_off_tests.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ] )
  ; ( "computation.ownership"
    , "Borrowed authority, shared deadline and nontransactional external effects"
    , [ "lib/agent_session/run_chatml_tool.ml"
      ; "lib/agent_session/one_off_execution.ml"
      ; "lib/agent_session/native_tool_invocation.ml"
      ; "lib/agent_session/script_tool_calls.ml"
      ]
    , "runtime.native.requests"
    , [ "test/chatml_composition/one_off_tests.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ] )
  ; ( "computation.outcomes"
    , "Structured complete/fail/cancelled results and preparation diagnostics"
    , [ "lib/agent_session/run_chatml_tool.ml"; "lib/agent_protocol/invocation.ml" ]
    , "runtime.native.requests"
    , [ "test/chatml_composition/one_off_tests.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ] )
  ; ( "validation.request"
    , "Four targets with required/forbidden fields, schemas and actual caller surface"
    , [ "lib/chat_response/authoring_validation.ml"
      ; "lib/agent_session/authoring_validation_tool.ml"
      ]
    , "runtime.native.requests"
    , [ "test/authoring_validation_test.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ] )
  ; ( "validation.identity_and_effects"
    , "Source-bound reports and deferred checks without evaluation or authority grants"
    , [ "lib/chat_response/authoring_validation.ml"
      ; "lib/chat_response/generated_admission.ml"
      ; "lib/agent_session/authoring_services.ml"
      ]
    , "runtime.native.requests"
    , [ "test/authoring_validation_test.ml"
      ; "test/chatml_composition/authoring_repair_tests.ml"
      ] )
  ; ( "reference.operations"
    , "Strict nullable request fields, feature discovery and complete reference retrieval"
    , [ "lib/chat_response/authoring_context.ml"
      ; "lib/agent_session/authoring_context_tool.ml"
      ]
    , "authoring.reference"
    , [ "test/chatml_composition/authoring_context_tests.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ] )
  ; ( "reference.scope_and_paging"
    , "Exact invoking capabilities, immutable budgets, whole sections and context-bound \
       cursors"
    , [ "lib/chat_response/authoring_context.ml"
      ; "lib/agent_session/authoring_context_tool.ml"
      ; "lib/agent_session/authoring_services.ml"
      ; "lib/agent_session/authoring_reference_scope.ml"
      ]
    , "authoring.reference"
    , [ "test/chatml_composition/authoring_reference_scope_tests.ml"
      ; "test/chatml_composition/authoring_receipt_tests.ml"
      ] )
  ; ( "creation.capture"
    , "Captured source bundles, two-stage selection and generated-definition validation"
    , [ "lib/agent_session/generated_session_request.ml"
      ; "lib/agent_session/generated_session_tool.ml"
      ; "lib/agent_session/generated_definition.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "runtime.delegation.generated"
    , [ "test/agent_docs/docs_child_authoring.ml"; "test/agent_server_generated_test.ml" ]
    )
  ; ( "creation.retention_and_retries"
    , "Creation keys bind captured bytes/settings; stopped default and durable \
       relationship"
    , [ "lib/agent_server/session_factory.ml"
      ; "lib/agent_session/generated_session_request.ml"
      ; "lib/agent_store/delegation_store.ml"
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "creation.lifetime_and_authority"
    , "Owned/authorized-independent lifetimes and inherited revocable execution authority"
    , [ "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegation_lifecycle.ml"
      ; "lib/agent_server/delegated_runtime.ml"
      ; "lib/agent_session/delegation_authority.ml"
      ; "lib/agent_session/prompt_source_owner.ml"
      ; "lib/agent_session/prompt_source_lease.ml"
      ; "lib/agent_session/prompt_catalog.ml"
      ; "lib/agent_session/runtime_builder.ml"
      ; "lib/agent_store/prompt_source_receipt.ml"
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_generated_shell_test.ml"
      ; "test/agent_server_generated_test.ml"
      ; "test/chatml_source_owner/owner_tests.ml"
      ; "test/chatml_source_owner/catalog_tests.ml"
      ; "test/chatml_source_owner/lifecycle_tests.ml"
      ; "test/chatml_source_owner/receipt_tests.ml"
      ] )
  ; ( "management.relationship"
    , "Current direct management relationship and generation, rechecked across waits"
    , [ "lib/agent_server/session_factory.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_session/session_management_native.ml"
      ; "lib/agent_session/native_tool_invocation.ml"
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_helper_test.ml"; "test/agent_server_generated_test.ml" ] )
  ; ( "management.status"
    , "Bounded lifecycle metadata without transcript disclosure or approval authority"
    , [ "lib/agent_session/managed_session_tool.ml"
      ; "lib/agent_session/managed_session_service.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.delegation.submissions"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.submission"
    , "Plaintext idempotent submission, durable receipt correlation and stopped/busy \
       behavior"
    , [ "lib/agent_session/managed_send_tool.ml"
      ; "lib/agent_session/managed_submission.ml"
      ; "lib/agent_session/managed_submission_tracking.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.delegation.submissions"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.output"
    , "Nonconsuming bounded assistant output, redaction, fragments and cursor recovery"
    , [ "lib/agent_session/managed_read_tool.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/managed_output_page.ml"
      ; "lib/agent_server/managed_output_cursor.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.delegation.output"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.wait"
    , "Receipt versus output waits, monotonic bounded timeouts and no child cancellation"
    , [ "lib/agent_session/managed_wait_tool.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.delegation.output"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.stop"
    , "Idempotent graceful/cancel admission, retained history and cleanup progress"
    , [ "lib/agent_session/managed_stop_tool.ml"
      ; "lib/agent_session/managed_stop.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegation_lifecycle.ml"
      ]
    , "runtime.delegation.stop-helper"
    , [ "test/agent_server_generated_test.ml"; "test/agent_server_helper_test.ml" ] )
  ; ( "management.helper"
    , "Explicit delegated helper operations share native authority and result conventions"
    , [ "lib/agent_session/session_management.ml"
      ; "lib/agent_session/session_management_native.ml"
      ; "lib/agent_session/authoring_services.ml"
      ; "lib/agent_session/session_management_channel.ml"
      ; "lib/agent_server/session_helper_policy.ml"
      ; "lib/shell_access/shell_access_v2.ml"
      ]
    , "runtime.delegation.stop-helper"
    , [ "test/agent_server_helper_test.ml"
      ; "test/agent_server_config_test.ml"
      ; "test/chatml_composition/authoring_reference_scope_tests.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/agent_protocol/invocation.ml"
    , "0abdfc864f0bd694bf88e898a6a24680fd30c4371d9bee361077f9f12d96cc9c" )
  ; ( "lib/agent_session/session_management_channel.ml"
    , "500d45a8cbe760a50bc256df47255dc6fa5e1305f74a543d49c9c2e02e36de51" )
  ; ( "lib/agent_server/session_helper_policy.ml"
    , "e88605f0d6f0fc512589ccd9546e5f8e741216dfa321ec66919b64c88d2958e1" )
  ; ( "lib/shell_access/shell_access_v2.ml"
    , "e10c5709588bca7998686a275ea3b3620d809ae50f51549af56343a4248f1e87" )
  ; ( "lib/agent_server/delegated_runtime.ml"
    , "b2f4ae578f7dddd08bb5503f16af4a9c86c5b87c2c5250f318a425103297c374" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/managed_output_page.ml"
    , "a14130666a4495de0c35df1c01974922da31daed064f1f7622a9b686b9c38ba5" )
  ; ( "lib/agent_server/session_factory.ml"
    , "853288e33ea236daa68249f336d4e78f2163611da15c6b26922a1601103a6a5e" )
  ; ( "lib/agent_session/authoring_context_tool.ml"
    , "46e1d8969a881efd0a3600ac521dc3d810acef6648410d01b8591c8be084f4bd" )
  ; ( "lib/agent_session/authoring_reference_scope.ml"
    , "5db19890dff8b3188045d23899019afa642c6816f42f4a32b17e41365bf78983" )
  ; ( "lib/agent_session/authoring_services.ml"
    , "ca6aeaf3a1cede3d6642b9b2e188617c9e939375257b00d9a6481dd86d96c908" )
  ; ( "lib/agent_session/authoring_validation_tool.ml"
    , "870a2ca3827229a6d88c87145bc4214929f863605eb76722b29fb9f425658d86" )
  ; ( "lib/agent_session/delegation_authority.ml"
    , "1296afd747dd9317e822ef3c1efbdc795188f27a0cb6f25664b2673ecb848a74" )
  ; ( "lib/agent_session/generated_definition.ml"
    , "f2c2ead7ec5ef439115a19458eece9e7165289b2ecf6f663226011a32112b0c9" )
  ; ( "lib/agent_session/generated_session_request.ml"
    , "90af4ba27a871509ae8b0805f36c008d7167fc748104c7055b7660d960ce6a8d" )
  ; ( "lib/agent_session/generated_session_tool.ml"
    , "cbb070212dbf5e0981959d72d75d619aac80c05471600e55bb33807d8231362a" )
  ; ( "lib/agent_session/managed_read_tool.ml"
    , "7d276ca623c62edee59bcaf76b92336b0de36b10bfcc7f525e5aa6e9faa5aed0" )
  ; ( "lib/agent_session/managed_send_tool.ml"
    , "7ed190e714798a8341ea8bb50d51d3933d3e2808e8f06c1e0ee8b06a4ae0dee0" )
  ; ( "lib/agent_session/managed_session_service.ml"
    , "8f44b1f6f84e85c25a634b91062ac01b03f4864b161a219fc382593f6427f327" )
  ; ( "lib/agent_session/managed_session_tool.ml"
    , "1a05cdbaea789f5fc17b84674d07318bd561bf16d155191d30820944a84ef3b9" )
  ; ( "lib/agent_session/managed_stop.ml"
    , "2240eff3c77b83921d7044632ab60603fe43e18d7fdfb22497a1a5b401c3831d" )
  ; ( "lib/agent_session/managed_stop_tool.ml"
    , "ea5f7852a7a5b0b088419a09978dc9fb845399baa141aa6ca4dbfd0945cd6b5e" )
  ; ( "lib/agent_session/managed_submission.ml"
    , "b7a7c8697aa0d9915c4be6ba8cd7eb65288a7508694a51ef3ea2e1acd4313cc2" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_session/managed_wait_tool.ml"
    , "5cfa00cf26188815090cc4e9549cc4eb64c7e841aa9d883dabd67ac613375a27" )
  ; ( "lib/agent_session/native_tool_invocation.ml"
    , "e53c89afa87268c4db847583e4e77f285535abc4a827e04398b1edd25cdd6a90" )
  ; ( "lib/agent_session/one_off_execution.ml"
    , "946bf1eb9dc6c2da12f46d94f973a9ec7a4c11873b6ffab1424dedaeed8c6e7a" )
  ; ( "lib/agent_session/run_chatml_tool.ml"
    , "228d10f6edfb0a5568692332653ac8136d10588eebebbafc827226a6c122a079" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "5833a0b3237977d9338b78eae322dd8d2f77bdd85b6297ce071a7ea142921ab8" )
  ; ( "lib/agent_session/session_management.ml"
    , "18431c4fd97a2d24a72b927fe8f0d20b8aea561f1f119d8f122d943f2a137f86" )
  ; ( "lib/agent_session/session_management_native.ml"
    , "8ccb5239373707dacbceec1ce9a0c5146eea5360921ad0a5209bcc2928bce806" )
  ; ( "lib/agent_store/delegation_store.ml"
    , "2cc26b66d02a224c8dc449a3a85e17f2d33dc87dfe933a4def27b2f2c4f44413" )
  ; ( "lib/chat_response/authoring_context.ml"
    , "b9d759b7df1c3dfb613756547325ad8b52082b571cc5e51a7a03c6bedbd009b8" )
  ; ( "lib/chat_response/authoring_validation.ml"
    , "71b6c71022bde3b92bfd1954904fc516994532e938b74ebe0fb3b2f6ca948694" )
  ; ( "lib/chat_response/generated_admission.ml"
    , "8c422ff1ad004059d86a2160219fb2a5d493899dbf780d6f77ba7c2f52548add" )
  ; ( "lib/chat_response/one_off_request.ml"
    , "7427759464798805cd25c014bf8e1bd903066fea5c3678d806e3c64ec3ee14a3" )
  ; ( "lib/chat_response/one_off_script.ml"
    , "cffac7c805a03e71b84b168d7686b0bac019db7c4d431210d4560331a8f43986" )
  ; ( "lib/agent_session/prompt_source_owner.ml"
    , "88d5ea0f9cb37759181b3de8b908143989f607abbed09afbf79ca226248f0e29" )
  ; ( "lib/agent_session/prompt_source_lease.ml"
    , "22e2cb161139adff927ecda4b3e8a31d181ffa8d9dd193707929a3d3adaed744" )
  ; ( "lib/agent_session/prompt_catalog.ml"
    , "f8aba98abcadeaf44ef61e261fa0b579f88d1fcc44242d0aa5a25abe2ca9f2a8" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "3050c2602d6d961fc01f1518383a382c336f145409aec18d1d860a3ee46398b7" )
  ; ( "lib/agent_store/prompt_source_receipt.ml"
    , "d1089027dc9f0e69fb6db67815b2de78086d88a29334ac7c12ec6aa9f9b8258e" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.native.requests"
    , "595e1f3ab2d1b3c9c1ac5ae5e38b8741da882b4d49661ee7facf6922700300e7" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "551884bf0e3eec7b15c19abef77502c75744211a8a51494ca79cf603e9c56e97" )
  ; ( "one_off_v1"
    , "runtime.delegation.generated"
    , "0cdfdeedc220d149cee7ba02246e7c5fb388264cf135dfdf1501ffc376781030" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "6c1a68d59a4c2933fc6aaa0afc5df7da18c1ad47a4384b59d22b6809b2a9bcd7" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "4b556471c6543ddfae926a1b69d512160e35ec23e4460b7c74bb719a0be11336" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "a07e016fb496c3e31c929f3d54db1f1e7f9349df5488488a28db97fdada7b31f" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "397e4424ad1d1bdfabd6258c1c21081cd32b8adeab7a7f637c86bbf4deee46c9" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "32f0e7669440255557f6b59e829c3d7c34f13e54fe38108d9d32cee86db14b1d" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "dd874d6d66ab4fd587ad87e581c41eb6600770a503fd9194ced62ae506e0f4ca" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "84a7d46a8a9c74d87a70aa981f7b8ed0f9a0fa298cac4858012f7a6a6f72eabd" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "9af53241e05360c3117cd3ceb9999fc8868e2a753b46ab681136d2fd4f3f85b5" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "3cbc58417f4cd9c017304d4b912cf32e029ca9e4244e3c476a3bdcbf3027f582" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "b9c516844a1353f3e159dcee295f2671d8fa2d1bcbcc9c1c3d67174a49b655b5" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "e9667080be9c8ef2a7907c75244d35051a6084f2961bdf268eb1a3520fd10873" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "8569e95d1b54572ff8b169850665483250f1b07ffbba1928ae4c834ee04faeca" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "ad8e067d874c286eb4b067084b9558113617985ba0a190284290c15c583c3294" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "291f73d56a563fc0fc36a6219be5bee0a5aac7c78a139599e38328801209fca3" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "5c97c7a3c3931dfd06c674e3cfe7a85d04af41690b92f78ff9f1ed0112804175" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "90e176e59c0748e76fa807857980d78b39ae4e9f162da7384e0535337455df41" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "0299bb275624401d3e232e4814772d7ebdd2638fef377a68ac63b1dd106354b8" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "141b60ce65e5119e6c7b92859828b0e43e1f6c242325cf7b59c020e3361481af" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "92424def68b5e82d70d9342959545b9e08bbbd3098f198a4086af88bb8d44e79" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "4abe98ad1b9f860a28a97775f1c35c2e232f41f4c8c93e6e3c7ceb78ba38bd40" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "729e8c4d8b577e86d0825bb0188467675460b23bc88353b632c773ba10733478" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "b41be74c8bfd0f0d9c4b12cb59600bd2e5d9dc4fa5bbe460de4fded703917693" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "832cb9d7b8d97c7ba8c8bbd9c895a266ec24d4bb87d5c022eacf2241db28312f" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "80640567bdc99b6978a72dd952ed9201653d4e7c0b4284a9f820aef787bac38b" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "aa1f967a6c9ac11716916d8d02a346cb550c36e183e9a7f5f4d49f2c10ca0b62" )
  ]
;;
