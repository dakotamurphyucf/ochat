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
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_generated_shell_test.ml"
      ; "test/agent_server_generated_test.ml"
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
    , "f50c9b6f8725b0e6d1f7ef88d993a19c507015c23a68817890f8f391f3a98032" )
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
    , "49c76ab4b8409a0daaf73653cae2d548a4340e9424d652bb4101f2af5532301e" )
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
    , "395c05cb876b720af5c416d65202281e2878c3fd8732cebf56263d4f1edde0e4" )
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
    , "50720d22460aca46887a47bd6b807356805155fbec6bbbd0020cff3bd06e4277" )
  ; ( "lib/chat_response/generated_admission.ml"
    , "8c422ff1ad004059d86a2160219fb2a5d493899dbf780d6f77ba7c2f52548add" )
  ; ( "lib/chat_response/one_off_request.ml"
    , "7427759464798805cd25c014bf8e1bd903066fea5c3678d806e3c64ec3ee14a3" )
  ; ( "lib/chat_response/one_off_script.ml"
    , "46b9b6627a8a2952c162e86ecc725067d177ddeae53546d575fcffec80bd508d" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.native.requests"
    , "4e0611511a4a060a1d9825bc414f6227b81c7129cd30952150d75e458249da90" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "551884bf0e3eec7b15c19abef77502c75744211a8a51494ca79cf603e9c56e97" )
  ; ( "one_off_v1"
    , "runtime.delegation.generated"
    , "6318c6d6fb266000bfb3a3228fcba731dfccb62526af5a80a1252f8aa6f75041" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "2ed821ba7b4b9497341ab4e64c03289a61c38ade071a88b16566d8addf86e010" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "e38fee8056921f7a06bd602311b64547b868c0d090613c35a6193d4709c51706" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "f7aabcd1eebd7738793f66946fe98830f1a61223db9e4323539b444141ca3072" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "a64384e0c4ca6fe44f3ce2c2f7a3ea033f49ea46a3ff30d38e6585e23517fd99" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "02325a15bb50a98d9798c025a0a1b0e03d2e13f69390081944d9edef6cfcf5b4" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "dd874d6d66ab4fd587ad87e581c41eb6600770a503fd9194ced62ae506e0f4ca" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "c1d475fcf26295c9847590003f6023869d39dee18d88e692a33e2a8523509254" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "c8219b0b7416ef7cfbebccb75ecdaeed0530e5be52541210fc2dbcbe06d7ce80" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "8b5defe4e99599c165d74204329f47881aba6ec9197c9950de1576e62fc67d60" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "47f692c2cd1e676ac926bb482764a240f81f5158d67ef3beb5a8d9f6a735f851" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "60cac0056f315eb3a9e0296d33f8ecf62e66b6bdc5748a9372a17310eece3f00" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "a53edf3b450b1e3bc159be090d9dae6bc3f17ec6ca299dac919da9e59346a09a" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "ad8e067d874c286eb4b067084b9558113617985ba0a190284290c15c583c3294" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "0747a80f85b27855d2bb7ded8a24526eff2d92d9f362d5c8b1fabb06025bef11" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "ba134e2c7ea6766c31de617ce8fc891c0a61ecf0aaefcd34eba3c8529486c3b1" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "525e19dc8be66db7a896839317aa5a2e859f47ca25c9d658ed56b4c8f4705538" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "320c62158e13a88cfc5e918fbb294a6518f67830f0eec0a7b90f0b3bb5c71b8d" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "19391614d6c1fd3d4e4797d321c54913ce4fdeefbe8f94f4ec4d4455dbda656e" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "4649c8a1085768c7496e036df2e710439927d2df79826fe0d73a5a95e0e7fd61" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "4abe98ad1b9f860a28a97775f1c35c2e232f41f4c8c93e6e3c7ceb78ba38bd40" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "4a0967140271740eff9af6806c07e990f1bfe5086c3147d7a2f639ef9641d316" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "b37a1bef483587eca050d5be506af7f9be28a45d22da5abf875fc85b29ad0351" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "6dadcedf4ff705054384ba47a3c0c34ac83de5f2207d0f4fce1d0029433794fb" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "b33ae539810ef1c925c2539349e95e3f83f60b41209d83f9e2dadd568439f641" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "8db32cb3380d2ecc6c52794c0d7c5885bdf6bf8353eedc8b79b35c809ff0b07c" )
  ]
;;
