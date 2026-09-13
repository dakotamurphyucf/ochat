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
    , "c3b3bdbd8bea666da4028cc96cfeab2e664ceb079b127f7fbead3903016dcf04" )
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
    , "5284681766b2a68a0675fed3f440108f05c00302b1354e0eb99b4c45ccb1f8b8" )
  ; ( "lib/agent_server/session_factory.ml"
    , "4813db51f28489f145bed3f660610a08906322a8745b8ce517256c503651b452" )
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
    , "5a3a3f40befac0c438f754821a63e84110753342c4d21f5001e708453d37d6ec" )
  ; ( "lib/agent_session/managed_stop_tool.ml"
    , "ea5f7852a7a5b0b088419a09978dc9fb845399baa141aa6ca4dbfd0945cd6b5e" )
  ; ( "lib/agent_session/managed_submission.ml"
    , "5321852969e178731ad30c7d74ff2d4d3c7292d706884d9d868c17084ab40e62" )
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
    , "525102be15fde1969281aba17e737e874d9d3d64f415a9538cba3793d6e4b2a7" )
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
    , "51825c9cdf0c8e1b9565bd5a3d4fd128e199f143106c1e0cd6d1c87226b6330e" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "c9f79dadcfb5990b44148869e667f79c5bf0ba3be5bf8f6e6b4d6b794bebe77a" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "7fe70ca8323053796cb17998911a0df79e4d723b2fb420c15943eb0e5ec1e178" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "59fd80e10afdbe32cb8cf4f71e786b4ae0c7b0b9fd7ecf54ab64b970b70ddd36" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "5f36e18640b9c6d3aef770083de9cb563e6497f33473d855d5dde83ecd8b65e0" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "02325a15bb50a98d9798c025a0a1b0e03d2e13f69390081944d9edef6cfcf5b4" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "dd874d6d66ab4fd587ad87e581c41eb6600770a503fd9194ced62ae506e0f4ca" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "6194fe03328947d9fa4a4515cb7a293ea373f96e326b5a3345e59126820599d4" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "7e6f09d2b34d59b254cafd7c7a1712dd0106d4040f750e85e9d3c4aff9488c04" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "b340aa765c9878f39461d51e9177e575ada9606e4170ac7736f3e9399f95736b" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "88f216873e967bc650ac9e19bba62f4b4ce2a5a98c61aa961915edbae7fa72a8" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "75b0cb98767b3a7a6716266bd2b297471a7f492afbbab29267cb77510c080370" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "a53edf3b450b1e3bc159be090d9dae6bc3f17ec6ca299dac919da9e59346a09a" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "ad8e067d874c286eb4b067084b9558113617985ba0a190284290c15c583c3294" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "b25d5553c8cbdb15258451a2967df007cf89669345cb89e8c26f8174942c59c1" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "c0d6f7b97036ab1774f39e1d252f0ef8a1d7cfe642668451d9b4e6f561deffbd" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "c00a9a96e46108ac8e925ad16a0e5e23e3da5a44ed91075278b42ade6e515e3e" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "d0eb68cb1c23a1683bdb63d3ad1f451c430b91f4b7feba42053f063b1390698d" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "cbeec59ca7da50494458c68aff150786458b6cc0d73e70b41b983b3e9ea879c5" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "4649c8a1085768c7496e036df2e710439927d2df79826fe0d73a5a95e0e7fd61" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "4abe98ad1b9f860a28a97775f1c35c2e232f41f4c8c93e6e3c7ceb78ba38bd40" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "f8cff031f2db36753e411d0fcc70ad74e5ef800049a87eb5aff60e6f2bbb44bc" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "ef170f487b64aa45c346d7190e35042cc1eb7f1a6c62ec96a82e1fd64c984d6f" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "c6a17f744e2aebc362b21ec13db375e64998abfa1ac10ce7a4d10598b4675940" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "6a909102631cb52501d44b2e42a438efa8a8f2d23847dd4885fd3596b4451720" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "4a31e0e615c6205a34881181ec4a5b1c3c2291f808aff4b633c03817272e7827" )
  ]
;;
