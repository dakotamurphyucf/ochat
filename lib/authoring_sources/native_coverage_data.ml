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
      ]
    , "runtime.delegation.stop-helper"
    , [ "test/agent_server_helper_test.ml"
      ; "test/chatml_composition/authoring_reference_scope_tests.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/agent_protocol/invocation.ml"
    , "3badd86d40e4ae6546132c664944bb851b42b40a14aa9fab9c916d4c10d6a5a0" )
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
    , "d399692b68fc957edd499549d009cb011a711cc458bcba840d59c576546f68ac" )
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
    , "83f1335d116c18a671ec7dc303bae0564c88705ca08c759145c81d591ba047a1" )
  ; ( "lib/agent_session/one_off_execution.ml"
    , "946bf1eb9dc6c2da12f46d94f973a9ec7a4c11873b6ffab1424dedaeed8c6e7a" )
  ; ( "lib/agent_session/run_chatml_tool.ml"
    , "228d10f6edfb0a5568692332653ac8136d10588eebebbafc827226a6c122a079" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "6f06f3bc9c161bbef819b66ece0e9aa973427f4ecc97cde90971d946639c1760" )
  ; ( "lib/agent_session/session_management.ml"
    , "18431c4fd97a2d24a72b927fe8f0d20b8aea561f1f119d8f122d943f2a137f86" )
  ; ( "lib/agent_session/session_management_native.ml"
    , "8ccb5239373707dacbceec1ce9a0c5146eea5360921ad0a5209bcc2928bce806" )
  ; ( "lib/agent_store/delegation_store.ml"
    , "525102be15fde1969281aba17e737e874d9d3d64f415a9538cba3793d6e4b2a7" )
  ; ( "lib/chat_response/authoring_context.ml"
    , "a6cd864bafd98a646bdfa8350687c01aa3ae7249c6f09b4fae9651f740d06aea" )
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
    , "08247085f608d939df450e75ad2a928707c9b240971b2f7f36b8dc9dfe58ee6d" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "e3c330d25a8c1a19e6677add02f0b9a9408ad3b6027694b2fbad94501de415ad" )
  ; ( "one_off_v1"
    , "runtime.delegation.generated"
    , "e2e9ed3ba656f51150ffb00a8f4273400966eebaee9cda30c5a1cad6d3af70f9" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "17b48186efc2c2fd5d4ba611fe9e4358ea27c65c063229bafce31256c7267637" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "6e99c13f8d2b0d09416ccfaa55e69909addb59f91143c301942e554280b278dd" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "3ae2b9d57189808f818d447eab6ce80866c8c3039707dafcbda9b7a251ab10d0" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "9a9d106a252e3691be4dd1c468ca8d30d4a11b28f60bd9c5b042b2c342965ae3" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "321e0289ad35a624c0c65bcc4e4d166c1b6006383b45fc2a6d6cfe030823798e" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "998c7e7ba566c9473beb4a128b4948666442c729c46149e0ddeeefe7180b6e21" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "e964a11baf62135668cb910c42619550b313577fc261fa1ea52264a234cd302b" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "bcd0db8a9ed0db9b3f6ff727e0fa9ac74897472a80e45ea516505ed532f5eb3e" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "da0a9cba0aac40ea69d6b6c15be07d341ebdd4cdc682e8006e88bfcea4c61b0f" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "67d31b631ba3c2766ab261cec97b01172698b264ce80ba8e9fc7ec8a0b82cbfb" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "aac084665c57ba08ce6cc0ac21180a9c83d91e22d14e4ff079a48f8429d96a76" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "8b7caeaded1aa6c1549b0327d81e488f1cb78a7fcfd6157343b54526cd48c972" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "0f2083d04e9f7bbb2587a0222fb63e79e10fe5aacec4f57e8d662577cd3359f4" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "99daeb13a722623a3703a995d63942337bd719a7e9cf3893f9ed30f62733ba18" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "f818a1d102e7233e3320701e4416248c7c5144bf8c1d534dd42aa89b30f4dcff" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "9420b5486be88fe7bedf57d81c91eac9b3c94b42b769e222ab36a5cc4e7e2682" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "ecdc6df96e55a28b1545263905ad301c0c11ebe0396c441abb5b49f9320647ab" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "9824474632fb1e8cad0784d467d9bf9fd1321cb7d7f818ab5fadbfc51737199c" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "8bf6af0f4b08bb388a318c710bec68608ccdaedae68ad0848323a816e575951d" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "b2935c722f9605b1d24116aa346971e87d1ce3b1835f5d0d6d36af6b8430c8f3" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "3f6f180be67516041ca12546272b18e1e4a551737fe6244e7d185521bfc13986" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "cea1c1770f6dbdd92c673c2e581024b8bc824d38d193372564dd3621379b9fc7" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "5f737edfe0933af42c76c70c43e758b9f4e69cc9768d81a77528eed3ea22212a" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "8ea44d3c87956c8166ce1ad82751305022deda75f7d7f8def4cd89b8aaefa56c" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "c3c6bdda309423b5a3ee52f0ce911b32eba1b74920369d358d618534d7c44f7f" )
  ]
;;
