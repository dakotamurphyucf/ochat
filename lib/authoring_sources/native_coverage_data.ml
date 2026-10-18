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
      ; "lib/agent_session/native_tool_policy.ml"
      ; "lib/agent_session/script_tool_calls.ml"
      ; "lib/agent_server/developer_background_job_runner.ml"
      ; "lib/agent_server/developer_claim_deadline.ml"
      ; "lib/agent_server/developer_managed_tool_executor.ml"
      ; "lib/agent_server/developer_script_job_execution.ml"
      ]
    , "runtime.native.requests"
    , [ "test/chatml_composition/one_off_tests.ml"
      ; "test/chatml_composition/native_contract_tests.ml"
      ; "test/chatml_script_routing/routing_tests.ml"
      ; "test/developer_root_owner/native_executor_tests.ml"
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
      ; "lib/agent_session/authored_source_preparation.ml"
      ; "lib/agent_store/prompt_source_receipt.ml"
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_generated_shell_test.ml"
      ; "test/agent_server_generated_test.ml"
      ; "test/chatml_source_owner/owner_tests.ml"
      ; "test/chatml_source_owner/catalog_tests.ml"
      ; "test/chatml_source_owner/lifecycle_tests.ml"
      ; "test/chatml_source_owner/receipt_tests.ml"
      ; "test/chatml_source_owner/script_tests.ml"
      ; "test/chatmd_content_capture/capture_tests.ml"
      ; "test/chatml_source_owner/runtime_preparation_tests.ml"
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
  [ ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_protocol/invocation.ml"
    , "4d5abb95474bcdef6c360b666a3b57d2203a7b378193df5c49266897acac4981" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "bee393bda7895aeef41c69acab2b672d7c5e338a306cd01a179a5facdad7a8a3" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "e7931b4f548e321069c442029d71c6cca47db54af468d7d30957ae6cdbd4a909" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_session/session_management_channel.ml"
    , "500d45a8cbe760a50bc256df47255dc6fa5e1305f74a543d49c9c2e02e36de51" )
  ; ( "lib/agent_server/session_helper_policy.ml"
    , "e88605f0d6f0fc512589ccd9546e5f8e741216dfa321ec66919b64c88d2958e1" )
  ; ( "lib/shell_access/shell_access_v2.ml"
    , "81159199c2de8c80e35f192692ee28f3bdac4ca0929435835950bcaef83050d5" )
  ; ( "lib/agent_server/delegated_runtime.ml"
    , "b2f4ae578f7dddd08bb5503f16af4a9c86c5b87c2c5250f318a425103297c374" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/managed_output_page.ml"
    , "a14130666a4495de0c35df1c01974922da31daed064f1f7622a9b686b9c38ba5" )
  ; ( "lib/agent_server/session_factory.ml"
    , "d2d5a54ba45cc0bac164299a46b18f0c62cff2004f03acfbd905b059dfc64272" )
  ; ( "lib/agent_session/authoring_context_tool.ml"
    , "6e318b45f799b485f802dc6d5e4d8bc031721e379cc8800f6ed484d59694840d" )
  ; ( "lib/agent_session/authoring_reference_scope.ml"
    , "5db19890dff8b3188045d23899019afa642c6816f42f4a32b17e41365bf78983" )
  ; ( "lib/agent_session/authoring_services.ml"
    , "ca6aeaf3a1cede3d6642b9b2e188617c9e939375257b00d9a6481dd86d96c908" )
  ; ( "lib/agent_session/authoring_validation_tool.ml"
    , "b0196f46eec21c0ecead94b1165e7ec5ccfc5c3662a955d4f6230a22278ec59b" )
  ; ( "lib/agent_session/delegation_authority.ml"
    , "1296afd747dd9317e822ef3c1efbdc795188f27a0cb6f25664b2673ecb848a74" )
  ; ( "lib/agent_session/generated_definition.ml"
    , "f2c2ead7ec5ef439115a19458eece9e7165289b2ecf6f663226011a32112b0c9" )
  ; ( "lib/agent_session/generated_session_request.ml"
    , "90af4ba27a871509ae8b0805f36c008d7167fc748104c7055b7660d960ce6a8d" )
  ; ( "lib/agent_session/generated_session_tool.ml"
    , "11441478cc77a17a521839ffbdb8dee18f2161b12cab4c5b68c5bcea121dd726" )
  ; ( "lib/agent_session/managed_read_tool.ml"
    , "a0d2a3489a18bfae9da7cef1f99efceb15d791466b57aa0b483fdee6a971839d" )
  ; ( "lib/agent_session/managed_send_tool.ml"
    , "a2a85ea756d1125f28f517d0d583a1c812d29c1dc27aaa5ceec36ac69f72931b" )
  ; ( "lib/agent_session/managed_session_service.ml"
    , "8f44b1f6f84e85c25a634b91062ac01b03f4864b161a219fc382593f6427f327" )
  ; ( "lib/agent_session/managed_session_tool.ml"
    , "b75fd939bb31337da830126f0c39c0777557dfc02ee2280b3c17388d4f3aa42d" )
  ; ( "lib/agent_session/managed_stop.ml"
    , "2240eff3c77b83921d7044632ab60603fe43e18d7fdfb22497a1a5b401c3831d" )
  ; ( "lib/agent_session/managed_stop_tool.ml"
    , "22a5489b134b536c82b797f30e03a2c4fc4aadedcdc9c4b1e3cd49a3fab5ebef" )
  ; ( "lib/agent_session/managed_submission.ml"
    , "b7a7c8697aa0d9915c4be6ba8cd7eb65288a7508694a51ef3ea2e1acd4313cc2" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_session/managed_wait_tool.ml"
    , "2482da27415b0f542bbf5e2fa61bf97de70fb047790cb6ee5100ced465b43708" )
  ; ( "lib/agent_session/native_tool_invocation.ml"
    , "8fa02aa0350df2bbb85e7b7a43bcd4bda1a0863b352b760857bbb7c5187cecb1" )
  ; ( "lib/agent_session/native_tool_policy.ml"
    , "0d125cf75d5f61aa20a9452c979a70ebb43677e2458c9e9f1c9c2c5ad2c26938" )
  ; ( "lib/agent_session/one_off_execution.ml"
    , "946bf1eb9dc6c2da12f46d94f973a9ec7a4c11873b6ffab1424dedaeed8c6e7a" )
  ; ( "lib/agent_session/run_chatml_tool.ml"
    , "eadb7cf2ac6622e977db08798fbfce7661062c47883cdf4f28700f1c85249b75" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "d3e8058a55a16ded0bb9991981bfa5cbcb2eff37bddd618db0e7c876eccdd1ab" )
  ; ( "lib/agent_session/session_management.ml"
    , "18431c4fd97a2d24a72b927fe8f0d20b8aea561f1f119d8f122d943f2a137f86" )
  ; ( "lib/agent_session/session_management_native.ml"
    , "8ccb5239373707dacbceec1ce9a0c5146eea5360921ad0a5209bcc2928bce806" )
  ; ( "lib/agent_store/delegation_store.ml"
    , "23e90979a34b8093325f41aeb0db02ddbbb4f7b8ab24fd7a361b25bb57001216" )
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
    , "8f3f7aa842dd09a1aa299e8ff67ed8b6b771d8308b396d20f1a2e07ecb5c9ad9" )
  ; ( "lib/agent_session/prompt_source_lease.ml"
    , "7413c0ed3e84375cd7e95343ceb83d9485c02b4d378ff940336beb449680dbe0" )
  ; ( "lib/agent_session/prompt_catalog.ml"
    , "e70659b767477be0cf1a99fefa753841264846a5e426ac415cb160a6b1904c40" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "bd0252ef7e0d2fe7e9c33d5dfd85279e480b0474155fb3667085532052807fd3" )
  ; ( "lib/agent_session/authored_source_preparation.ml"
    , "d8eea801d69ffa262cfc1d0a9bee45123ce8d4b08f74c25c67df450cd247600c" )
  ; ( "lib/agent_store/prompt_source_receipt.ml"
    , "d1089027dc9f0e69fb6db67815b2de78086d88a29334ac7c12ec6aa9f9b8258e" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.native.requests"
    , "5ce300e2cce67ff2c39b548e3fd8d44b244207c7d7e6637626fe6ece0f7780ff" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "551884bf0e3eec7b15c19abef77502c75744211a8a51494ca79cf603e9c56e97" )
  ; ( "one_off_v1"
    , "runtime.delegation.generated"
    , "d302c4bb2cdb44cd4297c4ca7f32eb729e45397b7ac62f4d859f2eb00b1bd13d" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "1df7b2397456f58627d2eec7874ebc68f7e970a0520387acf4d00077df1e67a7" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "294fef69f30e3914a0d780bb0ba0ca623a795ef2f5c5b44dbf1279a49e982252" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "e704e8f254c5f4ed3c79643664946addb797c40cf4f1c9658b7de7c566f67240" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "6c4f9c1464a500736f08b795dcf8862ccddb889d26aebfe0c860eb76322dd2d1" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "744d516de226470f4f9fe73951c9fe399b0d42028e1e61da7add0f9d61cf39e1" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "dd874d6d66ab4fd587ad87e581c41eb6600770a503fd9194ced62ae506e0f4ca" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "d4d35599dd7ac65a9254a2eadf63292e48701c7b386510b75b6ed8eca14bfe4f" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "60ff1b8bdb8c3a5c2373fe02852659cb050ce4e7c3b1d50577633c0e827e7e16" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "39492869229b5467e69cf51cafe30518dc0330ec500c7190e4593149ef939af9" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "56ba576cdcb8bbcc9a88ca6ea54a078e672794e98dc91f85756df84cec914163" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "e83744a8b285ce2e25f78903bc85cdfe5ebefb7f6f4abe7bfc5107f703c0facc" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "2cffdc2c42b8dc4daf6c040ba46553b7ba919860809f589a19b800e8bc81c0bd" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "ad8e067d874c286eb4b067084b9558113617985ba0a190284290c15c583c3294" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "70fef1d772ad30ee50f0553374e920992775f9e7a8a17c101c979c9579b68340" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "1feca3b363edaf92332d37ab49e3613232b8545d9363214ab0304d0cd2cb343a" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "7c12acb18c38877e0e5c58cc1022611daae4152f5280bd678ae7ce07e9de6f97" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "22db6b9c2f1fa711d824659324974549a99cf2f89532fd7f4d1ef80609dccb40" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "bf9807b10345a9dfe9ffcfa6696f4e0187ac4641566e667267a32461c0ac7a5e" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "ff2e3bb85a521f6daf0b6f64f2d6fb6f669e7decf35c98335d5d7dbb986a4c28" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "4abe98ad1b9f860a28a97775f1c35c2e232f41f4c8c93e6e3c7ceb78ba38bd40" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "7394ed04d093974f70797617c65c168e1a87f1d4fca5ed062adf9df3c156df85" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "7477a6b975c7c6a6ab336d6b52df0903c7f57fd2a1162dc42f4fd6ec3d273e9c" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "6ca1fc68b8236b98a1cf80f1b475e5f945ce96b1aeb84ce225500f9947a32c00" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "d3a7ff35a1127114f0b8f89428dafce06c26bca1704e8930e48f90f7483993b3" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "fd5987bafa6c687aa5bdb2e3ae3cda7d2f9fe3789a03a4ea7494d964c0cc467e" )
  ]
;;
