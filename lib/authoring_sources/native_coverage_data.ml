(* Reviewed native operation semantics. This taxonomy complements the runtime
   catalog's descriptor snapshots; source hashes detect drift, not new features.
   References name behavior suites, which must be run separately. *)
let features =
  [ ( "instructions.source_admission"
    , "Explicit read-only root discovery and exact capture through current source \
       grants; reading never activates"
    , [ "lib/agent_server/authoring_instruction_sources.ml"
      ; "lib/agent_session/instruction_source_port.ml"
      ; "lib/agent_session/instruction_source_request.ml"
      ]
    , "instructions.sources"
    , [ "test/instruction_sources/runtime_tests.ml"
      ; "test/instruction_sources/native_runtime_tests.ml"
      ; "test/chatml_composition/instruction_runtime_tests.ml"
      ] )
  ; ( "instructions.selected_effects"
    , "Five selected effect-specific native admissions, exact captured CAS and ACKed \
       receipts; refresh never replaces selection"
    , [ "lib/agent_session/instruction_native_admission.ml"
      ; "lib/agent_session/instruction_native_request.ml"
      ; "lib/agent_session/instruction_native_tool.ml"
      ; "lib/agent_session/instruction_command_admission.ml"
      ; "lib/agent_session/instruction_service.ml"
      ]
    , "instructions.sources"
    , [ "test/chatml_composition/instruction_runtime_tests.ml"
      ; "test/instruction_sources/native_runtime_tests.ml"
      ; "test/agent_session/agent_authority_tests.ml"
      ] )
  ; ( "computation.request"
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
      ; "lib/agent_server/recurrence_authored_source.ml"
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
  ; ( "recurrence.selected_metadata"
    , "Nine explicitly selected same-controller native metadata operations with genuine \
       invocation/current original checks and admitted write semantics"
    , [ "lib/agent_session/recurrence_native_admission.ml"
      ; "lib/agent_session/recurrence_native_tool.ml"
      ]
    , "runtime.recurrence"
    , [ "test/recurrence_creation/recurrence_native_request_tests.ml"
      ; "test/chatml_composition/recurrence_runtime_tests.ml"
      ; "test/chatml_composition/recurrence_execution_tests.ml"
      ; "test/factory_catalog_publication/recurrence_policy_tests.ml"
      ; "test/lifecycle_helpers/recurrence_helper_tests.ml"
      ] )
  ; ( "recurrence.capture_and_retry"
    , "Exact echoed generation/CAS and original request identity; persistent authored or \
       existing target capture without receipt authority"
    , [ "lib/agent_session/recurrence_native_schema.ml"
      ; "lib/agent_session/recurrence_native_request.ml"
      ; "lib/agent_session/recurrence_service.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.recurrence"
    , [ "test/recurrence_creation/recurrence_native_request_tests.ml"
      ; "test/chatml_composition/recurrence_runtime_tests.ml"
      ; "test/chatml_composition/recurrence_execution_tests.ml"
      ; "test/factory_catalog_publication/recurrence_policy_tests.ml"
      ; "test/lifecycle_helpers/recurrence_helper_tests.ml"
      ] )
  ; ( "recurrence.observation"
    , "Bounded controller evidence and separately authorized historical target snapshots \
       with a final same-service disclosure guard"
    , [ "lib/agent_session/recurrence_query.ml"
      ; "lib/agent_session/recurrence_target_view.ml"
      ; "lib/agent_server/session_factory.ml"
      ]
    , "runtime.recurrence"
    , [ "test/recurrence_creation/recurrence_native_request_tests.ml"
      ; "test/chatml_composition/recurrence_runtime_tests.ml"
      ; "test/chatml_composition/recurrence_execution_tests.ml"
      ; "test/factory_catalog_publication/recurrence_policy_tests.ml"
      ; "test/lifecycle_helpers/recurrence_helper_tests.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/agent_server/authoring_instruction_sources.ml"
    , "5a43fd47d8b0c1916d562f79dd27dfbd3ceb019dfb3f974cede919fc16a83b8e" )
  ; ( "lib/agent_session/instruction_source_port.ml"
    , "3521e99e67d7d698a5186fa2d43dfd999135e7e50e8106e60124de40ac4329a2" )
  ; ( "lib/agent_session/instruction_source_request.ml"
    , "c4993419caf79a25be6543253f8e63549918ed3010b5b6017991ddad7c9d0a7b" )
  ; ( "lib/agent_session/instruction_native_admission.ml"
    , "d5617a7fb0e72b7c3d8ff94f8fec8fe0a8650a6b797e688400a87cc2035ac925" )
  ; ( "lib/agent_session/instruction_native_request.ml"
    , "4960a708518b49ab66dcd01ab15ae99b6bac611e603c6f1c46c71e6653ba8a27" )
  ; ( "lib/agent_session/instruction_native_tool.ml"
    , "6145372a8e88260b77f77cdb6ed8b9567e4d6b5102aa5f26fc5d954f76145139" )
  ; ( "lib/agent_session/instruction_command_admission.ml"
    , "b09c970ef5a80ed479fe180210d1ac28865c79ed7b2b4a33109eb85ce2168899" )
  ; ( "lib/agent_session/instruction_service.ml"
    , "d5353074b86ed3aa8282f9f819947c225f81ea7ad8327345a368d8ee4d1e1ad2" )
  ; ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_protocol/invocation.ml"
    , "d96de30866cc716a5f8f34315e4ae2585bbfd8e7cf765b77d2b2042836872223" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "9450ea610c424f91480b70ca733226137fa0b18325f07bf789da3e582e8d1876" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "e7931b4f548e321069c442029d71c6cca47db54af468d7d30957ae6cdbd4a909" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_session/session_management_channel.ml"
    , "500d45a8cbe760a50bc256df47255dc6fa5e1305f74a543d49c9c2e02e36de51" )
  ; ( "lib/agent_server/session_helper_policy.ml"
    , "e88605f0d6f0fc512589ccd9546e5f8e741216dfa321ec66919b64c88d2958e1" )
  ; ( "lib/shell_access/shell_access_v2.ml"
    , "06f99e979fd39c4c72590ffe6faee9f0790a77b9ad141e4d05919b68771b51cc" )
  ; ( "lib/agent_server/delegated_runtime.ml"
    , "b2f4ae578f7dddd08bb5503f16af4a9c86c5b87c2c5250f318a425103297c374" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/managed_output_page.ml"
    , "a14130666a4495de0c35df1c01974922da31daed064f1f7622a9b686b9c38ba5" )
  ; ( "lib/agent_server/recurrence_authored_source.ml"
    , "e8f4b287579ae891c1d8197671c335e4e07afb02105ac206a44025701111bf3b" )
  ; ( "lib/agent_server/session_factory.ml"
    , "f88a2f916a6a2782042b8a8a0070976adb8532cf1ea23e0e87749542a1761aa4" )
  ; ( "lib/agent_session/authoring_context_tool.ml"
    , "6e318b45f799b485f802dc6d5e4d8bc031721e379cc8800f6ed484d59694840d" )
  ; ( "lib/agent_session/authoring_reference_scope.ml"
    , "5db19890dff8b3188045d23899019afa642c6816f42f4a32b17e41365bf78983" )
  ; ( "lib/agent_session/authoring_services.ml"
    , "ca6aeaf3a1cede3d6642b9b2e188617c9e939375257b00d9a6481dd86d96c908" )
  ; ( "lib/agent_session/authoring_validation_tool.ml"
    , "b0196f46eec21c0ecead94b1165e7ec5ccfc5c3662a955d4f6230a22278ec59b" )
  ; ( "lib/agent_session/delegation_authority.ml"
    , "c46726c90eaebb01ea32a72786540461bbafb5bed5331d9b42d906e280afb120" )
  ; ( "lib/agent_session/generated_definition.ml"
    , "b2f538dbffb99ed7e6a2c7d073341ab7e4a2fe33a17d3d364aba7d2b9101dc6b" )
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
    , "a620bd629e1a582d80f612ac95debd8fbed317681256b07d449154bebe5e4148" )
  ; ( "lib/agent_session/native_tool_policy.ml"
    , "0d125cf75d5f61aa20a9452c979a70ebb43677e2458c9e9f1c9c2c5ad2c26938" )
  ; ( "lib/agent_session/one_off_execution.ml"
    , "c1258ae2a13db05133c06681a30e7e8038e5e439d20d2eae03bf5e832878ed65" )
  ; ( "lib/agent_session/run_chatml_tool.ml"
    , "eadb7cf2ac6622e977db08798fbfce7661062c47883cdf4f28700f1c85249b75" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "ba391e326cf6e4cc7711d2070c912b728f5343f70b2c3f18235c935f59cd04cc" )
  ; ( "lib/agent_session/session_management.ml"
    , "18431c4fd97a2d24a72b927fe8f0d20b8aea561f1f119d8f122d943f2a137f86" )
  ; ( "lib/agent_session/session_management_native.ml"
    , "8ccb5239373707dacbceec1ce9a0c5146eea5360921ad0a5209bcc2928bce806" )
  ; ( "lib/agent_store/delegation_store.ml"
    , "995e942b820e7812192f247ec6d7cea6b69f9137c8fb1b93dbca0db7ab19da88" )
  ; ( "lib/chat_response/authoring_context.ml"
    , "b12dd8cc2405019d95b7713039fc6737397df2619da736f6478a5143cb1b358b" )
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
    , "a6b86884830197ff69113d8989280ef72bd33b61ccfa10d2e1b91bffbfc82505" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "0fec32fbef60654f76947834245aaf0bd2fcf5027b1e2b37df06111972dbee6f" )
  ; ( "lib/agent_session/authored_source_preparation.ml"
    , "d8eea801d69ffa262cfc1d0a9bee45123ce8d4b08f74c25c67df450cd247600c" )
  ; ( "lib/agent_store/prompt_source_receipt.ml"
    , "d1089027dc9f0e69fb6db67815b2de78086d88a29334ac7c12ec6aa9f9b8258e" )
  ; ( "lib/agent_session/recurrence_native_admission.ml"
    , "6dedc770d93c9329092e2ec809b1e5f4158065aef2a9fba365770f012dc71297" )
  ; ( "lib/agent_session/recurrence_native_schema.ml"
    , "b35c2fc2e32603cfc6442f775ba80deef311d2a208768187e7aa023da2028b8a" )
  ; ( "lib/agent_session/recurrence_native_request.ml"
    , "03f798a7c41d5e9faffa17c1af94939238518cccac4592684ab1a155386c22ea" )
  ; ( "lib/agent_session/recurrence_native_tool.ml"
    , "816ff274052c38bb80b6a841cc1869143f7ac5459cc7df4198f326b23d650c85" )
  ; ( "lib/agent_session/recurrence_query.ml"
    , "5e6091a6154727980b4e6c69bb742ea2be8439317a7f36f035a8d9b5324fdeb4" )
  ; ( "lib/agent_session/recurrence_target_view.ml"
    , "fcb6bd74ac79a90cff4e472a9f90610d1425b68f94bd5fb347de1552eb8411aa" )
  ; ( "lib/agent_session/recurrence_service.ml"
    , "ee3c04f4c48a296d9ce311656c386709da550569513ac3995d44f30d52cccd41" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "instructions.sources"
    , "6c01d31beec1423681006f2320554f8f09c4163b27a8633bd2d8c2fa602a4852" )
  ; ( "tool_v1"
    , "instructions.sources"
    , "66d91bb62409d861fb5a355506c12db41be038092317e625c57c608da95533f0" )
  ; ( "moderator_v1"
    , "instructions.sources"
    , "552553a84efd975cfe41652872c94ea6c06db57593d050795fc8a31f6a33a070" )
  ; ( "delegated_moderator_v1"
    , "instructions.sources"
    , "854af913368900b0355a1dec3b19e230a1a917fd1224b201ce096f85f471efa8" )
  ; ( "one_off_v1"
    , "runtime.native.requests"
    , "5ce300e2cce67ff2c39b548e3fd8d44b244207c7d7e6637626fe6ece0f7780ff" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "3144c80ec148fea7da1a3e326d550cd088d5406db6cec9ad046213a78bd818cf" )
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
    , "e8ad92de376e5f06f713365be43cdfacd88f3ba86016000cee7965865306ce60" )
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
    , "e0c3b31a2e977b6f79ba24a61288c9091a4befa260cf9ba7de97b9a405e0dd64" )
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
    , "a53e47861488d01e10a95ad400c675ede2648ce0b05e6e4b4f258b12f5ad70f2" )
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
  ; ( "one_off_v1"
    , "runtime.recurrence"
    , "c7c4d6f5879afae1a4550bbd42cd9ec3db94f2849c88789d8e9815210a5e3ebb" )
  ; ( "tool_v1"
    , "runtime.recurrence"
    , "1465856336850d3303cc0c5c6073a75414645cfffa417c6e196348b5a134e025" )
  ; ( "moderator_v1"
    , "runtime.recurrence"
    , "8bad6285d1f892174d2de3dba36c803a7af17c2720bd025596c40cb2bfe15980" )
  ; ( "delegated_moderator_v1"
    , "runtime.recurrence"
    , "67c46da5b0c221e9d663c4c7d25bee64ff31977d3326891e01ef35d359b9a150" )
  ]
;;
