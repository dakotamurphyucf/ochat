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
    , "Selected native and explicit shared operator admissions, exact captured CAS and \
       ACKed receipts; refresh never replaces selection"
    , [ "lib/agent_session/instruction_native_admission.ml"
      ; "lib/agent_session/instruction_admission.ml"
      ; "lib/agent_session/instruction_native_request.ml"
      ; "lib/agent_session/instruction_native_tool.ml"
      ; "lib/agent_session/instruction_command_admission.ml"
      ; "lib/agent_session/instruction_service.ml"
      ; "lib/agent_client/connection.ml"
      ; "lib/agent_client/instruction_controls.ml"
      ; "lib/agent_client/instruction_controls.mli"
      ; "lib/agent_client/session_handle.ml"
      ; "lib/agent_client/session_handle.mli"
      ; "lib/agent_protocol/command.ml"
      ; "lib/agent_protocol/command.mli"
      ; "lib/agent_protocol/instruction_control.ml"
      ; "lib/agent_protocol/instruction_control.mli"
      ; "lib/agent_protocol/method_result.ml"
      ; "lib/agent_protocol/method_result.mli"
      ; "lib/agent_protocol/public_result.ml"
      ; "lib/agent_server/authorization.ml"
      ; "lib/agent_server/command_handler.ml"
      ; "lib/agent_server/daemon.ml"
      ; "lib/agent_server/instruction_operator_service.ml"
      ; "lib/agent_server/instruction_operator_service.mli"
      ; "lib/agent_server/principal_projection.ml"
      ]
    , "instructions.sources"
    , [ "test/chatml_composition/instruction_runtime_tests.ml"
      ; "test/instruction_sources/native_runtime_tests.ml"
      ; "test/agent_session/agent_authority_tests.ml"
      ; "test/chatml_composition/instruction_control_codec_tests.ml"
      ; "test/chatml_composition/instruction_control_tests.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ; "lib/agent_session/generated_session_request.ml"
      ; "lib/agent_store/delegation_store.ml"
      ]
    , "runtime.delegation.creation"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "creation.lifetime_and_authority"
    , "Owned/authorized-independent lifetimes and inherited revocable execution authority"
    , [ "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/descendant_accounting_host.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ]
    , "runtime.delegation.output"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.wait"
    , "Receipt versus output waits, monotonic bounded timeouts and no child cancellation"
    , [ "lib/agent_session/managed_wait_tool.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ]
    , "runtime.delegation.output"
    , [ "test/agent_server_generated_test.ml" ] )
  ; ( "management.stop"
    , "Idempotent graceful/cancel admission, retained history and cleanup progress"
    , [ "lib/agent_session/managed_stop_tool.ml"
      ; "lib/agent_session/managed_stop.ml"
      ; "lib/agent_session/session_management.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
  ; ( "goals.selected_metadata"
    , "Selected same-session Goal metadata, Start and Resume tools with lexical native \
       admission, transferred original custody and atomic callback-staged commands"
    , [ "lib/agent_session/goal_native_admission.ml"
      ; "lib/agent_protocol/goal_resume_request.ml"
      ; "lib/agent_session/goal_native_metadata_tool.ml"
      ; "lib/agent_session/goal_native_start_tool.ml"
      ; "lib/agent_session/goal_native_metadata_service.ml"
      ; "lib/agent_session/goal_native_metadata_view.ml"
      ; "lib/agent_session/goal_run_evidence.ml"
      ]
    , "runtime.goal_metadata"
    , [ "test/factory_catalog_publication/goal_native_metadata_tests.ml"
      ; "test/factory_catalog_publication/goal_native_start_tests.ml"
      ; "test/factory_catalog_publication/goal_native_resume_tests.ml"
      ; "test/goal_domain/goal_resume_request_tests.ml"
      ] )
  ; ( "recovery.history_tools"
    , "Selected local ordinary history, exact part recovery and foreground protected \
       delivery"
    , [ "lib/agent_session/recovery_history_native_request.ml"
      ; "lib/agent_session/recovery_history_native_tool.ml"
      ; "lib/agent_session/recovery_history_native_service.ml"
      ; "lib/agent_server/recovery_history_native_host.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_session/recovery_effective_release.ml"
      ; "lib/chat_response/generated_admission.ml"
      ; "lib/chat_response/managed_tool_registry.ml"
      ; "lib/chat_response/tool_capability.ml"
      ]
    , "runtime.recovery_history"
    , [ "test/agent_session/recovery_history_access_tests.ml"
      ; "test/tool_capability_test.ml"
      ; "test/agent_server_e2e/scenarios/recovery_history_provider_scenario.ml"
      ] )
  ; ( "recovery.note_tools"
    , "Operation-owned note reads, selected revisions, guarded delivery and durable \
       mutations"
    , [ "lib/agent_session/recovery_note_access.ml"
      ; "lib/agent_session/recovery_note_native_request.ml"
      ; "lib/agent_session/recovery_note_native_tool.ml"
      ; "lib/agent_session/recovery_note_native_service.ml"
      ; "lib/agent_session/recovery_note_query.ml"
      ; "lib/agent_session/recovery_note_share.ml"
      ]
    , "runtime.recovery_notes"
    , [ "test/agent_session/recovery_note_native_tests.ml"
      ; "test/agent_server_e2e/scenarios/recovery_note_provider_scenario.ml"
      ; "test/agent_server_e2e/scenarios/recovery_note_shared_provider_scenario.ml"
      ] )
  ; ( "recovery.window_tools"
    , "Selected foreground capacity, authored checkpoint coverage and durable window \
       intent"
    , [ "lib/agent_session/recovery_context_native_tool.ml"
      ; "lib/agent_session/recovery_context_service.ml"
      ; "lib/agent_session/recovery_authored_checkpoint.ml"
      ; "lib/agent_session/recovery_rollover_batch_cut.ml"
      ; "lib/agent_session/recovery_checkpoint_cut.ml"
      ; "lib/agent_session/recovery_rollover_evidence.ml"
      ; "lib/agent_session/recovery_ordinary_selection.ml"
      ; "lib/agent_session/recovery_rollover_hint_ref.ml"
      ; "lib/agent_session/recovery_successor_guidance.ml"
      ]
    , "runtime.context_windows"
    , [ "test/agent_session/recovery_rollover_tests.ml"
      ; "test/agent_server_e2e/scenarios/recovery_checkpoint_scenario.ml"
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
      ; "lib/agent_session/recurrence_goal_capture.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
      ]
    , "runtime.recurrence"
    , [ "test/recurrence_creation/recurrence_native_request_tests.ml"
      ; "test/chatml_composition/recurrence_runtime_tests.ml"
      ; "test/chatml_composition/recurrence_execution_tests.ml"
      ; "test/factory_catalog_publication/recurrence_policy_tests.ml"
      ; "test/recurrence_creation/goal_allocation_policy_tests.ml"
      ; "test/lifecycle_helpers/recurrence_helper_tests.ml"
      ] )
  ; ( "recurrence.observation"
    , "Bounded controller evidence and separately authorized historical target snapshots \
       with a final same-service disclosure guard"
    , [ "lib/agent_session/recurrence_query.ml"
      ; "lib/agent_session/recurrence_target_view.ml"
      ; "lib/agent_server/session_factory.ml"
      ; "lib/agent_server/delegated_policy_vector.ml"
      ; "lib/agent_server/delegated_policy_call.ml"
      ; "lib/agent_server/native_resource_authority.ml"
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
  [ ( "lib/agent_client/connection.ml"
    , "92a1167b8fbd61c6b6b9d2cababf5201e2f3a55a66d4ace23ef1e0ea07d5d16a" )
  ; ( "lib/agent_client/instruction_controls.ml"
    , "d0de8d5a069a3188af7ccb29cc548c876821a95ea20b8c483259a163cb6431b2" )
  ; ( "lib/agent_client/instruction_controls.mli"
    , "37b662d93ec0b3861ca86143a790b2f181070eebaa4df3f5a1534247469b92b2" )
  ; ( "lib/agent_client/session_handle.ml"
    , "aa0fb4407df23553a293946c5d54eb5c102ca3a7aaf36460a55856f063aa175f" )
  ; ( "lib/agent_client/session_handle.mli"
    , "9d49f9da818d1040076ad4e0d52cf88d4519194e01cd8b0afeb5eaea08cc7192" )
  ; ( "lib/agent_protocol/command.ml"
    , "32f51abd735629f17b3e5bc3b72121410e93024df4247bfd21170b6ae4e43c51" )
  ; ( "lib/agent_protocol/command.mli"
    , "b2194b23390b2bb248be95fda160d63b7175375923b7730be76754fc9015d396" )
  ; ( "lib/agent_protocol/instruction_control.ml"
    , "171128dbfd1dc2f62e9b966ddd5c7ce0e9748ef67923ad1b5abf863f4b4383dc" )
  ; ( "lib/agent_protocol/instruction_control.mli"
    , "714d1af33e80402808046ead8b7a7b6acff53a4b502bf238211e6294b01af13e" )
  ; ( "lib/agent_protocol/method_result.ml"
    , "4a066ec45b706902d0a776bf02aeca1eaea3a19daf7906473bd6f61f8e96e60d" )
  ; ( "lib/agent_protocol/method_result.mli"
    , "c600d1699b81b7385eba79fe5c6cba60754ffdf5fb44f86dad6f24e7599763cb" )
  ; ( "lib/agent_protocol/public_result.ml"
    , "30cd6b85abb7976dd458461e061e3bd821e2de79e37ca21a7a274dd668ba0603" )
  ; ( "lib/agent_server/authorization.ml"
    , "ba914b280063788776eca7d3a32507e88d587d268b50bbc9b1a9204729bb0a0a" )
  ; ( "lib/agent_server/command_handler.ml"
    , "f5c16349caca3725de46a5104dfa72ffd1cca140332f367cf80711b48321aca5" )
  ; ( "lib/agent_server/daemon.ml"
    , "69257893caa1579c0b2c77e1c096d8dc459c82a24c1b777b377256ffdfa7bc92" )
  ; ( "lib/agent_server/instruction_operator_service.ml"
    , "7bcfd6d6f88f311b50c50af7a300b9b94bbcf32641b54aeef9900f902a2d0bc8" )
  ; ( "lib/agent_server/instruction_operator_service.mli"
    , "68ea938a6334edf04432f1e340d5cc11309b603291e961aad8976f029167f31d" )
  ; ( "lib/agent_server/principal_projection.ml"
    , "41699d959ef65380f2b13e567a43009f5d69d9448251dd5bf3affdb22b21a42e" )
  ; ( "lib/agent_server/authoring_instruction_sources.ml"
    , "5a43fd47d8b0c1916d562f79dd27dfbd3ceb019dfb3f974cede919fc16a83b8e" )
  ; ( "lib/agent_session/instruction_source_port.ml"
    , "3521e99e67d7d698a5186fa2d43dfd999135e7e50e8106e60124de40ac4329a2" )
  ; ( "lib/agent_session/instruction_source_request.ml"
    , "c4993419caf79a25be6543253f8e63549918ed3010b5b6017991ddad7c9d0a7b" )
  ; ( "lib/agent_session/instruction_admission.ml"
    , "e4459cb6ffacb836f90fe35a17faca04943d080798290e1b8a9ed98afa484a16" )
  ; ( "lib/agent_session/instruction_native_admission.ml"
    , "d5617a7fb0e72b7c3d8ff94f8fec8fe0a8650a6b797e688400a87cc2035ac925" )
  ; ( "lib/agent_session/instruction_native_request.ml"
    , "4960a708518b49ab66dcd01ab15ae99b6bac611e603c6f1c46c71e6653ba8a27" )
  ; ( "lib/agent_session/instruction_native_tool.ml"
    , "6145372a8e88260b77f77cdb6ed8b9567e4d6b5102aa5f26fc5d954f76145139" )
  ; ( "lib/agent_session/instruction_command_admission.ml"
    , "54d7f3d0c509406d0084eecb34854a5cbea47908d50c44bbc2d7ccfca4c49976" )
  ; ( "lib/agent_session/instruction_service.ml"
    , "c1cae0622c1a68c3ae0dadac0cb3755a587417fcb0cf3f4b5edfacaeb2982694" )
  ; ( "lib/agent_server/developer_claim_deadline.ml"
    , "04bb3b5eae209fbc7af9b938e65d9896f9838421dbc48ea823ac802fd0803f63" )
  ; ( "lib/agent_protocol/invocation.ml"
    , "1664307adefac83f0bff4ecc2e67bbc7ab71f121b036e6326030a5804ab290c5" )
  ; ( "lib/agent_server/developer_background_job_runner.ml"
    , "89b0cd7a7d04842c81b3bd632a1bf1f8563b9e31af9e83b571795f521da2b720" )
  ; ( "lib/agent_server/developer_managed_tool_executor.ml"
    , "7037fa9099d655af26c9596eaf901caffa2d02edd5d14b6e99b38bb4d737f1ec" )
  ; ( "lib/agent_server/developer_script_job_execution.ml"
    , "4e30550f6585c6afa97ddd12a12042d0be26b84f18aad7f6f92b2f3a3bf4bb32" )
  ; ( "lib/agent_session/session_management_channel.ml"
    , "500d45a8cbe760a50bc256df47255dc6fa5e1305f74a543d49c9c2e02e36de51" )
  ; ( "lib/agent_server/session_helper_policy.ml"
    , "e88605f0d6f0fc512589ccd9546e5f8e741216dfa321ec66919b64c88d2958e1" )
  ; ( "lib/shell_access/shell_access_v2.ml"
    , "06f99e979fd39c4c72590ffe6faee9f0790a77b9ad141e4d05919b68771b51cc" )
  ; ( "lib/agent_server/delegated_runtime.ml"
    , "acedc14b103f77c9f54a7f659daae9198cc36fa9ef82af5cb4f28af55c32017f" )
  ; ( "lib/agent_server/delegation_lifecycle.ml"
    , "5bfed8ecc3cd3f7976457adbab4a5e875f8b56bd08f4856d18149c694303ad49" )
  ; ( "lib/agent_server/managed_output_cursor.ml"
    , "d5db2b48aa4a0c4a1b05704054ef3c41557ea31c8c4b92d8f538406185af472c" )
  ; ( "lib/agent_server/managed_output_page.ml"
    , "2e6f63c3c97e3a87aa6d37b2ddc94e497b0f87d962d2b4d31586672aac945d5b" )
  ; ( "lib/agent_server/recurrence_authored_source.ml"
    , "6d9ab0d58716b09e6cb709a37d6dcb3398a4c4af6bf33a45727aded557e865d2" )
  ; ( "lib/agent_server/native_resource_authority.ml"
    , "6c1d600bb41fa15279a82bc66b479c850778eda7ba40dac7911a4819df9b991b" )
  ; ( "lib/agent_server/delegated_policy_call.ml"
    , "1527c1a798b49bb509cea4ab3eba4c1dbcbec82396d72f094f430b0d53c19fc0" )
  ; ( "lib/agent_server/delegated_policy_vector.ml"
    , "6004f0ff15cf25b2282aa7b510eced951f4ecdabf223285799d6f7956263500b" )
  ; ( "lib/agent_server/descendant_accounting_host.ml"
    , "3baff14cf9c606e899c08f95805cc3ca9f070827b46ec531e1c230314820b0fd" )
  ; ( "lib/agent_server/session_factory.ml"
    , "78f61a8bb786bd959f0fd1b16619c2d70bd20dce47b22cbcf572a0a770fd5472" )
  ; ( "lib/agent_session/authoring_context_tool.ml"
    , "6e318b45f799b485f802dc6d5e4d8bc031721e379cc8800f6ed484d59694840d" )
  ; ( "lib/agent_session/authoring_reference_scope.ml"
    , "5db19890dff8b3188045d23899019afa642c6816f42f4a32b17e41365bf78983" )
  ; ( "lib/agent_session/authoring_services.ml"
    , "ca6aeaf3a1cede3d6642b9b2e188617c9e939375257b00d9a6481dd86d96c908" )
  ; ( "lib/agent_session/authoring_validation_tool.ml"
    , "b0196f46eec21c0ecead94b1165e7ec5ccfc5c3662a955d4f6230a22278ec59b" )
  ; ( "lib/agent_session/delegation_authority.ml"
    , "f918642670a3bbd15c6d6dabb4e2d23423ffb1a16d722c08503a187d8343b44a" )
  ; ( "lib/agent_session/generated_definition.ml"
    , "ed31e81ef5b6a8353ddbf74307857756455316a924dcbdbb84c2a7f6f13c7f9c" )
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
    , "f82d46404e5083e7e20610a1e3bef6ac8d2c2a2fc1fbd8fad4e5fa104a1c8a07" )
  ; ( "lib/agent_session/managed_submission_tracking.ml"
    , "6c8804b2d0c2033631b965769af63c5240afeb6dabff72695c818dc1a68baed8" )
  ; ( "lib/agent_session/managed_wait_tool.ml"
    , "2482da27415b0f542bbf5e2fa61bf97de70fb047790cb6ee5100ced465b43708" )
  ; ( "lib/agent_session/native_tool_invocation.ml"
    , "bd518e1e8c19a622141e881060cd039e92e25bd0dc33caabf1925a9c105773a3" )
  ; ( "lib/agent_session/native_tool_policy.ml"
    , "3b6ff346554c99eab19f15b35562acfc8575d556226ca4881ac6ed7e2213c5f5" )
  ; ( "lib/agent_session/one_off_execution.ml"
    , "c1258ae2a13db05133c06681a30e7e8038e5e439d20d2eae03bf5e832878ed65" )
  ; ( "lib/agent_session/run_chatml_tool.ml"
    , "eadb7cf2ac6622e977db08798fbfce7661062c47883cdf4f28700f1c85249b75" )
  ; ( "lib/agent_session/script_tool_calls.ml"
    , "a242925f1b617445e36748161773678f0e0adad74268cbc006988a116c0d92e4" )
  ; ( "lib/agent_session/session_management.ml"
    , "18431c4fd97a2d24a72b927fe8f0d20b8aea561f1f119d8f122d943f2a137f86" )
  ; ( "lib/agent_session/session_management_native.ml"
    , "8ccb5239373707dacbceec1ce9a0c5146eea5360921ad0a5209bcc2928bce806" )
  ; ( "lib/agent_store/delegation_store.ml"
    , "d3704db75bdf7a7b49b77042c52368fab87565443fa527ecba2a84927526ba09" )
  ; ( "lib/chat_response/authoring_context.ml"
    , "4a688ad7aa00adf9dbaf45333f60ca20b9b0e418b394126ac3ac93acd972b43e" )
  ; ( "lib/chat_response/authoring_validation.ml"
    , "71b6c71022bde3b92bfd1954904fc516994532e938b74ebe0fb3b2f6ca948694" )
  ; ( "lib/chat_response/generated_admission.ml"
    , "137adaa4beca5c2d5f0d6f36c5c18e25ffe3370b5086376720b0df1a3cb9bc17" )
  ; ( "lib/chat_response/managed_tool_registry.ml"
    , "e53613a0ee0695e433ac88bae5576dacd76960f8542d2c5d6689b4b15ecffa43" )
  ; ( "lib/chat_response/tool_capability.ml"
    , "babf318ca64c34e0c1e4b14d498a796cff36b2ac521808d2dc814cb32e087c9f" )
  ; ( "lib/chat_response/one_off_request.ml"
    , "7427759464798805cd25c014bf8e1bd903066fea5c3678d806e3c64ec3ee14a3" )
  ; ( "lib/chat_response/one_off_script.ml"
    , "cffac7c805a03e71b84b168d7686b0bac019db7c4d431210d4560331a8f43986" )
  ; ( "lib/agent_session/prompt_source_owner.ml"
    , "dfcdd3b0ac3beb8890329dcb39478245bfec0b6ce6b8f895ea705ad094517340" )
  ; ( "lib/agent_session/prompt_source_lease.ml"
    , "7413c0ed3e84375cd7e95343ceb83d9485c02b4d378ff940336beb449680dbe0" )
  ; ( "lib/agent_session/prompt_catalog.ml"
    , "68c382d61c3afed36e41f56ddfb1700ffa85bc96e9310b18f210104aab158173" )
  ; ( "lib/agent_session/runtime_builder.ml"
    , "69e3d11162f6c0217b6b12b4654b2b4af0c730fd56532a9e8a20575b5b1d49cc" )
  ; ( "lib/agent_session/authored_source_preparation.ml"
    , "d8eea801d69ffa262cfc1d0a9bee45123ce8d4b08f74c25c67df450cd247600c" )
  ; ( "lib/agent_store/prompt_source_receipt.ml"
    , "d1089027dc9f0e69fb6db67815b2de78086d88a29334ac7c12ec6aa9f9b8258e" )
  ; ( "lib/agent_session/recovery_history_native_request.ml"
    , "9a590ba486c2dac0c9e6ed626a8212c5d3873faf9f41b09589b2273456ac934c" )
  ; ( "lib/agent_session/recovery_history_native_tool.ml"
    , "f718fdba8b290d35a75b4a795ae3e0b0d24ff9597c0653bd7387b162ff59d688" )
  ; ( "lib/agent_session/recovery_history_native_service.ml"
    , "a62ebb0bd41532a68115dc543da957c2122595746792c91724a6a82d59fdb860" )
  ; ( "lib/agent_server/recovery_history_native_host.ml"
    , "f8bb0805d894ec104d0ee3e74d1fbd55c59be2c5cd5b2299b228703ed616cab0" )
  ; ( "lib/agent_session/recovery_effective_release.ml"
    , "3d599d9e734011aabf69c966d74adda9481a3be3f69b8cd25c98574373b1d401" )
  ; ( "lib/agent_session/recovery_note_access.ml"
    , "526b0eff312e38a1b8d22c923ff0a6fd4a9927c553dbf1472d2bfbe75df062e8" )
  ; ( "lib/agent_session/recovery_note_native_request.ml"
    , "b3a49e0c30765f5e065717604f8c006bcb93d3ed6acd85b3219cbcc2f3885a73" )
  ; ( "lib/agent_session/recovery_note_native_tool.ml"
    , "b932759361cd23d5e9dfc65a3fda0779a07e9c69437630e79a1968516c7d8879" )
  ; ( "lib/agent_session/recovery_note_native_service.ml"
    , "400a199e28a3e91a813cd2811f072a67e6b5503c0c07d2d4b9cb6e3297c37e31" )
  ; ( "lib/agent_session/recovery_note_query.ml"
    , "2acbe58eca4e1606832e9731b8e36f5a2011c10bd1cdcbd9fa95123f4b5b9e26" )
  ; ( "lib/agent_session/recovery_note_share.ml"
    , "acb9e33cf47fa9e267b3dc1adf562950ca87afd5a13f9a023259c82face86254" )
  ; ( "lib/agent_session/recovery_context_native_tool.ml"
    , "905953db3ad5f80c9abb121ef4c1f6eb92ae0e456548b9879148fd46c6e475d6" )
  ; ( "lib/agent_session/recovery_context_service.ml"
    , "9d333f72f525e43cb0c20fef9bf035a76e11cd9d29f9e34714c75bbfeb4be440" )
  ; ( "lib/agent_session/recovery_authored_checkpoint.ml"
    , "2b423ab992ce0b81cb9e7589328e2abf712d304b4ac9f31ce4b0ebb7fe76d68a" )
  ; ( "lib/agent_session/recovery_checkpoint_cut.ml"
    , "6f4cef6f87a0be3cabd81859cc209b44b6fdf36bc191ea6c1387ecd71c247c5a" )
  ; ( "lib/agent_session/recovery_rollover_batch_cut.ml"
    , "1480c60c9b1ee9653f686cd084699f4c6c40e727d2b804343b2c1cf1d9140571" )
  ; ( "lib/agent_session/recovery_rollover_evidence.ml"
    , "0a367a7b377f30130ade6664a6391273aab671380dc081d757af3b4d2d705b9a" )
  ; ( "lib/agent_session/recovery_ordinary_selection.ml"
    , "9898e1f33305be79c471607cac6805ea1c0edf3392658923f9d440b3b16c1e5a" )
  ; ( "lib/agent_session/recovery_rollover_hint_ref.ml"
    , "03a1c9849d5ae2e2b9576e306b27d2219732e382632e78c15e073ef78c6fe550" )
  ; ( "lib/agent_session/recovery_successor_guidance.ml"
    , "ff53ca018e661ae237386f751e0b31f5a8c36c74cb6a35e44f1bbf35a2b12f80" )
  ; ( "lib/agent_session/recurrence_native_admission.ml"
    , "6dedc770d93c9329092e2ec809b1e5f4158065aef2a9fba365770f012dc71297" )
  ; ( "lib/agent_session/recurrence_native_schema.ml"
    , "b35c2fc2e32603cfc6442f775ba80deef311d2a208768187e7aa023da2028b8a" )
  ; ( "lib/agent_session/recurrence_native_request.ml"
    , "03f798a7c41d5e9faffa17c1af94939238518cccac4592684ab1a155386c22ea" )
  ; ( "lib/agent_session/recurrence_native_tool.ml"
    , "816ff274052c38bb80b6a841cc1869143f7ac5459cc7df4198f326b23d650c85" )
  ; ( "lib/agent_session/recurrence_query.ml"
    , "cc374dc00364a78cd96534fb1852d4294b4144d592839ad358036d373dae015f" )
  ; ( "lib/agent_session/recurrence_target_view.ml"
    , "f7c130f424774bab58b92e4e972df930d8cbd6052fd5ec7bab6688b2dbd0d2b7" )
  ; ( "lib/agent_session/recurrence_goal_capture.ml"
    , "4645402d84d9e9367afa8713f6afe04a893d69195b32134e9ec596cdb04d11a9" )
  ; ( "lib/agent_session/recurrence_service.ml"
    , "32eb74206cd9831f192dadad6744d44b3151f95293369d8282ce1ee9af2eda54" )
  ; ( "lib/agent_session/goal_native_admission.ml"
    , "b5c03a89fd40afb66932b6a17c32f79d24c515371b54cb85ede32d4c73335082" )
  ; ( "lib/agent_session/goal_native_start_tool.ml"
    , "9e80d161cbf8d2aacd0feb278ffa0ea1b341707646f170c264054e3421e889f6" )
  ; ( "lib/agent_protocol/goal_resume_request.ml"
    , "68869b54344fb5ccf197f96a793a37bb5c7b59c1b5269b7ad6eb38fd68c4d760" )
  ; ( "lib/agent_session/goal_native_metadata_tool.ml"
    , "48ccb38137f24eb250c34dea60c2401102dde18a27551b22b4668b0d9adb2e1b" )
  ; ( "lib/agent_session/goal_native_metadata_service.ml"
    , "2c16c09adda2e82bf0864deebc5882f5ce215c8f779bf6f5f73a6fd816400af5" )
  ; ( "lib/agent_session/goal_native_metadata_view.ml"
    , "49c0331d49f9694d4a82311eac01eb5faf83941a37dc44e5adcf3d67ee38dfc2" )
  ; ( "lib/agent_session/goal_run_evidence.ml"
    , "811900bf74678b6d342a2f63375d0598f06758ea4a709853007d8c2de32da02a" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "runtime.recovery_history"
    , "38a2319a85a53c95913e14ab7a2d16d698a26649d6e7b5ad8dd8fa2ef3b40d44" )
  ; ( "tool_v1"
    , "runtime.recovery_history"
    , "e59bf374b02e919455ee7cab02cd3830ec3d172eb48450ba665d69687d0b17de" )
  ; ( "moderator_v1"
    , "runtime.recovery_history"
    , "e4d25379c584d34581cbce8bea7f402525c0073ed2504ad8834e134b45f5a1b2" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery_history"
    , "450fbde15e490ae9b67b80085c4ed7a16bb4c4bcde631a03d55f94af82848311" )
  ; ( "one_off_v1"
    , "instructions.sources"
    , "4cbb224a500d3e811fe54e5cacfd8c8f91612e6fa2236c79f42f4b0c7655b2e3" )
  ; ( "tool_v1"
    , "instructions.sources"
    , "e492f0d45dec5c0235390596e998941a1b6ecdd1f1343f41f620cdc6c9ea4067" )
  ; ( "moderator_v1"
    , "instructions.sources"
    , "a8a1474fa78ef8f1638bf2b1fa22e638d6f2262256db336c8ac4ac34f802d271" )
  ; ( "delegated_moderator_v1"
    , "instructions.sources"
    , "673480da622c4484163c03d0f00beaab7465e6d5fa2425a44a7e5bd818aa48bb" )
  ; ( "one_off_v1"
    , "runtime.native.requests"
    , "5ce300e2cce67ff2c39b548e3fd8d44b244207c7d7e6637626fe6ece0f7780ff" )
  ; ( "one_off_v1"
    , "authoring.reference"
    , "3144c80ec148fea7da1a3e326d550cd088d5406db6cec9ad046213a78bd818cf" )
  ; ( "one_off_v1"
    , "runtime.delegation.generated"
    , "4c3d977be2e96a35c480fd15d021acfc3481e0940ea9aae24a4d40fe5560def3" )
  ; ( "one_off_v1"
    , "runtime.delegation.creation"
    , "7f9802bf87113575fdc33519a0a1132001c3a68006dbe5867a59ce557527a01a" )
  ; ( "one_off_v1"
    , "runtime.delegation.submissions"
    , "b52f081399fa8011c71b42b2580162f0d02a7211c2258fd4deaaa027385cb864" )
  ; ( "one_off_v1"
    , "runtime.delegation.output"
    , "02efeba5c277e021b885e3bf0f51d5e42cbe393b9e2aac0614a72c85ec26ee8c" )
  ; ( "one_off_v1"
    , "runtime.delegation.stop-helper"
    , "91fc7b035669ee0523bf0de0200c56453fb02f31bb3f3142c3932a1bc110dd39" )
  ; ( "tool_v1"
    , "runtime.native.requests"
    , "744d516de226470f4f9fe73951c9fe399b0d42028e1e61da7add0f9d61cf39e1" )
  ; ( "tool_v1"
    , "authoring.reference"
    , "e8ad92de376e5f06f713365be43cdfacd88f3ba86016000cee7965865306ce60" )
  ; ( "tool_v1"
    , "runtime.delegation.generated"
    , "020561ed129d3557db50c25e8c20e1ac70a0cc88db509a68f5c348908c68cc27" )
  ; ( "tool_v1"
    , "runtime.delegation.creation"
    , "4e846227dea2625320fe857a72f1664e83d6c71bd9ec3dfd0f37844a74e6e8f6" )
  ; ( "tool_v1"
    , "runtime.delegation.submissions"
    , "01f45fedb71d8887a5ee218999b5d235a8df9735ae4e6f4ef53960d11f6e79dd" )
  ; ( "tool_v1"
    , "runtime.delegation.output"
    , "028a03c46a94d177581c665be06a02e3b4a53ecb82128966b6fac7651972362b" )
  ; ( "tool_v1"
    , "runtime.delegation.stop-helper"
    , "98fa21790b5815ecd65054da1b5f83fcd90540963b3c7e1c9164f18db5d70d39" )
  ; ( "moderator_v1"
    , "runtime.native.requests"
    , "2cffdc2c42b8dc4daf6c040ba46553b7ba919860809f589a19b800e8bc81c0bd" )
  ; ( "moderator_v1"
    , "authoring.reference"
    , "e0c3b31a2e977b6f79ba24a61288c9091a4befa260cf9ba7de97b9a405e0dd64" )
  ; ( "moderator_v1"
    , "runtime.delegation.generated"
    , "846e1d9d4c0ea61265dc7d9a3ecc96c2b5df4deb518f4d0ddd820692866c11f5" )
  ; ( "moderator_v1"
    , "runtime.delegation.creation"
    , "8348192ef4efb13e3332269ca7afbe9457904b22522ae923720e64aa4a799ff9" )
  ; ( "moderator_v1"
    , "runtime.delegation.submissions"
    , "6c225c3a41aab7c943ec21694cb06cfd488add317fac63bf83c8cef7536a7c48" )
  ; ( "moderator_v1"
    , "runtime.delegation.output"
    , "998d69ec781589481d1f3acee67df386ed05fb477a073796aaa9ea66e9b3fc7a" )
  ; ( "moderator_v1"
    , "runtime.delegation.stop-helper"
    , "1f32ddb61f69acf6adc46f9455d0f159e4305747f9738e213e4c29983769b6f2" )
  ; ( "delegated_moderator_v1"
    , "runtime.native.requests"
    , "ff2e3bb85a521f6daf0b6f64f2d6fb6f669e7decf35c98335d5d7dbb986a4c28" )
  ; ( "delegated_moderator_v1"
    , "authoring.reference"
    , "a53e47861488d01e10a95ad400c675ede2648ce0b05e6e4b4f258b12f5ad70f2" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.generated"
    , "7f65a5404d89d717fbd07896c183d04b35ec156fc846d29e5e5a02b76af624d2" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.creation"
    , "882576799c6582212dc212b8228ebabe211b6a5f806a3353cb14d342c9cfdcea" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.submissions"
    , "85358ce9691063b6454e2086f2341ed41a2a382b95529d16e36d7bd6acca207f" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.output"
    , "b9f66e8f2dd3a1d332cab1e35d93717331afbccb7bb12ae28a0bc471ff2c0205" )
  ; ( "delegated_moderator_v1"
    , "runtime.delegation.stop-helper"
    , "1cc2b63d172178c6d22bdeee69376def6c26e8e3e4ee56a9a54671e168b49de9" )
  ; ( "one_off_v1"
    , "runtime.recurrence"
    , "3ec1bae20eb9312ec86f282e96b505cadbbfbd9a8eada03326c9c1f5a926e1aa" )
  ; ( "tool_v1"
    , "runtime.recurrence"
    , "f4a39986db916b6891bfbf8d2fc1e1e89ae61b51ee783b5b22d9285e5c2f9a8d" )
  ; ( "moderator_v1"
    , "runtime.recurrence"
    , "04fb3591cde63f3c53252282812b8dcfb83c164417b5b6a3f6e73e8b0ab7c057" )
  ; ( "delegated_moderator_v1"
    , "runtime.recurrence"
    , "9caa6c7750073640259235210b396ff8ed629382d3f07dadab45e6716bdf0fed" )
  ; ( "one_off_v1"
    , "runtime.goal_metadata"
    , "44ad5546e7c97ff0a76627d5fc911a181a460f279a94fd41722793a35c7f9a06" )
  ; ( "tool_v1"
    , "runtime.goal_metadata"
    , "f645a4c4cb3063ec6722cf8e02e4dc1a23bef9f6b2d8856354f330eaf751dde0" )
  ; ( "moderator_v1"
    , "runtime.goal_metadata"
    , "6798c9f05f914a2061ea871a59dc76dc94efeeb6bad53d3bb316d3fb6b5490b4" )
  ; ( "delegated_moderator_v1"
    , "runtime.goal_metadata"
    , "4f6d7742b639ab6d13441a1ca1b3b654d6a1ba1c37b8d950973e2b108f966351" )
  ; ( "one_off_v1"
    , "runtime.context_windows"
    , "0d3320e4f43369c7f1dff657ea82757658bbac02c5c827a592242fbdfe67b10a" )
  ; ( "tool_v1"
    , "runtime.context_windows"
    , "61f833deeb51dd64c5c006430a9638dc90481e029ffa3916ec422284bf7cab4d" )
  ; ( "moderator_v1"
    , "runtime.context_windows"
    , "ab826a126a647f94866e13475ab21632d0cb7ccdb38bf503d093770a6fe46980" )
  ; ( "delegated_moderator_v1"
    , "runtime.context_windows"
    , "c70cb9ce2a3df3adf6d7271d9cda4b9bd84ba15b7c6db62224f46f776809d18e" )
  ; ( "one_off_v1"
    , "runtime.recovery_notes"
    , "29bd47eefc500611825c2eafd5989929fcb4a9629e270828ad5618188571b41d" )
  ; ( "tool_v1"
    , "runtime.recovery_notes"
    , "a43ece4980f874811c0576a14636ac4b71f928920e3aa80ba98e80034dc387a2" )
  ; ( "moderator_v1"
    , "runtime.recovery_notes"
    , "56b2bd3caa4f6e4e5b63eaa437dd34233730359b795253009b17c456f5e98c48" )
  ; ( "delegated_moderator_v1"
    , "runtime.recovery_notes"
    , "9b93296d1c0bdb428ce6aacf184f221e262e517bf7648b816d0d6d549168ed5d" )
  ]
;;
