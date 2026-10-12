(* Maintained ChatMD declaration rules. Review source changes and topic closures;
   CI never regenerates these pins. This is not automatic discovery of all
   legacy shell/MCP or native-runtime semantics. *)
let features =
  [ ( "markup.syntax"
    , "Closed tags, attributes, comments and RAW boundaries"
    , [ "lib/chatmd/chatmd_ast.ml"
      ; "lib/chatmd/chatmd_lexer.mll"
      ; "lib/chatmd/chatmd_parser_engine.mly"
      ; "lib/chatmd/chatmd_parser.ml"
      ; "lib/chatmd/chatmd_parser_builder.ml"
      ; "lib/chatmd/chatmd_located_ast.ml"
      ; "lib/chatmd/chatmd_tokens.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.literal-resource-text"; "test/chatmd_parser_test.ml" ] )
  ; ( "sources.capture"
    , "Captured imports, relative sources, namespaces and bounded expansion"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/source_loader.ml"
      ; "lib/agent_session/prompt_revision_builder.ml"
      ; "lib/chatmd/chatmd_import_expansion.ml"
      ; "lib/chatmd/chatmd_parser_input.ml"
      ; "lib/chatmd/chatmd_owned_parser.ml"
      ; "lib/chatmd/chatmd_source_bundle.ml"
      ; "lib/source.ml"
      ; "lib/chatmd_shell_spec/source_ref.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.missing-import-rejected"
      ; "test/chatmd_source_bundle_test.ml"
      ; "test/chatmd_owned_input/input_tests.ml"
      ; "test/chatmd_owned_input/capture_tests.ml"
      ; "test/chatmd_documents/document_tests.ml"
      ; "test/chatmd_prompt_content/content_tests.ml"
      ; "test/builtin_instructions/builtin_instructions_tests.ml"
      ; "test/agent_docs/docs_child_authoring.ml"
      ; "test/chatml_source_owner/owner_tests.ml"
      ; "test/chatml_source_owner/project_tests.ml"
      ; "test/prompt_content_custody/owner_tests.ml"
      ; "test/chatmd_content_capture/capture_tests.ml"
      ; "test/chatml_source_owner/catalog_tests.ml"
      ] )
  ; ( "generated.messages"
    , "Plain initial messages without resource loading or stored identities"
    , [ "lib/chat_response/generated_admission.ml"
      ; "lib/chat_response/initial_prompt_history.ml"
      ; "lib/chatmd/prompt.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.resource-message-rejected"; "test/generated_admission_test.ml" ] )
  ; ( "generated.configuration"
    , "Model and reasoning choices with bounded, identity-free generation config"
    , [ "lib/chat_response/generated_admission.ml"; "lib/chatmd/prompt.ml" ]
    , "chatmd.definitions"
    , [ "chatmd.plain-agent"; "test/generated_admission_test.ml" ] )
  ; ( "tools.inherited"
    , "Exact inherited references, two-stage narrowing and no implementation replacement"
    , [ "lib/chat_response/generated_admission.ml"
      ; "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.inherited-reader"
      ; "chatmd.builtin-reconfiguration-rejected"
      ; "test/generated_admission_test.ml"
      ] )
  ; ( "scripts.lifecycle"
    , "Single extension lifecycle moderator and generated surface restrictions"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.lifecycle-moderator"; "test/generated_admission_test.ml" ] )
  ; ( "scripts.sources_and_limits"
    , "Inline/source exclusivity, explicit kind/API, limits and script identities"
    , [ "lib/chatmd/chatmd_script_declaration.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ; "lib/chatmd_shell_spec/duration.ml"
      ]
    , "chatmd.definitions"
    , [ "test/chatmd_extension_test.ml"; "test/chatml_script_routing/startup_tests.ml" ] )
  ; ( "tools.extension_bindings"
    , "Standalone and moderator tool bindings and declaration registry checks"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd_shell_spec/extension_spec.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ]
    , "chatmd.declarations.schemas"
    , [ "test/chatmd_extension_test.ml"; "test/managed_tool_registry_test.ml" ] )
  ; ( "tools.schemas"
    , "Captured input/output/completion contracts and closed schema dialect"
    , [ "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd_shell_spec/tool_schema.ml"
      ]
    , "chatmd.declarations.schemas"
    , [ "test/tool_schema_test.ml"; "test/chatml_composition/standalone_tests.ml" ] )
  ; ( "tools.uses"
    , "Exact standalone dependencies, duplicate/cycle checks and moderator exclusion"
    , [ "lib/chatmd/prompt.ml"; "lib/chatmd/chatmd_extension_declaration.ml" ]
    , "chatmd.declarations.schemas"
    , [ "test/chatmd_extension_test.ml"; "test/managed_tool_registry_test.ml" ] )
  ; ( "authoring.policy"
    , "Auto/manual/preload declarations and inherited authority boundaries"
    , [ "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd_shell_spec/extension_spec.ml"
      ; "lib/chat_response/authoring_policy.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.plain-agent"; "test/authoring_policy_test.ml" ] )
  ; ( "authoring.help"
    , "Explicit package/task/topic/helper metadata without new authority"
    , [ "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd_shell_spec/authoring_metadata.ml"
      ; "lib/chat_response/authoring_policy.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ]
    , "chatmd.definitions"
    , [ "test/chatmd_extension_test.ml"; "test/authoring_policy_test.ml" ] )
  ; ( "tools.authored_persistence"
    , "Author opt-in, invocation mode and reusable instance IDs"
    , [ "lib/chatmd/prompt.ml"; "lib/chat_response/agent_tool_contract.ml" ]
    , "chatmd.definitions"
    , [ "test/chat_response_conversion_and_prompts/agent_tool_persistence_test.ml" ] )
  ; ( "validation.inert"
    , "No executable preprocessing or initializer evaluation during admission"
    , [ "lib/meta_prompting/preprocessor.ml"
      ; "lib/chatmd/prompt.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.meta-preprocessing-rejected"
      ; "chatmd.validation-does-not-initialize"
      ; "test/generated_admission_test.ml"
      ] )
  ; ( "tools.file_roots"
    , "Configured readable roots, source/host path variables and confined calls"
    , [ "lib/chatmd/chatmd_read_file_declaration.ml"
      ; "lib/chatmd/chatmd_read_file_spec.ml"
      ; "lib/chatmd_shell_spec/path_expr.ml"
      ; "lib/chat_response/tool.ml"
      ; "lib/functions.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"; "test/read_file_roots_test.ml" ] )
  ; ( "tools.shell_binding"
    , "Named shell runtimes, explicit modes and centralized process authority"
    , [ "lib/chatmd/chatmd_shell_declaration.ml"
      ; "lib/chatmd_shell_spec/shell_tool_spec.ml"
      ; "lib/chatmd_shell_spec/manifest_compiler.ml"
      ; "lib/chat_response/shell_tool.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"
      ; "test/chatmd_shell_parser_test.ml"
      ; "test/chat_response_shell_tool_test.ml"
      ; "test/shell_activation/activation_tests.ml"
      ; "test/chatml_native_hooks/shell_tests.ml"
      ] )
  ; ( "tools.agent_backed"
    , "Local/remote authored agent definitions versus inherited generated children"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/source_loader.ml"
      ; "lib/chat_response/tool.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"
      ; "test/chat_response_conversion_and_prompts/agent_tool_persistence_test.ml"
      ; "test/chatml_script_routing/driver_input_tests.ml"
      ] )
  ; ( "tools.mcp_binding"
    , "Catalog selection precedence, scoped connections and captured tool schemas"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chat_response/tool.ml"
      ; "lib/chat_response/mcp_discovery_cache.ml"
      ; "lib/mcp/mcp_tool.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"; "test/mcp_delegation_test.ml" ] )
  ; ( "messages.resources"
    , "Resource-bearing root messages remain inert during parsing and are rejected in \
       generated children"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chat_response/converter.ml"
      ; "lib/chat_response/fetch.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"
      ; "test/generated_admission_test.ml"
      ] )
  ; ( "messages.trace"
    , "Stored tool-call correlation and rejection of fabricated generated history"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chat_response/initial_prompt_history.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"
      ; "test/generated_admission_test.ml"
      ] )
  ]
;;

let implementation_sources =
  [ ( "lib/chat_response/agent_tool_contract.ml"
    , "ee2119b9d7e7530c13c6780e35fb5cd03dae6781a58438193987ee55d79fa714" )
  ; ( "lib/chat_response/authoring_policy.ml"
    , "78a9aaae6ad9a0626577dbcd83c8138458550c104bb99fa7808bbed1eb97a1d3" )
  ; ( "lib/chat_response/generated_admission.ml"
    , "8c422ff1ad004059d86a2160219fb2a5d493899dbf780d6f77ba7c2f52548add" )
  ; ( "lib/chat_response/initial_prompt_history.ml"
    , "daf50b9c9a5337b562b5d78c8f37b12033aba72f561491f506443095e694fed9" )
  ; ( "lib/chatmd/chatmd_ast.ml"
    , "1e23a862fcee8b5df7a410d62477b83cfb25bdd2b9a5ce67c9ce05ff50e2ccc9" )
  ; ( "lib/chatmd/chatmd_attributes.ml"
    , "778ffad3961edb8e13c2a82d94fd8f83517833cb8aae4135728627e7b6a428b6" )
  ; ( "lib/chatmd/chatmd_extension_declaration.ml"
    , "86149832fe114b2b3db920887f323f67ee1c3ccbb6ccd914deddec6604b57037" )
  ; ( "lib/chatmd/chatmd_import_expansion.ml"
    , "352742796302f685bc0c465b5b4dc625a2db47151fa62341b84ad1119a60eac3" )
  ; ( "lib/chatmd/chatmd_lexer.mll"
    , "a34003587cc6bf0b985eae0ef7b1fdb674d2313ac1fed8c41fdcd269162f3070" )
  ; ( "lib/chatmd/chatmd_parser_engine.mly"
    , "60bbfe6052bca41a3c95367a34785a737aba691d2bfabeb70a388976ea7c7e73" )
  ; ( "lib/chatmd/chatmd_script_declaration.ml"
    , "bc397210046f19941127779f8e7ab6b052eb07271398cbe10ba6b9b9c1c787df" )
  ; "lib/source.ml", "6c18a151df64c7489a2fcb745e5f23e54c754696d0bd4b27c53505846317299b"
  ; ( "lib/chatmd/chatmd_source_bundle.ml"
    , "c3e0f8f89c8538ddcec98873739455a673b0e6e894282c21c96046c096bb6ae5" )
  ; ( "lib/chatmd/prompt.ml"
    , "74bf3366740e30f3d9213b8e6ab153a5c686fbbd2a651c37e21728057265d914" )
  ; ( "lib/chatmd/source_loader.ml"
    , "202688ce4248d9042c4d865f45f5909a593506d5f1c269c5db0419d9c3d4b85e" )
  ; ( "lib/chatmd_shell_spec/authoring_metadata.ml"
    , "8fa283f545896b7efbaab8bcf17a3ab7549d61c04873403c94961fc4ca0b789d" )
  ; ( "lib/chatmd_shell_spec/duration.ml"
    , "ceed8e65e4ca9b5292e94e0d1e929288c1d4d62c2ed58ec46425e5076c525231" )
  ; ( "lib/chatmd_shell_spec/extension_spec.ml"
    , "9350ac42d6c95d436e610b988569ad43b1833d77280e1a99501d5cee266a11cf" )
  ; ( "lib/chatmd_shell_spec/source_ref.ml"
    , "a836f06b96426664cb2ee5aeb0038c1bc54829cfe7eabde962b6865f64164043" )
  ; ( "lib/chatmd_shell_spec/tool_schema.ml"
    , "bff94e09d295dd3fe8edb56e961b293cc760fb99a534bbec5e88ed56a345e00e" )
  ; ( "lib/meta_prompting/preprocessor.ml"
    , "5eedb68b3db23b16a8769522104d2f4e6f9f2e3cce2e11e40c92dc5c2ad879b8" )
  ; ( "lib/chat_response/converter.ml"
    , "f5efe54fe5b97001bed86782b9ade136811996fbd61930330ed13b73ee39b153" )
  ; ( "lib/chat_response/fetch.ml"
    , "170db560c0693c7ec37f2a34c1907133feaffddd079571fcc32552bab59549ca" )
  ; ( "lib/chat_response/mcp_discovery_cache.ml"
    , "b7c02f701beaaf87c1cd04bf06065cb5564f2702f923704d8e8676f13d5724fb" )
  ; ( "lib/chat_response/shell_tool.ml"
    , "2450c632c18067d0c984087461b9085f24314dd79a026a42a4f35b1014816f92" )
  ; ( "lib/chat_response/tool.ml"
    , "b6b01a30a41a4f3c01648448368f24e6ae0962ca079b84f1ebaf837679d462d8" )
  ; ( "lib/chatmd/chatmd_read_file_declaration.ml"
    , "f604a8e767cae51c15b31eb0d8a266d9ea1efd429f77cbb7b068940d38a2768e" )
  ; ( "lib/chatmd/chatmd_read_file_spec.ml"
    , "9864c1077c135be35f6109c537a75447199131899e3cfbec70ac4c00012c8f63" )
  ; ( "lib/chatmd/chatmd_shell_declaration.ml"
    , "2b5d331094c7abf2b61f5aa537cc836dae313f80fa9bef7575ea7ad23c61027a" )
  ; ( "lib/chatmd_shell_spec/manifest_compiler.ml"
    , "f10c82f13dc06b2b821adf2306ab6117ed19aa613e1ee8ec96ea9bfc7747bf1e" )
  ; ( "lib/chatmd_shell_spec/path_expr.ml"
    , "c64b5c5033c352628fb8fc89b13fb382ee965b999c29293eb0e8f9df220ce97a" )
  ; ( "lib/chatmd_shell_spec/shell_tool_spec.ml"
    , "85214971617bee329014f225c9374c0c9202130be66309c84d54c23944ed73c4" )
  ; "lib/functions.ml", "05c5428d7870e3fe9711a3f3239ae1e0919c86fa5022c4ea5c7944559d7d2eb2"
  ; ( "lib/mcp/mcp_tool.ml"
    , "f9e69c49231b625fb479a77182b606113ae9369dce8df1c67c99c3eaddfdac49" )
  ; ( "lib/chatmd/chatmd_parser.ml"
    , "e9fa656cee4e6ee30bca2f3471e96d3bad99583d46b620632d9b3375d406eb11" )
  ; ( "lib/chatmd/chatmd_parser_builder.ml"
    , "c5a0ee224f1928fa6ed0e0f96fdcd973e37dc840a841501f27d89e47b83b62e9" )
  ; ( "lib/chatmd/chatmd_located_ast.ml"
    , "e826015d477ac2279aaca2251f12fc31ad1e0026afd562dfc7fd96da7d276751" )
  ; ( "lib/chatmd/chatmd_tokens.ml"
    , "12d64ec1df81a7ebf9156e4db78a350b7bf45bd112b8e000e8b96d4354225e78" )
  ; ( "lib/chatmd/chatmd_parser_input.ml"
    , "ff5449579e75dad66338700683d56b373ee9baedbb6433214e21af5a4df1ab57" )
  ; ( "lib/chatmd/chatmd_owned_parser.ml"
    , "8d2369479651190b5176c3231d93aac6f0c497b0e50a57ccebc88c92362a5808" )
  ; ( "lib/agent_session/prompt_revision_builder.ml"
    , "f810e1cc7e94b6ad52e9a4592693f5ca2ed23798f922f44b63b6fef368f05c8e" )
  ]
;;

let topic_contracts =
  [ ( "tool_v1"
    , "chatmd.definitions"
    , "b21d52b70e7016cfb832b240197a71ea6cf97e2c6e31d2c6b38af4105a4676d3" )
  ; ( "moderator_v1"
    , "chatmd.definitions"
    , "86f7ce1c5fd15a5db8dcc060ade4e9a904285d2f9e6676ad6bf8b4b3ba07e026" )
  ; ( "delegated_moderator_v1"
    , "chatmd.definitions"
    , "a838ff83e79862df92e77736f59cf0ee73f6cf39023108d71a33f7d5c3054fc4" )
  ; ( "tool_v1"
    , "chatmd.declarations.schemas"
    , "78b3bb2c5fe2cc62ffe305eecfdf873eec39950493d5168dfa46eac6b77eae21" )
  ; ( "moderator_v1"
    , "chatmd.declarations.schemas"
    , "9a6905d9e72718e687a27dcbb588ceae3f83011a517f4b10f1d74a3e7ede7585" )
  ; ( "delegated_moderator_v1"
    , "chatmd.declarations.schemas"
    , "f7b59b2b93517a9dca100c2779fca31f3619a78384858a754c402da7b772dcb9" )
  ; ( "tool_v1"
    , "chatmd.capabilities"
    , "570a706c3144d8083da920fafaeba8fce0f84806e5731e79da030f462f9bf47a" )
  ; ( "moderator_v1"
    , "chatmd.capabilities"
    , "f9d1ee93efe5055ee9a92e219bc13e0fec2456c90eb85f91f863dc6a460f92e6" )
  ; ( "delegated_moderator_v1"
    , "chatmd.capabilities"
    , "7f32a249a40363994ac118c87063c438a6651fd26e3ca2b4dac010f4e9b6f44d" )
  ]
;;
