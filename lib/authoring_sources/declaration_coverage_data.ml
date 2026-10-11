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
    , [ "test/chatmd_extension_test.ml" ] )
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
    , "bee7d50da0a047693ddbfe2d109e2e1f52f9ce72bb987288b7e6e4e52ccb03e5" )
  ; ( "lib/chatmd/chatmd_import_expansion.ml"
    , "e545faf2c187fa83934c3340d0b6d4919fc6bd5650b4682b9574122272986434" )
  ; ( "lib/chatmd/chatmd_lexer.mll"
    , "a34003587cc6bf0b985eae0ef7b1fdb674d2313ac1fed8c41fdcd269162f3070" )
  ; ( "lib/chatmd/chatmd_parser_engine.mly"
    , "60bbfe6052bca41a3c95367a34785a737aba691d2bfabeb70a388976ea7c7e73" )
  ; ( "lib/chatmd/chatmd_script_declaration.ml"
    , "7a195f7836a0aa4a50ca7924a22056dabd17a5fe6bb87ad076b96a7e939dc00b" )
  ; "lib/source.ml", "6c18a151df64c7489a2fcb745e5f23e54c754696d0bd4b27c53505846317299b"
  ; ( "lib/chatmd/chatmd_source_bundle.ml"
    , "c3e0f8f89c8538ddcec98873739455a673b0e6e894282c21c96046c096bb6ae5" )
  ; ( "lib/chatmd/prompt.ml"
    , "6d4e6ab7ca62e6ddf6c965d32f83370b00fc0ffc4b8315306c71f58b30315842" )
  ; ( "lib/chatmd/source_loader.ml"
    , "c9d72bd6fa376b4f6513c26fe3a11713243d00dd0c230570afc4cf4d403e2461" )
  ; ( "lib/chatmd_shell_spec/authoring_metadata.ml"
    , "8fa283f545896b7efbaab8bcf17a3ab7549d61c04873403c94961fc4ca0b789d" )
  ; ( "lib/chatmd_shell_spec/duration.ml"
    , "ceed8e65e4ca9b5292e94e0d1e929288c1d4d62c2ed58ec46425e5076c525231" )
  ; ( "lib/chatmd_shell_spec/extension_spec.ml"
    , "437d0954eb5ee7a0acc44e26293a907a3bda609e9f399d4d4fdccf784d1fcc1c" )
  ; ( "lib/chatmd_shell_spec/source_ref.ml"
    , "a836f06b96426664cb2ee5aeb0038c1bc54829cfe7eabde962b6865f64164043" )
  ; ( "lib/chatmd_shell_spec/tool_schema.ml"
    , "bff94e09d295dd3fe8edb56e961b293cc760fb99a534bbec5e88ed56a345e00e" )
  ; ( "lib/meta_prompting/preprocessor.ml"
    , "5eedb68b3db23b16a8769522104d2f4e6f9f2e3cce2e11e40c92dc5c2ad879b8" )
  ; ( "lib/chat_response/converter.ml"
    , "36e976557af15716f55460fb0855babdd52ef90f6ca6d47f53a43a69f46d44be" )
  ; ( "lib/chat_response/fetch.ml"
    , "170db560c0693c7ec37f2a34c1907133feaffddd079571fcc32552bab59549ca" )
  ; ( "lib/chat_response/mcp_discovery_cache.ml"
    , "b7c02f701beaaf87c1cd04bf06065cb5564f2702f923704d8e8676f13d5724fb" )
  ; ( "lib/chat_response/shell_tool.ml"
    , "5d21f554475442cc67f0feaabaa3635f30fc5c678c926ca916eb99f804ef35ac" )
  ; ( "lib/chat_response/tool.ml"
    , "477798dfc2e13e78dd8f481448aca3bffc00ce74d7662bf5ffc1b76d001c626d" )
  ; ( "lib/chatmd/chatmd_read_file_declaration.ml"
    , "f604a8e767cae51c15b31eb0d8a266d9ea1efd429f77cbb7b068940d38a2768e" )
  ; ( "lib/chatmd/chatmd_read_file_spec.ml"
    , "9864c1077c135be35f6109c537a75447199131899e3cfbec70ac4c00012c8f63" )
  ; ( "lib/chatmd/chatmd_shell_declaration.ml"
    , "2b5d331094c7abf2b61f5aa537cc836dae313f80fa9bef7575ea7ad23c61027a" )
  ; ( "lib/chatmd_shell_spec/manifest_compiler.ml"
    , "ef278118e63c8549fb483c6bfb7e9c2a82aa674f1deb0442fb5f183dcfb8e484" )
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
    , "42130395da8eb0465c96d59a5233ae24adb300e5bd6094609a5cf60d007ca573" )
  ]
;;

let topic_contracts =
  [ ( "tool_v1"
    , "chatmd.definitions"
    , "58444d523a000fa5599467b629823b5041914b1c3e6d86b357616b36246ef948" )
  ; ( "moderator_v1"
    , "chatmd.definitions"
    , "ce3bab32ee02963939cd6a7d2fa4dda8fc7b56406550c21a3fa23bfb5c6d6465" )
  ; ( "delegated_moderator_v1"
    , "chatmd.definitions"
    , "4630a7cccd765e162431feb20472ec36665deae8f28a29bf213a2fab6e5a414b" )
  ; ( "tool_v1"
    , "chatmd.declarations.schemas"
    , "532ae64b35fbc2026dcc3fd9a8f92bac3364522e59d6ba9ebac32f0ba810484d" )
  ; ( "moderator_v1"
    , "chatmd.declarations.schemas"
    , "8d327bcd1275fdba014e4464a716aa4b6d9d2014dbb74fae06e5ee5f776d9241" )
  ; ( "delegated_moderator_v1"
    , "chatmd.declarations.schemas"
    , "a00242c0b64b0ad0fc4b4f0566d72e4e3c4265ea907aa2bccc0387396c22c514" )
  ; ( "tool_v1"
    , "chatmd.capabilities"
    , "db79d06691be96ee2b87182a1648784e929e647305496c240cb8557d033f776c" )
  ; ( "moderator_v1"
    , "chatmd.capabilities"
    , "7a46f2933297a8f4ae8a5cbff9297fcbe1c886ae3a8aec262fe1d624918496e5" )
  ; ( "delegated_moderator_v1"
    , "chatmd.capabilities"
    , "deea15ff07097606844fd4ed1927a83cedcfb1e646e9538ed21944bcbeabf998" )
  ]
;;
