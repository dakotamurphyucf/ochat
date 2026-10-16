(* Maintained ChatMD declaration rules. Review source changes and topic closures;
   CI never regenerates these pins. This is not automatic discovery of all
   legacy shell/MCP or native-runtime semantics. *)
let features =
  [ ( "markup.syntax"
    , "Closed tags, attributes, comments and RAW boundaries"
    , [ "lib/chatmd/chatmd_ast.ml"
      ; "lib/chatmd/chatmd_attribute_lex.ml"
      ; "lib/chatmd/chatmd_lexer.mll"
      ; "lib/chatmd/chatmd_tag_scope.ml"
      ; "lib/chatmd_shell_spec/shell_element.ml"
      ; "lib/chatmd/chatmd_parser_engine.mly"
      ; "lib/chatmd/chatmd_parser.ml"
      ; "lib/chatmd/chatmd_parser_builder.ml"
      ; "lib/chatmd/chatmd_located_ast.ml"
      ; "lib/chatmd/chatmd_tokens.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.literal-resource-text"
      ; "test/chatmd_parser_test.ml"
      ; "test/chatmd_authoring_schema/schema_tests.ml"
      ] )
  ; ( "sources.capture"
    , "Captured imports, relative sources, namespaces and bounded expansion"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_extraction_map.ml"
      ; "lib/chatmd/source_loader.ml"
      ; "lib/agent_session/prompt_revision_builder.ml"
      ; "lib/agent_session/prompt_script_sources.ml"
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
      ; "test/chatmd_owned_input/extraction_tests.ml"
      ; "test/chatmd_owned_input/capture_tests.ml"
      ; "test/chatmd_documents/document_tests.ml"
      ; "test/chatmd_prompt_content/content_tests.ml"
      ; "test/builtin_instructions/builtin_instructions_tests.ml"
      ; "test/agent_docs/docs_child_authoring.ml"
      ; "test/chatml_source_owner/owner_tests.ml"
      ; "test/chatml_source_owner/project_tests.ml"
      ; "test/chatml_source_owner/script_tests.ml"
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
      ; "lib/chatmd/chatmd_declaration_schema.ml"
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
      ; "lib/chatmd/chatmd_declaration_schema.ml"
      ; "lib/chat_response/generated_admission.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.lifecycle-moderator"; "test/generated_admission_test.ml" ] )
  ; ( "scripts.sources_and_limits"
    , "Inline/source exclusivity, explicit kind/API, limits and script identities"
    , [ "lib/chatmd/chatmd_script_declaration.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
      ; "lib/chatmd_shell_spec/chatmd_script_spec.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ; "lib/chatmd_shell_spec/duration.ml"
      ]
    , "chatmd.definitions"
    , [ "test/chatmd_extension_test.ml"; "test/chatml_script_routing/startup_tests.ml" ] )
  ; ( "tools.extension_bindings"
    , "Standalone and moderator tool bindings and declaration registry checks"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
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
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
      ]
    , "chatmd.declarations.schemas"
    , [ "test/chatmd_extension_test.ml"; "test/managed_tool_registry_test.ml" ] )
  ; ( "authoring.policy"
    , "Auto/manual/preload declarations and inherited authority boundaries"
    , [ "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
      ; "lib/chatmd_shell_spec/extension_spec.ml"
      ; "lib/chat_response/authoring_policy.ml"
      ; "lib/chatmd/chatmd_attributes.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.plain-agent"; "test/authoring_policy_test.ml" ] )
  ; ( "authoring.help"
    , "Explicit package/task/topic/helper metadata without new authority"
    , [ "lib/chatmd/chatmd_extension_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
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
      ; "lib/chatmd/chatmd_shell_schema.ml"
      ; "lib/chatmd/chatmd_moderator_runtime_declaration.ml"
      ; "lib/chatmd/chatmd_declaration_schema.ml"
      ; "lib/chatmd_shell_spec/shell_tool_spec.ml"
      ; "lib/chatmd_shell_spec/manifest_compiler.ml"
      ; "lib/chat_response/shell_tool.ml"
      ]
    , "chatmd.capabilities"
    , [ "test/agent_docs/docs_chatmd_capabilities.ml"
      ; "test/chatmd_shell_parser_test.ml"
      ; "test/chatmd_authoring_schema/schema_tests.ml"
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
  ; ( "lib/chatmd/chatmd_tag_scope.ml"
    , "9b1b69944fdd20fd5cc71a840eb6b37f78a2d057f18b0c3a33958acbab088c55" )
  ; ( "lib/chatmd/chatmd_declaration_schema.ml"
    , "ed46b7e39e2dbb92d6df7e238a0984b7807e34adaad343bfdbda70a901315be2" )
  ; ( "lib/chatmd/chatmd_shell_schema.ml"
    , "f48e38afc50260ddc3061215fb69addeb982eb42360224d8fc232e48fa6b5199" )
  ; ( "lib/chatmd/chatmd_moderator_runtime_declaration.ml"
    , "7ffa64b5169faa139c636757d80ddea7f09d525f4f94668cab33819c7f6997a6" )
  ; ( "lib/chatmd/chatmd_ast.ml"
    , "0a6ce1ca5b5a0eaeffb1e4a847b643dffc3b86690fc74efe65c6e0586933ae55" )
  ; ( "lib/chatmd/chatmd_attributes.ml"
    , "778ffad3961edb8e13c2a82d94fd8f83517833cb8aae4135728627e7b6a428b6" )
  ; ( "lib/chatmd/chatmd_extension_declaration.ml"
    , "295fe15c00bc2abfd922e86ba899b094d772e65de712d213a9dcd1222f9626f0" )
  ; ( "lib/chatmd/chatmd_extraction_map.ml"
    , "09108c15e177f79dbf5229a24c35d52e4a7ef927d5ed586b57ade1a886497497" )
  ; ( "lib/chatmd/chatmd_import_expansion.ml"
    , "8df0daffef119d3ba6ec7c9bdcec5a8076e75867ac64e56754713f5e300a1184" )
  ; ( "lib/chatmd/chatmd_attribute_lex.ml"
    , "95457bf7123c4b17d73c50b01d3dec72f3f6818697a6c25327444411d2b2b521" )
  ; ( "lib/chatmd/chatmd_lexer.mll"
    , "132074f0406a61bbeafdcf35d88dc484949f56553ed41718d3c7188ddcb23735" )
  ; ( "lib/chatmd/chatmd_parser_engine.mly"
    , "94e9fb5543628175f53c2810536459a191fa21ec82312288a8ca0404b9487b78" )
  ; ( "lib/chatmd/chatmd_script_declaration.ml"
    , "c956aec536fb11a0897375828e561c6a32afd63d0efef11c87132b0985f46dd6" )
  ; "lib/source.ml", "6c18a151df64c7489a2fcb745e5f23e54c754696d0bd4b27c53505846317299b"
  ; ( "lib/chatmd/chatmd_source_bundle.ml"
    , "5bd5d602cee857ede7d25645a6e7fc970bd82943b5eac30526faab98d069f4e1" )
  ; ( "lib/chatmd/prompt.ml"
    , "9d35e23e50ad499469c2a6c067d9c20ae8683c23d0c26c8b6dcc1883e7feef6d" )
  ; ( "lib/chatmd/source_loader.ml"
    , "82bf509199938ddef8dec39ca7450968b11f4f63e94d1f90b142fa0ed225a77b" )
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
    , "c6cdff248b4433655daa72064ce42e90d9522a7e2a632b78f555f86961bc0b0e" )
  ; ( "lib/chat_response/fetch.ml"
    , "170db560c0693c7ec37f2a34c1907133feaffddd079571fcc32552bab59549ca" )
  ; ( "lib/chat_response/mcp_discovery_cache.ml"
    , "9365e8d63b16864e3535ec8c961282d259797cb84f8046691691f2c98eaecc8a" )
  ; ( "lib/chat_response/shell_tool.ml"
    , "2450c632c18067d0c984087461b9085f24314dd79a026a42a4f35b1014816f92" )
  ; ( "lib/chat_response/tool.ml"
    , "794ed9c2a672118538f328a07a2ca627282291c405a939f1ec864295aa489761" )
  ; ( "lib/chatmd/chatmd_read_file_declaration.ml"
    , "446e56ceec98db2b0eeabed3b6f86e37127d71c03fb0594380b3daba9ba78eba" )
  ; ( "lib/chatmd/chatmd_read_file_spec.ml"
    , "9864c1077c135be35f6109c537a75447199131899e3cfbec70ac4c00012c8f63" )
  ; ( "lib/chatmd/chatmd_shell_declaration.ml"
    , "f47b4ff0c3f2ec7cb494e576df9e6c3313683995a42cd05e2a54f4c3f6ff93cc" )
  ; ( "lib/chatmd_shell_spec/manifest_compiler.ml"
    , "f10c82f13dc06b2b821adf2306ab6117ed19aa613e1ee8ec96ea9bfc7747bf1e" )
  ; ( "lib/chatmd_shell_spec/path_expr.ml"
    , "c64b5c5033c352628fb8fc89b13fb382ee965b999c29293eb0e8f9df220ce97a" )
  ; ( "lib/chatmd_shell_spec/chatmd_script_spec.ml"
    , "ca8508039cb59b0ea897f07b400bf0dd1d81ba19cecb1c28c0dd725968839a5b" )
  ; ( "lib/chatmd_shell_spec/shell_element.ml"
    , "76b1afa4036446a644dd4d4e1c04f6ce35e43240bc42d33b43678cd0f7e9f71f" )
  ; ( "lib/chatmd_shell_spec/shell_tool_spec.ml"
    , "6b4ef1c95060cb600baa51d813502519a6fd4bac47c5716ad84de326555c324b" )
  ; "lib/functions.ml", "05c5428d7870e3fe9711a3f3239ae1e0919c86fa5022c4ea5c7944559d7d2eb2"
  ; ( "lib/mcp/mcp_tool.ml"
    , "f9e69c49231b625fb479a77182b606113ae9369dce8df1c67c99c3eaddfdac49" )
  ; ( "lib/chatmd/chatmd_parser.ml"
    , "e9fa656cee4e6ee30bca2f3471e96d3bad99583d46b620632d9b3375d406eb11" )
  ; ( "lib/chatmd/chatmd_parser_builder.ml"
    , "f2988ba10623f4895afbd08b8fe1b88fc9386dfd6cf59c5d225a7173ab12d96d" )
  ; ( "lib/chatmd/chatmd_located_ast.ml"
    , "341430fa852619c06226b6ea037a65fe3f2439e9d41a6146fe98a92cd15ef17a" )
  ; ( "lib/chatmd/chatmd_tokens.ml"
    , "12d64ec1df81a7ebf9156e4db78a350b7bf45bd112b8e000e8b96d4354225e78" )
  ; ( "lib/chatmd/chatmd_parser_input.ml"
    , "ff5449579e75dad66338700683d56b373ee9baedbb6433214e21af5a4df1ab57" )
  ; ( "lib/chatmd/chatmd_owned_parser.ml"
    , "54814044e438ec34474351f6e0c7f8e4c926626013089c6c8183b2ac0717cf2b" )
  ; ( "lib/agent_session/prompt_revision_builder.ml"
    , "51fff4a63fcef9aa11a0d5815e153650f6ab0f92d49249aa1eddb6a4952371d7" )
  ; ( "lib/agent_session/prompt_script_sources.ml"
    , "996751e15ce03b37c6d144c2e5fd576963b3e72d37803c31cefb9400fa1a34c6" )
  ]
;;

let topic_contracts =
  [ ( "tool_v1"
    , "chatmd.definitions"
    , "5b9b5b2ba497b01827d3ec31fd6882718e89bf24e96ae19087dbbbf3ba6f90b6" )
  ; ( "moderator_v1"
    , "chatmd.definitions"
    , "da0418fdd654ff78945d688675d3618ff85995f3616a9630d7c67f2b36970bcc" )
  ; ( "delegated_moderator_v1"
    , "chatmd.definitions"
    , "eb09c707837d7fe6aedb8bf0a2056614226706d4e6b63a6d721aa255aaab494d" )
  ; ( "tool_v1"
    , "chatmd.declarations.schemas"
    , "46d7a96febbda590be00299504621c91c720cc2d0b865a82c840e83c0eb24386" )
  ; ( "moderator_v1"
    , "chatmd.declarations.schemas"
    , "df17b1db7086f63d835c113c6dd65f4050366d6687c6e2cfabf303dbcc74121e" )
  ; ( "delegated_moderator_v1"
    , "chatmd.declarations.schemas"
    , "f5657829c37cc2ab561768a6c0b4a9cbd441c7672e0401ed458b61969ddcfb35" )
  ; ( "tool_v1"
    , "chatmd.capabilities"
    , "78c0b817ac77f944b95d709eb2ae3ebf65255258ac1df8c816bdb372edbc689c" )
  ; ( "moderator_v1"
    , "chatmd.capabilities"
    , "4f01ba1662978b0de549c6a3a680ba831f89d757155610763ec323db14b57d18" )
  ; ( "delegated_moderator_v1"
    , "chatmd.capabilities"
    , "51955def80615c5f261d495ab80a9cbefb198ae2b89a39c3a348a4ca15f7f47f" )
  ]
;;
