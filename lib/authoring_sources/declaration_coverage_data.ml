(* Maintained ChatMD declaration rules. Review source changes and topic closures;
   CI never regenerates these pins. This is not automatic discovery of all
   legacy shell/MCP or native-runtime semantics. *)
let features =
  [ ( "markup.syntax"
    , "Closed tags, attributes, comments and RAW boundaries"
    , [ "lib/chatmd/chatmd_ast.ml"
      ; "lib/chatmd/chatmd_lexer.mll"
      ; "lib/chatmd/chatmd_parser.mly"
      ]
    , "chatmd.definitions"
    , [ "chatmd.literal-resource-text"; "test/chatmd_parser_test.ml" ] )
  ; ( "sources.capture"
    , "Captured imports, relative sources, namespaces and bounded expansion"
    , [ "lib/chatmd/prompt.ml"
      ; "lib/chatmd/source_loader.ml"
      ; "lib/chatmd/chatmd_import_expansion.ml"
      ; "lib/chatmd/chatmd_source_bundle.ml"
      ; "lib/chatmd_shell_spec/source_ref.ml"
      ]
    , "chatmd.definitions"
    , [ "chatmd.missing-import-rejected"
      ; "test/chatmd_source_bundle_test.ml"
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
    , "5b79d9d5d2daa6a98d321d1d3b897c573c72c94ec52e8094a0d43e78ac91a0ed" )
  ; ( "lib/chatmd/chatmd_ast.ml"
    , "1e23a862fcee8b5df7a410d62477b83cfb25bdd2b9a5ce67c9ce05ff50e2ccc9" )
  ; ( "lib/chatmd/chatmd_attributes.ml"
    , "778ffad3961edb8e13c2a82d94fd8f83517833cb8aae4135728627e7b6a428b6" )
  ; ( "lib/chatmd/chatmd_extension_declaration.ml"
    , "bee7d50da0a047693ddbfe2d109e2e1f52f9ce72bb987288b7e6e4e52ccb03e5" )
  ; ( "lib/chatmd/chatmd_import_expansion.ml"
    , "66af153cad4ee2b40cace4621533c369d609fd1ffb7a6ac11e24a762abacdb7a" )
  ; ( "lib/chatmd/chatmd_lexer.mll"
    , "60d52120d7905028e4966080d9e7c59f81bda86ee8ae8957b67c44b0f4ee68bd" )
  ; ( "lib/chatmd/chatmd_parser.mly"
    , "2d29ec016ea80169b4b8b4c088dd2232a11fa3184c4b8cc9acb7f1d6223c3c55" )
  ; ( "lib/chatmd/chatmd_script_declaration.ml"
    , "7a195f7836a0aa4a50ca7924a22056dabd17a5fe6bb87ad076b96a7e939dc00b" )
  ; ( "lib/chatmd/chatmd_source_bundle.ml"
    , "65269d209dfa68e9a4e93abf63893558a9b2eedd61c18ff6e505ea03c21387ee" )
  ; ( "lib/chatmd/prompt.ml"
    , "188a52b9b69246f526132332ce6198651ec8f4ddd5646ab65971cc825ce3b749" )
  ; ( "lib/chatmd/source_loader.ml"
    , "f0da29bc7c28d44b31ae0ca1fc4b9a5b99bbe429bbf8978cb0f50a51e36b20e7" )
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
  ]
;;

let topic_contracts =
  [ ( "tool_v1"
    , "chatmd.definitions"
    , "cc750508b16a20397f89b708f0167553e0f6a7a9afebe723dfb0eda89ae7b31c" )
  ; ( "moderator_v1"
    , "chatmd.definitions"
    , "8a5022cffd870f196eb1860e9fc1c055ab67ed44fc5f3daf8d7852f4469b05ba" )
  ; ( "delegated_moderator_v1"
    , "chatmd.definitions"
    , "66c6c1c7665e6d5576bdbb29283044ddd6c30780692671e6f19a76d81c1281cc" )
  ; ( "tool_v1"
    , "chatmd.declarations.schemas"
    , "88b13f6d9c33de9e3200530e19f652f4708fce1a7063cc1a01706efcecc49943" )
  ; ( "moderator_v1"
    , "chatmd.declarations.schemas"
    , "3146ea89d88712637feb0384bb861b4abb297d24ce423bda7547dde9d0a7bcd2" )
  ; ( "delegated_moderator_v1"
    , "chatmd.declarations.schemas"
    , "89b2d3aa52bb9fa7786b6e40891dfaea43de78fc6be121b3771c92ad64c154fb" )
  ]
;;
