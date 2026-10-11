(* Reviewed language rules and evidence. These rows are a maintained semantic
   taxonomy, not automatic discovery of every possible behavior. Source pins are
   coarse drift detectors; implementation changes require review, not auto-refresh. *)
let features =
  [ ( "lex.identifiers"
    , "ASCII identifiers, reserved keywords and backtick tags"
    , [ "lib/chatml/chatml_lexer.mll" ]
    , "chatml.evaluation"
    , [ "evaluation.identifier-boundaries"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "lex.numeric_literals"
    , "Decimal integer/float tokens and machine integers"
    , [ "lib/chatml/chatml_lexer.mll"; "lib/chatml/chatml_eval.ml" ]
    , "chatml.evaluation"
    , [ "language.source-and-precedence"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "lex.strings"
    , "Recognized/literal escapes, UTF-8 bytes and physical newlines"
    , [ "lib/chatml/chatml_lexer.mll" ]
    , "chatml.evaluation"
    , [ "evaluation.literal-escapes"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "lex.comments"
    , "Nested comments, quote delimiters and source positions"
    , [ "lib/chatml/chatml_lexer.mll" ]
    , "chatml.evaluation"
    , [ "language.source-and-precedence"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "syntax.precedence"
    , "Explicit operator precedence and parser conflict boundaries"
    , [ "lib/chatml/chatml_parser.mly" ]
    , "chatml.evaluation"
    , [ "language.source-and-precedence"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "syntax.delimiters"
    , "Arity delimiters, expression sequences and literal-only patterns"
    , [ "lib/chatml/chatml_parser.mly" ]
    , "chatml.evaluation"
    , [ "evaluation.sequence-in-array"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "syntax.task_sugar"
    , "Lexical let-star/let-plus desugaring to bind/map"
    , [ "lib/chatml/chatml_lexer.mll"; "lib/chatml/chatml_parser.mly" ]
    , "chatml.task-effects"
    , [ "task-effects.nested-map"
      ; "task-effects.reuse"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.annotations"
    , "Located binding annotations, scoped parameters, supported constructors and closed \
       rows"
    , [ "lib/chatml/chatml_parser.mly"
      ; "lib/chatml/chatml_type_expr.ml"
      ; "lib/chatml/chatml_type_alias.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ]
    , "chatml.inference"
    , [ "inference.closed-record-annotation-rejected"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.arrow_arity"
    , "Exact nary functions, unit parameters, function results and legacy arrow \
       flattening"
    , [ "lib/chatml/chatml_type_alias.ml"; "lib/chatml/chatml_parser.mly" ]
    , "chatml.inference"
    , [ "inference.explicit-arrow-arity"
      ; "inference.generic-tree-and-nary-functions"
      ; "test/chatml_aliases/substitution_tests.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.aliases"
    , "Scoped parameterized transparent aliases and regular contractive recursive types"
    , [ "lib/chatml/chatml_type_alias.ml"
      ; "lib/chatml/chatml_type_validity.ml"
      ; "lib/chatml/chatml_inferred_type.ml"
      ]
    , "chatml.inference"
    , [ "inference.structural-aliases"
      ; "inference.generic-tree-and-nary-functions"
      ; "test/chatml_aliases/substitution_tests.ml"
      ; "test/chatml_aliases/constraint_tests.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.generalization"
    , "Value restriction, weak variables and expansive bindings"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.expansive-function-rejected"
      ; "test/chatml_type_identity/type_identity_test.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.recursive_bindings"
    , "Function-only recursion and monomorphic recursive placeholders"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.recursive-value-rejected"; "test/agent_docs/docs_chatml_authoring.ml" ]
    )
  ; ( "types.record_fields"
    , "Field requirements and inferred versus annotated row openness"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.open-helper-update"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "types.module_kinds"
    , "Distinct module/record representations with shared field projection"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.module-record-projection"
      ; "inference.module-update-rejected"
      ; "test/chatml_module_contract/module_contract_tests.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.record_update"
    , "Immutable row extension and type-changing overrides"
    , [ "lib/chatml/chatml_typechecker.ml"; "lib/chatml/chatml_eval.ml" ]
    , "chatml.inference"
    , [ "records.type-changing-update"
      ; "inference.annotated-copy-input"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.record_join"
    , "Matching branch shapes and compatible shared-tail overrides"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "records.join-rejected"
      ; "inference.branch-shape-rejected"
      ; "inference.conditional-update"
      ; "inference.exact-helper-rejected"
      ; "test/chatml_record_width/branch_tests.ml"
      ; "test/chatml_record_width/record_width_tests.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.variant_rows"
    , "Structural tags, payload arity and row closing"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "variants.closed-total"
      ; "inference.variant-unit-arity"
      ; "inference.variant-arity-rejected"
      ; "test/chatml_variant_arity_test.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.pattern_binders"
    , "Unique labels, pattern/lambda binders and simultaneous recursive names"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.duplicate-parameter-rejected"
      ; "inference.duplicate-recursive-name-rejected"
      ; "inference.sequential-rebinding"
      ; "test/och159_verification/och159_verification_test.ml"
      ; "test/chatml_typechecker_test.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.match_coverage"
    , "Conservative coverage, redundant arms and closed variants"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "variants.closed-wildcard-rejected"; "test/agent_docs/docs_chatml_authoring.ml" ]
    )
  ; ( "types.equality"
    , "Equality obligations survive abstraction and check visible record fields"
    , [ "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_equality_requirement.ml"
      ; "lib/chatml/chatml_lang.ml"
      ]
    , "chatml.inference"
    , [ "inference.generic-equality-boundary"
      ; "inference.equality-visible-fields"
      ; "test/chatml_equality/equality_tests.ml"
      ; "test/chatml_equality/recursive_tests.ml"
      ; "test/chatml_equality/examples.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.mutable_storage"
    , "Homogeneous mutable storage, integer indexes and aliases"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "mutation.value-restriction"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "types.branch_checking"
    , "Boolean conditions and static checking of all branches"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.unreachable-branch-still-checked"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "scope.modules"
    , "Non-recursive module scope, explicit exports and open collision checks"
    , [ "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ]
    , "chatml.modules"
    , [ "modules.outer-not-exported"
      ; "test/chatml_module_scope/module_scope_tests.ml"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "scope.module_types"
    , "Qualified transparent aliases retain defining scopes and explicit exports"
    , [ "lib/chatml/chatml_type_alias.ml"; "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.module-types-and-separators"
      ; "test/chatml_module_scope/type_scope_tests.ml"
      ] )
  ; ( "types.module_interfaces"
    , "Transparent interfaces check fixed-arity universal schemes and shared weak types"
    , [ "lib/chatml/chatml_module_signature.ml"
      ; "lib/chatml/chatml_signature_check.ml"
      ; "lib/chatml/chatml_type_probe.ml"
      ; "lib/chatml/chatml_alias_equivalence.ml"
      ; "lib/chatml/chatml_type_alias.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ]
    , "chatml.inference"
    , [ "inference.checked-module-interface"
      ; "test/chatml_module_scope/signature_check_tests.ml"
      ; "test/chatml_module_scope/signature_arity_tests.ml"
      ; "inference.universal-arity-rejected"
      ; "test/chatml_module_scope/signature_cancellation_tests.ml"
      ; "test/chatml_module_scope/type_probe_tests.ml"
      ] )
  ; ( "scope.module_interface_visibility"
    , "Checked public selections agree with runtime exports and private closure scope"
    , [ "lib/chatml/chatml_module_visibility.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ]
    , "chatml.inference"
    , [ "inference.checked-module-interface"
      ; "test/chatml_module_scope/inline_signature_tests.ml"
      ] )
  ; ( "syntax.statement_separators"
    , "Optional double-semicolon statement boundaries preserve expression sequencing"
    , [ "lib/chatml/chatml_lexer.mll"; "lib/chatml/chatml_parser.mly" ]
    , "chatml.inference"
    , [ "inference.module-types-and-separators"
      ; "test/chatml_module_scope/type_scope_tests.ml"
      ] )
  ; ( "eval.order"
    , "Function/argument, field, payload and element evaluation order"
    , [ "lib/chatml/chatml_eval.ml" ]
    , "chatml.evaluation"
    , [ "evaluation.argument-and-field-order"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "eval.capture"
    , "Lexical closures, shadowing and recursive environments"
    , [ "lib/chatml/chatml_resolver.ml"; "lib/chatml/chatml_eval.ml" ]
    , "chatml.evaluation"
    , [ "language.closures-loops-recursion"; "test/agent_docs/docs_chatml_authoring.ml" ]
    )
  ; ( "eval.control_flow"
    , "Selected branches, first matching arm, looping and discarded values"
    , [ "lib/chatml/chatml_eval.ml"; "lib/chatml/chatml_lang.ml" ]
    , "chatml.evaluation"
    , [ "language.closures-loops-recursion"; "test/agent_docs/docs_chatml_authoring.ml" ]
    )
  ; ( "eval.mutation"
    , "Array/ref mutation, copy-update and shallow aliases"
    , [ "lib/chatml/chatml_eval.ml"; "lib/chatml/chatml_lang.ml" ]
    , "chatml.evaluation"
    , [ "test/chatml_typechecker_test.ml"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "eval.failures"
    , "Arithmetic/bounds failures and effects before errors"
    , [ "lib/chatml/chatml_eval.ml" ]
    , "chatml.evaluation"
    , [ "evaluation.float-zero-failure"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "diagnostics.source_ownership"
    , "Exact captured revisions, validated byte positions and lexical failure ownership"
    , [ "lib/chatml/chatml_parse.ml"
      ; "lib/chatml/chatml_owned_source.ml"
      ; "lib/chatml/chatml_checkpoint.ml"
      ; "lib/chatml/chatml_builtin_spec.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ; "lib/chatml_sources/chatml_source.ml"
      ; "lib/chatml_sources/chatml_diagnostic.ml"
      ]
    , "chatml.evaluation"
    , [ "test/chatml_sources/source_invariants.ml"
      ; "test/chatml_diagnostics/diagnostic_invariants.ml"
      ; "test/chatml_owned_diagnostics/ownership_tests.ml"
      ; "test/chatml_owned_diagnostics/checkpoint_tests.ml"
      ] )
  ; ( "eval.task_values"
    , "Eager task construction versus later interpretation"
    , [ "lib/chatml/chatml_eval.ml"; "lib/chatml/chatml_lang.ml" ]
    , "chatml.task-effects"
    , [ "task-effects.eager-error"
      ; "task-effects.reuse"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "codec.json"
    , "Recursive JSON representation, float numbers and object entries"
    , [ "lib/chatml/chatml_value_codec.ml" ]
    , "chatml.json"
    , [ "language.structured-data-pipeline"; "test/agent_docs/docs_chatml_authoring.ml" ]
    )
  ; ( "codec.persistence"
    , "Data-only value encoding and rejection of executable values"
    , [ "lib/chatml/chatml_value_codec.ml"; "lib/chatml/chatml_lang.ml" ]
    , "chatml.evaluation"
    , [ "test/chatml_value_codec_test.ml"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ]
;;

let implementation_sources =
  [ ( "lib/chatml/chatml_alias_equivalence.ml"
    , "53e1bb3e19369b3ae6e3a55225c9e4a41c1e81001a21bb41b1c787a033f2f32e" )
  ; ( "lib/chatml/chatml_module_signature.ml"
    , "b8769503c6f4a4c816eee29e3a6ad4ec154de66ac33883c25bb3437584d83119" )
  ; ( "lib/chatml/chatml_module_visibility.ml"
    , "8483156ba7674a7cd6e7d3e37898ef86f48fb279273d6ebcec94a02c7966bfa0" )
  ; ( "lib/chatml/chatml_signature_check.ml"
    , "e91d796eb4ced58ad834fe909d4726e9b1bd3c8279700d7b40fc3da4e582c8a9" )
  ; ( "lib/chatml/chatml_type_probe.ml"
    , "9198cb575e3d2fd81d44367360ddfaf8e7ebcf9c4b90379c7862e64cc25ac2ea" )
  ; ( "lib/chatml/chatml_type_expr.ml"
    , "d96824e77f90d07a4e243929c527229407327645e28a698eaa3b77739ff2584c" )
  ; ( "lib/chatml/chatml_inferred_type.ml"
    , "672f8d4c721b175c68d6b14e9982444b80891e9bec529cbd905f367ca6cda961" )
  ; ( "lib/chatml/chatml_type_alias.ml"
    , "1931b83e7b7c504f12e2b22374cfebc3bd894808de353c1133c0a50aad6aa214" )
  ; ( "lib/chatml/chatml_type_validity.ml"
    , "e1fd576d7740c4f6d1abe1bc8ee4942b4a837459d13795088a07455cfae4d583" )
  ; ( "lib/chatml/chatml_equality_requirement.ml"
    , "d5eac886d08cdc3e3619b7d1f1ccab0e391ad282e08bc2e9835bc84acc519b48" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "c43a885f71ff7aa39288926c1ba3700fc705836b0dfcbf28b431cd8b22181a65" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "9832e2b22a199d823cfa0e7d3ac9c452b13d720f8df5f55b7c95aa4f26e1d72f" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "9d2e76055020ab4ba6b3217187dbd8c0f3c9e6c395a287a64007db63fa0e4071" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "28d850932718a25893c0a74e8072a03324cbd0ab254cb18829748499848b55f7" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "f877eff25c34ab0554cf2bbdb79d1cc911929288dd43a51587fca9c149d39020" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "400e93e79b8c8e695bfd71a0179803b9c2dae20c6d063d225eec26630a742306" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ; ( "lib/chatml/chatml_parse.ml"
    , "0a83dc3d473a0ea94cd215895d3cc94b96b799f1bb3472a89bb85f937b6f304f" )
  ; ( "lib/chatml/chatml_owned_source.ml"
    , "84b3a946d19d92a4530ec9a13f2ec0fe72adaad9790ec167f085372ddf26feeb" )
  ; ( "lib/chatml/chatml_checkpoint.ml"
    , "a574f6bbc3b71389e2789bf74863354f6eab3ffbe8c9c254f9b88bc1acefb879" )
  ; ( "lib/chatml/chatml_builtin_spec.ml"
    , "8ff17f2e6960d5b9cad2ee3e5a06dc5505f146f41b0d1210984358e167a68cc1" )
  ; ( "lib/chatml_sources/chatml_source.ml"
    , "af9021a33f112a570bc4cb38658a6f04630158035ebf6343104487e3c16485a8" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "d1eec64c1b3b4603670d2e275af253bfedaf3cfb1a5c21d37631e4c2c4545764" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "6b807263ea172de27afc5c82f90ac6ca8783fabb2265dd39d09c578416d01f8d" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "ba71ec0b241bd19e748769943172bb1c29736390fdf7339f9429938ee408d408" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "3a2d78833236ef2fabca9afe1049fe8a77bd3ad51d7b8c50c9695a3bb05bc7e0" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "a4ba08f87739189477ad046379e994f05ac7baec7bfbbc13049f8504e7989009" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "febdecf0284664e7475dc3013c7a91e6e1b47549d6a6cbaa91db9fab15a5b5d0" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "9070f442761ad6e7ccae5b6e38c44186431eda212f899de3ef614f66ff1b2f79" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "b2c1edf3b3d94bea54eda9fe87232f202ba004bfffeb78a1ba51a37f104eef3a" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "ea9d4de0641e1033cf298a5998baabda45f9dc7433157e437ec1bb0be09cfe1c" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "57dd3cb533962d869d748883acf2abcf7ad538e618b7bacff1fe4891edddf059" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "ceaa7aa751e1a509796a715510e41a2d89d5e4e3b78d1d3793ca1233dcb41f0e" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "9221c18bd6da87a3b1de83193da31fb96f311691ea8b73765c74b6b1a7caf956" )
  ; ( "one_off_v1"
    , "chatml.json"
    , "01da25a0d9acfa54ce73ef15feaae1a542a7dd8264eab96c90aee0fcbbd98a72" )
  ; ( "tool_v1"
    , "chatml.json"
    , "88d23cdf5a4e867840c8dc32be9a926880c7f47c04201e5244b2111cb40d2b24" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "b5e8915509043d1680b344b503743b2449b1f8176b5d08e3209f0488e524a3d0" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "89761b9a873fac5ef85e92684379fcbfc82178d87f906b0d3a96f1f957eb27ba" )
  ; ( "one_off_v1"
    , "chatml.modules"
    , "0816f136d62dd5d9658575c215a0a79b46d100258b6a54e1dc24d507c2c105a6" )
  ; ( "tool_v1"
    , "chatml.modules"
    , "0de68d9c0161078cd70773c48c0ab52ba4909fd92b08ceb88d92b581a83ff4c8" )
  ; ( "moderator_v1"
    , "chatml.modules"
    , "03c3a0324e487d2d5527be1233b943e59f1b67433cc524f087fbaee0fd8344a7" )
  ; ( "delegated_moderator_v1"
    , "chatml.modules"
    , "c6801cbc049fc6d5a9f9a21998568633e9e6f785a81d86b9eb5f9d0351f4df55" )
  ]
;;
