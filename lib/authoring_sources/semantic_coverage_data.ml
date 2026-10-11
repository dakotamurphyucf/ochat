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
  ; ( "types.products"
    , "Ordered heterogeneous products retain arity, nesting, equality and weak sharing"
    , [ "lib/chatml/chatml_tuple.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_type_alias.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ; "lib/chatml/chatml_value_codec.ml"
      ]
    , "chatml.inference"
    , [ "language.tuple-values-and-match"
      ; "inference.tuple-products"
      ; "test/chatml_tuples/product_invariants.ml"
      ; "test/chatml_tuples/semantic_tests.ml"
      ; "test/chatml_tuples/persistence_tests.ml"
      ; "test/chatml_tuples/boundary_tests.ml"
      ] )
  ; ( "types.tuple_bindings"
    , "Irrefutable tuple bindings preserve whole-RHS schemes and distinct argument/leaf \
       slots"
    , [ "lib/chatml/chatml_binding_pattern.ml"
      ; "lib/chatml/chatml_binding_value.ml"
      ; "lib/chatml/chatml_parser.mly"
      ; "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ]
    , "chatml.inference"
    , [ "language.tuple-binders"
      ; "inference.tuple-binders"
      ; "inference.tuple-weak-rejected"
      ; "test/chatml_bindings/binder_invariants.ml"
      ; "test/chatml_bindings/semantic_tests.ml"
      ; "test/chatml_bindings/cancellation_tests.ml"
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
  [ ( "lib/chatml/chatml_binding_pattern.ml"
    , "d52f16e4403d887830eae7d6cd74df840318c8088007301c805fb3224311dc6f" )
  ; ( "lib/chatml/chatml_binding_value.ml"
    , "4d8e2dc9fc05c7bd578e1cd12e989ad32acca54252df0dfaa1a26515015eea71" )
  ; ( "lib/chatml/chatml_tuple.ml"
    , "287538bb4046c2780d6ea23a9fe96abea941f790eb2450410210b6f6318eb12d" )
  ; ( "lib/chatml/chatml_alias_equivalence.ml"
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
    , "ea499a1f29c146d3c6dade9f6afaedaf018310b5ba03dec958f36439b7b8ac04" )
  ; ( "lib/chatml/chatml_inferred_type.ml"
    , "c7321a73faf94a345643d3b366710cc834e7540f1c1e6b5c0066b7b5810ac570" )
  ; ( "lib/chatml/chatml_type_alias.ml"
    , "32112952e58b6ccc26a8e7e26e97fc8b48181f3284ccc0e7510def2810aefc2a" )
  ; ( "lib/chatml/chatml_type_validity.ml"
    , "e1fd576d7740c4f6d1abe1bc8ee4942b4a837459d13795088a07455cfae4d583" )
  ; ( "lib/chatml/chatml_equality_requirement.ml"
    , "d5eac886d08cdc3e3619b7d1f1ccab0e391ad282e08bc2e9835bc84acc519b48" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "07fbdb1caa2575efcf735f083ebb387a66829cbe22bd063be418046c8902ab61" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "ab1d916075b51bfd2fa2d3bc85649f43f467274135f754ddcff46506c9ae59a5" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "7e1ad82856e9e6e1a1a26f6202e535a0f55dd4671c5083429d205abb448e7c5e" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "fdd0b670b664c57d0b054628d8ffa32269b9b4203f181fac4472e953f3fb47ab" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "97094673fdab3ef7045c59195e8d7e84b36e90fc2a4e6c4aecf2a66e44dc9cc7" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "be923d2dd7d1555854e573e9cdae113d589173108bc19ada5b6c1fd705ec8210" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/chatml/chatml_parse.ml"
    , "0a83dc3d473a0ea94cd215895d3cc94b96b799f1bb3472a89bb85f937b6f304f" )
  ; ( "lib/chatml/chatml_owned_source.ml"
    , "84b3a946d19d92a4530ec9a13f2ec0fe72adaad9790ec167f085372ddf26feeb" )
  ; ( "lib/chatml/chatml_checkpoint.ml"
    , "a574f6bbc3b71389e2789bf74863354f6eab3ffbe8c9c254f9b88bc1acefb879" )
  ; ( "lib/chatml/chatml_builtin_spec.ml"
    , "3881c43e9818b44130bbb77dbdcf46b22b5a9e01c01b708d39d1323f0e7fc612" )
  ; ( "lib/chatml_sources/chatml_source.ml"
    , "af9021a33f112a570bc4cb38658a6f04630158035ebf6343104487e3c16485a8" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "26e5565b90eed1bbc3a1c22233dfffbff62ba5fbf88a6f062da40adb1886836d" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "df7bb85b160031e5be1f09186fe76dd1667fd43a3b2b36b6b05e56266129ef6c" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "061a15ff4c42b9541bddea79e4cc6f1d3f514de251eda3e9b293fb677545e9fe" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "7813658a7aa0d0ad6f02e5b91fc42120ca797296ad27c9213aaf9ae3fbeb2ab0" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "f683e13e9d4098146e976a3640e69952ebd9b0f77e44f49a3c072c414473c16f" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "7386077708c5f4aa9f8aebab6cbd6e6712ab54c43b301312457ce366b799bff8" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "35f4b38221394505040af4a6892d4ca92238f35071d08c23cd84a576dffa819f" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "904fe3b7f7e251f651c89c9d49aa449dabe7bcd47f0de6d808f41a6c5397390a" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "582c7f6b811c51b6301212a0564132fb07ea8be6d89f419c5d43c6475ec3d041" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "65ba204ea5691f2c33f1a6e9f36b871b5e3978e1c5af7bb87338cc40ff70c137" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "f26513eaa3ae368a0834ae673773c685810907aee27eed4221995bb88eea35cd" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "4f7ab4f8a44fc52f22a876663a1a07d9930c809680c1a731ab02594c73ad5b8e" )
  ; ( "one_off_v1"
    , "chatml.json"
    , "5f176d03ec3c915c1f828b160535a18bd1a8beb9389b5944db636825f53d16b6" )
  ; ( "tool_v1"
    , "chatml.json"
    , "fd93d6baeadfec1eacb64e36e48a8451775ee03478b1c29f9c3dc34405d8ca03" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "65e21dd76ef4fd141c14c097bcd5076b44c53bc746384c638f7131df456e6e29" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "25a0a41e50b72f8343c273d51854d4b1cfe8689d186036bfb2dac06d63247d23" )
  ; ( "one_off_v1"
    , "chatml.modules"
    , "30ea7778d712d46c20ac99db1ab4f62cd07e9b3793f81e5c18de82aefa4c9a0d" )
  ; ( "tool_v1"
    , "chatml.modules"
    , "4ab493f20e5b89f8f9ffeb1c464dea0330526d314184d047aca55e546c4454fd" )
  ; ( "moderator_v1"
    , "chatml.modules"
    , "f87d7adeb308b101d8d407bacbe488b99d3899d2a7d36ecaafa3f0b09422ce4d" )
  ; ( "delegated_moderator_v1"
    , "chatml.modules"
    , "ee03082d7111cbbbf24f6ea6d3a6ee5dbdd813bee473979dbadd2986610a8003" )
  ]
;;
