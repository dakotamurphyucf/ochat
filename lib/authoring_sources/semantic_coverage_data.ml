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
    , "Recursive scheme copying, value restriction, weak variables and expansive bindings"
    , [ "lib/chatml/chatml_typechecker.ml"; "lib/chatml/chatml_type_scheme.ml" ]
    , "chatml.inference"
    , [ "inference.expansive-function-rejected"
      ; "types.recursive-json-callback"
      ; "test/chatml_type_scheme/graph_tests.ml"
      ; "test/chatml_type_scheme/occurs_tests.ml"
      ; "test/chatml_type_scheme/authored_tests.ml"
      ; "test/chatml_linked/recursive_scheme_tests.ml"
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
  ; ( "scope.configured_dependencies"
    , "Source-owned configured checking and entry-rooted value dependency selection"
    , [ "lib/chatml_graph/chatml_configured_lookup.ml"
      ; "lib/chatml_graph/chatml_dependency_graph.ml"
      ; "lib/chatml_graph/chatml_dependency_session.ml"
      ; "lib/chatml/chatml_value_scope.ml"
      ; "lib/chatml/chatml_configured_resolution.ml"
      ; "lib/chatml/chatml_configured_check.ml"
      ; "lib/chatml/chatml_static_surface.ml"
      ; "lib/chatml/chatml_compiler_contract.ml"
      ; "lib/chatml/chatml_module_reference.ml"
      ; "lib/chatml/chatml_contract_encoding.ml"
      ; "lib/chatml_linked/chatml_linked_program.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_type_alias.ml"
      ]
    , "chatml.inference"
    , [ "test/chatml_graph/configured_checker_tests.ml"
      ; "test/chatml_linked/linkage_tests.ml"
      ; "test/chatml_linked/inference_tests.ml"
      ; "test/chatml_linked/contract_tests.ml"
      ; "test/chatml_graph/session_invariants.ml"
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
      ; "lib/chatml/chatml_builtin_callback.ml"
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
  ; ( "stdlib.collection_composition"
    , "Ordered bounded collection callbacks and shallow table snapshots"
    , [ "lib/chatml/chatml_builtin_composition.ml"
      ; "lib/chatml/chatml_table_snapshot.ml"
      ]
    , "chatml.collections"
    , [ "test/chatml_stdlib_composition/composition_tests.ml" ] )
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
  [ ( "lib/chatml/chatml_builtin_callback.ml"
    , "dcab6dc6fb6995c46f2064706cac671abf1996bca5085cd467bf530f2b4c8e5b" )
  ; ( "lib/chatml/chatml_builtin_composition.ml"
    , "d33b2433431c604aacfce2558b87e83a465292d0ce1518309605c3fd1a5b53ba" )
  ; ( "lib/chatml/chatml_table_snapshot.ml"
    , "e303927cdb84dba6c6616dc1b48b672cd1d6a74b741ac37b0f3b979576398a92" )
  ; ( "lib/chatml_graph/chatml_configured_lookup.ml"
    , "c34afbab927cd26ce0ca4fca9062c12786e6b04bfa3e50a520223294105c7934" )
  ; ( "lib/chatml_graph/chatml_dependency_graph.ml"
    , "61b968e8bb38885385f1779628ca7d11cc76c621491ca957cc3fc4d93c755e0f" )
  ; ( "lib/chatml_graph/chatml_dependency_session.ml"
    , "da87c8021750834d2f018c7196182360eead9d468a7e836d7e7eb2db25ccc27d" )
  ; ( "lib/chatml/chatml_value_scope.ml"
    , "dc8e9355121e4cc31c1f8fa11bc61195c7ba30f38a3616a3d88e4f99a7853345" )
  ; ( "lib/chatml/chatml_configured_resolution.ml"
    , "7fa80f72909a1255cfe59ab238942e7eec3344a3def2f693867ef9dd3bc1b037" )
  ; ( "lib/chatml/chatml_configured_check.ml"
    , "b50504b9534218084519d83fac53ed1c4f86a94b398886d6f26771882ae8573f" )
  ; ( "lib/chatml/chatml_static_surface.ml"
    , "07c8f7a97e1e6da286ff4e103181ce4b22b12ebb6f370ac19918f171c0723111" )
  ; ( "lib/chatml/chatml_compiler_contract.ml"
    , "057abd58ea8c9513c836e36dd87ad664a01bf76d0c264ccb3b61249e2a9c2763" )
  ; ( "lib/chatml/chatml_module_reference.ml"
    , "f2e9c1ff541b67a243effe6b4f04b8c0f15763000f9cd42603d6a9a746d0a728" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml_linked/chatml_linked_program.ml"
    , "a344c9d811ed02ff820abffb442a297d72dfeb699f47f546e221698d7e6750a8" )
  ; ( "lib/chatml/chatml_binding_pattern.ml"
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
    , "4ba6b8864df9019fa9ee54a8efeda4389fc2bf0ecd98b436cc9a785666d092a5" )
  ; ( "lib/chatml/chatml_type_probe.ml"
    , "9198cb575e3d2fd81d44367360ddfaf8e7ebcf9c4b90379c7862e64cc25ac2ea" )
  ; ( "lib/chatml/chatml_type_expr.ml"
    , "ea499a1f29c146d3c6dade9f6afaedaf018310b5ba03dec958f36439b7b8ac04" )
  ; ( "lib/chatml/chatml_inferred_type.ml"
    , "c7321a73faf94a345643d3b366710cc834e7540f1c1e6b5c0066b7b5810ac570" )
  ; ( "lib/chatml/chatml_type_alias.ml"
    , "ac90688d554007b7232ce0d3c5449c570a5dafb7be8cf97296de8b260ff1ff6b" )
  ; ( "lib/chatml/chatml_type_validity.ml"
    , "e1fd576d7740c4f6d1abe1bc8ee4942b4a837459d13795088a07455cfae4d583" )
  ; ( "lib/chatml/chatml_equality_requirement.ml"
    , "d5eac886d08cdc3e3619b7d1f1ccab0e391ad282e08bc2e9835bc84acc519b48" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "26e9c0d0374407252f65f8fb8f7439652ed2e181db63a4d938af8f7d74eb82d4" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "bfe0c6b1ae8712e412bd761057b536aba3c6d954226fc39a709cc9c4e715d9e4" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "7e1ad82856e9e6e1a1a26f6202e535a0f55dd4671c5083429d205abb448e7c5e" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "fdd0b670b664c57d0b054628d8ffa32269b9b4203f181fac4472e953f3fb47ab" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "95e24e79caf35864c474fdfc3b62bdfdb57556dee00bb9845cb8b70a3c83e062" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "5d5fedcac818be38bcbbdf550b3a35b634f51b9f7fbd28c7d79aa6a2399e5a25" )
  ; ( "lib/chatml/chatml_type_scheme.ml"
    , "fbfe49403016d3367736f369881a50491bf38f0f574106be753ef5be3f506961" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/chatml/chatml_parse.ml"
    , "0a83dc3d473a0ea94cd215895d3cc94b96b799f1bb3472a89bb85f937b6f304f" )
  ; ( "lib/chatml/chatml_owned_source.ml"
    , "84b3a946d19d92a4530ec9a13f2ec0fe72adaad9790ec167f085372ddf26feeb" )
  ; ( "lib/chatml/chatml_checkpoint.ml"
    , "a574f6bbc3b71389e2789bf74863354f6eab3ffbe8c9c254f9b88bc1acefb879" )
  ; ( "lib/chatml/chatml_builtin_spec.ml"
    , "85a25e01db7c9e5b801c2b37a78404bb6314a6c7f986667e21e2c1fcb9d5d212" )
  ; ( "lib/chatml_sources/chatml_source.ml"
    , "af9021a33f112a570bc4cb38658a6f04630158035ebf6343104487e3c16485a8" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "044844b91bd49db621891199bed13b9112f2af2e3c569e079a43ef58e7c9d6be" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "c278974fc62cc66a90cba20f1524990469ae36ee3e60597060e482f5f32c9e30" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "e989899518ae09f2867f4b288454136f0a8ecf0edeaaec9a0ec08a2621c0d309" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "eee596ae2580a568ab07c03b7ab7c62489d6a32b33976433c0835fa21197b875" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "9f676f82422a00a916a78d57159bea7caedf1c63718254ca417ddf921165745d" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "9d4d8dfa916a577974c528a814649ade85aeeb9df4c85e4463e5d8b01019265d" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "5891373a14be2d71fd0164f83b7ee6bcd6f05e60ee1932cfd2ebfc204a0bc3d2" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "7af8ed54d15ab807ece133b1d8c1bdbcd62c0d28cca8a3372f82b5c71826c763" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "c1cf4990803e28d10498ed66666235a14bbea0004d2a18f5db4e0f26d05b05b5" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "88f2928bb7f73a55208abca6c16ac9f79a3a0dcbedbff5f4f71f35e202b4baab" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "daf3a2479e16d3753c0ed15f7fd3a412a4d899a796f5e3ba703878a722ae5517" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "a5855463fc7c1128ab285f7ecdca5f7e42881aee4999f520ff5c46f90ed0d5ea" )
  ; ( "one_off_v1"
    , "chatml.json"
    , "6cfc6cb31e83c4b433fe2795df6beecc5f4a159847d8d77a37f9de010fbf8c22" )
  ; ( "tool_v1"
    , "chatml.json"
    , "be99236c481eff12507cb15cf708bcb3fb4459bda51a1cd7162ae1b23d52af6a" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "3a6d575755af1958b8e2d1b7db61f5bc9ad1e58a421775091cb520df5dc82ede" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "41f1e64c3d7ee1101a12e0c2952e696a9a8435185eebcf2351ae6538d4410699" )
  ; ( "one_off_v1"
    , "chatml.modules"
    , "4dc0b2bfef418ef05fc770e858970a75e888011d997dfa2d5046dd0139f3a14d" )
  ; ( "tool_v1"
    , "chatml.modules"
    , "a007861e06a46ed61b7c935b8eb0f4a4fe18494d201ae167b556707cdff57fb6" )
  ; ( "moderator_v1"
    , "chatml.modules"
    , "8146afddbda3b649a7ed0b67b14cf0f3483bec0eac6b3aa085463b2367102bcb" )
  ; ( "delegated_moderator_v1"
    , "chatml.modules"
    , "65bdd8ff10a23e66731a8e61cab362523e3fa5d19b268efe0af53814ae553ee0" )
  ; ( "one_off_v1"
    , "chatml.collections"
    , "2f58080d1c26e61faf504c5f909fc3ac6f36cabc8ddec546da92b53f8423d765" )
  ; ( "tool_v1"
    , "chatml.collections"
    , "33979523c45b73b689a7520c7c4750523b8b93944fed3fcd4e69f52a79b3cb35" )
  ; ( "moderator_v1"
    , "chatml.collections"
    , "a6eb920d02b8a34e367d66734132741a3f55bff1bc100186e79a5e649622e5c8" )
  ; ( "delegated_moderator_v1"
    , "chatml.collections"
    , "f7d9dc9af62a37cc4295fcbe1d9b2f520b8f8bbe3e710d843b78af5bf56fce47" )
  ]
;;
