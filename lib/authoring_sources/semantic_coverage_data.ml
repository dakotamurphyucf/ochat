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
      ; "lib/chatml/chatml_semantic_binding.ml"
      ; "lib/chatml/chatml_semantic_recorder.ml"
      ; "lib/chatml/chatml_semantic_elaboration.ml"
      ; "lib/chatml/chatml_semantic_scope.ml"
      ; "lib/chatml/chatml_semantic_host.ml"
      ; "lib/chatml_linked/chatml_checked_graph.ml"
      ; "lib/chatml_semantics/chatml_semantic_budget.ml"
      ; "lib/chatml_semantics/chatml_semantic_error.ml"
      ; "lib/chatml_semantics/chatml_semantic_identity.ml"
      ; "lib/chatml_semantics/chatml_semantic_limits.ml"
      ; "lib/chatml_semantics/chatml_semantic_partial.ml"
      ; "lib/chatml_semantics/chatml_semantic_publication.ml"
      ; "lib/chatml_semantics/chatml_semantic_query.ml"
      ; "lib/chatml_semantics/chatml_semantic_snapshot.ml"
      ; "lib/chatml_semantics/chatml_semantics.ml"
      ; "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_type_alias.ml"
      ]
    , "chatml.inference"
    , [ "test/chatml_graph/configured_checker_tests.ml"
      ; "test/chatml_linked/linkage_tests.ml"
      ; "test/chatml_linked/inference_tests.ml"
      ; "test/chatml_linked/contract_tests.ml"
      ; "test/chatml_graph/session_invariants.ml"
      ; "test/chatml_semantic_facts/lexical_tests.ml"
      ; "test/chatml_semantic_facts/member_tests.ml"
      ; "test/chatml_semantic_facts/driver_tests.ml"
      ; "test/chatml_semantics/query_tests.ml"
      ; "test/chatml_semantics/affinity_tests.ml"
      ; "test/chatml_semantics/failure_tests.ml"
      ; "test/chatml_semantics/domain_tests.ml"
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
      ; "lib/chatml/chatml_syntax_sites.ml"
      ; "lib/chatml/chatml_syntax_name.ml"
      ; "lib/chatml/chatml_tokens.ml"
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
      ; "test/chatml_parser_sites/site_tests.ml"
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
  [ ( "lib/chatml/chatml_semantic_binding.ml"
    , "ce9d44482241a529229a5e4dcf10a00d523c90e112f5d68112ab16ab3a4ef516" )
  ; ( "lib/chatml/chatml_semantic_recorder.ml"
    , "9673669295fabae6f5f1fa413fe60f09a5c92b98f22c63945c379eb6802c5e86" )
  ; ( "lib/chatml/chatml_semantic_elaboration.ml"
    , "f2a116cfba2373081b7770165b8c6b1e280735a1f621b385e4262cba654dea66" )
  ; ( "lib/chatml/chatml_semantic_scope.ml"
    , "612dfd76887cb67560f9f13ae02ebece08f17c197ff68701b16ad537fb472e12" )
  ; ( "lib/chatml/chatml_semantic_host.ml"
    , "9f1255d27277450fc5de6bd3d7bddc7cb3160c91f89a63f3f9110b5865150ee3" )
  ; ( "lib/chatml_linked/chatml_checked_graph.ml"
    , "5a236b6811ea43464254fc0eb9b506decda0fa129cd2740fa8e1dd9c5a2900f9" )
  ; ( "lib/chatml/chatml_builtin_callback.ml"
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
    , "0136bdd6a8d3a12c56eae31f227ec42fbe10cbacd06d858325403dd40a27940f" )
  ; ( "lib/chatml/chatml_configured_resolution.ml"
    , "a27f6d77fd71ee481e5ac5671b95be84f1be12243be88ee640daaeb3902c3e7b" )
  ; ( "lib/chatml/chatml_configured_check.ml"
    , "e893811c1e1c9f5f83b62a5bdc56ed7461979225d1d2a5644dfac37e71be3ff9" )
  ; ( "lib/chatml/chatml_static_surface.ml"
    , "07c8f7a97e1e6da286ff4e103181ce4b22b12ebb6f370ac19918f171c0723111" )
  ; ( "lib/chatml/chatml_compiler_contract.ml"
    , "057abd58ea8c9513c836e36dd87ad664a01bf76d0c264ccb3b61249e2a9c2763" )
  ; ( "lib/chatml/chatml_module_reference.ml"
    , "f2e9c1ff541b67a243effe6b4f04b8c0f15763000f9cd42603d6a9a746d0a728" )
  ; ( "lib/chatml/chatml_contract_encoding.ml"
    , "7f8c6c1a49818abaf5ecbfe7cc1df2f4511930939cc1c573e3d422c94548c138" )
  ; ( "lib/chatml_linked/chatml_linked_program.ml"
    , "a1e4203a423527acfc538471ed848004f5211ae57231700d1a6760ef3204ab47" )
  ; ( "lib/chatml/chatml_binding_pattern.ml"
    , "d52f16e4403d887830eae7d6cd74df840318c8088007301c805fb3224311dc6f" )
  ; ( "lib/chatml/chatml_binding_value.ml"
    , "4d8e2dc9fc05c7bd578e1cd12e989ad32acca54252df0dfaa1a26515015eea71" )
  ; ( "lib/chatml/chatml_tuple.ml"
    , "287538bb4046c2780d6ea23a9fe96abea941f790eb2450410210b6f6318eb12d" )
  ; ( "lib/chatml/chatml_alias_equivalence.ml"
    , "53e1bb3e19369b3ae6e3a55225c9e4a41c1e81001a21bb41b1c787a033f2f32e" )
  ; ( "lib/chatml/chatml_module_signature.ml"
    , "a721d2fa4cd9cf50c466407929294e6e52ad31867cecd89f4548eefc29b05e1e" )
  ; ( "lib/chatml/chatml_module_visibility.ml"
    , "8483156ba7674a7cd6e7d3e37898ef86f48fb279273d6ebcec94a02c7966bfa0" )
  ; ( "lib/chatml/chatml_signature_check.ml"
    , "964b104016b715b0454aa1d03233de5e342df3b7cb2f1f4a5ef9763f5f4e28bc" )
  ; ( "lib/chatml/chatml_type_probe.ml"
    , "9198cb575e3d2fd81d44367360ddfaf8e7ebcf9c4b90379c7862e64cc25ac2ea" )
  ; ( "lib/chatml/chatml_type_expr.ml"
    , "cac9b04852affcbb36a9c82b1b6b041f4506b41d436fa817ad22af63fc693246" )
  ; ( "lib/chatml/chatml_inferred_type.ml"
    , "c7321a73faf94a345643d3b366710cc834e7540f1c1e6b5c0066b7b5810ac570" )
  ; ( "lib/chatml/chatml_type_alias.ml"
    , "a970257ed2d2cc404d197bd78d55ef0c8caa473d11e46aa88813423721290fdf" )
  ; ( "lib/chatml/chatml_type_validity.ml"
    , "e1fd576d7740c4f6d1abe1bc8ee4942b4a837459d13795088a07455cfae4d583" )
  ; ( "lib/chatml/chatml_equality_requirement.ml"
    , "d5eac886d08cdc3e3619b7d1f1ccab0e391ad282e08bc2e9835bc84acc519b48" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "2ed1cb2abca1e856a4aedf4a764fa4e72ff6b6d2466e30a79fa8754c95f4441b" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "f8d63210438cadd286834a77bb62eaa371cc4f94aea08751aa84a87bc49333f0" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "380fbb57b697b380ecfeab15af38daeb7b33bc01b3702b04aa37c862d2a28a4a" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "d86b011029133547857dcb9f266f20985a8ea184ed8112f8552a29483004ebf2" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "6d23e09cf8001512cc4a14b906eabcf227285aab0c298504f567e3aeb5c50482" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "a90febbecdf0930e08d7ff80293c1633104377b384f40f626966ce30ca403303" )
  ; ( "lib/chatml/chatml_type_scheme.ml"
    , "fbfe49403016d3367736f369881a50491bf38f0f574106be753ef5be3f506961" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "34d04d5b6b6a3e15a4cbdf75cf1a380c3eeb593045620893ba11c05b15d02eb7" )
  ; ( "lib/chatml/chatml_parse.ml"
    , "43af74968f3b9fbdd2135f29ee5f5b1f09adf5605a0130979fd63fae1e6f9623" )
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
  ; ( "lib/chatml/chatml_syntax_sites.ml"
    , "6c3722757d12ae2474b24d3884eb84d99a05c0955ffa271b11b86d6c5d85db93" )
  ; ( "lib/chatml/chatml_syntax_name.ml"
    , "62dd2442b16981e7f17df7d5e4abff23f2b0cf56bddd81d0429df2c4b5a650aa" )
  ; ( "lib/chatml/chatml_tokens.ml"
    , "b3ec3e6688013010dce58c332167a1a4c9bb2e9ac24504f0b12a1fcd7502d601" )
  ; ( "lib/chatml_semantics/chatml_semantic_budget.ml"
    , "023a83afe321d9f3bd3f653fd57ed128c537e9779bd403c9ea1cb3a367a67dbe" )
  ; ( "lib/chatml_semantics/chatml_semantic_error.ml"
    , "8f59dcc2fe99ac6317df1354de7ebd316bc6ad353dfacace5117793a7a7e5a56" )
  ; ( "lib/chatml_semantics/chatml_semantic_identity.ml"
    , "a96b7da8fb27b831166352bdbad421973bff54f35dcc09bdb97b41e2e5f03fc0" )
  ; ( "lib/chatml_semantics/chatml_semantic_limits.ml"
    , "2ffd322dc2466606bcb8b96d2f57a6d8481fa0558e2ddccd219c9c6c81d57bdb" )
  ; ( "lib/chatml_semantics/chatml_semantic_partial.ml"
    , "44951e8152f10b536d2f9a41440b5857805182c985698838e6b3c63c5cc3be0d" )
  ; ( "lib/chatml_semantics/chatml_semantic_publication.ml"
    , "42afab1155304afc42344ff5cff511ef94534c119a9e7a1b477082835f85934a" )
  ; ( "lib/chatml_semantics/chatml_semantic_query.ml"
    , "2a8b718f84bee6caf8ba9b0f8268eda73fab087ec16a5e70fa5b99b9ea0e78f0" )
  ; ( "lib/chatml_semantics/chatml_semantic_snapshot.ml"
    , "ee7467f7546774b8a5dce71779d9af0b957110c5369f25f79477dd6e48ed8d90" )
  ; ( "lib/chatml_semantics/chatml_semantics.ml"
    , "afc6441de7d1671dcdeb378f592f7f6ee70235bcd54298666d42ee5eba37a109" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "6b2642ffc7445e47ba5b45ace3344d08829ebe1208c9fbb7ff1466d96d6c5319" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "3088fff6733d96bf0b8dbc51c05a40c066e3dc797704effe75c6f298131275ae" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "4d532b66f3cb5bd4aa307c27c84e04f3c841b6d65c2c1ee5b8bea66c8b95cdfe" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "1525e048a7d4b86582f186338ce015f2a7aef0acbea31ce7560a3217a91f70af" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "03901d7e8141832e25d8da43fe1ca390a7dfc0bb7fff125bcb97d424313149fe" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "66172ea85bd80d3abde9a0399f44ebc75cb3e70dc2d724428a8f1503de6589ed" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "405858e8d8848efae7ab630117e70bf92444a35b5e90374bce1d290bac2a3039" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "a9e9c43daf2d072f655f0da610e11febe4c520623d56c333834ea29723eb3750" )
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
