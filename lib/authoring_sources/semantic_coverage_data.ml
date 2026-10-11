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
    , "Binding annotations, supported constructors and closed rows"
    , [ "lib/chatml/chatml_parser.mly"; "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.closed-record-annotation-rejected"
      ; "test/agent_docs/docs_chatml_authoring.ml"
      ] )
  ; ( "types.arrow_arity"
    , "Arrow flattening, zero arity and function-valued arguments"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.explicit-arrow-arity"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "types.aliases"
    , "Ordered structural aliases and contractive recursive types"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "inference.structural-aliases"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
    , [ "records.type-changing-update"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
    , "Known-type restrictions and generic identity-comparison limitation"
    , [ "lib/chatml/chatml_typechecker.ml"; "lib/chatml/chatml_lang.ml" ]
    , "chatml.inference"
    , [ "inference.generic-equality-boundary"
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
    , "Ordered module exports, open collision checks and lexical scope"
    , [ "lib/chatml/chatml_typechecker.ml"
      ; "lib/chatml/chatml_resolver.ml"
      ; "lib/chatml/chatml_eval.ml"
      ]
    , "chatml.modules"
    , [ "modules.outer-not-exported"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
  [ ( "lib/chatml/chatml_eval.ml"
    , "68a24e628af3d9aede64f726acad0af125aac3b9e961b008df354670b68036a6" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "b554291282ec62909a325f2dddb9bd761f4e0a660399320555d3473c04c2c4d0" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "ab2c6ece7bf9fa3465b7cf011d2f566a4972b357e525d98b5e0d81bfaab4cc89" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "b9b98ef6b2bf946e7f772dfeceb9e037ec2126e2ff88202615cdcb125a14370d" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "679b48fde2c4319c055bc52e67cffd29f4168163634514f14111cd4de7bb075d" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "2264a450778416c4078f6831e8249acc91ba87e43dd0f499f77a25b273f8b517" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "51dee548177b54f693e03c25fc2b07de5df3f7a19ece052367e846670a17096e" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "6437ef794dffb3d9ecc6d751aad667b209c9b1f5ac7a8a3233577a7210cfedc2" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "89381fedee373b2b60df0ac373a88834a68db3d59d38889af0b6e26fed995004" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "1b412f1e2781d17b358d71cf97e45b73a51615bdf7ad90ed6001a5edfcfa2a3c" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "37031f6791c12f9a295477e12a81c0b5d0342bec8f4dc6c0b25e64589cda67e9" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "0e23041425f6d3f7780de16fcbc4a858f76383f7e25eea765c76309340c72442" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "5a148977de0153082540e3e1e9a4a6d86c4339cba3154f1e45cf0af500da94d4" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "0d2bdf964f90cf65f906b9fcd22875a87b3b6e3e095c0d7520ae11886d4658fe" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "2f79102968c363bfaea021a29a659cd8884c0500393e902f016121e7568179d9" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "18490fafcce366cf5614e94e209af5b3d87b5a8c31ba02889a43298ba93229bb" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "2047a4d0529c25c310abf3dcd9497e36666236adec134c6a6903b3df00aef3a5" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "7a51fde518ecdb590931a0c7002a06b7427982edebc6b235557382dcef119fe5" )
  ; ( "one_off_v1"
    , "chatml.json"
    , "a5d1d781b237f6338443f72a71613c363ff010734152c726c38564f24a06e01b" )
  ; ( "tool_v1"
    , "chatml.json"
    , "2bf6810324967c05395b048d2dbb10dac6e8c39f0ee34de78bac82fd030dd73c" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "02890cb2623c3d964cc84be4e8286e89e62d1ac53acd73c81b1971f53a929029" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "2cd14598304ae20b3b0b234a1b8cadf225e647f2f12e99bd3fe7391be428759f" )
  ; ( "one_off_v1"
    , "chatml.modules"
    , "242cdb69d6c9725371e7b7c3be2f2c550855014e8cc10b322a00f059b25ddcb8" )
  ; ( "tool_v1"
    , "chatml.modules"
    , "db1981fdfc6958f38d90937cc4ebc10ab8eabe6f5bd1417ebe53b216ea04cf72" )
  ; ( "moderator_v1"
    , "chatml.modules"
    , "61bc7ff14cec74f66d790dd51ba0a76a46b59486160b5f15b6f4caf9cdda3b8f" )
  ; ( "delegated_moderator_v1"
    , "chatml.modules"
    , "1604b4dbc3e11f3884c524e303ee18a7c2a48fdee938bd3e347dd57ecfe57c3a" )
  ]
;;
