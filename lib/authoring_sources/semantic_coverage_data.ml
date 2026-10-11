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
  [ ( "lib/chatml/chatml_type_expr.ml"
    , "42cfb14d5b001bc0b10bda3b261acdfd170bf21de83173fc271216cbec4bda8e" )
  ; ( "lib/chatml/chatml_inferred_type.ml"
    , "672f8d4c721b175c68d6b14e9982444b80891e9bec529cbd905f367ca6cda961" )
  ; ( "lib/chatml/chatml_type_alias.ml"
    , "a38b93f3a7908134c451708a8116b6b4b4efa8b192137d6a8e2b6205ae21510f" )
  ; ( "lib/chatml/chatml_type_validity.ml"
    , "e1fd576d7740c4f6d1abe1bc8ee4942b4a837459d13795088a07455cfae4d583" )
  ; ( "lib/chatml/chatml_equality_requirement.ml"
    , "d5eac886d08cdc3e3619b7d1f1ccab0e391ad282e08bc2e9835bc84acc519b48" )
  ; ( "lib/chatml/chatml_eval.ml"
    , "79e194163e761e5e69ec524a7a08264a1725805457dce949d9bc94d5b848ecac" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "d8fcdf563cddcfe4e1da17c7071b36e13139faef561d41a68a90f3ee155539ab" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "c059f593e8b50952a43e1f599ab68223b284ad239164a6069bbe1eeadc618ff6" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "a12d91fffae5c1e820e6fab32a76651549e85b6321137c7705538ea1e069f635" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "4c5a9633373f8cf9fe83f28bbe6dcd02eefa5cf0d94c23d4c21dc35f219cc7b5" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "31c6a581032dbb7041e04de2143771418a4d155dcfc8fe31fcd4021c99445a82" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "85e628530838f41bf0a0edc2cd67fc1f39c8bd0d7e3c8ef7578c608c2a7cb45a" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "22db3e939e98c2a9624a6eb81998390737e078fdac140236ee0f3ee20cc1d448" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "8fac4bc0ec4804396bbe5a6676d30becf779d778ce26332ad0d2a66e7273816a" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "a96ce4069c160d3251b01d226de12cd85e7f9bd6c2b315d7e63705434c497521" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "dbc289a3148888aa7063815ac38fbffd4f49363d86934f56975fb8fae5a82142" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "55e28e7a35ab38205231eef59a495fc2b7570b4aa696d19087b6b4a975059cf8" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "9f0ef9b48fd5838a2853f720e179b4ea715129fea67c5e7124a1f87f7101c7aa" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "d5ed9cc7751769552ffe72a5fe82a49563cf1e89fe44fe1057842a0c2468daa4" )
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
    , "3c7cc833a4e269630ce05618962735a57d1314f5ccb2103132cd635564e10179" )
  ; ( "tool_v1"
    , "chatml.json"
    , "883764484b30fa88e7f5d0a5d76ae2936f07582ce0efad00ff88434360952328" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "c99c2b1124271bfdbe031fe1af27b00b092be7fb701f89e6bffeacd0d26003d9" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "3973947d8425800f26054b9ef09ba3c44e806e504acbe90dd039f6170802e8ba" )
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
