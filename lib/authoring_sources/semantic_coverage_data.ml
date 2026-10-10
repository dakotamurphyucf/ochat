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
  ; ( "types.record_update"
    , "Immutable row extension and type-changing overrides"
    , [ "lib/chatml/chatml_typechecker.ml"; "lib/chatml/chatml_eval.ml" ]
    , "chatml.inference"
    , [ "records.type-changing-update"; "test/agent_docs/docs_chatml_authoring.ml" ] )
  ; ( "types.record_join"
    , "Common-field joins and shared tail identity"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "records.join-rejected"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
    , "c99dae867ef6554d348e499b454fc0afa4ee04c2adedb5c212895000e24570b1" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "25208912ca7f431b3ffa6b6edec9f72d9a643724a56bd90a849c2157b0df3b97" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "19fb962eba914f35d2a1c40c15dc95ecfc49d7316a4e60557c4069f0a352e151" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "0ab604ef8269be8b1d847f1538ea250ba25f7ad9456dab0e57189921166896f3" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "c45fe17514158eb2179be067170bfd9aba17c521c4260f5f75cfba4e3ccd1339" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "61eb3932293b0703bda5075088e7f76b333a1598abed61a15b928371ad58c151" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "c705228682baee23dc8ec03dfbb06d57808e79c754d374a433df558a627fbeaa" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "7260940da59569567111de465beebf7a4c84875acd0ca02fa6f588904284516f" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "f28b4409fd1497ae6a20cd76c89afda7c10f2ce57378588bd8f969710a7ddaa4" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "de4730371e88a3bea91ba98f2ef303c9ebf38aac237ba8d701153c8e644e4d19" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "891d098b492e63eceafc9c1eba9b6069a82e08a06c18661be8ce155dcfc89d1f" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "739d75ff8d4b2183b00b13967ccc35b26d23f07fe319370c0575bbdf28564b65" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "11f63fb503104c5515168c91d1d13a53043320ead580f0062c459aaa39ea40e4" )
  ; ( "one_off_v1"
    , "chatml.json"
    , "4a976060eb6ae6a23fba932336c0dd428e807e8b15515111831e4fd938f06ead" )
  ; ( "tool_v1"
    , "chatml.json"
    , "956722cbdf85e6e4cb18963d1781341afb2a381cf5b96ebf366db0f8d5755ad7" )
  ; ( "moderator_v1"
    , "chatml.json"
    , "22e825b5f5c99e7cdeb7843ce3717979b98c55ad6239278d608deff4047ec6d3" )
  ; ( "delegated_moderator_v1"
    , "chatml.json"
    , "dd02b454b31316ded12a324c4ac8fdd6f19fc929eafcd2e26fb8ee047b8dd439" )
  ; ( "one_off_v1"
    , "chatml.modules"
    , "1e1957c1a62ae270794aff600519fe5786a872f7ffed188f3c02a12d36d03fa8" )
  ; ( "tool_v1"
    , "chatml.modules"
    , "9fc0039b8cc7edfbde3663fc7d647813cba8aee0edd0c6a2efe783ef456bf884" )
  ; ( "moderator_v1"
    , "chatml.modules"
    , "372d9c3f633ef2222653733262cb2c996bce082bb46d4cd8949485bfe429723b" )
  ; ( "delegated_moderator_v1"
    , "chatml.modules"
    , "c24e4732d4e49c379aa55e822a993d7842199093a0d8e5ebf19a5a950c92cf5b" )
  ]
;;
