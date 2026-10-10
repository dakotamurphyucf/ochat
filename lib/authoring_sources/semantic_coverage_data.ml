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
    , [ "variants.closed-total"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
    , "97d4e028fc9bdbbb1caf5a9d43f84fc3f91ba3f6b033c5fe176b86f38725c4b2" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "bf3a8a6990c054446a4d451768c9978c81f74e8b69ee1701e1d2b9a856f0fa2d" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "1b768cbd760b86f1f502e7a8f37a1ef983bd4077c64368a84590a1079fff2f62" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "2602a25f691613dae39edb27202aa8ee7964b5c19e2d19c3f8e58097b9730d4d" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "ea2c9aba33ac53677059fbc11fc548bd84c88471a3ecd4f23d59830e15478a65" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "d1858ec59ca7d3d6eb044bb0e87e82b467734c1d35675b924ab6f2341c8648b7" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "4e8a5788cd59bbf632e9c4456e13521258739a241c9b308e68cf1f908c60b6fb" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "a85ac5ea85281e4e2ea9474682a48d0d6e07fe10f5b42b2ce9ec34a5b347a2f6" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "2a71ec7ac6c3a96003c43e2e6f168729bb6102111eca30424e78ce73bef1d986" )
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
