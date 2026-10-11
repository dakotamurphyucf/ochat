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
    , "Unique labels/binders, structural and literal patterns"
    , [ "lib/chatml/chatml_typechecker.ml" ]
    , "chatml.inference"
    , [ "test/chatml_typechecker_test.ml"; "test/agent_docs/docs_chatml_authoring.ml" ] )
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
  [ ( "lib/chatml/chatml_eval.ml"
    , "f28c5f6f3a773b83a05e12558e67bb255e2c2293f1cfcdacc2004a09fbd1ca8a" )
  ; ( "lib/chatml/chatml_lang.ml"
    , "944594533c46d658d2a6a900fb020f47b2ec5c9859363d52bf00629a1926d47f" )
  ; ( "lib/chatml/chatml_lexer.mll"
    , "ab2c6ece7bf9fa3465b7cf011d2f566a4972b357e525d98b5e0d81bfaab4cc89" )
  ; ( "lib/chatml/chatml_parser.mly"
    , "b9b98ef6b2bf946e7f772dfeceb9e037ec2126e2ff88202615cdcb125a14370d" )
  ; ( "lib/chatml/chatml_resolver.ml"
    , "5139c4940c89b1f443b94a688e97262e41c9362d5cc368a02ed0f71d4cbae6be" )
  ; ( "lib/chatml/chatml_typechecker.ml"
    , "1ed043449f56a05796149d7007b655bbe7374b1c3e9224d2e31b1bdd06440cc8" )
  ; ( "lib/chatml/chatml_value_codec.ml"
    , "0f621d743b97b856dd40270fc8c51444ca18e6098ceb7ff2481a22cf999d85be" )
  ; ( "lib/chatml/chatml_parse.ml"
    , "3fc909f7cede4765a4f7b0eda7bb51cb7d7185d55105e518a963037f98fb5d1e" )
  ; ( "lib/chatml/chatml_owned_source.ml"
    , "84b3a946d19d92a4530ec9a13f2ec0fe72adaad9790ec167f085372ddf26feeb" )
  ; ( "lib/chatml/chatml_checkpoint.ml"
    , "a574f6bbc3b71389e2789bf74863354f6eab3ffbe8c9c254f9b88bc1acefb879" )
  ; ( "lib/chatml/chatml_builtin_spec.ml"
    , "efcd1f8f4532149c6992eb446e3944e1599c068e22eb76b5d027f370a61a6df4" )
  ; ( "lib/chatml_sources/chatml_source.ml"
    , "af9021a33f112a570bc4cb38658a6f04630158035ebf6343104487e3c16485a8" )
  ; ( "lib/chatml_sources/chatml_diagnostic.ml"
    , "cd4f59d52868eff58a8eaabd2cc8222d8c65f677066b3d8ab16c7b5c7a81fde5" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.evaluation"
    , "e4fc7f56d8a13101f82dbe61dcf11db1132b048dd027f4789b78ed42a272b38a" )
  ; ( "tool_v1"
    , "chatml.evaluation"
    , "95a410789358124c989f97055fc5bece1281022d4dc4ec927df62cdba1ff8651" )
  ; ( "moderator_v1"
    , "chatml.evaluation"
    , "816f31063788a3654a7e915d3eddcef7858f6f7bedf183c5dfddde9bef24a9eb" )
  ; ( "delegated_moderator_v1"
    , "chatml.evaluation"
    , "399be7d1455d4697e050444850d149089c557154d09bc9326f1f1a4a137c92e9" )
  ; ( "one_off_v1"
    , "chatml.inference"
    , "817c07004170371628c14ebca4ac8154ce8b0be0ed44617b92ba8bcc7b90a122" )
  ; ( "tool_v1"
    , "chatml.inference"
    , "799e888156276c39c1964de435365187f04f0a613b8e29b625649bbbdbf0345e" )
  ; ( "moderator_v1"
    , "chatml.inference"
    , "0394fb16a07489b30e13b5cfea1bba1c4d284cbc99f9d86f87b029f0d3854e3c" )
  ; ( "delegated_moderator_v1"
    , "chatml.inference"
    , "666c785dbafd279f1f5a554a75d48f9ab1e9a26e4fd6e4056f01f2209394c207" )
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
