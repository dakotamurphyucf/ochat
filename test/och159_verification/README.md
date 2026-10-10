# Bounded ChatML correctness verification

This suite checks the parser → checker → resolver → evaluator pipeline against
independent, finite reference cases. It supplements the existing tests; it does
not claim complete type soundness or verification of every builtin/host adapter.

| Contract | New checks or existing coverage |
| --- | --- |
| Array values and local mutation | Lengths 0–8: reverse, shallow copy independence, in-place reverse, all 165 valid subwindows and every valid write index; list-based expected results. Invalid set/swap operations preserve the original array and report their source line. |
| String byte semantics | Five strings, including multibyte UTF-8: 60 byte windows and first-match searches over five patterns. An independent byte scanner supplies expected results, including empty input/pattern. |
| Higher-order collection callbacks | Lengths 0–8, 36 list-oracle comparisons for map/filter/fold and mutation-observable callback order/count. |
| Static argument and mutation contracts | Ten wrong-type/value-restriction/recursive/row examples must reject before evaluation; paired valid row-polymorphic and recursive programs execute. |
| Lexical ownership | Rebinding a ref does not alter an earlier closure; separately created closures retain independent mutable state. |
| Compiler isolation | Serial baseline plus 12 concurrent requests with conflicting local alias names/types; each successful compiled program executes twice with fresh mutable globals. Rejected programs keep their own source locations. Existing `chatml_compilation_test.ml` covers cancellation, simple concurrency and initializer nonexecution. |
| Simultaneous binder policy | Annotated/unannotated duplicate lambda parameters and local/top-level duplicate recursive-group names reject. Sequential let/let-rec/local rebinding remains accepted. Diagnostics use real available AST spans, not invented token precision. |
| Task descriptions and host operations | Existing `chatml_standalone_test.ml` exercises execution limits, cancellation, fresh entrypoint globals and continuations. `chatml_composition/` covers admitted host operations, lifecycle ownership, result/receipt contracts and authority rejection. These are separate from pure builtin checks; no live provider credentials are used here. |
| Dynamic JSON boundary | Existing `chatml_value_codec_test.ml` validates JSON conversion; inferred ChatML types do not validate arbitrary external JSON. |
| Recursive traversal and diagnostics | Existing `chatml_type_traversal_test.ml` and `chatml_diagnostic_budget_test.ml` check bounded traversal/cancellation and diagnostic rendering. This suite adds lexical/recursive/row consumer cases and source anchoring, not an unbounded fuzz campaign. |

Run from the worktree root, using the repository's installed toolchain:

```sh
opam exec --switch=default -- dune build --root . --build-dir=_build-och159 -j2 \
  @test/och159_verification/runtest \
  @test/runtest-chatml_typechecker_test \
  @test/runtest-chatml_runtime_test \
  @test/runtest-chatml-compilation
```

The workload is bounded by the enumerated lengths/cases, with no timing-based
assertions, random seeds or external provider/network dependency. Independent
oracles cover the selected pure operations, not the entire builtin inventory.
Known module/record, physical record-width, variant-arity, equality and module
initialization findings are owned by OCH-154–158; this suite must not freeze their
current bugs as expected behavior. Required-entrypoint diagnostics still need
source/declaration identity work in OCH-127, while lambda/recursive binder-token
precision requires AST source-span support there. Oversized String.slice/Array.sub lengths are regression-tested at Int.max_value;
overflow-safe checks preserve language runtime errors instead of leaking host
exceptions. Broad JSON exception handling and forged-table candidates remain
inspection-only until reproduced. OCH-177 owns the separately reproduced fresh
inference-variable identity defect; no security or soundness claim follows from
suspicion.
