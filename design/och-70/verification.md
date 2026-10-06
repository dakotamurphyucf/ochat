# Verification and reproducibility

Worktree branch `impl/och-70-chatml-audit`, audited base `58e9d2c5567d596fbe52ba1041e37d12dbde102b`. Only design/och-70 and test/och70_chatml_audit are ticket-owned versioned changes. No implementation fixes, provider requests, credentials, real host tools, external network, desktop work, shared package/switch/default mutation, push or merge.

## Environment

Use the existing default opam switch read-only. Every command below was invoked with an explicit workdir in the ticket worktree; Dune uses `--root .` because the worktree sits under another repository's ignored scratch tree. The worktree has its own _build. OCaml 5.3.0; Dune 3.21.1; ocamlformat 0.28.1. The root .ocamlformat Jane Street profile was used. This consumes the available approved switch, not a re-created exact CI environment (CI metadata pins Dune 3.24.2). No environment readiness for other platforms or final distribution is inferred.

## Finished checks

From the ticket worktree root:

```sh
opam exec --switch=default -- ocamlc -version
opam exec --switch=default -- dune --version
opam exec --switch=default -- ocamlformat --version
opam exec --switch=default -- dune runtest --root . test/och70_chatml_audit
opam exec --switch=default -- ocamlformat --check test/och70_chatml_audit/audit_tests.ml
```

All finished with exit0 after the observed expect output was reviewed and incorporated. New suite: four expect blocks, 47 source comparisons plus ten compiler-policy observations (57 total). The first three probing runs and initial host-policy run intentionally used empty/new expectations and returned exit1 with reviewable diffs. Only ticket-owned observations were promoted or manually inserted from the reviewed diff. Nothing was automatically accepted for unchanged baseline tests.

Build the seven existing runners (the first group was built across two invocations; the second completed in one):

```sh
opam exec --switch=default -- dune build --root . \
  test/.chatml_typechecker_test.inline-tests/inline-test-runner.exe \
  test/.chatml_runtime_test.inline-tests/inline-test-runner.exe \
  test/.chatml_parse_test.inline-tests/inline-test-runner.exe
opam exec --switch=default -- dune build --root . \
  test/.chatml_standalone_test.inline-tests/inline-test-runner.exe \
  test/.chatml_execution_budget_test.inline-tests/inline-test-runner.exe \
  test/.chatml_diagnostic_budget_test.inline-tests/inline-test-runner.exe \
  test/.chatml_type_traversal_test.inline-tests/inline-test-runner.exe
```

Run each from **the worktree's test directory**, with explicit workdir there. The following command was executed separately with NAME set to each of chatml_parse_test, chatml_typechecker_test, chatml_runtime_test, chatml_standalone_test, chatml_execution_budget_test, chatml_diagnostic_budget_test and chatml_type_traversal_test:

```sh
opam exec --switch=default -- \
  ../_build/default/test/.NAME.inline-tests/inline-test-runner.exe \
  inline-test-runner NAME -source-tree-root .. -diff-cmd -
```

All seven runner executions finished exit0 with empty logs. PPX wrote .ml.corrected artifacts even on matching output; a Python byte comparison confirmed all seven exactly equaled the original tracked sources before removing only those generated artifacts. No baseline test was modified or promoted. The existing suites include ordinary type/recursive/mutation/module/arity cases; nonexecuting initialization and isolated namespace checks; caller cancellation; bounded diagnostic/type traversals; execution fuel/loop/callback/task limits; nested/expired budget ancestry and failure not disguised by Task.catch. This does not cover every possible workload or host adapter.

One early runner call without required arguments ran no tests and returned1; another root-directory call had an incorrect PPX source location and returned2. Those are excluded from passed evidence. A concurrent Dune build produced the checker runner but was interrupted (exit130) when it remained pending; rebuilding the remaining runtime/parser runners separately completed exit0. Direct initial attempts to run those absent binaries returned127 and are also excluded. Final invocations used the correct test-directory working location. No package repair was attempted. Parser generation emitted pre-existing Menhir conflicts: 33 shift/reduce states, two reduce/reduce states, 218/29 resolutions and one never-reduced production. They remain documented investigation inputs, not demonstrated defects by count alone.

## Acceptance boundaries

The new tests intentionally assert current observations. F1/F2/F3/F5 demonstrate accepted programs failing because static and runtime representations/availability disagree; F4 demonstrates inconsistent static equality admission. A fix owner must change the corresponding expectations to the chosen correct policy, retaining positive cases and adding broader regressions. Passing this audit suite means reproductions are stable, not that bugs are fixed.

Not run: whole-repository dune runtest, ChatMD/agent-native integration E2E, live provider tests, every builtin/host-return adapter, fuzzing, differential reference-engine campaigns, repeated/concurrent compilation stress, large allocation/heap measurements, remote transports, production transactions, recovery, release packaging or Linux qualification. Compiler and interpreter budgets are cooperative and their allocation accounting is an estimate, not a hard memory sandbox. No full type-soundness claim follows from passing fixtures.

Root owns publishing concrete follow-ups and attaching relevant implementation acceptance blockers before closing OCH-70. OCH-73/M4-R1 consumes findings but need not wait for all fixes. All promised tests are finished; no test/build process remains at handoff. Local worktree scratch notes retain per-command logs and intermediate troubleshooting; durable evidence here records successful checks and the failed/excluded attempts explicitly.
Actual host: Darwin arm64. Relevant installed versions: Core v0.17.1, Eio/Eio_main1.3, menhirLib20260209, ppx_jane v0.17.0, ppx_expect v0.17.2. Core/ppx_expect differ from CI metadata's v0.17.2/v0.17.3; the audit does not claim exact CI-package equivalence. Read-only `opam list --switch=default --installed --columns=name,version core eio eio_main ppx_jane ppx_expect menhirLib` recorded these versions.
