# Local canonical Goal retirement qualification

This executable exercises the actual durable 256-attempt ledger boundary. It is
an explicit local qualification, outside normal CI and `runtest`:

```sh
dune build @test/goal_accounting_retirement/goal-accounting-retirement-local
```

Or run the executable directly:

```sh
dune exec test/goal_accounting_retirement/main.exe
```

Use the repository's isolated toolchain. No live provider, credential or network
is required. The existing Factory fixture supplies an offline provider and owns a
private temporary store, its daemon and all cleanup. This directory defines no
inline-test or `runtest` alias. Linking the wrapped Factory test helper library
does not run its inline tests.

The fixture creates a real Goal/episode, executes exactly one real provider, then
closes the Goal and replaces retained usage from 7 to 11. It supplies 256 genuine
unrelated canonical admissions, each released as `Interrupted Host_interrupted /
Definitely_not_submitted`, with zero pressure-backend dispatches. Assertions keep
the Goal row present through admission 255, verify its one atomic fold on 256,
reject late resurrection, and physically reopen the Store to verify retained
accounting, receipt identity, and no provider replay. The 256-attempt/256-turn/
4 MiB durable profile is unchanged. No ledger rows or completions are seeded.

The run has a 900-second fixture timeout; the shared helper still defaults to
30 seconds. Each admission begins with an unprotected cancellation check and
cooperative yield, outside the actual protected admission/release. Phase and
completed-admission counts print every 32 admissions through the fixture's live
Eio stdout, with Eio monotonic elapsed time. Assertion failures/cancellation retain
their original backtrace; failure diagnostics cannot replace the primary error.
Success messages follow all accounting/reopen assertions; exit zero additionally
requires the outer fixture cleanup to finish.

Allow several minutes for the full profile: each admission validates and persists
the growing retained window. Dune may buffer the alias's output; run the executable
directly to watch its progress as it runs. Keep the full exit result when recording
qualification. Reaching 256 admissions alone does not prove atomic retirement or
recovery; the subsequent assertions and cleanup must also pass.

Compact accounting/retirement law tests remain in the ordinary test suite. See
[Goal accounting](../../docs-src/development/goal-accounting.md) and
[test tiers](../../docs-src/agent-server/testing.md).
