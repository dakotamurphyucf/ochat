# `Shell_access` architecture

`ochat.shell_access` is the policy-aware process execution library beneath the
ChatMD runtime. It operates on already-authorized immutable configuration and
does not parse ChatMD.

## Core layers

- `Command` and `Chain`: structured argv and conservative pipelines/
  conditionals without general shell expansion.
- `Input` and `Limits`: bounded stdin, wall/idle/output and OS resource bounds.
- `Capabilities`, `Effect`, `Analyzer`: inferred behavior and runtime ceilings.
- `Executable`, `Resolver`: canonical path, trust, SHA-256/stat identity, and
  immediate pre-spawn replacement checks.
- `Matcher`, `Policy`: composable matchers with deny-over-ask-over-allow.
- `Approval`: manifest/executable/argv/cwd/environment/input-bound grants and
  rewrite-capable reviewers.
- `Interceptor`, `Secret_filter`: before/after transformation and repeated safe
  output finalization.
- `Audit`: structured envelope, sequence/identity, and explicit failure policy.
- `Backend`: direct, Seatbelt, bubblewrap, external/fake implementations and
  confinement classification.
- `Execution_plan`, `Executor`: immutable plan and Eio-owned execution.

## Execution invariants

Capability failure precedes approval. Hard denial precedes substitution.
Rewrite restarts preparation. Required confinement never selects direct.
Pipelines use concurrent Eio flows; cancellation closes pipes and kills/reaps
owned processes. Output is bounded, UTF-8 normalized, terminal-sanitized, and
secret-redacted after every custom transformation.

See [the shell security guide](../../guide/chatmd-shell-security.md) and
[implementation architecture](../../guide/chatmd-shell-runtime-internals.md).
