# `Shell_access` architecture

`ochat.shell_access` is the policy-aware process execution library beneath the
ChatMD runtime. It operates on already-authorized immutable configuration and
does not parse ChatMD.

## Core layers

Related public module maps: [shell runtime](../shell_runtime/architecture.doc.md)
and [shell declaration/compiler](../chatmd_shell_spec/architecture.doc.md).

For daemon/local host ownership, authorization bootstrap and all seven path
variables, start with [shell host integration](../../guide/chatmd-shell-host-integration.md).
The linked [child process setup](process_spawn.doc.md) enforces selected OS
process limits before the sandbox backend executes. It is not a transport or an
independent sandbox. Session actor
lifetime owns runtime cancellation; disconnecting a detached client does not
cancel its processes. Restart does not resurrect a running process.

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
- `Sanitized_stream`: incremental terminal/UTF-8 normalization, cross-chunk
  literal redaction, and bounded safe-prefix disclosure.
- `Audit`: structured envelope, sequence/identity, and explicit failure policy.
- `Backend`: direct, Seatbelt, bubblewrap, external/fake implementations and
  confinement classification.
- `Execution_plan`, `Executor`: immutable plan and Eio-owned execution.
- `Request_channel`: bounded private process requests with host-owned authority
  checks, used by the [helper transport](../../bin/ochat_agent_helper.doc.md).
  It supplies no daemon credentials or session-management authority itself.

## Execution invariants

Capability failure precedes approval. Hard denial precedes substitution.
Rewrite restarts preparation. Required confinement never selects direct.
Pipelines use concurrent Eio flows; cancellation closes pipes and kills/reaps
owned processes. Completed results retain the existing terminal filtering,
literal redaction, byte bounds, and repeated finalization after custom
transformations. Their legacy byte truncation is not UTF-8-boundary-aware;
the incremental progress contract below does not change canonical results.

## Sanitized streaming API

`Executor.run config invocation` retains the completed-result path, including
after-interceptors, and emits no process-output progress.

`Executor.streaming_support config` validates the live-streaming subset. Hosts
should check it before publishing a sanitized-stream tool. It rejects **every
after-interceptor** and secret/replacement configurations outside the conservative
literal-filter subset. `Chat_response.Shell_tool.create` maps rejection to
`shell.tool_stream_unsupported`; it does not silently buffer or downgrade.
See the [exact compatibility restrictions](../../guide/chatmd-shell-security.md#sanitized-live-progress),
including the replacement's first/last-byte boundary rule.

`Executor.run_streaming config invocation ~on_progress` repeats that check, then
uses the same authorization and execution path as `run`. Native pipe reads can
emit safe prefixes before process completion; simulated and substitute results
emit only after completed-result checks. The returned canonical result is
unchanged. There is no pre-policy or raw-byte observer API.

Progress records contain `channel` and `text`. All events use one combined
`Stdout` append stream, **including stderr**, so concatenating channel output
cannot reconstruct a configured secret. Per-pipe decoders remain independent;
merged-channel filters protect cross-command joins, and a final disclosure
filter protects the combined presentation. Intermediate pipeline stdout is not
published. Progress spans selected commands and can be more conservatively
redacted than the last-command canonical stdout; it is transient, not history.

Each event is valid UTF-8 and at most 4096 bytes. Invocation-wide source-channel
and total budgets bound disclosure and replacement expansion; combined progress
uses the total budget. Observer calls are serialized, and suspended final-tail
delivery shares the invocation wall deadline. Ordinary observer exceptions do
not replace the canonical result. Cancellation propagates. Failed/cancelled
invocations discard pending tails without retracting previously safe progress.

`Sanitized_stream.support` and `create` validate a literal filter against a
positive byte budget. `feed` accepts arbitrary raw chunk boundaries; `finish`
flushes the final safe suffix; `discard` closes without flushing. Closing is
idempotent. Matching coalesces overlapping/adjacent secrets. Use independent
instances for independent raw pipes and serialize access. This low-level
sanitizer does **not** authorize execution or protect an application that later
recombines independently filtered streams without a final disclosure filter.

See [the shell security guide](../../guide/chatmd-shell-security.md) and
[implementation architecture](../../guide/chatmd-shell-runtime-internals.md).
