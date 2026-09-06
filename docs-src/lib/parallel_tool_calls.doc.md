# Parallel tool calls

Ochat can run callable tools concurrently, but provider permission to propose
multiple calls and host execution concurrency are different controls.

## Quick-start

In the legacy file-backed TUI:

```sh
chat-tui --no-config -file agents/explorer.chatmd --parallel-tool-calls
chat-tui --no-config -file agents/explorer.chatmd --no-parallel-tool-calls
```

Parallel execution defaults on. These flags select legacy mode unless another
host is explicitly selected; native `--local` and daemon-connected modes reject
them. Supplying both flags is an error. There is no
`OCHAT_PARALLEL_TOOL_CALLS` environment setting or `--max-parallel` CLI flag.
See [host selection](../bin/chat_tui.doc.md).

## Provider messages and execution

The Responses protocol carries `Function_call` and `Custom_tool_call` items.
A provider request's `parallel_tool_calls` setting does not by itself create
concurrent execution. Non-streaming `Response_loop.run_entries` requests
parallel-call support but resolves its returned calls with ordinary sequential
`List.map`.

The legacy and shared in-memory streaming adapters use Eio promises and a
semaphore of eight for ordinary parallel callable tools. This is an internal
per-loop limit, not a global limit on all nested agents or jobs. Special paths
such as forks have their own ownership. Agent-host jobs also obey the separate
[capacity controls](../agent-server/chatml-orchestration.md).
Disabling legacy parallel execution runs an ordinary call synchronously.

## Ordering and cancellation

Tools may overlap and progress events may interleave. Final tool outputs are
collected in their assigned call sequence, not completion order. The next
foreground model request waits for that collection; a fast tool completing does
not mean the assistant can already react while a slower sibling is pending.
ChatML background jobs are a separate orchestration mechanism.

Each output retains its provider call correlation and application-owned history
identity. Cancellation or failure can prevent execution or leave external effects
uncertain. There is **no at-least-once or exactly-once tool execution guarantee**.
Do not automatically retry irreversible tools based only on missing output.

## Limitations & caveats

Tools writing shared resources must coordinate those effects themselves.
Concurrency is not filesystem isolation. Permission/moderator gates, shell
runtime limits and cancellation still apply. Stream idle deadlines are not
universal total-turn or per-tool deadlines.

## Reference implementation

- [Shared streaming adapter](../../lib/chat_response/in_memory_stream.ml):
  `make_tool_promise`, `await_calls`.
- [File-backed streaming driver](../../lib/chat_response/driver.ml).
- [Non-streaming loop](../../lib/chat_response/response_loop.ml).
- [TUI stream presentation](../../lib/chat_tui/stream.ml).
