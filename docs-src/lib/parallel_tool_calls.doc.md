# Parallel Tool Calls

`parallel_tool_calls` is an opt-in feature that lets the runtime dispatch
several tool calls **concurrently** instead of serially.  When enabled it can
dramatically improve latency for workflows that invoke slow external tools, at
the cost of a slightly more complex execution model.

---

## 1 · Quick-start

```bash
# Enable at the CLI …
chat_tui --parallel-tool-calls …

# …or via environment variable (same effect, lower precedence)
export OCHAT_PARALLEL_TOOL_CALLS=1
```

If neither the flag nor the environment variable is set, tool calls are
executed **one-by-one** – the legacy behaviour.

---

## 2 · JSON schema overview

When a model message contains more than one entry inside
`choices[].message.tool_calls`, the scheduler interprets it as *potentially
parallelisable*:

```jsonc
{
  "choices": [
    {
      "message": {
        "role": "assistant",
        "tool_calls": [
          { "id": "a1", "type": "function", "name": "search",   "arguments": {/*…*/} },
          { "id": "b2", "type": "function", "name": "download", "arguments": {/*…*/} }
        ]
      }
    }
  ]
}
```

With `--parallel-tool-calls` **both** `search` and `download` are launched in
their own fibers, and their individual completions are streamed back as soon
as they arrive.

Invariants:

* Each `tool_calls[i]` keeps its original `id`.
* The driver guarantees **at-least-once** execution of every call.
* Partial results are forwarded through the regular streaming channel the
  moment a tool finishes.

---

## 3 · CLI flag reference

Flag                   | Effect
-----------------------|-------------------------------------------
`--parallel-tool-calls`| Enables the concurrent scheduler (default: on).
`--max-parallel` *N*   | Upper-bounds concurrency (default: number of logical CPUs).

If the flag is provided multiple times the last occurrence wins.

---

## 4 · Limitations & caveats

1. **Side-effects** – tools that mutate shared resources (files, DB rows …)
   must implement their own locking strategy.
2. **Non-determinism** – completion order is *not* deterministic; do **not**
   rely on a fixed sequence of deltas.
3. **Resource pressure** – launching a large number of heavy processes can
   starve the host; tune `--max-parallel` if needed.
4. **Timeout semantics** – the global request timeout still applies; slow
   calls can block faster ones if the timeout is hit.
5. **Debugging** – logs interleave; prefix each log line with the call `id`
   to retain readability.

---

## 5 · Reference implementation

The feature spans two primary modules:

* **`chat_response.Driver`** – owns the concurrent scheduler, result
  aggregation and failure handling.
* **`chat_tui.Stream`** – consumes live deltas and keeps the UI responsive
  while tools are still running.

For deep-dive API documentation run:

```bash
dune build @doc
xdg-open _build/default/_doc/_html/index.html
```

Both modules are thoroughly annotated; feel free to explore them for
implementation details.

---

## 6 · Example session

```
$ chat_tui --parallel-tool-calls
> Assistant: I will perform two actions.
> (tool a1:search)   …started
> (tool b2:download) …started
> (tool a1:search)   …finished (120 ms)
> Assistant: Search results received, analysing…
> (tool b2:download) …finished (410 ms)
> Assistant: All tasks complete 🎉
```

Notice how the assistant already reacts to the search results while the file
download is still in flight.

---

Happy hacking!  Feedback and questions welcome on the project issue tracker or
in the `#ochat` Matrix room.

