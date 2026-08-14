# Driver – ChatMarkdown Orchestration Layer

`lib/chat_response/driver.ml`

---

## 1 · Overview

`Driver` is the **high-level façade** that turns a *ChatMarkdown* document
into successive calls to the OpenAI *chat completions* API.  It hides the
plumbing – parsing, configuration, tool dispatch and response streaming –
behind a handful of easy-to-use functions so that command-line utilities,
tests and TUIs can focus on the user experience instead of network
book-keeping.

```
conversation.chatmd   ─► Driver.run_completion                ─► updated file on disk
                       └► Driver.run_completion_stream (+UI) ─► incremental deltas

(nested <agent/> prompts) ─► Driver.run_agent ─► assistant text (string)

(purely in-memory)      ─► In_memory_stream.run_completion_stream_in_memory_entries
```

## 2 · Public API

| Function | Purpose |
|----------|---------|
| `run_completion` | Blocking, single-turn helper – read `.chatmd`, execute model, append answer. |
| `run_completion_stream` | Streaming variant used by the TUI – fires callbacks on every delta. |
| `run_agent` | Evaluate a self-contained `<agent>` prompt inside the current conversation. |
| `In_memory_stream.run_completion_stream_in_memory_entries` | Identity-bearing helper working on canonical in-memory history. |

### Optional flags shared by several helpers

| Flag | Default | Effect |
|------|---------|--------|
| `parallel_tool_calls` | `true` | Execute tool invocations concurrently under a bounded semaphore while still inserting outputs **in the original call order** so the document remains deterministic. |
| `history_compaction` | `false` | Collapse repeated reads of the same file into a single entry before sending the history to the model.  Significantly reduces prompt size in conversations that iterate over large documents. |
| `meta_refine` | `false` | Enable the *meta-refine* experimental feature which lets the model run an additional self-critique pass.  Can also be toggled globally by setting the environment variable `OCHAT_META_REFINE=1`. |

### 2.1 `run_completion`

```ocaml
Driver.run_completion
  ~env                  (* Eio runtime *)
  ?prompt_file          (* optional template to prepend once              *)
  ~output_file          (* evolving ChatMarkdown conversation on disk     *)
  ()
```

* Appends `prompt_file` once (if given) to `output_file`.
* Parses the resulting XML buffer with `Prompt.Chat_markdown`.
* Extracts configuration (`<config/>`) and declared tools (`<tool/>`).
* Converts the prompt to canonical `History_entry.t` values.
* Recursively calls `Response_loop.run_entries` until;
  – no pending function calls remain; or   
  – the model produced a plain assistant answer.
* Renders assistant messages, reasoning blocks and tool-call artefacts
  back into `output_file`, then inserts an empty `<user>` placeholder for
  the next turn.

### 2.2 `run_completion_stream`

As above but:

* Uses the *streaming* OpenAI endpoint, producing incremental token
  deltas.
* Executes tool calls **as soon** as their arguments are fully received,
  then resumes streaming.
* Invokes `?on_event` for every `Openai.Responses.Response_stream.t` so
  that callers can update a TUI or web client in real time.

Typical usage inside a Notty TUI:

```ocaml
let on_event = function
  | Responses.Response_stream.Output_text_delta d ->
      View.append_text ui d.delta
  | _ -> ()

Eio_main.run @@ fun env ->
  Driver.run_completion_stream
    ~env
    ~output_file:"conversation.chatmd"
    ~on_event
    ()
```

### 2.3 `run_agent`

```ocaml
Driver.run_agent ~ctx prompt_xml inline_items
```

Evaluates a **nested agent** within a running conversation.  The function
takes an *independent* ChatMarkdown snippet (`prompt_xml + inline_items`),
spawns a recursive response loop and returns all assistant messages as a
single concatenated string.  It is primarily used by the built-in `fork`
tool.

### 2.4 Identity-bearing in-memory execution

`In_memory_stream.run_completion_stream_in_memory_entries` takes a
caller-owned allocator and canonical `History_entry.t list`. Existing entries
retain their IDs and newly accepted assistant and tool-output occurrences
receive IDs from that allocator.

## 3 · Implementation Highlights

1. **Configuration discovery** – `<config/>` blocks are parsed with
   `Config.of_elements`, exposing model, temperature, reasoning effort and
   other tuning knobs.
2. **Tool wiring** – user-declared `<tool/>` and `<agent/>` blocks are
   converted to `Ochat_function.t` values via `Tool.of_declaration`.
3. **Caching** – network responses and agent expansions are stored in a
   TTL-LRU `Cache.t` persisted under `~/.chatmd/cache.bin`.
4. **Streaming book-keeping** – private hash-tables keep track of open
   assistant messages, reasoning summaries and function calls while the
   stream is live, ensuring that the output buffer on disk stays
   well-formed even if the program is terminated abruptly.

## 3.1 · Invariants

The driver maintains several important invariants that callers can rely on:

* **Document integrity** – the `.chatmd` buffer is always well-formed XML.
  Even in streaming mode every opening tag is flushed to disk before the
  corresponding closing tag is emitted.
* **Deterministic ordering of tool calls** – when `parallel_tool_calls = true`
  results are still inserted **in the order the calls appeared in the
  stream**, not when the underlying fibers finish.
* **Shared context** – nested agents created with `run_agent` inherit the
  parent cache and the current working directory contained in `Ctx.t`.
* **Crash-resilience** – partial assistant messages are closed on shutdown so
  that a subsequent run can resume without repairing the file.

## 4 · Examples

### 4.1 CLI one-shot

```ocaml
Eio_main.run @@ fun env ->
  Driver.run_completion
    ~env
    ~output_file:"conversation.chatmd"
    ()
```

### 4.2 Live TUI

```ocaml
open Notty_unix

let ui = Ui.empty  (* simplified *)

let on_event = function
  | Responses.Response_stream.Output_text_delta d -> Ui.append_text ui d.delta
  | _ -> ()

Eio_main.run @@ fun env ->
  Driver.run_completion_stream
    ~env
    ~output_file:"conversation.chatmd"
    ~on_event
    ()
```

### 4.3 Nested agent

```ocaml
let assistant_answer =
  Driver.run_agent ~ctx "<system>Translate</system>" [ CM.Text "Bonjour" ]
in
assert (String.equal assistant_answer "Hello")
```

### 4.4 In-memory variant

```ocaml
let initial_history = [] in

Eio_main.run @@ fun env ->
  let allocator =
    History_entry.Allocator.create
      ~namespace:"example"
      ~next_sequence:0
    |> Result.get_ok
  in
  let history' =
    In_memory_stream.run_completion_stream_in_memory_entries
      ~env
      ~allocator
      ~history:initial_history
      ~tools:None
      ()
  in
  Format.printf "History length = %d\n" (List.length history')
```

## 5 · Limitations / TODOs

* **Back-pressure** – the current streaming loop stores deltas in memory
  before flushing them to disk, which may become an issue for very large
  outputs.
* **Error recovery** – transient network failures result in a restart of
  the whole turn; finer-grained retry logic could be implemented.
* **Provider payload migration** – canonical entries currently wrap OpenAI
  response items; a later migration can change the payload without changing
  application-owned history identity.

## 6 · Related modules

* [`Converter`](converter.doc.md) – ChatMarkdown → OpenAI item
  translation.
* [`Moderation`](moderation.doc.md) – shared ChatML lifecycle events,
  history projection, and overlay state.
* [`Response_loop`](response_loop.doc.md) – recursive resolution of tool
  calls.
* [`Tool`](tool.doc.md) – runtime representation of `<tool/>` and
  `<agent/>` blocks.
* [`Ctx`](ctx.doc.md) – immutable execution context threaded through the
  pipeline.

---

© The Ochat authors – released under the same licence as the source
code.  Feel free to copy-edit.

## Shell runtime orchestration

`Chat_response.Agent_runtime` now centralizes shell manifest compilation,
administrative/trust/signature checks, exact manifest authorization, immutable
registry instantiation, and declared-tool construction for main driver,
nested-agent, and TUI paths. A shell tool is never published with a missing or
unauthorized runtime.

Moderator `Process.run` receives the registry adapter, model reviewers receive
the configured completion callback, and session-backed approval/audit state is
owned by the surrounding execution context. There is no production fallback to
`Custom_command_runner.run` or another direct-spawn path.
