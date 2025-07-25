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

(purely in-memory)      ─► Driver.run_completion_stream_in_memory_v1
```

## 2 · Public API

| Function | Purpose |
|----------|---------|
| `run_completion` | Blocking, single-turn helper – read `.chatmd`, execute model, append answer. |
| `run_completion_stream` | Streaming variant used by the TUI – fires callbacks on every delta. |
| `run_agent` | Evaluate a self-contained `<agent>` prompt inside the current conversation. |
| `run_completion_stream_in_memory_v1` | Headless helper working on an in-memory history. |

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
* Converts the prompt to `Openai.Responses.Item.t list` via `Converter`.
* Recursively calls `Response_loop.run` until;   
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

### 2.4 `run_completion_stream_in_memory_v1`

Variant of `run_completion_stream` that never reads or writes the
filesystem – ideal for unit tests or back-end services that keep the
complete history in a database.

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

## 5 · Limitations / TODOs

* **Back-pressure** – the current streaming loop stores deltas in memory
  before flushing them to disk, which may become an issue for very large
  outputs.
* **Error recovery** – transient network failures result in a restart of
  the whole turn; finer-grained retry logic could be implemented.
* **API stability** – the suffix `_v1` in
  `run_completion_stream_in_memory_v1` signals that the signature may
  change in the future.

## 6 · Related modules

* [`Converter`](converter.doc.md) – ChatMarkdown → OpenAI item
  translation.
* [`Response_loop`](response_loop.doc.md) – recursive resolution of tool
  calls.
* [`Tool`](tool.doc.md) – runtime representation of `<tool/>` and
  `<agent/>` blocks.
* [`Ctx`](ctx.doc.md) – immutable execution context threaded through the
  pipeline.

---

© The Ochat authors – released under the same licence as the source
code.  Feel free to copy-edit.

