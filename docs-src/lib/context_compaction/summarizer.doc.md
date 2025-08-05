# Context_compaction.Summarizer

Utilities for turning a selection of chat history items into a single
textual summary that can be injected back into the system prompt when
the conversation no longer fits in the model context.

This module is **not** responsible for deciding *which* messages are
important – that task belongs to
[`Context_compaction.Relevance_judge`](relevance_judge.doc.md).  Its
sole job is to take the items already deemed relevant and compress
them into a much shorter form while preserving the technical details
needed for the assistant to keep working on the same task.

---

## High-level algorithm

1. **Render transcript** –
   [`Openai.Responses.Item.t`](../../../../lib/openai/responses.mli) is
   converted into a plain-text transcript (role prefixes such as
   `user:` and `assistant:` are added).

2. **Decide execution mode** – based on the presence of
   *both* an `Eio_unix.Stdenv.base` capability and the `OPENAI_API_KEY`
   environment variable:

   * **Online** – perform a blocking call to the OpenAI
     `/v1/responses` endpoint using model **`gpt-4o ("gpt4_1")`** with
     a temperature of `0.3` and `100 000` output-token budget.

   * **Offline stub** – truncate the transcript to `2 000` characters
     and return it verbatim.  This keeps tests deterministic and
     ensures the function never throws.

3. **Return summary** – either the model-produced summary or the stub
   is returned to the caller as a plain UTF-8 string ready to be placed
   in a system or developer message.

---

## Public API

```ocaml
val summarise :
  relevant_items:Openai.Responses.Item.t list
  -> env:Eio_unix.Stdenv.base option
  -> string
```

### `summarise ~relevant_items ~env`

* **`relevant_items`** – ordered list of conversation items that must
  survive compaction.  Items can be:
  * user / assistant messages
  * function-call requests (`Function_call`)
  * tool outputs (`Function_call_output`, `Web_search_call`, …)

* **`env`** – optional Eio capability used for network and file-system
  access.  Passing `None` forces the offline stub.  Passing `Some env`
  attempts the online path and silently downgrades to the stub on any
  failure (missing key, network error, server refusal, …).

* **Return value** – textual summary (UTF-8) safe for direct inclusion
  in a system or developer message.

* **Exceptions** – none.  All internal failures are swallowed and
  converted to the stub summary.

---

## Usage example

```ocaml
open Context_compaction

let make_user_msg text : Openai.Responses.Item.t =
  let open Openai.Responses in
  let open Input_message in
  Item.Input_message
    { role = User
    ; content = [ Text { text; _type = "input_text" } ]
    ; _type = "message"
    }

let () =
  let items =
    [ make_user_msg "Implement MD5 hashing in OCaml"
    ; make_user_msg "Oops, please make it pure in OCaml 5″
    ]
  in
  (* Force the offline path by omitting the Eio env *)
  let summary = Summarizer.summarise ~relevant_items:items ~env:None in
  Format.printf "Offline summary:@.%s@." summary
```

When run, the example prints the first lines of the transcript because
no `Eio` environment was provided.

Providing an `env` obtained via `Eio_main.run` together with a valid
`OPENAI_API_KEY` would instead instruct the model to produce a
rich summary.

---

## Internal helpers

Although private to the module, two constants describe the behaviour
and may be of interest:

| Name | Purpose |
|------|---------|
| `prompt` | Hard-coded system prompt steering the model towards an ordered, heavily-structured summary. |
| `max_stub_chars` | Length of the offline stub in UTF-8 characters (`2000`). |

---

## Known limitations / future work

1. **Prompt brittleness** – the quality of the generated summary is
   highly sensitive to the hard-coded prompt.  Consider externalising
   the prompt or adding expect tests asserting structure.
2. **No streaming support** – the function blocks until the summary is
   fully generated.  This is fine for our use case but may become an
   issue with slower models or extremely large transcripts.
3. **Single function API** – only one public function is exposed.
   Additional helpers (e.g. pretty-printers) could improve
   debuggability.

---

## See also

* [`Context_compaction.Relevance_judge`](relevance_judge.doc.md) – how
  *relevant* items are selected.
* [`Context_compaction.Compactor`](compactor.doc.md) – orchestrates the
  full compaction pipeline and stores the summary in the session.
