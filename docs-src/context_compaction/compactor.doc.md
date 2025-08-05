# `Context_compaction.Compactor`

Conversation–history compactor that keeps your chat under the model’s
context window without throwing away crucial information.

---

## Overview

`Compactor` orchestrates the three lower-level building blocks that make
up the **context-compaction** pipeline:

| Step | Module | Responsibility |
|------|--------|----------------|
| 1.   | [`Config`](./config.doc.md) | Provide runtime parameters such as *context limit* and *relevance threshold*. |
| 2.   | [`Relevance_judge`](./relevance_judge.doc.md) | Decide which messages are important enough to survive. |
| 3.   | `Summarizer` | Turn the survivors into a single textual summary. |

The output is a **new chat history** that contains:

* the very first message of the original transcript (usually the system
  prompt), and
* *at most one* additional `system` message with the generated summary.

This design minimises token usage while still giving the LLM enough
context to pick up the conversation where it left off.

---

## Algorithm in Detail

1. **Load configuration** – `Config.load ()` looks for a JSON file in
   XDG-conformant locations and overlays any values it finds onto the
   built-in defaults.

2. **Filter by relevance** – Each `Openai.Responses.Item` is rendered to
   plain text (role prefix + content).  The text is scored by
   `Relevance_judge.is_relevant`.  Messages that do not reach
   `cfg.relevance_threshold` are dropped.

3. **Summarise** – The remaining messages are concatenated and fed into
   `Summarizer.summarise`, producing a concise recap of the
   conversation.  The summary is then truncated to
   `cfg.context_limit` characters.  In practice **1 character ≈ 1
   token** is a safe upper bound, making a separate tokenizer
   unnecessary.

4. **Build new history** – `Compactor` keeps the original first item and
   appends the summary wrapped in a `system` message.  If the input list
   is empty the function generates a synthetic «You are a helpful
   assistant.» prompt first.

5. **Exception safety** – Any unexpected exception causes the function
   to fall back to the identity transformation and return the original
   `history` unchanged.  The caller therefore never needs to guard with
   `try … with`.

---

## Public Interface

### `compact_history`

```ocaml
val compact_history :
  env:Eio_unix.Stdenv.base option ->
  history:Openai.Responses.Item.t list ->
  Openai.Responses.Item.t list
```

Compacts `history` as described above.

**Parameters**

* `env` – optional Eio standard environment that grants network access.
  Provide this when you want the summariser to call the OpenAI API.  Pass
  `None` in offline contexts; the pipeline switches to deterministic
  stubs.
* `history` – full conversation transcript to compact.

**Returns** a list of `Openai.Responses.Item.t` that is either one or two
elements long and always starts with the original first item.

**Never raises.**

---

## Usage Examples

### Basic compaction before an LLM call

```ocaml
open Context_compaction

let send_request ~env history fresh_user_msg =
  let compacted = Compactor.compact_history ~env:(Some env) ~history in
  let request_history = compacted @ [ fresh_user_msg ] in
  Openai.Client.chat_completion ~history:request_history
```

### Offline unit tests

```ocaml
let%expect_test "compaction keeps summary under limit" =
  let history = (* synthetic transcript … *) in
  let compacted = Compactor.compact_history ~env:None ~history in
  assert (List.length compacted <= 2);
  ();;
```

---

## Interaction with Other Modules

* **`Config.context_limit`** – upper-bounds the length of the generated
  summary.
* **`Config.relevance_threshold`** – influences which messages
  `Relevance_judge` keeps.
* `Summarizer` may perform costly LLM calls; keep an eye on rate limits
  and API quotas in production setups.

---

## Known Limitations

1. **Naïve token budgeting** – character count is a safe *upper* bound
   but can still exceed the true token limit by ~30 % for some scripts.
2. **First-message preservation** – Always keeping the very first item is
   a heuristic that works for typical system prompts but may be
   sub-optimal for exotic prompt styles.
3. **Lack of incremental summarisation** – The entire transcript is
   reprocessed on every compaction call.  Caching or incremental diffs
   could reduce latency.

---

## Change Log

* **v0.1** – Initial implementation: relevance filtering, LLM summary,
  exception-safe fallback.

