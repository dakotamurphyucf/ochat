# `Chat_tui.Conversation` – Bridging OpenAI responses and the TUI

`Chat_tui.Conversation` is a *pure* utility that turns the rich, typed
OpenAI response AST produced by the [`openai`](https://github.com/olinicola/ocaml-openai)
library into the simple `(role * text)` tuples expected by
`Chat_tui.Renderer`.

It performs no I/O and keeps no mutable state – you can safely call it
from the model, the renderer, unit-tests or background workers without
fear of side-effects.

---

## Table of contents

1. [Why does this exist?](#why-does-this-exist)
2. [`pair_of_item`](#pair_of_item)
3. [`of_history`](#of_history)
4. [Known limitations](#known-limitations)

---

### 1&nbsp;·&nbsp;Why does this exist? <a id="why-does-this-exist"></a>

The OpenAI chat API returns a *heterogeneous* list of variants – user
input, assistant output, function calls, tool invocation results, and so
on.  Most of them contain plain text somewhere inside the nested record
structure, but the *location* and *format* vary greatly.

`Chat_tui.Conversation` hides this complexity behind two functions that
extract the textual content, sanitize it, and normalise the role names
so the renderer can treat every entry uniformly.

Safety is a primary concern: the conversion strips escape sequences and
truncates unreasonably large tool output so the terminal UI cannot be
corrupted or locked up by malicious / buggy data.

---

### 2&nbsp;·&nbsp;`pair_of_item` <a id="pair_of_item"></a>

```ocaml
val pair_of_item : Openai.Responses.Item.t -> Types.message option
```

Takes a single OpenAI response item and returns an optional
`(role, content)` tuple ready for rendering.

Behaviour summary:

* **Input / assistant messages** – concatenates all text parts and keeps
  newlines.
* **Function/tool calls** – formats the invocation as
  `name(arguments)` so the user can see what exactly was executed.
* **Tool output** – returns the JSON result up to 2 000 bytes; excess
  data is replaced by `…truncated…` to keep the UI responsive.
* **Reasoning summaries** – joins the partial strings emitted by the
  model into a single paragraph.
* **Non-textual items** – returns `None`, effectively filtering the
  entry out.

All text passes through `Chat_tui.Util.sanitize ~strip:true` so ASCII
control characters become harmless spaces.

Example – render the **assistant** response "*Hello*":

```ocaml
let open Openai.Responses in
let item = Item.Output_message
             { role = Assistant
             ; content = [ { text = "Hello" } ]
             } in

match Chat_tui.Conversation.pair_of_item item with
| Some (role, txt) -> assert (role = "assistant" && txt = "Hello")
| None -> assert false
```

---

### 3&nbsp;·&nbsp;`of_history` <a id="of_history"></a>

```ocaml
val of_history : Openai.Responses.Item.t list -> Types.message list
```

Maps `pair_of_item` over a complete OpenAI response, dropping every
entry without a textual representation.  The relative order is
preserved, therefore the indices of the resulting list match the one
returned by the API.

Typical usage inside the model layer:

```ocaml
let messages = Chat_tui.Conversation.of_history response.output in
Model.{ model with messages }
```

---

### 4&nbsp;·&nbsp;Known limitations <a id="known-limitations"></a>

1. **Display width** – The module deals purely with bytes and
   code-points; East-Asian wide glyphs still count as one unit.  The
   renderer (Notty) takes care of visual width.
2. **Arbitrary truncation limit** – 2 000 bytes for tool output is a
   pragmatic value chosen during manual testing.  Feel free to adjust
   if your workflow regularly produces larger payloads.


