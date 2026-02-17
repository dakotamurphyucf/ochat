# `Chat_tui.Type_ahead_provider` — fetch a type-ahead completion suffix

`Chat_tui.Type_ahead_provider` is the “backend” for the TUI’s type-ahead feature.
Given the current draft buffer plus a cursor position, it asks the OpenAI API
for a short suffix that can be inserted at that cursor.

The module is designed for interactive use:

- **low latency:** small prompt, small output cap
- **single candidate:** only one completion is tracked at a time
- **no tools:** tool calls are disabled for this request
- **cancellable:** the request runs under an `Eio.Switch.t`

The higher-level wiring lives in `Chat_tui.App_reducer` (debounce + lifecycle
events), `Chat_tui.Controller` (accept/dismiss/preview keys), and the renderer
(inline “ghost” suffix + preview popup).

---

## API

```ocaml
val complete_suffix
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> cfg:Chat_response.Config.t
  -> history_items:Openai.Responses.Item.t list
  -> draft:string
  -> cursor:int
  -> string
```

### Parameters

- `sw`: cancellation switch. Failing the switch cancels the request.
- `env`: used to obtain the network stack via `Eio.Stdenv.net`.
- `dir`: working directory used by the OpenAI client (request context).
- `cfg`: base OpenAI configuration (model, temperature, max tokens).
- `history_items`: recent conversation context (best-effort summarised).
- `draft`: full editor buffer.
- `cursor`: cursor position as a byte index within `draft`.

### Return value

Returns the *suffix* to insert at `cursor`. The returned text is:

- capped to a small maximum length,
- stripped of accidental code fences, and
- sanitised with `Chat_tui.Util.sanitize ~strip:false` so it is safe to render
  and insert.

If the OpenAI credentials are missing (no `OPENAI_API_KEY`), returns the empty
string.

---

## How the cursor position is represented

The provider inserts a literal marker into the draft excerpt to indicate the
insertion point (e.g. `⟦CURSOR⟧`). The prompt instructs the model to return only
the text to insert *after* that marker, and to avoid repeating any content that
appears before it.

Because the excerpt is bounded (for latency), the provider may include an
ellipsis prefix/suffix (`…`) when it drops context far from the cursor.

---

## Limitations / notes

- Cursor offsets are treated as byte indices (consistent with the editor).
- The provider is heuristic: it uses only a small slice of the history and the
  local draft excerpt; it may return an empty string when unsure.
- The module does not perform relevance checks; consumers should validate that a
  result still applies to the current editor snapshot (generation + base input +
  base cursor). `Chat_tui.App_reducer` does this before storing a completion in
  the model.

