# `Preprocessor` – ChatMarkdown meta-refinement hook

> Library: `ochat.meta_prompting`

The **Preprocessor** module offers a single helper – `Preprocessor.preprocess` –
that can be inserted just before a prompt is parsed as ChatMarkdown / ChatML.
If *enabled* it executes the *Recursive Meta-Prompting* refinement loop on the
raw prompt and returns the improved version; otherwise it returns the input
unchanged.  The transformation preserves ChatMarkdown validity by encoding the
extra information as HTML comments so that downstream parsers remain oblivious
to it.

---

## When is the pre-processor active?

The hook is **opt-in** and is triggered by *either* of the following:

1. The environment variable `OCHAT_META_REFINE` is set to a truthy value.  The
   following (case-insensitive) spellings are accepted: `1`, `true`, `yes`,
   `on`.
2. The sentinel HTML comment `<!-- META_REFINE -->` is present anywhere in the
   prompt.

Both checks are inexpensive (simple string comparisons) and therefore safe to
run on every prompt.

---

## API

### `preprocess : string -> string`

``ocaml
val preprocess : string -> string
``

• **Input**  – the raw UTF-8 prompt text.
• **Output** – the refined prompt if the pre-processor is enabled, otherwise
  verbatim copy of the input.

Internally the function constructs a minimal [`Prompt_intf.t`] record from the
input, runs [`Recursive_mp.refine`] on it, and serialises the result back to a
plain string.

Because all metadata is inserted as HTML comments the returned value is still
valid ChatMarkdown and can be fed straight into the normal parser (which is
exactly what `Chatmd.Prompt.parse_chat_inputs` does).

---

## Usage example

```ocaml
open Ochatchatmd (* fictional namespace *)

let raw = """
<!-- META_REFINE -->
<user>Summarise this article in two sentences.</user>
""" in

let refined = Preprocessor.preprocess raw in
Printf.printf "Refined prompt:\n%s\n" refined
```

In most situations you do **not** need to call the function explicitly – the
documentation of `Chatmd.Prompt.parse_chat_inputs` guarantees that it is always
invoked.

---

## Implementation notes

* **Performance** – when disabled the function performs only two lightweight
  checks (`Sys.getenv` and `String.is_substring`), after which it returns the
  input unchanged.
* **Thread-safety** – the helper is purely functional and side-effect-free; it
  can be used concurrently without additional synchronisation.

---

## Limitations & future work

1. The current *Recursive Meta-Prompting* strategy is intentionally simple – it
   mostly appends metadata indicating the iteration.  Replacing it with a more
   sophisticated LLM-based refinement agent requires no changes to the public
   API.
2. The function operates synchronously; prompts that trigger network calls in
   the refinement loop will block the caller.  An asynchronous variant might
   be added in the future if needed.

---

_Last updated: {{DATE}}_

