# `Chat_response.Config`

Helpers for reading the `<config/>` element in a ChatMarkdown prompt.

## Overview

`Config` collapses the repetitive task of *finding* the (optional) `<config/>`
element into a single reusable helper.  A ChatMarkdown document contains a
heterogeneous list of `top_level_elements`.  Only one of them – if any – is a
`Config` node carrying OpenAI generation parameters.  Most call-sites simply
want the first occurrence, or a sensible set of defaults when the element is
missing.

`Config.of_elements` does precisely that.  The record type returned is
`Prompt.Chat_markdown.config`, re-exported here as `t`.

## API

### `type t`

An alias to `Prompt.Chat_markdown.config`.

````ocaml
val max_tokens        : int option
val model             : string option
val reasoning_effort  : string option
val temperature       : float option
val show_tool_call    : bool
val id                : string option
````

### `default : t`

Configuration with every optional field unset and `show_tool_call = false`.
Use this value when no `<config/>` element is present.

### `of_elements : Prompt.Chat_markdown.top_level_elements list -> t`

Returns the first `<config/>` found in the list or `default` otherwise.

```ocaml
let elements = Prompt.Chat_markdown.parse_chat_inputs ~dir "/tmp" doc in
let cfg      = Config.of_elements elements in
match cfg.max_tokens with
| None   -> print_endline "model may pick its own token budget"
| Some n -> Printf.printf "caller capped answer to %d tokens\n" n
```

## Usage Example

```ocaml
(* Read a ChatMarkdown file and honour temperature setting. *)
let doc      = Io.load_doc ~dir:(Eio.Stdenv.fs env) "prompt.chatmd" in
let elements = Prompt.Chat_markdown.parse_chat_inputs ~dir:(Eio.Stdenv.fs env) doc in
let cfg      = Chat_response.Config.of_elements elements in
let temp     = Option.value cfg.temperature ~default:1.0 in
...
```

## Limitations

*The module intentionally keeps a very small footprint.  It does not validate
the semantic correctness of individual fields – that task is handled by the
OpenAI request layer.*

