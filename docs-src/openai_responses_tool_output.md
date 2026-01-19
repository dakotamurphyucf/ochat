# Responses API: tool output supports images

This repository integrates OpenAI’s **Responses API** via the OCaml module
`Openai.Responses`.

OpenAI’s schema for a tool output item (`"type": "function_call_output"`) allows
the `output` field to be either:

1. a JSON string (legacy/simple tool output), or
2. a JSON array of *content parts* which can include images.

This note documents how `ochat` models and handles that union.

## Wire schema overview

### String output

```json
{
  "type": "function_call_output",
  "call_id": "call-123",
  "output": "plain text"
}
```

### Structured output (array of parts)

```json
{
  "type": "function_call_output",
  "call_id": "call-123",
  "output": [
    { "type": "input_text",  "text": "hello" },
    { "type": "input_image", "image_url": "https://…/a.png" },
    { "type": "input_image", "image_url": "https://…/b.png", "detail": "high" }
  ]
}
```

Notes:

- In tool outputs, OpenAI allows `detail` to be absent; `ochat` decodes it as an
  `option`.
- Some payloads represent `image_url` as `{ "url": "…" }`; decoding tolerates
  both the string and object form.

## OCaml types

The union is represented by `Openai.Responses.Function_call_output.Output.t`:

```ocaml
type Output.t =
  | Text of string
  | Content of Output_part.t list
```

Parts are represented by `Openai.Responses.Function_call_output.Output_part.t`:

```ocaml
type Output_part.t =
  | Input_text of { text : string }
  | Input_image of { image_url : string; detail : Input_message.image_detail option }
```

## JSON decoding/encoding

Decoding accepts both schema branches:

- `"output": "…"` → `Output.Text "…"`
- `"output": [ … ]` → `Output.Content [ … ]`

Encoding preserves the branch:

- `Output.Text s` encodes as a JSON string.
- `Output.Content parts` encodes as a JSON array of part objects.

This is intentionally *round-trippable*: decoding a response and re-encoding it
does not collapse arrays into strings.

## Rendering vs persistence

Many UI/prompt surfaces are string-based. Two helper functions exist to avoid
duplicating policy:

- `Output.to_display_string` flattens to text for humans.
  - `Text s` renders as `s`.
  - `Content parts` renders as newline-separated text, with images rendered as
    a lightweight marker like `<image src="…" detail="…"/>`.

- `Output.to_persisted_string` is used for writing tool outputs to disk.
  - `Text s` persists as `s` (preserves existing UX and diffs).
  - `Content _` persists as JSON (so a later load can recover images).

## ChatMarkdown conversion

ChatMarkdown `<tool_response>` blocks can carry either plain text or structured
items.

`Chat_response.Converter` emits:

- `Output.Text` when the response is textual.
- `Output.Content` when the `<tool_response>` contains image items.

When loading from ChatMarkdown, if a textual `<tool_response>` looks like it
contains a JSON array, the converter attempts to parse it back into
`Output.Content` to recover persisted multimodal outputs.

## Tests

The file `test/openai_responses_function_call_output_output_test.ml` exercises:

- decoding/encoding for string outputs
- decoding/encoding for array outputs including images and optional `detail`

