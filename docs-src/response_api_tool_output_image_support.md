## Plan: add image support for Responses API tool output (`function_call_output.output`)

### Goal

Update the OCaml codebase to correctly **decode, encode, and process** OpenAI Responses API tool outputs where the `output` field is **either**:

1) a JSON string, **or**
2) a JSON array of content parts (`input_text` / `input_image`).

This corresponds to the OpenAI schema where a “tool call output” item (in Responses API terminology: a `function_call_output` output item) may have:

- `output: string`, or
- `output: [{type:"input_text", text:...} | {type:"input_image", image_url:..., detail?:...}, ...]`

### Current repository state (what exists today)

The Responses endpoint is implemented in:

- `lib/openai/responses.ml`
- `lib/openai/responses.mli`

Tool calls are represented as:

- `Openai.Responses.Item.Function_call` (JSON `"type": "function_call"`)
- `Openai.Responses.Item.Function_call_output` (JSON `"type": "function_call_output"`)

Today, `Openai.Responses.Function_call_output.t` is a record with:

- `output : string`

and `[@@deriving jsonaf]` is used for encoding/decoding the record. Because of this, decoding will fail if `output` is an array.

Tool output values (`fco.output`) are treated as plain strings across the app:

- persisted to disk (`chat_tui/persistence.ml`)
- shown in the TUI (`chat_tui/conversation.ml`)
- compacted/redacted (`chat_response/compact_history.ml`, `context_compaction/*`)
- converted to/from ChatMarkdown (`chat_response/converter.ml`, `chat_response/driver.ml`)

### Proposed design (types + behavior)

#### 1) Represent tool output as a union

Replace `output : string` with a type that can represent both schema branches:

- `Text of string` (legacy / simple output)
- `Content of part list` (structured output supporting images)

Where `part` is:

- `input_text` with `{ text : string }`
- `input_image` with `{ image_url : string; detail : [high|low|auto] option }`

Notes:

- The Responses `Input_message` schema already has `input_text`/`input_image` constructors, but `Input_message.image_input.detail` is currently *required* in this repo. The tool-output schema allows `detail` to be absent. This means either:
  - implement a dedicated `Tool_output_part` type with `detail : detail option`, or
  - reuse `Input_message.content_item` but make its image `detail` decoding tolerant and/or default to `auto`.

Recommendation: **dedicated type** for tool outputs to avoid changing input-message semantics globally.

#### 2) Implement custom Jsonaf codec for the `output` field

Because the schema is `oneOf` (string or array), derive-based decoding won’t be sufficient.

Implement manual JSON conversion for:

- `Function_call_output.t_of_jsonaf`
- `Function_call_output.jsonaf_of_t`

Behavior:

- if `output` is a JSON string => `Text s`
- if `output` is a JSON array => decode each element by its `type`:
  - `input_text` => part `Input_text { text }`
  - `input_image` => part `Input_image { image_url; detail = Some/None }`

Encoding should preserve the branch:

- `Text s` => JSON string
- `Content parts` => JSON array of objects

#### 3) Provide shared render/persist helpers

Many modules expect a string today. Introduce a single rendering function to minimize churn:

- `Openai.Responses.Function_call_output.Output.to_display_string : Output.t -> string`

Suggested formatting:

- `Text s` => `s`
- `Content parts` => concatenate text; render images as placeholders (e.g. `<image src="..." detail="..."/>`).

For persistence, prefer **round-trippable** behavior:

- if `Text s` => persist `s` unchanged (preserves existing UX)
- if `Content parts` => persist the JSON array (`Jsonaf.to_string (Output.jsonaf_of_t ...)`) so a later load can recover images.

### Impacted files / audit checklist

#### Core API types + codecs

- `lib/openai/responses.mli`
- `lib/openai/responses.ml`

#### Tool-output constructors (wrap strings; produce Content when available)

- `lib/chat_response/response_loop.ml`
- `lib/chat_response/converter.ml`
- `lib/chat_response/driver.ml`
- `lib/chat_response/fork.ml`
- `lib/chat_tui/app.ml`

#### Tool-output consumers (render/redact/persist)

- `lib/chat_tui/conversation.ml`
- `lib/chat_tui/persistence.ml`
- `lib/chat_tui/stream.ml`
- `lib/chat_response/compact_history.ml`
- `lib/context_compaction/compactor.ml`
- `lib/context_compaction/summarizer.ml`

#### Tests

- `test/chat_tui_tool_metadata_test.ml`
- `test/chat_tui_parallel_tool_calls_test.ml`
- add a new test focused on JSON decoding/encoding of `function_call_output.output` for both schema branches.

### Implementation steps (high-level)

1) Add new tool-output output type and parts type.
2) Replace `Function_call_output.output : string` with the new union type.
3) Implement manual Jsonaf codec for `Function_call_output` (or at least custom parse/emit for `output`).
4) Propagate compilation fixes:
   - wrap existing `output = <string>` as `output = Text <string>`
   - update consumers to use `to_display_string` or persistence helpers.
5) Teach ChatMarkdown conversion:
   - `<tool_response>` with structured items => produce `Content` parts (including images)
   - `<tool_response>` with text => `Text` (optionally: parse JSON array string to recover structured output).
6) Update compaction/redaction to handle both branches.
7) Update persistence to be round-trippable for structured outputs.
8) Update and extend tests; run `dune build` + `dune runtest`.
9) Update docs (`document` for touched modules, at least `openai/responses`).

---

## Actionable TODO list

| # | Task | Files (primary) | Depends on | Output/Acceptance criteria | Status |
|---:|------|------------------|------------|----------------------------|--------|
| 1 | Add OCaml types for tool output union + parts (`Text` vs `Content`) | `lib/openai/responses.mli`, `lib/openai/responses.ml` | — | New types compile; no behavior changes yet | ☐ |
| 2 | Implement Jsonaf decode/encode for `output` accepting string or array | `lib/openai/responses.ml` | 1 | Can decode/encode both branches; array supports `input_text` and `input_image` with optional `detail` | ☐ |
| 3 | Update `Function_call_output.t` to use new `output` type | `lib/openai/responses.mli`, `lib/openai/responses.ml` | 2 | Project compiles after subsequent call-site updates | ☐ |
| 4 | Update all constructors of tool outputs to wrap strings (`Text`) | `lib/chat_response/*`, `lib/chat_tui/app.ml`, tests | 3 | No remaining record literals assigning `output` a raw string | ☐ |
| 5 | Add shared rendering helper (`to_display_string`) and update consumers | `lib/openai/responses.ml` + consumers | 3 | UI/serialization/persistence uses helper; no type errors | ☐ |
| 6 | Update persistence to round-trip structured outputs (JSON array on disk) | `lib/chat_tui/persistence.ml` | 5 | Structured outputs persisted without losing images; legacy text unchanged | ☐ |
| 7 | Update ChatMarkdown converter to emit `Content` for `<tool_response>` with items | `lib/chat_response/converter.ml` | 3,5 | `<tool_response>` can carry images without flattening to text | ☐ |
| 8 | Update compaction/redaction logic for union output | `lib/chat_response/compact_history.ml`, `lib/context_compaction/*` | 3,5 | Redaction doesn’t crash and preserves structure appropriately | ☐ |
| 9 | Add/adjust tests for decoding/encoding + tool-output lifecycle with images | `test/*` (new + existing) | 2–8 | `dune runtest` passes; new tests cover array `output` with image | ☐ |
| 10 | Run `dune build` and `dune runtest` | — | 1–9 | Clean build + tests | ☐ |
| 11 | Update docs for Responses module and any touched public APIs | `lib/openai/responses.mli`, `docs-src/*` | 10 | Documentation matches new type + behavior | ☐ |

