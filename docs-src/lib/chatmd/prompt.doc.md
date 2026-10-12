# ChatMarkdown prompt parsing

`Prompt.Chat_markdown` converts ChatMD into typed messages, configuration and
runtime declarations. Parsing does not initialize ChatML or authorize tools.
Source loading and optional authored preprocessing remain explicit host work.

## Ordinary parsing

`parse_chat_inputs` retains the existing authored parsing interface and default
meta preprocessing. `parse_chat_inputs_without_preprocessing` accepts a supplied
source loader and skips that transformation. Both use the same lexer, grammar,
import expansion and declaration conversion as the owned preparation path.

Imports resolve relative to their declaring source. Expansion rejects active
import cycles, excessive import nesting and duplicate namespace aliases. Unknown
markup inside recognized elements remains literal text; it does not add a new
language tag. See the [ChatMD reference](../../overview/chatmd-language.md) for the
supported vocabulary and the contexts that expand imports.

## Exact inputs and declaration ownership

Hosts that need to retain code use `prepare_chat_inputs` with explicit limits,
checkpoint, loader and preprocessing callback. The callback runs once for the
root; imports use their raw bytes. The returned immutable `Parsed.t` contains:

- Converted elements and the exact root parser input.
- Root and imported input ownership, including authored/effective bytes and hashes.
- Script declarations with their actual qualified IDs, extracted bodies and
  parser-produced ranges.

A declaration range covers its tag, including the closing tag where present.
Offsets are half-open byte offsets in the effective parser input. When a
preprocessor changes the root, those offsets do not describe the original file.
An external script body belongs to its loaded source; its declaration range still
identifies the ChatMD tag that selected it. Source-reference lines and byte columns
are one-based; CRLF counts as one line break. UTF-8 columns count bytes.

The ordinary `Script` value retains an optional source reference. Parser-produced
values carry their source; manually constructed values may have none. The owned
`Declaration` API provides the exact range and input association.

## Captured replay and bounds

`replay_chat_inputs` accepts a retained root input and a captured source loader.
It verifies the authored root bytes and parses the retained effective bytes.
Imports, scripts and schemas resolve through captured input; missing captures
fail. Replay has no preprocessing callback and cannot fall back to live files.
This is authored replay, distinct from generated definitions, whose validation
continues to reject executable meta preprocessing.

One owned invocation bounds each input, total parse calls and tokens, and both
per-file and expanded markup depth. Imports share its counters. Referenced
script/schema reads preserve any tighter loader bound. Checkpoint exceptions and
cancellation propagate, including cancellation requested by the final callback.

`Prompt_revision_builder.capture` prepares each distinct queued root once and
retains an immutable authored inventory plus parser results. Its joint limit is
256 authored files and 8 MiB of authored bytes plus distinct transformed effective
payloads. Fresh publication replays the retained root against the installed
captured tree. Persisting effective-input associations across process restart is
a separate artifact-custody contract; a `Parsed.t` alone is not durable storage.

## Interfaces and evidence

[Prompt interface](../../../lib/chatmd/prompt.mli),
[input and range ownership](../../../lib/chatmd/chatmd_parser_input.mli),
[capture owner](../../../lib/agent_session/prompt_revision_builder.mli).
The compact [owned-input tests](../../../test/chatmd_owned_input/) exercise exact
byte ranges, preprocessing/replay, aggregate limits and interruption invariants.
