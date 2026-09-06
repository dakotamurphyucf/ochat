# Chat_tui.Type_ahead_provider: private draft suggestions

[User setup and privacy](../../guide/chat_tui.md#type-ahead-availability-and-privacy).

## Shared lifecycle

All hosts use `Type_ahead_ui` on the UI owner and `Type_ahead_controller` for
serialized background work. The legacy reducer carries `Typeahead` events;
native local and daemon sessions use the same adapter in `App.Agent_mode`.

The coordinator owns one active switch and one replaceable pending snapshot.
Automatic debounce emits `Ready`, allowing the UI to recheck eligibility before
admission. Cancellation unwinds the previous request before replacement. Quit
cancels and joins work. Neither workers nor the provider mutate `Model.t`.

Both TUI input loops request a redraw when the adapter changes the typeahead
status, even if the editor controller returns `Unhandled`. This makes manual
Ctrl+Space show `[suggesting]` from an idle editor without relying on a pending
typing redraw. Automatic admission and completion events also request redraws.

Snapshots include attachment/session identity, context epoch, editor generation,
base draft and byte cursor. Results are accepted only while all still match.
Canonical history changes are conservatively invalidating; streaming text alone
does not schedule requests. Read-only/off/disconnected paths make no requests.

## Provider API

`prepare config ~messages ~draft ~cursor` consumes only already-visible role/text
pairs. It windows the draft around a UTF-8 boundary-clamped cursor to 8192 bytes
(excluding markers). History is off by default; opting in selects up to three
newest visible user/assistant/developer/system texts, budgets newest-first with
a combined 16384-byte limit, then emits chronological context. Tool outputs,
tool arguments, reasoning, image payloads and unprojected history are excluded.

`complete_suffix ~sw ~env ~config input` returns `Ok text` or a sanitized
`Unavailable` / `Timeout` error. It sends developer instructions and plain
user context with no tools. The validated model defaults to `gpt-5.6-luna`;
reasoning and verbosity are low. The independent output cap defaults to 200.
Matching outer fences and marker artifacts are removed; invalid UTF-8 and
terminal controls are sanitized; insertions are capped at 4096 bytes.

The developer instruction includes the original partial-word example:
`mary had a li⟦INSERT⟧` should insert `ttle lamb`, not `little lamb`.
It treats context as data and requests short, insertion-only text without
Markdown fences. The user message encloses the bounded history in
`<<<|completion-context-start|>>>` / `<<<|completion-context-end|>>>` and the
bounded draft in `<<<|draft-buffer-start|>>>` / `<<<|draft-buffer-end|>>>`.
These labels do not opt in to history or expand either input budget.

`complete_with` injects the request callback and Eio clock for offline tests.
The total request deadline is ten seconds; cancellation propagates. There is
no fallback model or automatic retry.

## No-log transport

`Openai.Responses.post_private_response_exn` uses the existing local
`OPENAI_API_KEY` and `API_URL` endpoint configuration, without a directory or
logging callback. Its bounded reader rejects bodies over 256 KiB before JSON
parsing. Raw errors are discarded by the provider; status messages never include
draft, context, output, credentials, headers or exception strings.
Existing `post_response` callers keep their logging policy.

This is local editor assistance, not a session operation. Enabling it authorizes
transmission of unsent text and additional provider charges; token bounds are
not a spending cap.

Sources: [config](../../../lib/chat_tui/type_ahead_config.mli),
[coordinator](../../../lib/chat_tui/type_ahead_controller.mli),
[UI adapter](../../../lib/chat_tui/type_ahead_ui.mli),
[provider](../../../lib/chat_tui/type_ahead_provider.mli).
