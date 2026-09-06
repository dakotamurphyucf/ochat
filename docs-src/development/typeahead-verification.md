# Typeahead parity verification

Implementation: shared `Type_ahead_config`, `Type_ahead_controller`, and
`Type_ahead_ui`; bounded `Type_ahead_provider` and private Responses transport.
The [TUI guide](../guide/chat_tui.md#type-ahead-availability-and-privacy)
describes the maintained behavior, setup and privacy contract. The earlier
standalone implementation plan is not included in this checkout; the source,
tests and verification summary below remain available.

## Offline checks

```sh
dune runtest test/chat_tui_input_and_stream_scheduling
dune build @agent-e2e-typeahead
dune build @agent-docs-check
```

Fast tests use injected completion and clocks: defaults/ranges/key validation,
manual/auto/off, read-only/disconnect, raw XML/pages/startup, stale results,
debounce replacement, cancellation serialization, editor-only acceptance,
UTF-8/history/output limits, sanitized errors and a ten-second total deadline.

The opt-in PTY alias starts real legacy, native local, Unix-daemon and HTTP-daemon TUIs with
an isolated loopback Responses fixture and disposable filesystem roots. It
checks explicit request payload/model/no tools, no default history, no automatic
manual-mode requests, preview dismissal, Tab/Shift+Tab acceptance, unchanged
session history before submission, undo, no raw provider logs or persisted private
canaries, redacted provider errors, cancellation on quit, read-only observer
gating, and terminal restoration. Auto traces in all three modes replace an
in-flight request, discard its late response, and avoid requesting again merely
because a suggestion was accepted. CLI checks exercise config precedence,
`--no-config`, and validation before startup.
It is not part of normal `dune runtest` and never uses a real key or paid API.

## Recorded automated results (2026-09-06)

- Full `dune runtest` and the focused typeahead suite passed.
- `@agent-e2e-typeahead`: configuration plus seven manual/auto host/transport
  traces passed; the automatic-response manual fixture's PTY self-check passed.
- The delayed/long-response fixture follow-up passed the same opt-in alias.
  Its PTY self-check observed `[suggesting]`, verified the two-second delay,
  scrolled the fifteen-line preview to its end marker, accepted the completion,
  and verified clean exit and terminal restoration. This is not a Zed visual
  confirmation or a live-model quality result.
- `@agent-e2e-tui-auto`: all ten pre-existing traces passed, including streaming,
  approval/compaction/cancellation, reconnect, client-local presentation and
  local/Unix/HTTP terminal journeys.
- No paid requests were made. Fixtures use generated/isolated daemon credentials
  and a fake local provider key; owned temporary roots are cleaned up.
- Operator contracts/coverage inventory were refreshed. The original README's
  contents were preserved.
- Final combined `dune build @runtest @agent-e2e-tui-auto @agent-docs-check`
  passed, including the exact-256-KiB/one-byte-over response boundary test.
  Documentation validation covered 291 pages and 37 protocol methods.

## Human visual check — passed for the exercised Zed paths

Current Zed pass: the user confirmed local auto-mode request status and ghost
text, expanded-preview scrolling, line/all acceptance, and undo. Ctrl-R redo
failed. A regression using Notty's actual DC2 decoding reproduced the failure:
the binding accepted only lowercase `r`, while terminal input decodes to
uppercase Ctrl-R. The handler now accepts both cases and raw DC2, including
the separate Insert-mode raw-XML toggle. The resumed Zed redo check
passed after relaunch; automated PTY traces now exercise the actual Ctrl-R byte
as well. The user also confirmed terminal resizing and manual request/preview
behavior, but manual `[suggesting]` did not appear. An idle-editor PTY regression
reproduced a missing redraw after Ctrl+Space: the adapter changed status while
the controller returned `Unhandled`. Both host input loops now redraw on status
changes. The manual fixture self-check separates typing from Ctrl+Space, and
the cross-mode traces require `[suggesting]` before releasing a response. The
resumed Zed manual-status check passed after relaunch.

The user then confirmed the Unix- and HTTP-daemon manual status/preview,
line/all acceptance and undo/redo checks; cancellation without a late suggestion;
and simultaneous foreground `Thinking` and typeahead `[suggesting]` indicators.
An HTTP relay cut cleared the suggestion while preserving the draft and showing
reconnecting. Resuming restored connected status without another suggestion;
the fixture observer saw no new provider request after request 26. After quit,
`echo terminal-ok` printed normally and `stty size` reported 25 rows × 156 columns.
The relay and fixture processes exited cleanly and removed their disposable roots.
The current Zed version was not supplied. Legacy mode has automated PTY coverage,
not a separate human visual sign-off in this pass. No paid API calls were made.

Headless PTY success is not evidence of visual correctness in Zed. Arrange a
brief user-assisted session after automated checks pass. Use an isolated fake
provider, not a real provider credential. Check ghost text and popup layout,
Ctrl+Space preview, Tab/Shift+Tab, Escape stages, undo, multiline/wide text,
disconnect/reconnect draft preservation without transmission, and clean quit.
Record Zed version, terminal size, modes/transports checked and any corrections.

Run the existing isolated manual fixture in one terminal:

```sh
dune build bin/chat_tui.exe bin/ochat_agent_server.exe
dune exec test/agent_server_e2e/agent_server_e2e.exe -- --scenario tui-manual --case typeahead
```

It prints temporary launcher paths. In a second Zed terminal, invoke the printed
local or Unix/HTTP launcher with `--typeahead manual` (or `--typeahead auto`).
The launcher supplies disposable home/cache/data roots, a fake key, and a loopback
provider endpoint. Do not replace them with real credentials. Type a draft and
press Ctrl+Space; this fixture automatically returns deterministic multiline/wide
text after a two-second delay per request, before the ten-second request deadline.
Each response has fifteen lines, including `界` and `café`, numbered preview lines,
and an `END-OF-SUGGESTION request N` marker. Each request is delayed independently
so rapid cancellations do not queue cumulative delays. Do not issue operator
`suggest N` commands for requests already handled by this automatic driver.
Type `quit` in the fixture terminal only after
closing the test TUIs; owned files and services are cleaned up.

### Assisted fixture checklist

1. Record Zed version and terminal dimensions. Start with the local launcher and
   `--typeahead auto`. Type a synthetic draft, pause, and confirm `[suggesting]`
   remains visible during the response delay, then becomes ghost text with a
   hidden-lines indicator. The draft itself must remain unchanged.
2. Use Ctrl+Space to open the preview. Check the first lines and wide characters.
   Scroll with Ctrl+Shift+Up/Down and Page Up/Down; Ctrl+Shift+Right jumps to the
   end marker, and Ctrl+Shift+Left returns to the top. Resize the terminal and
   ensure the input remains usable and the preview fits the available space.
3. Shift+Tab accepts one logical line (including its newline) and retains the
   rest of the suggestion. Repeat, then Tab accepts all remaining lines. Neither
   action should itself start another suggestion request or submit a message.
4. Escape until Normal mode, then use `u` to undo the last acceptance and Ctrl+R
   to redo it. Check both draft text and cursor placement. Separate line
   acceptances should have separate undo steps.
5. Return to Insert mode and edit during `[suggesting]`. Check that the old
   request's late result does not overwrite the new suggestion. The operator
   request indices and response end markers distinguish successive requests.
6. Restart with `--typeahead manual`: typing alone must make no suggestion
   request; Ctrl+Space requests one and shows `[suggesting]` during the delay.
   Check preview dismissal and successive Escape behavior.
7. Repeat the essential status/preview/acceptance/undo checks with the Unix and
   HTTP daemon launchers. While a foreground fixture response is held, request
   typeahead and check that `Thinking` and `[suggesting]` are independent.
   Check disconnect/reconnect draft preservation and no unsolicited request
   on reconnect. Quit all TUIs and verify the shell can print `terminal-ok`.
8. Record confirmations, screenshots for any defects, and fixes/retests. Complete
   this fixture check before proceeding to the live-provider check below.

Normal-mode `u` / Ctrl+r now call the existing model undo/redo functions; these
keys were previously documented but unwired. Suggestion acceptance adds the usual
undo snapshots. The original README remains untouched.

These confirmations cover the exercised fixture paths, not live-model quality.

## Live-provider check — completed, bounded manual smoke check

After the fixture pass, the user ran the separate tool-free local launcher with
`gpt-5.6-luna`, `API_URL=api.openai.com`, manual typeahead, zero history messages,
and a 200-output-token cap. The existing terminal credential was inherited, not
printed or written into the launcher. Three live results were explicitly reported:

- Partial-word completion inserted only the missing suffix; reported latency
  was approximately one second.
- A coding-review instruction received a useful continuation, with similarly
  quick latency.
- Mid-draft insertion preserved existing trailing text without duplication.

A fourth, multiline exercise was proposed; the user concluded that the feature
was working as expected and chose to stop. No detailed fourth result or exact
total request count was collected, so this record does not claim a separate
live multiline or auto-mode pass. Those mechanics have deterministic fixture
coverage. No live failures were reported. Actual token usage and billed cost
were not collected; the run was guided by the existing $15 ceiling and at most
five initial manual requests, not an application-enforced dollar cap.

The live smoke check is accepted for the exercised behaviors, not a comprehensive
model-quality evaluation. No further live requests are planned. After the user
confirmed the live TUI was closed, a process check found no remaining test TUI
using the temporary workspace. The live launcher, synthetic prompt, and isolated
config/cache/data/temporary directories were removed; absence of the test root
was verified. Repository documentation and the user's normal sessions were retained.

### Procedure for a future repeat

This is a separate, explicitly paid/manual exercise, never part of a Dune test
alias. Its purpose is to check real response latency and completion quality;
the deterministic fixture cannot establish either. Do not start it until the
assisted fixture checklist passes.

1. Prepare a separate disposable local TUI session with a minimal, tool-free
   ChatMD prompt and synthetic draft text. Keep history inclusion at zero. Do
   not reuse the fixture launchers: they deliberately set a fake provider key
   and loopback endpoint. Do not send repository text, secrets, or real chat
   history for this check.
2. Use the existing local `OPENAI_API_KEY` without printing or recording its
   value. Explicitly set `API_URL=api.openai.com` for the TUI process to bypass
   the shell's proxy override. Use `--typeahead-model gpt-5.6-luna`, initially
   `--typeahead manual`, and the default 200 output-token cap.
3. Agree on the live-run spending ceiling and verify model pricing/access
   before starting. Bound the initial run to at most five manual requests;
   output-token and request-count bounds alone are not a dollar-cost guarantee.
   Do not silently switch models or retry repeatedly on failure.
4. Try a partial word (`mary had a li`), a short coding-task instruction, an
   insertion before existing trailing text, and a naturally multiline draft.
   Judge insertion-only output, lack of repeated surrounding text, usefulness,
   latency/status behavior, preview, line/all acceptance, and undo/redo. The
   nursery-rhyme example need not produce an exact string in a live response.
5. If those checks pass and the spending allowance permits it, perform one
   short auto-mode check with a deliberate pause and one cancellation. Then
   turn typeahead off or quit to prevent unintended additional requests. Do
   not submit drafts as foreground agent requests during this exercise.
6. Record model, endpoint, mode, request count, observed latency/errors,
   available usage/cost information, and the user's quality assessment without
   credentials or private prompt/response logs. Mark live behavior separately
   from fixture correctness; report inaccessible models, timeouts, and unmet
   quality expectations rather than treating them as a passing result.
