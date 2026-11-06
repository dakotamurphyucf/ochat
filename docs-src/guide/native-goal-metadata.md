# Selected native Goal metadata and controls

Select only the native tools that the authored workflow needs:

```xml
<tool name="goal_get"/>
<tool name="goal_create"/>
<tool name="goal_objective"/>
<tool name="goal_progress"/>
<tool name="goal_hold"/>
<tool name="goal_abandon"/>
```

These declarations select existing host capabilities. They grant no permission,
source identity, runtime lease or budget. The host checks the actual invocation,
selected binding, original principal, current session/generation and source.
Get requires transcript-view authority; mutations require send authority and an
actual supported callback transaction. Ordinary legacy and extensibility-v1
moderator callbacks use that same transaction. A context without a genuine
callback owner cannot stage a mutation.

## Read the actual basis

Call `goal_get` with `{"goal_id":null}` for all retained Goals, or with an actual
Goal ID string to select one. The `goal_id` field is required, including for the
all-Goals request. The result has `session`, `generation`, `catalog_revision`,
`source` and `goals`. Each row contains the canonical `goal` and either its
canonical `accounting` projection or null when no account exists.

Generation and revision fields at the top level are canonical nonnegative decimal
strings. `source` is the actual Run source object, whose own generation remains
a nonnegative JSON integer. Copy the complete source unchanged. A null source
cannot be replaced with an invented source to create or edit a Goal. These
values are fences and observations; they do not confer permission.

Accounting preserves admitted turn/attempt totals, untracked totals and the
canonical usage representation. Missing or unknown usage is not zero. The Goal
contains its actual budget, objective, progress, state and episode attribution.
Get returns at most the bounded sixteen-entry catalog, not the raw Session state,
provider records, runtime handles or command receipt history.

## Stage a held Goal

A Create request carries exactly these fields. Substitute the observed values
rather than using the illustrative source below as authority:

```json
{
  "generation": "0",
  "source": {
    "script_id": "controller",
    "source_sha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "generation": 0,
    "installation_epoch": "1"
  },
  "goal_id": "goal_release_notes",
  "expected_catalog_revision": "0",
  "key": "create-release-notes-v1",
  "objective": "Prepare release notes with references to the accepted changes.",
  "budget": {
    "admitted_turns": null,
    "admitted_attempts": null,
    "observed_tokens": null,
    "deadline": null
  }
}
```

A budget counter is a canonical decimal string or null. `observed_tokens` is null
or an object with `component` and decimal-string `ceiling`. Supported components
are `input`, `output`, `reported_total`, `cached_input`, `cache_write_input` and
`reasoning_output`. A deadline is null or the canonical timestamp accepted by the
Goal budget codec. Each budget field is required, even when null.

Create stages a new Goal held for source review with its actual creator/source
and stable account. It does not enable execution or create a Workflow Run.
Changing objective or progress preserves that Goal and its accumulated account.
These six controls do not supply episode Start or Resume. Achievement uses the
separately selected checked orchestration `Goal.finish` action and its actual
owned-work settlement rules.

## Edit or control the observed Goal

`goal_objective` and `goal_progress` take `generation`, the full observed `source`,
`goal_id`, `expected_catalog_revision`, `expected_goal_revision`, `key`, `kind`
and `value`. Use kind `objective` with `goal_objective`, or `progress` with
`goal_progress`; value is the validated text. Both expected revisions are decimal
strings copied from Get.

`goal_hold` and `goal_abandon` take `generation`, `goal_id`, both expected revision
fields, `key`, `kind` and `reason`. They have no source field: the host still
checks actual source/original authority independently. Use kind `hold` or
`abandon` matching the selected tool. Hold records the public needs-input reason;
it does not expose internal source-review or budget-hold constructors. Abandon
records the explicit abandonment reason. Neither control fabricates a terminal
Run result or resets admitted accounting.

For example, within an actual callback:

```chatml
let* result = Tool.call("goal_get", `Object([{key = "goal_id"; value = `Null}])) in
match result with
| `Ok(view) -> Task.pure(view)
| `Error(message) -> Task.fail(message)
```

Every successful mutation result is `{"staged":true,"effect":"goal_hold"}`
with the matching selected tool name. It reports local staging, not a durable
command ACK. Subsequent Get calls in that same transaction see its validated
projection. Only the actual callback commit accepts the command receipts.
`Task.catch` rollback removes speculative mutations and their receipts. An
uncaught callback failure does not publish those native changes; its genuine
callback/Run failure handling remains separate.

You may create a goal, edit its metadata and abandon it within one callback.
The committed result retains the goal, its command receipts and its initial
zero-usage account; no execution episode is created. OChat validates the actual
creation step against the ledger at the start of the transaction. Later metadata
changes do not reset that baseline or erase existing accounting.

## Preserve retry identity and current authority

Keep the exact key and immutable original request when retrying. Do not silently
refresh generation, source or CAS under that key. A retained exact command is
DATA and does not acquire execution permission. A changed request under the same
key conflicts. A new intended edit must use a new key and an actual current Get.

The native token is checked during the selected implementation. Final completion
and callback commit use the actual transferred command custody with the original
principal and current host policy; the lexical token is not extended past the
implementation. Source replacement, revocation or a changed callback owner still
denies the commit. A selected control cannot combine a fresh Hold/Abandon with
contradictory same-Goal Continue or request-turn scheduling in one checkpoint.
