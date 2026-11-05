# Recurring automation from an agent

A recurrence belongs to the session that creates it. Select the controls in that
session's ChatMD prompt, read its current metadata, create a paused definition,
and explicitly enable it when it is ready. A configured daemon or embedded
execution host runs eligible work through the ordinary session and Run owners.
A connected TUI is not the scheduler.

```xml
<tool name="recurrence_list"/>
<tool name="recurrence_get"/>
<tool name="recurrence_create"/>
<tool name="recurrence_edit"/>
<tool name="recurrence_resume"/>
<tool name="recurrence_pause"/>
<tool name="recurrence_cancel"/>
<tool name="recurrence_status"/>
<tool name="recurrence_history"/>
<tool name="weekly_worker" agent="workers/weekly.chatmd" local persistence="persistent"/>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>
```

Declare a worker as a normal authored ChatMD file with
`persistence="persistent"` and one versioned lifecycle moderator. A standalone
agent tool without persistent resource ownership cannot be selected as this
autonomous authored target. The worker's moderator handles `Session_start` and
`Session_resume` explicitly, using the ordinary scoped Run decisions for its
selected mode. Its configured ChatML project,
selected environment, code and approved private tools remain part of its captured
source. No UI registration is required. Declaring these controls or a worker does
not grant a controller execution host, target visibility, new provider credentials
or permission to use the worker's resources.

## Read before writing

`recurrence_list({})` returns a versioned object with `basis` and `result.entries`.
`recurrence_get({"id":"rec_weekly"})` returns `result.entry`. Read the current
`basis.generation`, `catalog_revision`, `source`, and `lifecycle_revision`; an entry
also gives its `control_revision`. Revisions are canonical decimal strings,
while generation is a nonnegative JSON integer.

Create, Edit and Resume require the controller's actual lifecycle moderator source
observer, as in the declaration above. A tools-only controller can return
`basis.source: null`; it may still inspect metadata or use Pause/Cancel, but cannot
invent a script/source identity to submit a sourceful command. Follow the
[versioned moderator contract](../guide/chatml-authoring-runtime.md) for executable
lifecycle behavior; the inert controller above only supplies that declared source.

Every write carries the exact basis the caller observed. For Create, use null
control revision. For Edit and Resume, echo the entry's control revision and the
current source/lifecycle basis. Pause and Cancel use the same generation/catalog/
control CAS with both source and lifecycle fields explicitly null, allowing safe
fencing even when the selected source has become unavailable.

The shape below illustrates a Create request; replace the basis values with the
actual read, rather than inventing source identity or refreshing CAS invisibly.

```json
{
  "id": "rec_weekly",
  "basis": {
    "generation": 0,
    "expected_catalog_revision": "0",
    "expected_control_revision": null,
    "source": {
      "script_id": "controller",
      "source_sha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "generation": 0,
      "installation_epoch": "1"
    },
    "expected_lifecycle_revision": "0",
    "key": "weekly-create-v1"
  },
  "capture": {
    "schedule": {
      "kind": "interval",
      "rule": {"version": 1, "kind": "interval", "mode": "fixed_delay", "period_ms": 3600000},
      "first_due": "2100-01-01T09:00:00Z"
    },
    "target": {"kind": "authored_child", "name": "weekly_worker", "mode": "workflow"},
    "policy": {
      "misfire": {"kind": "coalesce"},
      "grace_ms": "60000",
      "max_pending": 1,
      "max_history": 64,
      "max_calendar_steps": 256
    }
  }
}
```

Create starts paused. Read Get again and call Resume with a new key and its exact
basis to enable future occurrences. Edit replaces a paused definition and captures
the currently selected source once. Pause fences future launches; Cancel permanently
retires that recurrence identity. Both preserve already admitted and historical
work. A source/control conflict asks the caller to inspect a fresh read and make
an explicit new request; the service never substitutes current CAS.

Keep the complete acknowledged request and key for retry. An identical request
returns its original receipt, including after a later definition edit. Changing
generation, source, target selector, input, schedule, policy or CAS under that key
is an idempotency conflict. An old generation can identify an exact retained ACK;
it cannot authorize a fresh write against a new generation. A new intentional Edit
uses a new key and captures the selected source again.

## Choose an existing target or a new authored worker

An existing target contains the full host/session reference and ordinary input:

```json
{
  "kind": "existing_session",
  "session": {"server_id": "srv_example", "session_id": "ses_example"},
  "input": {"kind": "plain_text", "text": "Prepare the next report.", "attachments": []}
}
```

The host reads its actual source/configuration and retains those selectors in the
definition. Target acquisition and preview remain permission checked. A source
preview is immutable selection data; it does not launch, initialize or admit a Run.

A fresh authored target names an explicitly declared worker and chooses
`single_turn` or `workflow`. Capture retains the actual authored program and its
private resource association. Retries reuse the acknowledged target only after
checking the same original name and mode. This native selector accepts no caller
path, project override, environment override, artifact carrier or capability pins.
Operator-created Artifact targets continue to use the existing host APIs.

Code capture does not freeze external data or make resource permissions immortal.
The worker's live resource/source authority is checked again at construction and
effect boundaries. Replacing its defining source, changing a private binding,
stopping an owning parent, or revoking current authority can deny a later launch.

## Timing, missed work and host absence

Fixed-rate intervals retain their nominal series. Fixed-delay intervals choose the
next occurrence from actual acknowledged completion. Calendar rules use an
explicit named zone, local time and inclusive start/end dates; daylight-saving
gaps choose skip or the end of the gap, and repeats choose earlier, later or both.
Daily, weekday-set and month-day-set patterns are supported. These are Ochat
calendar rules, not cron or RRULE strings.

Misfire policy is bounded skip, coalesce or catch-up with an explicit finite limit.
Pending work, retained history and calendar search have separate finite bounds.
An unresolved occurrence prevents overlap for its target; unknown outcomes do not
prove absence or authorize an automatic retry.

Host absence and client detachment differ. A configured live host can continue
without a client; an absent or disabled execution host cannot. Stopping a controller
pauses its recurrences and does not automatically re-enable them on restart.
Process death preserves interrupted/unconfirmed evidence rather than replaying an
in-flight handler. There is no external exactly-once, rollback, external
cancellation or unbounded catch-up guarantee.

## Inspect status and retained history

Status reports effective control, planning availability, bounded ready/lookahead
metadata and unresolved/latest occurrences. History reports retained attempts,
rejections, uncertainty, omitted ranges and protected creation counts. Definition
views omit private input, files, artifact carriers, resource pins and credentials.

Controller history is retained evidence. Run/session/outcome details require a
separate current transcript-read and visibility check on the actual target.
Denied targets are redacted; missing targets are marked unavailable. Missing exact
Run detail is not an absent-admission or no-effects proof. Corruption, shutdown and
transient errors remain errors instead of being hidden as an empty history.

Visible targets are separately authorized historical snapshots at reported
session/Run revisions. They are not an atomic multi-target view or current execution
permission. Before disclosure the service repeats the same native/inherited host
check and verifies its actual controller observation and basis without yielding.
A revoked caller rejects the whole read, including otherwise redactable targets.

Metadata writes retain the existing admitted-native-call contract. Host/inherited
policy is checked at native admission and immediately before Actor submission;
actual local original, binding, generation, profile, stop and cancellation checks
remain at the writer boundary. This is not an atomic cross-Actor policy transaction
during writer IO. A real ACK stays acknowledged. Every future launch and target
effect needs its own genuine current authority; stored metadata grants none.

## Use the pure request package from ChatML

The [recurrence-requests package](../examples/libraries/recurrence-requests/chatml.project.json)
exports explicit basis/target request constructors and typed interval, calendar and
bounded policy helpers. Import it through the ordinary local ChatML project system,
then invoke an explicitly selected tool:

```ocaml
let run ctx input =
  let* result = Tool.call("recurrence_list", Requests.list()) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)
```

The installed source inventory can copy the complete ordinary package without
activating it:

```sh
ochat libraries show recurrence-requests
ochat libraries export recurrence-requests -parent /existing/workspace -directory recurrence-copy
```

The project is at
`recurrence-copy/examples/libraries/recurrence-requests/chatml.project.json`.
Select its `pure` environment through an ordinary explicit Local dependency;
`Requests` above is the application's chosen alias for `Recurrence_requests`.
Exporting source does not add a project-source grant, register a native tool or
start an execution host. Retrieve the installed `runtime.recurrence` authoring
topic for this same guide, and `reference.tools` for the invoking session's exact
selected schemas.

The package performs no Tool.call, capture, approval, hidden CAS refresh or effect
itself. Helpers check their finite structural inputs; the native decoder remains
authoritative for integer endpoints, dates/zones/calendar semantics, attachment
contracts and total encoded limits. Importing the package or selecting the
`recurrence` instruction grants no capability.

The helper accepts generation values only from 0 through 9007199254740991,
ChatML JSON's exact integer range. It rejects a larger value instead of rounding
the echoed CAS; this helper limit does not narrow the native host decoder's
integer domain. Keep decimal revision/epoch strings as strings. An application
that cannot exactly represent a returned generation must not submit a rounded
request or silently substitute a newer generation.
