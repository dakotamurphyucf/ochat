# Retained inference observations

Inference observations describe actual model attempts separately from committed
host turns. They use provider-independent contracts; clients do not need OpenAI
response types. The selected execution boundary is described in
[selected inference](neutral-inference.md).

## Meaning of the numbers

Each usage component distinguishes actual, estimated and unknown evidence.
Missing values and explicit `null` remain distinguishable from an actual zero.
Cancelled or interrupted requests do not acquire fabricated final counts.
An observation identity belongs to one actual attempt. A higher revision replaces
the prior snapshot; an identical revision deduplicates, and a conflicting same
revision is rejected. Repeated cumulative reports are never added together.

Cached input and reasoning output may be subsets of input and output. The summary
reports those components separately without adding subsets to their containers.
It does not calculate pricing. Context observations remain separate from usage:
an input-token report is not a context-capacity estimate, and an absent estimator
does not establish a model's window size.

A provider terminal closes an attempt. A completed host turn requires the actual
turn operation's successful durable commit, including its history. Failed,
cancelled and interrupted host turns retain distinct outcomes. Compaction does
not become a completed conversational turn merely because it used a model.

## Retention and persistence

The daemon-managed session ledger retains a bounded window of attempts and host
turn receipts. Its default bounds are 256 attempts, 256 turn receipts and 4 MiB
of encoded ledger data; the session document has a separate overall bound.
Active records remain pinned. Safe terminal retirement advances explicit coverage
counters, so retained totals must not be presented as lifetime totals.

When safe capacity is unavailable, tracking records an untracked admission and
its reason before allowing inference to proceed. A telemetry capacity limit does
not discard future data or silently turn unknown consumption into zero. Codec,
persistence and authentication failures remain errors. Diagnostic quota loss has
an explicit omitted count.

Named JSON schema conversion preserves older session data and unknown fields.
Migrated sessions report pre-tracking history as unknown. Resets and rebuilds
preserve monotone admission ordinals and retained prior-generation evidence;
they do not reset the ledger to an apparently complete empty history.

Ledger values carry an immutable document admitted under their complete quota
and structural profile. Repeated validation under an equal profile reuses that
admission and still checks the exact session and generation. Any different
profile repeats complete original-document, domain and reserved-capacity
checks. Every edit admits its final carrier before publication; provisional
retirement plans cannot supply reusable evidence. Unknown fields, including
their number spellings, remain intact. This reuse does not acknowledge a
durable commit.

A local synthetic benchmark used 128 retained attempt rows, a 352,463-byte
ledger, 64 KiB of preserved future data and a single attempt-state update. The
median of three batches of three calls reduced CPU time for the complete pure
session transition from 803 ms to 201 ms. Cumulative allocations per
transition fell from 4.75 GB to 1.28 GB (decimal bytes). These are cumulative
allocated bytes, including transient objects, rather than resident memory or a
heap bound. The benchmark excludes provider, transport and persistence
latency; it is not an end-to-end speedup claim.

## Runtime ownership and recovery

The actual resource graph acquires its tracking owner before model-capable
initialization. Owner acknowledgement is cancellation-protected so a caller can
install cleanup before observing pending cancellation. Admission allocates and
durably acknowledges an attempt identity
before dispatch. Strict tracking callbacks run independently of display callbacks.
The identity bracket removes temporary routing entries on every exit, including
preparation-start failure and callback exceptions.

Tracking ownership grants no execution permission. Session initialization, tool
authorization, cancellation and foreground/background ownership retain their
existing checks. An operation or invocation association is recorded only when
the executing host supplies that actual association; another concurrent operation
is never guessed from the actor's current state.

Closing a graph first excludes new admissions and joins its actual work, then
reconciles residual attempts while the actor and writer remain available. A
stopped session may still have a legitimately retained graph; stopping alone does
not revoke that graph's accounting owner. Recovery classifies an acknowledged
but unstarted attempt as definitely not submitted. A running attempt remains
possibly submitted unless stronger durable evidence exists.

Compaction of an unloaded session creates a short-lived accounting graph inside
its cancellable auxiliary lease, without initializing the agent runtime. Loaded
sessions reuse their actual retained graph. In either case, model attempts are
acknowledged before dispatch; auxiliary graph cleanup follows the joined request
scopes. Compaction advances the conversation's compaction counter, not session
identity generation, and does not count as a completed conversational turn.

Pure `Administration.reset` and `rebuild` return validated replacements and reject
active inference evidence. Their `plan_reset` and `plan_rebuild` counterparts
preserve the original ledger for preparation before runtime retirement. Those
planning candidates cannot be persisted directly: the trusted actor commit uses
the current ledger after owned work has joined, then advances its generation.

## Client reads

`session.inference_summary` returns retained totals and coverage.
`session.inference_observations` returns ordered attempt rows with bounded pages.
The OCaml SDK exposes these through `Agent_client.Inference_views` and checks the
connection's negotiated optional features. Feature negotiation grants no access.

Summary visibility follows session visibility. Detailed rows require transcript
access; configuration, account aliases and diagnostics additionally require
Diagnostics permission. Responses select safe typed fields and never expose raw
requests, credentials, private instructions, tool bodies or arbitrary provider
error text.

Reading an unloaded session does not activate it, construct a provider, run
recovery effects or repair its journal tail. The read owns and releases its store
handle; opening that handle may update lock metadata. Authorization is checked
before I/O and against the recovered session identity.

Cursors bind the principal, current scope set, session, generation, accounting
revision and disclosure/page choices. They do not contain credentials. Changed
accounting requires restarting pagination rather than mixing revisions. Each
query response is bounded by its explicit response policy, including its actual
RPC ID, envelope and cursor. This is not a new aggregate HTTP batch limit. A first
row that cannot fit returns a typed error instead of a non-advancing cursor.

## OCaml client contract

After connection initialization has selected the inference-read features, call
`Agent_client.Inference_views.summary connection session_id` for totals and
coverage. Build an `Agent_protocol.Inference_query.Request.t` for paginated rows;
request configuration and diagnostics only when the caller needs them and has
permission. The helpers return typed protocol errors, including unsupported
features and cursors that must be restarted.

[Interface](../../lib/agent_client/inference_views.mli) ·
[implementation](../../lib/agent_client/inference_views.ml)

The following excerpt is the current callable contract.

```ocaml
(** Retained-window inference reads over an initialized connection. Optional
    support is checked against its actual successful initialization. Feature
    selection is not authority: the server still checks session visibility and
    Diagnostics before exposing detailed safe configuration/diagnostic fields.
    No lifetime total, provider DTO or private request body is reconstructed. *)

val summary
  :  Connection.t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Inference_query.Summary.t, Agent_protocol.Error.t) result

val observations
  :  Connection.t
  -> Agent_protocol.Inference_query.Request.t
  -> (Agent_protocol.Inference_query.Response.t, Agent_protocol.Error.t) result
```
