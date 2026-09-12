# X06 response watcher qualification fixture

For complete agent entrypoints and setup instructions, use the
[helper/native-watcher bundle](../x07-helper-session/bundle/README.md).

This is user-authored polling built from existing ChatML jobs, one-shot schedules,
subscriptions and notifications. It does not use a native child-response push API.
The shared extension runtime is enabled by default; the agent must declare its
tools and retain authority to manage the selected child.

`watcher.chatml` provides `watch_initial_state` and `watch_on_event`; the daemon
fixture combines these with the X07 helper moderator's state and event handler.
`tools.chatmd` declares two moderator-handled tools and a standalone probe. The
test copies the schema and probe to the filenames used by those declarations.

Call `notify_when_agent_responds` with `session_id` and exactly one of:

- `receipt_id`: wait for that submission to finish, then read its first output page.
- `cursor`: watch outputs after a caller-obtained, caught-up read snapshot. The
  probe requires the same generation and an idle child with no active operation
  before reporting an available output page.

The immediate result is a pending subscription reference. State retains the
original invocation, target, epoch, job/timer, deadline, backoff, retry count and
last delivered identity. The first probe runs immediately as background work;
pending probes schedule another one-shot timer. Completion publishes one
correlated notification requesting a model turn. Output is bounded to one page;
use its continuation cursor to read further pages or fragments. A cursor is not
silently replaced after expiry or generation changes.

`cancel_response_watch` accepts `subscription_id`. It cancels the watch and its
owned work without stopping the child. Cancellation publishes a retained
`No_wake` notification because the foreground tool already returns its result.

The author configures timeout (30 seconds for waiting for a response), backoff
(25 ms to 1 second), transport retries (two), permission-wait policy (`wait`) and
stopped-child policy (`fail`) in the source. This response deadline is separate
from child-creation latency and the default 10-second individual script budget.
`watch_retry_interrupted_probe` explicitly permits a fresh probe after a recovered
`background.interrupted` result, within the same retry/deadline bounds. This is
safe for this read-only probe; it is not a general policy to replay interrupted
tools with side effects.
The subscription's extra second permits explicit timeout publication before host
expiry; host expiry remains a fallback if the moderator cannot run.

`agent_server_helper_test.ml` runs this same watcher twice. The helper backend
uses the actual sandboxed `ochat-agent-helper` process with all six native lifecycle
registrations absent. The native backend replaces `watch_session_request` with
`native-request.chatml`, using the scoped `agent_wait`, `agent_read`, and
`agent_status` tools. Provider responses are deterministic local fixtures.

The integration fixture supplies a host wall clock with a different epoch from
the process clock and advances it on completed Eio waits. This checks that
foreground and idle moderator callbacks use the same clock as persisted workflow
deadlines, while excluding fixture CPU/setup time from the response window.
Real monotonic process limits and the external test timeout remain active.
This is functional recovery qualification, not a wall-clock latency benchmark
or a measurement of expiry during downtime.

Current integration coverage includes delayed receipt completion, future output
from a cursor, concurrent-watch cancellation without child termination, foreign
child denial and exactly one retained notification per watch across daemon restart.
The restart scenario also leaves receipt and cursor watches active, shuts down
the daemon, and requires automatic recovery without a new parent message. The
interrupted submission reports `watcher.target_failed`; the expired cursor reports
`agent.read.cursor_expired`, without silently replacing it or rerunning the child.
Both notifications remain in history even if the automatic follow-up budget
suppresses an additional model turn. Completed probe jobs must finish delivery.

Orderly shutdown gives admitted moderator callbacks the configured grace period
to commit. An abrupt crash or grace expiry inside a handler can still leave an
ambiguous interrupted event. The runtime deliberately does not replay that event;
explicit event reconciliation remains necessary. This fixture does not claim
automatic recovery from arbitrary interrupted external effects.
This fixture alone does not complete X06, X07 or E09.02.

`chatml_response_watcher_test.ml` separately compiles these exact sources and
checks the algorithm against a recording host: capped exponential backoff, bounded
transport/interruption retries, author-selected permission/stopped policies,
stale epochs, duplicate terminal callbacks, deadline precedence and cancellation
of a watch with an active probe. Probe checks distinguish available output from
an active operation, generation changes, expired cursors, permission waits and
foreign-child denial. These tests complement the daemon integration; the recording
host does not establish durability or OS confinement.
