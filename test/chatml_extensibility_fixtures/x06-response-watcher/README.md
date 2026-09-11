# X06 response watcher qualification fixture

This is user-authored polling built from existing ChatML jobs, one-shot schedules,
subscriptions and notifications. It does not use a native child-response push API.
The features remain behind internal extensibility qualification.

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
The subscription's extra second permits explicit timeout publication before host
expiry; host expiry remains a fallback if the moderator cannot run.

`agent_server_helper_test.ml` runs this same watcher twice. The helper backend
uses the actual sandboxed `ochat-agent-helper` process with all six native lifecycle
registrations absent. The native backend replaces `watch_session_request` with
`native-request.chatml`, using the scoped `agent_wait`, `agent_read`, and
`agent_status` tools. Provider responses are deterministic local fixtures.

Current integration coverage includes delayed receipt completion, future output
from a cursor, concurrent-watch cancellation without child termination, foreign
child denial and exactly one retained notification per watch across daemon restart.
Active-watch restart, stale/duplicate callback injection and the full deadline,
transport, permission and stopped-child policy matrix remain to be qualified.
This fixture alone does not complete X06, X07 or E09.02.
