# ChatML authoring: background work and delivery

Background work separates an initial tool acknowledgement from its eventual
result. A script starts owned work, retains its identity and lets the session
continue. The host later records completion; a moderator or qualified host
adapter can publish it as a separate runtime notification. A pending result is
not an ordinary tool response that will be overwritten later.

These APIs require an internally qualified host with their operation adapters.
Their presence in a compiler surface does not install a scheduler or grant tool
authority. `Job` is available in all four extensibility script surfaces.
`Subscription`, `Schedule`, `Notification` and `Ingress` are moderator operations,
also available to generated moderators only under their delegated host's admission.
One-off and standalone scripts do not gain those modules by reading this guide.

## Start and inspect owned jobs

`Job.start_tool(name, input)` returns a task of job ID. `Tool.spawn(name, input)`
is an alias for this same operation. It captures an already selected tool and
its input; it does not construct a tool from its name. `Job.start_script(request)`
accepts a one-off request with `source`, `input`, explicit `tools` and optional
lowering limits/timeout, and checks the exact `main input` contract. Compilation
does not evaluate the script initializer. Neither operation accepts a closure,
ambient process handle or replacement authority from the script.

Starts reserve work. The worker may run only after the owning transaction commits.
The saved request pins source/contract, tool configuration and resource ceilings;
worker admission resolves current bindings and rechecks policy. New tools added
to the registry do not widen old jobs. A job ID alone cannot restore a removed
tool or disclose work outside the caller's selected authority.

`Job.get(id)` returns bounded JSON metadata: `id`, `status`, `attempt`, timestamps
and `completion`. Status can be `queued`, `running`, `waiting_permission`,
`waiting_completion`, `succeeded`, `failed`, `cancelled` or `interrupted`.
Completion is null while work is nonterminal. A retained result may be inline
or an artifact descriptor. Use `Job.read_result(id)` for the authorized materialized
completion; it does not rerun the work or consume another reader's result.

Materialized completion JSON has `type: "succeeded"` and `value`,
`type: "failed"` with error fields, `type: "cancelled"` with `reason`, or
`type: "expired"`. These correspond to the ChatML completion variants
`Succeeded(json)`, `Failed(tool_error)`, `Cancelled(reason)` and `Expired`.
They differ from initial invocation outcomes `Complete`, `Fail` and `Pending`.
A `Job.get` progress snapshot is transient, bounded display data. It may be
truncated or absent after restart; it is neither full tool output nor completion.

`Job.cancel(id)` returns a task of unit and requests cancellation of existing
owned work. It is an immediate host operation: catching a later script failure
does not undo this cancellation. Tool calls and other external effects already
performed also cannot be rolled back by `Task.catch`.

## Acknowledge before publishing a result

A standalone handler may return `Pending(Job(id), acknowledgement)`; a
moderator-handled tool resolves its exact invocation with that outcome. The job
must be a surviving owned start, not an unrelated readable job. The acknowledgement
must satisfy the tool's output schema. For a subscription, the acknowledgement
must refer to a surviving subscription created by that same invocation.

Retain invocation/work correlation in returned moderator state. Background jobs
started under a moderator borrow retain that exact source. Qualified hosts deliver
terminal jobs as `Internal_event` JSON with `kind: "background_job_completed"`,
`job_id`, `attempt` (a decimal string) and `result`. The result can be an artifact
reference; the example calls `Job.read_result` instead of assuming it is inline.
Do not substitute a compiler-listed event constructor for this host delivery
contract. A job created without a moderator does not acquire a later moderator
as its owner; qualified root standalone pending tools use a host completion adapter.

## Shell-backed coordinator example

The following is the complete X03 coordinator, checked against its integration
fixture on both ordinary and delegated moderator compiler surfaces. Its
[ChatMD definition](../../test/chatml_extensibility_fixtures/x03-background-shell/agent.chatmd)
binds `begin_work` and the fixed `fixture_work` shell tool. The shell policy is
explicitly an isolated test policy; production authors must supply their own
authorized tool binding. The coordinator returns its acknowledgement before
publishing completion, and requests a model turn only for success.

<!-- ochat-authoring-example: {"id":"background.shell-coordinator","surface":"moderator_v1","fixture":"test/chatml_extensibility_fixtures/x03-background-shell/coordinator.chatml","also_check":["delegated_moderator_v1"]} -->
```ocaml
let initial_state = `Empty
let rec find_field fields name index =
  if index == Array.length(fields) then `Null else
  let entry = fields[index] in
  if entry.key == name then entry.value else find_field(fields, name, index + 1)
let field json name = match json with
| `Object(fields) -> find_field(fields, name, 0)
| _ -> `Null
let string json = match json with | `String(value) -> value | _ -> ""
let rec owner state job = match state with
| `Empty -> `None
| `Work(work, rest) ->
  if work.job == job then `Some(work.invocation) else owner(rest, job)
let completion result = match field(result, "type") with
| `String("succeeded") -> Task.pure(`Succeeded(field(result, "value")))
| `String("cancelled") -> Task.pure(`Cancelled(string(field(result, "reason"))))
| `String("expired") -> Task.pure(`Expired)
| `String("failed") ->
  let retryable = match field(result, "retryable") with | `Bool(value) -> value | _ -> false in
  Task.pure(`Failed({ code = string(field(result, "code")); message = string(field(result, "message"));
    retryable = retryable; details = field(result, "details") }))
| _ -> Task.fail("expected a terminal job result")
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* job = Job.start_tool("fixture_work", `Object([])) in
  let* () = Invocation.resolve(p.context.invocation_id,
    `Pending(`Job(job), `Object([
      { key = "job_id"; value = `String(job) },
      { key = "status"; value = `String("accepted") }
    ]))) in
  Task.pure(`Work({ job = job; invocation = p.context.invocation_id }, state))
| `Internal_event(payload) ->
  (match field(payload, "kind") with
  | `String("background_job_completed") ->
    let job = string(field(payload, "job_id")) in
    (match owner(state, job) with
    | `None -> Task.pure(state)
    | `Some(invocation) ->
      let* result = Job.read_result(job) in
      let* terminal = completion(result) in
      let wake = match terminal with | `Succeeded(_) -> `Request_turn | _ -> `No_wake in
      let reference = { key = "shell-result"; invocation_id = `Some(invocation); work = `Some(`Job(job)) } in
      let* delivery = Notification.publish(reference, terminal, wake) in
      Task.pure(state))
  | _ -> Task.pure(state))
| _ -> Task.pure(state)
```

The [X03 integration test](../../test/chatml_composition/background_shell_tests.ml)
checks acknowledgement-before-notification history, a second user turn while the
process runs, one retained completion and cancellation reaching the actual process.
The static documentation check does not run that shell tool. This short fixture
retains its correlation list; a long-running application also needs an explicit
state-retention policy within the host's state limits.

## Track a workflow with subscriptions

`Subscription.create(kind, lifetime_ms, wake)` returns a task of ID. Use
`None` for the host's lifetime default or `Some(positive_ms)` within its limit.
Creation belongs to a currently dispatched moderator-handled tool. Ordinary
events can operate on existing source-owned subscriptions but cannot create new
pending acknowledgements. The originating declaration supplies `completion_schema`
when present; it is distinct from the initial acknowledgement's `output_schema`.

`Subscription.get(id)` returns JSON state. `complete(id, expected_epoch, json)`,
`fail(id, expected_epoch, tool_error)` and `cancel(id, expected_epoch, reason)`
return the retained JSON state after the terminal proposal. The first terminal
commit wins. Inspect the returned `result`; a repeated completion can return an
earlier winner rather than the result just proposed. A stale epoch cannot finish
an active subscription, and an overdue active subscription expires.

`Subscription.arm(id, expected_epoch, timer_id, job_id)` links optional timer/job
IDs and advances the epoch. Use `None` to remove a link. A new timer must be owned,
scheduled and not already bound. Arming replaces/cancels the old outstanding timer
in the same staged transaction; a timer cannot be reused for a different epoch.
Include the expected epoch in application timer data and retained state so stale
callbacks can be ignored. Read the updated status rather than treating an ID as
proof that a rearm or terminal mutation succeeded.

A linked job is a reference, not a new capability or an automatic command to stop
that job. Cancelling a watch does not inherently stop the child agent or external
process being watched. Finishing the subscription cancels its outstanding linked
timer. Host cancellation of a parent job also resolves its owned dependencies;
an already committed terminal winner remains retained. Notification publication
is separate from the subscription's terminal state change.

## Schedule checks and choose recovery behavior

`Schedule.after_ms(delay_ms, json)` returns a task of timer ID. It schedules one
data event; it does not block the handler or create an automatic recurring loop.
`after_ms_with_policy(delay_ms, json, misfire)` additionally chooses what happens
when a timer is overdue during recovery: `Deliver_once_immediately` (the default),
`Skip_if_expired` or `Fail`. `Schedule.get(id)` reads JSON status, and
`Schedule.cancel(id)` returns unit while preserving an already terminal timer.

While a host is running, elapsed-time scheduling uses monotonic time; recovery
reconstructs timing from persisted due times and the selected misfire policy.
The script receives its JSON payload as `Internal_event`. Put application IDs
and epochs in that payload. User JSON is never decoded into a privileged native
event constructor. `Runtime.emit(json)` likewise emits data, not an authority grant.

To poll for a child response, start a bounded probe job. If it reports no result,
schedule one future check, arm the subscription with the new timer and retain
its epoch. On a matching tick, start the next probe. Stop scheduling after a
terminal result, cancellation or deadline. Apply bounded backoff and an explicit
retry policy instead of creating an unbounded stream of jobs or model turns.
The [X06 watcher](../../test/chatml_extensibility_fixtures/x06-response-watcher/watcher.chatml)
implements this pattern with receipt/cursor predicates, stale-callback checks,
deadline, backoff, interruption policy and separate permission/stopped-child policy.

## Publish data and request a model turn

`Notification.publish(correlation, completion, wake)` returns a task of delivery
ID. Correlation is a record with `key`, optional `invocation_id` and optional
`work` (`Job(id)` or `Subscription(id)`). Use the exact retained invocation/work
relationship. A work-backed publication must match its canonical terminal result;
do not transform a job's output and label the transformed value as that same
job's completion. `Notification.get(id)` reads owned provisional/retained status.

Publication stages durable intent. It does not immediately insert a model input
or execute a turn. The host verifies source, generation, current disclosure
authority and acknowledgement ancestry. Even nested script work must wait for
its enclosing model tool response to be published; omitting optional correlation
does not bypass this boundary. Failed or interrupted ancestry cannot authorize
a new result insertion.

Eligible data enters canonical history once at a safe input boundary, retaining
runtime provenance and delivery identity. It is a new notification, not a second
response for the original tool call. `Request_turn` asks the host for a follow-up
under its pause/rate/consecutive-turn policy. `Next_turn` and `No_wake` do not
request an automatic turn; committed data remains available in history. A denied
wake can leave the data committed. Wake acceptance means a foreground operation
was admitted, not that a provider request succeeded.

Data commit and wake disposition are separately persisted. A restored pending
wake does not insert the same history entry again. Several eligible deliveries
can share one admitted turn. After source or permission changes, the host can
discard an unadmitted wake while retaining already committed data and business
results. A correlation key is not a credential or a general external exactly-once
guarantee; retain the returned work/delivery identities.

## Receive external completion data

`Ingress.register(subscription_id, expected_epoch, namespace, schema)` returns a
task of registration ID for an active owned subscription. Use an `external.`
namespace and the supported JSON schema dialect. `Ingress.get(id)` reads status;
`Ingress.revoke(id, reason)` stages revocation. Registration captures host-approved
producer/source/policy/lifetime. A script cannot choose a producer principal or
make its returned ID an unrestricted event-injection credential.

An external producer must use an authenticated host transport with its explicitly
granted ingress scope. A submission is data, with its own retry/acknowledgement
identity; it cannot inject tools, model history or a native moderator event.
The moderator receives `Internal_event` data labelled `kind: "external_data"`,
with registration/event/subscription IDs, epoch (a decimal string), namespace and
`data`. Match the retained registration and workflow epoch before consuming it.
Complete the subscription and publish the actual retained terminal result under
the same source-owned rules as an internal completion.

The [X08 definition](../../test/chatml_extensibility_fixtures/x08-external-completion/agent.chatmd)
binds a string completion schema and retains registration/subscription correlation.
Its [socket integration](../../test/chatml_composition/ingress_socket_tests.ml)
exercises authenticated producer reconnect without transcript access.
The [crash fixture](../../test/agent_server_e2e/scenarios/crash_ingress_delivery.ml)
qualifies the recorded crash boundaries; it is separate from compiling this guide.

## Keep transaction and restart guarantees precise

New job reservations and subscription/timer/notification/ingress mutations are
selected before the owning save and acknowledged only after it succeeds.
Caught failures discard their staged mutations; whole-handler failure discards
all remaining provisional work. State returned by a failed moderator callback
does not become the retained checkpoint. External tool effects already executed
and cancellation of existing jobs are outside that rollback boundary.

Work is owned by the actual invocation/event, session generation, source and job
attempt. Host limits bound active work, nested depth, resources, state, lifetime
and retained records. Reaching a limit is an admission error; it does not justify
evicting unresolved work or retrying an effect under a fresh identity indefinitely.
Approvals remain owned by the executing invocation. A background context does
not silently approve calls merely because no user is currently typing.

Durability preserves records, not live closures, processes or arbitrary program
counters. Recovery rechecks captured bindings and source. Interrupted active
execution is recorded as interrupted; scripts must not assume it is automatically
safe to repeat. A job waiting on owned pending work can retain that dependency
and its original deadline across restart instead of rerunning the acknowledged
target. Stale attempts, generations, epochs and removed sources cannot claim new
effects or delivery. A changed moderator does not automatically inherit old events.

Use explicit safe/idempotent application retries when external effects may have
occurred before interruption. Stable receipts and delivery IDs prevent duplication
at their qualified runtime boundaries; they do not make an arbitrary remote API
or shell command exactly-once. Keep application correlation and recovery decisions
in serializable moderator state, and reconcile retained terminal records before
launching replacement work.
