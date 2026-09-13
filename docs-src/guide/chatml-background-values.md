# Background work values and result formats

Use [background operations](chatml-authoring-background.md) for ownership,
transactions, recovery and delivery policy. This reference explains the values
those operations exchange. A string ID identifies host-owned work; constructing
a variant around it does not create work, authorize access or establish invocation
correlation. Reading a result does not acknowledge it for another reader.

## Script value types

| Alias | Shape and availability |
|---|---|
| `work_ref` | `Job(string)` or `Subscription(string)`; standalone tools and both moderators. |
| `completion` | `Succeeded(json)`, `Failed(tool_error)`, `Cancelled(string)` or `Expired`; moderators. |
| `wake_policy` | `Request_turn`, `Next_turn` or `No_wake`; moderators. |
| `schedule_misfire` | `Deliver_once_immediately`, `Skip_if_expired` or `Fail`; moderators. |
| `notification_correlation` | Record with string `key`, optional string `invocation_id`, optional `work_ref` `work`; moderators. |
| `work_completion` | Record with integer `version`, `work_ref` `work`, optional string `originating_invocation`, and `completion` `result`; moderators. |

Use backticks on constructors in ChatML source. Option fields use `None` or
`Some(value)`, not JSON Null or an omitted record field. A tool_error has exactly
`code: string`, `message: string`, `retryable: bool` and `details: json`.
Do not confuse the completion's Succeeded/Failed constructors with an invocation's
Complete/Fail/Pending outcome or a Tool.call's Ok/Error result.

The one-off surface returns JSON and has none of these aliases. A standalone
tool can return Pending with a work_ref, but does not gain Subscription.create,
Schedule or Notification operations. Model-generated moderators retain these types
within the narrower delegated operation surface.

work_completion describes the compiler's Job_completed/Subscription_expired
constructor payloads. It is not a promise that the installed host emits those
constructors. Qualified background-job completion currently arrives as
Internal_event JSON; use the documented
[host event contract](chatml-authoring-background.md#acknowledge-before-publishing-a-result).
The session actor expires the retained subscription and cancels its linked timer;
do not wait for a compiler-listed Subscription_expired callback as proof of that
transition. Inspect retained results and the installed host's completion path.
JSON data that names a constructor is still data.

Wake controls automatic model-turn admission, separately from data publication.
Next_turn and No_wake both avoid requesting an automatic turn; neither discards
a committed notification. Misfire controls an overdue timer during recovery,
separately from wake. A zero-millisecond timer still schedules a later data event;
it is not an inline function call. Negative delays fail admission.

## JSON returned by inspection operations

These APIs return JSON, not ChatML records with dot-field access. Use Json.get_field
or Json.get_path and handle absence. Do not assume every API shares Job.get's shape.

| Operation | Result format |
|---|---|
| `Job.get(id)` | Bounded object with version, id, status string, progress, attempt number, created_at, completed_at and completion. Nonterminal completion is Null. |
| `Job.read_result(id)` | Materialized terminal completion object, or Null while nonterminal; it does not wait for work to finish. |
| `Subscription.get/complete/fail/cancel/arm` | Retained subscription object: schema_version, id, session_id, generation, invocation_id, kind, created_at, deadline, wake, epoch and optional ownership/result fields. |
| `Schedule.get(id)` | Versioned timer record. An owned timer wraps its base fields under schedule, alongside ownership; legacy unowned records have the base fields at the top level. |
| `Notification.get(id)` | Versioned delivery record. Ownership, wake disposition, disclosure pins or completion projection can wrap the base record under delivery, including two nested delivery wrappers. |
| `Ingress.get/revoke` | Versioned registration view with registration_id, subscription_id, string epoch, namespace, created_at, expires_at, revoked, limits and receipts. |

A subscription without a terminal winner omits result. Its epoch is a JSON number
in the retained record. The application epoch in an external-data or watcher-event
payload can instead be a decimal string. Those encodings are intentional:
keep the integer epoch in moderator state and compare event encodings according
to their contract; do not treat every number-like field as interchangeable.
Arming returns the retained state after admission. Check for a terminal result
before treating the proposed next epoch as active.

Job.get completion is either an inline completion object or a stored artifact
descriptor with type artifact, version, outcome and reference. Job.read_result
materializes the authorized terminal completion; inline completions already have
that same shape, while artifact descriptors must be resolved. Successful completion has type succeeded
and value; failed completion has type failed plus code/message/retryable/details;
cancelled has reason; expired has no result payload. An interrupted job is a job
status and is represented through its terminal completion contract.

A base timer exposes id, session_id, generation, payload, created_at, next_due_at,
misfire, status and delivery_count, with optional last_delivery_at. A base delivery
exposes id, correlation, source, completion, wake, created_at, attempt and status,
plus its version/owner fields. Status in these records is structured protocol JSON,
not necessarily Job.get's status string. Preserve the original envelope if you
need ownership, wake or disclosure metadata: unwrapping for a display summary
does not replace protocol validation or permission checks.

An ingress registration's revoked field is Null while not revoked and a reason
string after revocation. The returned limits and receipts are data, not producer
credentials. The producer still needs the authenticated host ingress scope.

## Script job requests

Job.start_script accepts the same strict JSON object as the one-off runner:
required source (string), input (any JSON), and tools (array of names), with optional
positive integer timeout_ms and a limits object. Unknown request/limit fields fail
validation. Explicitly select a subset of available tools; an empty array selects
no tools. A source string defines `let main input = ...` returning a JSON task.

The supported limit keys are fuel, max_tasks, max_calls, max_invocation_depth,
allocation_bytes, max_value_bytes, max_output_bytes, max_array_items, max_depth,
max_source_bytes and compile_timeout_ms. max_tasks/max_calls allow zero; the other
values are positive integers. Overrides lower the admitted host policy; they do
not raise it. timeout_ms bounds execution and compile_timeout_ms bounds compilation;
neither changes the compiler target or grants additional authority. These request
fields are not the same record as tool_context.limits.

## Checked typed completion handling

This complete moderator program examines a compiler-shaped completion record.
The illustrative job/invocation strings are data only; no host operations are
installed by the checker. The example shows the difference between a completion
label, correlation, wake and timer recovery policy, and executes on both moderator
surfaces. It does not publish the invented identity as a real job result.

<!-- ochat-authoring-example: {"id":"background-values.typed-completion","surface":"moderator_v1","also_run":["delegated_moderator_v1"],"result":{"work":"job:example-job","terminal":"failed:retry","invocation":"example-invocation","wake":"next-turn","misfire":"skip","expired":"expired","cancelled":"cancelled:stopped","success":"succeeded"}} -->
```ocaml
let work_label : work_ref -> string = fun work ->
  match work with
  | `Job(id) -> String.concat("job:", id)
  | `Subscription(id) -> String.concat("subscription:", id)
let terminal_label : completion -> string = fun result ->
  match result with
  | `Succeeded(value) -> "succeeded"
  | `Failed(error) -> String.concat("failed:", error.code)
  | `Cancelled(reason) -> String.concat("cancelled:", reason)
  | `Expired -> "expired"
let wake_label : wake_policy -> string = fun wake ->
  match wake with | `Request_turn -> "request" | `Next_turn -> "next-turn" | `No_wake -> "none"
let misfire_label : schedule_misfire -> string = fun policy ->
  match policy with
  | `Deliver_once_immediately -> "deliver"
  | `Skip_if_expired -> "skip"
  | `Fail -> "fail"
let initial_state = `Null
let on_event ctx state event =
  match event with
  | `Session_start ->
    let completed : work_completion =
      { version = 1; work = `Job("example-job");
        originating_invocation = `Some("example-invocation");
        result = `Failed({code = "retry"; message = "Try again"; retryable = true; details = `Null}) }
    in
    let reference : notification_correlation =
      { key = "review"; invocation_id = completed.originating_invocation;
        work = `Some(completed.work) }
    in
    Task.pure(`Object([
      {key = "work"; value = `String(work_label(completed.work))},
      {key = "terminal"; value = `String(terminal_label(completed.result))},
      {key = "invocation"; value = `String(Option.get_or(reference.invocation_id, ""))},
      {key = "wake"; value = `String(wake_label(`Next_turn))},
      {key = "misfire"; value = `String(misfire_label(`Skip_if_expired))},
      {key = "expired"; value = `String(terminal_label(`Expired))},
      {key = "cancelled"; value = `String(terminal_label(`Cancelled("stopped")))},
      {key = "success"; value = `String(terminal_label(`Succeeded(`Null)))}
    ]))
  | _ -> Task.pure(state)
```

## Checked metadata inspection

This example reads minimal illustrative metadata, not full protocol-valid records.
It demonstrates missing versus Null fields, owned timer nesting and a delivery
with two wrappers. The bounded unwrapping is a display helper for host-produced
metadata, not a validator for arbitrary submitted records. Production handlers
should branch on the actual status and retained terminal result before deciding
whether to retry, publish or stop.

<!-- ochat-authoring-example: {"id":"background-values.metadata","surface":"one_off_v1","result":{"job_status":"running","job_completion":null,"subscription_result_absent":true,"timer_status":{"type":"scheduled"},"delivery_id":"delivery-example","delivery_status":{"type":"committed"}}} -->
```ocaml
let field value key = Option.get_or(Json.get_field(value, key), `Null)
let rec delivery_body value remaining =
  if remaining == 0 then value else
  match Json.get_field(value, "delivery") with
  | `Some(next) -> delivery_body(next, remaining - 1)
  | `None -> value
let main input =
  let job = Json.parse("{\"status\":\"running\",\"completion\":null}") in
  let subscription = Json.parse("{\"epoch\":1}") in
  let timer = Json.parse("{\"schema_version\":2,\"schedule\":{\"status\":{\"type\":\"scheduled\"}}}") in
  let delivery = Json.parse("{\"schema_version\":5,\"delivery\":{\"schema_version\":4,\"delivery\":{\"id\":\"delivery-example\",\"status\":{\"type\":\"committed\"}}}}") in
  let body = delivery_body(delivery, 2) in
  Task.pure(`Object([
    {key = "job_status"; value = field(job, "status")},
    {key = "job_completion"; value = field(job, "completion")},
    {key = "subscription_result_absent"; value = `Bool(Option.is_none(Json.get_field(subscription, "result")))},
    {key = "timer_status"; value = Option.get_or(Json.get_path(timer, ["schedule", "status"]), `Null)},
    {key = "delivery_id"; value = field(body, "id")},
    {key = "delivery_status"; value = field(body, "status")}
  ]))
```

The [job launch tests](../../test/chatml_composition/job_launch_tests.ml),
[timer tests](../../test/chatml_composition/timer_tests.ml),
[subscription tests](../../test/chatml_composition/subscription_tests.ml),
[notification admission tests](../../test/chatml_composition/notification_admission_tests.ml)
and [ingress tests](../../test/chatml_composition/ingress_tests.ml) exercise compiled
operations through real session services. These data-only documentation examples
supplement that integration evidence; they do not establish durability themselves.
