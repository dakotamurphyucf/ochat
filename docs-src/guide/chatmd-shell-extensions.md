# ChatMD shell runtime extensions

Shell runtimes can delegate project-specific decisions to ChatML scripts or
custom executables without adding OCaml code. Extensions remain subordinate to
manifest authorization, administrative ceilings, worker-runtime confinement,
resource limits, typed protocols, and output redaction.

## Extension points

| Extension | Input | Result |
|---|---|---|
| Dynamic matcher | Normalized command context | Match/no-match. |
| Reviewer | Approval request and allowed scopes | Defer, approve, deny, or rewrite. |
| Before-interceptor | Prepared command context | Continue, rewrite, synthesize result, or reject. |
| After-interceptor | Finalized command result | Replace result or reject. |
| Effect analyzer | Command context | Add or explicitly replace effects. |
| Audit filter | Already-redacted event | Remove/redact mutable fields. |

Hard policy denial runs before an interceptor. A rewrite always restarts the
complete resolution, effect, capability, policy, approval, planning, and
verification pipeline.

## ChatML script declarations

Shipped shell script kinds are:

```xml
<script id="match" language="chatml" kind="shell_matcher">...</script>
<script id="review" language="chatml" kind="shell_reviewer">...</script>
<script id="before" language="chatml" kind="shell_before_interceptor">...</script>
<script id="after" language="chatml" kind="shell_after_interceptor">...</script>
<script id="effects" language="chatml" kind="shell_effect_analyzer">...</script>
<script id="audit" language="chatml" kind="shell_audit_filter">...</script>
```

`moderator` remains the conversation-moderator kind. IDs are unique across the
script registry even when kinds differ. Inline source and `src` are mutually
exclusive. Imported source provenance and source SHA-256 enter the manifest.

Scripts compile once per manifest. Unused scripts may produce diagnostics but
are not silently selected.

## Purpose-built surfaces

Shell scripts receive only the operations appropriate to their kind. Common
read-only context exposes normalized values such as command argv, basename,
canonical executable path/SHA-256/trust, cwd, effects, policy action/matches,
request kind, session identity, and safe path constructors.

Reviewer actions include defer, once/exact/prefix approval, deny, and rewrite.
Before-interceptor actions include continue, rewrite, synthetic response, and
reject. After-interceptors can replace a validated result or reject it. Effect
analyzers return typed effects. Audit filters return a validated filtered
event.

These surfaces do not inherit moderator `Process`, `Model`, `Tool`, network,
filesystem, or UI operations. Additional authority must be declared through a
separate operation/runtime binding. Configuration paths are delivered as
typed values rather than textual substitution into ChatML source.

Typed bridge values are versioned (`shell-context-v1`, `shell-policy-v1`,
`shell-approval-v1`, `shell-effect-v1`, and related result/audit values),
bounded, and exact-field validated. Malformed or future versions fail closed.

## Lifecycle, state, and transactions

Extension lifecycle is:

- `invocation`: fresh state for every call;
- `session`: retained for the agent session;
- `runtime`: retained for the compiled runtime instance.

Reviewers and interceptors default to session lifecycle; analyzers commonly
default to invocation. Calls to one stateful instance are serialized with an
Eio mutex while independent instances can run concurrently.

An extension action and its state update are transactional. State commits only
after one valid terminal action is returned. Timeout, cancellation, malformed
output, or runtime error rolls back the call. Fuel, task, value, array/depth,
output, and wall-time limits bound execution.

Serializable session/runtime state is snapshotted into typed session shell
state. Restore validates script ID, kind, source digest, and manifest identity.
Closures, refs, builtins, modules, and tasks do not cross persistence.

## Dynamic matchers

```xml
<policy default="ask">
  <rule id="project-rule" action="ask">
    <chatml_match script="match" function="on_event" failure="no_match"/>
  </rule>
</policy>
```

Dynamic matchers see immutable normalized context. Failure is conservative:
an allow rule cannot match because its matcher crashed, and a restrictive rule
cannot disappear because of failure.

## ChatML reviewers

```xml
<reviewers strategy="first_terminal">
  <reviewer
      id="project"
      kind="chatml"
      script="review"
      lifecycle="session"
      failure="deny"/>
  <reviewer id="human" kind="ui"/>
</reviewers>
```

A reviewer returns exactly one terminal action or defer. Malformed actions and
timeouts follow `failure`, which may not widen access.

## Model reviewers

```xml
<reviewer
    id="model-security"
    kind="model"
    agent="security-reviewer"
    model="gpt-5"
    failure="deny"/>
```

The reviewer receives a bounded redacted prompt and must produce one strict
versioned JSON decision. Markdown fences, prose, duplicate/unknown fields,
unsupported scopes/actions, malformed expiration, excessive output, timeout,
or transport failure fail closed. Tools are disabled by default; a reviewer
must not use the shell request it is evaluating.

## Interceptors

```xml
<interceptors>
  <interceptor id="python" phase="before" script="before" failure="deny">
    <match>
      <any>
        <basename value="python"/>
        <basename value="python3"/>
      </any>
    </match>
  </interceptor>
  <interceptor id="normalize" phase="after" script="after" failure="deny"/>
</interceptors>
```

Before-interceptors run in declaration order. The first rewrite restarts
preparation. A synthetic response or rejection terminates the before chain.
After-interceptors run over finalized safe output, in declaration order, and
each replacement is finalized again.

In schema version 1, only before-interceptors can declare `<match>`. An
after-interceptor receives every finalized result for its runtime.

## Effect analyzers

```xml
<effect_analysis>
  <analyzer
      id="project-effects"
      kind="chatml"
      script="effects"
      lifecycle="invocation"
      replace="false"
      failure="unknown"/>
</effect_analysis>
```

The safe default is additive. `replace="true"` is authority-relevant because
it may replace built-in analysis. Failure adds `unknown` rather than removing
an effect.

## Audit filters

```xml
<audit format="jsonl" path="${session_dir}/audit.jsonl" content="redacted">
  <filter script="audit" lifecycle="session"/>
</audit>
```

Core secret redaction occurs before and after the filter. Filters may remove
or redact mutable fields but cannot alter sequence, timestamp, request,
runtime, or manifest identity and cannot affect command behavior.

## Executable extensions

Every executable extension names a separate worker runtime:

```xml
<interceptor
    id="sanitize"
    phase="after"
    executable="${source_dir}/hooks/sanitize"
    sha256="..."
    runtime="hook-worker"
    protocol="shell-hook-json-v1"
    timeout="2s"
    max_input="1MiB"
    max_output="256KiB"
    failure="deny"/>
```

The parent resolves and fingerprints the hook, launches it through the worker
runtime, writes bounded JSON to stdin, closes stdin, reads stdout/stderr
concurrently, enforces wall/idle/output limits, validates one response, and
kills/reaps on cancellation. Hook stderr is redacted audit diagnostics; stdout
is protocol data, not command output.

Recommended worker posture:

- required sandboxing;
- no network;
- minimal roots;
- tight time/output limits;
- exact allow rule for a pinned hook executable;
- no interactive approval;
- no recursive interceptors.

Worker authority never inherits from the caller.

## `shell-hook-json-v1`

The runtime writes exactly one UTF-8 JSON object to stdin and closes it. The
hook writes exactly one UTF-8 JSON object to stdout and closes it. Leading and
trailing whitespace is allowed; extra values/text, duplicate keys, unknown
fields, invalid UTF-8, nonzero exit, or wrong action kind are failures.

Common context contains version/event, runtime and manifest IDs, request/plan
identity, request and stdin kinds, structured command, canonical executable
identity, cwd, effects, and policy. Secret values and environment values are
excluded unless explicitly safe and selected.

### Before responses

```json
{"action":"continue"}
```

```json
{"action":"rewrite","program":"safe-python","arguments":["script.py"]}
```

```json
{
  "action":"respond",
  "status":{"exited":0},
  "stdout":"validated result\n",
  "stderr":""
}
```

```json
{"action":"reject","reason":"Only repository-local scripts are permitted"}
```

Rewritten strings remain structured argv and cannot contain NUL. Rewrite
restarts authorization.

### After responses

```json
{
  "action":"replace_result",
  "status":{"exited":1},
  "stdout":"",
  "stderr":"normalized diagnostic\n"
}
```

or:

```json
{"action":"reject","reason":"Output violates disclosure policy"}
```

### Reviewer responses

```json
{"decision":"defer"}
```

```json
{"decision":"approve_once"}
```

```json
{"decision":"approve_scope","scope":"exact_session","expires_at":1786579200}
```

```json
{"decision":"deny","reason":"Not permitted by organization policy"}
```

```json
{"decision":"rewrite","program":"git","arguments":["status","--short"]}
```

Scopes must be offered by the manifest and host. Expiration is a finite Unix
timestamp.

## Dependency and reentrancy rules

Compilation builds edges for imports, inheritance, extension references,
worker runtimes, reviewer agents, moderator process bindings, and configured
operations. Complete cycles are rejected.

A ChatML hook cannot synchronously invoke the runtime waiting for it. Internal
worker execution bypasses agent-facing tool publication and cannot recursively
invoke itself. Asynchronous work cannot mutate the immutable plan already in
flight.

## External backend templates

External backends share the worker principle but produce the final launch argv
through typed atoms:

- literal argument;
- cwd;
- target executable;
- complete command argv (exactly once);
- repeated read/write roots with flags;
- network flag;
- resource-limit arguments.

No atom performs shell parsing or string interpolation. Wrapper and target are
fingerprinted and reverified. Confinement is reported as verified, declared,
or none; declared confinement requires explicit acceptance and authorization.

## Failure behavior

Extension failures use declared conservative outcomes such as `deny`, `error`,
`no_match`, `unknown`, or `keep` where the specific extension permits it. No
failure mode turns a reviewer failure into approval, removes a required effect,
or reveals unfiltered output. Protocol and runtime failures are emitted as
typed `shell.*` diagnostics and audit events.
