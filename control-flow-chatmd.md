Design: ChatMD Rules and Policy (event-driven, declarative, explicit)

Philosophy
- No hidden pre-processing: every side-effect caused by rules is materialized as normal ChatMD blocks in the transcript.
- Declarative, minimal surface: XML-like tags, simple boolean expressions, no templating engine.
- Composable, safe defaults: deny before auto-fix, once-per-session opt-in, cooldowns to avoid loops, explicit priorities.

New top-level blocks
1) event-driven behaviors (periodic reminders, triggers, auto-compaction, etc.)
 ```xml
 <rules> … </rules> 
 ```
2) per-tool constraints (prerequisites, input invariants, quotas)
```xml
<policy> … </policy>
```

Core mental model: Events → Guards → Actions
- Event types (initial set):
  - turn_start: before sending the next request to the model.
  - turn_end: after receiving the assistant turn.
  - pre_tool_call: when assistant requests a tool call, before execution.
  - post_tool_response: after a tool returns (success or error).
  - message_appended: whenever a message is added (any role).
- Guard: a boolean if="…" expression evaluated in the event context.
- Actions: insert messages, deny a tool call, call a tool or agent, compact context, set state, stop further rules.

Expression language (small, predictable)
- Literals: numbers, strings, booleans; arrays via JSON-like [a,b,c].
- Operators: == != < <= > >=, and, or, not, in, + - * / %, parentheses.
- Built-in variables:
  - turn_index (int, 1-based assistant turns)
  - context_tokens (approx token count for current model)
  - history_length (message count)
  - last_role (system|developer|user|assistant|tool|none)
  - event.name (string: “pre_tool_call”, …)
  - event.tool.name, event.tool.args (when relevant)
- Built-in functions:
  - ever_called("tool"), count_calls("tool")
  - called_since("tool", n_turns)
  - arg("path") for current tool call (dot path: target_lang, or jsonpath("$.target_lang"))
  - text_contains("needle", scope) where scope ∈ {"last_user","last_assistant"}
  - tokens(scope) where scope ∈ {"context","last_user","last_assistant"}
  - now() (monotonic ms), since_ms("rule_id")
- Determinism: no network side-effects in expressions; time only for cooldown comparisons.

Actions
- ```xml
  <insert role="user|assistant|developer|system" synthetic="true" via_rule="id">…markdown…</insert>
  ```
  - Appends a message block at that point; “synthetic” and “via_rule” annotate provenance.
- ```xml
  <deny reason="…"/>
  ```
  - Only valid in pre_tool_call; prevents the call and emits a <tool_response status="error"> with the reason, plus an optional developer reminder.
- ```xml
  <call tool="NAME" via_rule="id">{"json": "args"}</call>
  ```
  - Triggers a tool call; materialized as <tool_call> / <tool_response> with via_rule annotation. For safety, default max one auto-call per event unless overridden.
- ```xml
  <agent src="…">…</agent>
  ```
  - Equivalent to the existing inline agent; action form allows using it in reaction to events.
- ```xml
  <compact keep="system,developer,latest:6" strategy="relevance+summary" threshold_tokens="40000"/>
  ```
  - Invokes the built-in compaction pipeline; replaces history per existing semantics and appends a small developer note marking compaction.
- ```xml
  <set var="name" value="expr"/>
  ```
  - Sets a session-scoped runtime variable available to expressions.
- ```xml
  <stop/>
  ```
  - Stop further rules for this event.

Rule wiring and safety
- ```xml
  <on event="…" [tool="NAME|*"] [if="…"] [priority="0"] [once="false"] [cooldown_turns="0"] [cooldown_ms="0"]>…actions…</on>
  ```
  - Rules run in ascending priority; within the same priority, source order.
  - once="true": only fire the first time the guard holds this session.
  - cooldown_*: prevent repeated firings.
  - Max actions per event default: 1; configurable via <rules max_actions_per_event="1"/>.
- Observability: All actions write explicit ChatMD blocks; every synthetic block carries via_rule="ID" and synthetic="true".
- Failure: If an action fails (e.g., invalid JSON), append a developer message with the error; do not crash the session.

Tool policy (gatekeeping and invariants)
Simple, per-tool declarations collected under a <policy> block; avoids changing <tool …/> syntax.

- Prerequisites:
  ```xml
  <policy>
    <tool name="X">
      <requires tools="Y,Z" mode="enforce"/>  <!-- or mode="warn" -->
    </tool>
  </policy>
  ```
  Semantics: On pre_tool_call for X, if not ever_called("Y") or not ever_called("Z"), the runtime denies with a clear <tool_response status="error"> unless mode="warn".

- Input invariants (guards):
  ```xml
  <policy>
    <tool name="translate">
      <validate level="error">
        <check if='arg("target_lang") in ["en","fr","de"]' message="Unsupported target_lang"/>
        <check if='arg("text") != ""' message="text must be non-empty"/>
      </validate>
    </tool>
  </policy>
  ```
  Semantics: Checks are evaluated before execution. level="error" denies; level="warn" lets it proceed but appends a developer note.

- Quotas and cool-downs:
  ```xml
  <policy>
    <tool name="apply_patch">
      <quota per_turn="1" per_session="10"/>
      <cooldown turns="1"/>
    </tool>
  </policy>
  ```
  Semantics: Enforced via pre_tool_call denial with a specific reason.

Representative examples for your use cases

1) Every N messages, insert a user reminder
```xml
<rules>
  <on id="periodic-reminder"
      event="turn_start"
      if="turn_index % 5 == 0">
    <insert role="user" synthetic="true" via_rule="periodic-reminder">
      example reminder
    </insert>
  </on>
</rules>
```
2) Tool X only after Y and Z have been called
```xml
<policy>
  <tool name="X">
    <requires tools="Y,Z" mode="enforce"/>
  </tool>
</policy>
```

Optional, friendlier variant that suggests the fix:
```xml
<rules>
  <on id="x-prereqs"
      event="pre_tool_call"
      tool="X"
      if="not (ever_called('Y') and ever_called('Z'))">
    <deny reason="Tool X requires tools Y and Z first"/>
    <insert role="developer" synthetic="true" via_rule="x-prereqs">
      Call tools Y and Z with appropriate arguments, then retry X.
    </insert>
  </on>
</rules>
```

3) Invariants on tool call inputs
```xml
<policy>
  <tool name="translate">
    <validate level="error">
      <check if='arg("target_lang") in ["en","fr","de","es"]'
             message="target_lang must be one of en, fr, de, es"/>
      <check if='tokens("last_user") <= 4000'
             message="Input too long; please summarise first"/>
    </validate>
  </tool>
</policy>
```

4) Do B whenever A happens
- After odoc_search succeeds, call markdown_search to enrich results:
```xml
<rules>
  <on id="enrich-search" event="post_tool_response"
      tool="odoc_search"
      if="true">
    <call tool="markdown_search" via_rule="enrich-search">
      {"query": "related to " + arg("query")}    <!-- Reuses current call args if available -->
    </call>
  </on>
</rules>
```

- After apply_patch error, insert a developer hint:
```xml
<rules>
  <on id="patch-help" event="post_tool_response"
      tool="apply_patch"
      if='text_contains("Error", "last_tool")'>
    <insert role="developer" synthetic="true" via_rule="patch-help">
      Patch failed. Ensure the diff matches the V4A format and file paths exist.
    </insert>
  </on>
</rules>
```

5) Compact context over a threshold
```xml
<rules>
  <on id="auto-compact"
      event="turn_end"
      if="context_tokens > 40000"
      cooldown_turns="3">
    <compact threshold_tokens="40000"
             keep="system,developer,latest:8"
             strategy="relevance+summary"/>
  </on>
</rules>
```

Simplicity and backward compatibility
- Pure additive: existing ChatMD files continue to work unchanged; `<rules>` and `<policy>` are optional.
- Minimal surface:
  - Two blocks, one small expression language, a handful of actions.
  - No templating or hidden mutations; everything appears as normal ChatMD elements with provenance attributes.
- Safe by default:
  - pre_tool_call denial is the default for violations.
  - once/cooldown/priority prevent loops and “rule storms”.
  - Max one action per event unless you opt in to more.

Runtime integration plan (high-level)
- Parser: Add `<rules>/<policy>` nodes; allow both self-closing and block forms for future growth.
- Driver hooks:
  - fire turn_start before assembling request to OpenAI,
  - intercept pre_tool_call to enforce policy and validations,
  - fire post_tool_response on completion,
  - fire turn_end after assistant finishes streaming,
  - fire message_appended on each append.
- Evaluator: deterministic order (priority then source order); guard eval with a sandbox; enforce max_actions_per_event.
- Materialization:
  - All actions append standard ChatMD blocks with synthetic="true" and via_rule="…".
  - pre_tool_call deny emits a `<tool_response status="error">` with reason.
- Observability: optional trace line in a developer message whenever a rule fires and what it did; helpful in CI diffs.

Why this design hits the goals
- More dynamism without losing the Markdown/XML shape: rules are normal tags; actions produce normal messages and tool calls.
- Intuitive mental model (Events → Guards → Actions) mirrors how you already think about orchestration.
- Power when needed (preconditions, invariants, triggers, scheduling) with simple syntax and safe defaults.
- Keeps the “what the model sees = what’s in the file” invariant by recording every consequence explicitly.