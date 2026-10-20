# Develop and run a captured ChatML revision

An Authoring context identifies an exact source revision and environment.
Developer Check, Fixture, Live and REPL operations borrow that revision under
separate installed grants. A successful Check does not grant execution.

For an offline terminal session, use the complete
[local policy and launch example](../design/chatml-developer-runtime.md#chatml-developer-runtest-and-repl-design).
It creates a source file, prompt, workspace and explicit local Developer policy;
`chatml-repl --local` starts the first-party stdio host. For a configured Project,
select `--project chatml.project.json --entry main --environment default` in place
of `--source check.chatml` and authorize that exact `Local_project` selection in
the policy. A Project capture selects its source closure; it does not evaluate the
entry file into interactive bindings. Configured modules initialize when the REPL
uses them, so `Task.pure(Util.value)` probes an admitted `Util` module. A reset
clears interactive bindings while retaining that source capture. Source-root
indices refer to roots already installed by the host.
A remote session uses `--connect URI` with its authenticated connection profile
and the same explicit source selection. See the
[Developer runtime reference](../design/chatml-developer-runtime.md) for its flags
and policy records.

With that terminal running, enter:

```text
let saved = 8
:submit
Task.pure(saved)
:submit
:run 1
```

The second submission retains a Task reference. Only `:run 1` drives it. Type
errors preserve the previous committed bindings; the returned wire revision still
advances to serialize the submission attempt. Runtime errors can preserve earlier
successful statements and effects, so inspect the returned partial receipt.
`:reset` joins active work and starts a fresh REPL epoch on the same captured
source. To adopt saved file changes, create a new instance with a fresh capture.
After reconnecting, Observe the original command key before explicitly retrying
an uncertain request; a missing response is not proof that no work ran.
The compact historical receipt remains stable even when separately retained type
details have been evicted.

An editor uses the typed Authoring and Developer APIs for the fuller workflow:

1. Open the admitted Project selection, then Analyze its current `Expected`
   generation. Keep source IDs, digests and byte offsets with diagnostics.
2. Replace an editor document under its exact version. Queries can return
   provisional results for incomplete input. An older `Expected` becomes stale;
   applying it must conflict rather than silently select newer text. For a ChatMD
   script, retain its native ChatML site alongside the exact authored projection
   or external `src` origin; Unicode positions remain byte offsets.
3. Save explicitly through the editor's filesystem owner, then refresh the
   Authoring context. An editor overlay alone never writes the host file.
   To transfer a source closure, use Bundle begin/chunk/commit and open the
   published bundle under the installed upload/rebind policy instead.
4. Check that selected revision with a new command key. Fixture runs use admitted
   deterministic steps; Live runs also select the configured prompt and workspace.
   Each execution still checks its current method, source and runtime authority.
   Selecting another environment requires its own admitted selection.
5. Observe the original key to recover its immutable receipt. Publishing changed
   bytes creates another bundle revision; it does not rewrite an old capture or
   turn its bundle ID into a grant. A fresh execution needs a fresh key.
6. When recording is explicitly enabled, page the command or REPL trace by its
   original locator. Match the trace execution, submission and graph fingerprints
   to that run and match authored sites to exact source ID/digest/byte ranges.
   Trace pages require current disclosure and source-read authority and contain
   no source contents or values.

The repository's joined qualification runs the same authored phases over
configured HTTP and a genuine local stdio host, without a provider request:

```sh
env -u OPENAI_API_KEY -u OPENAI_KEY -u ANTHROPIC_API_KEY \
  -u GEMINI_API_KEY -u GOOGLE_API_KEY dune build \
  @test/agent_server_e2e/agent-e2e-developer-workflow
```

The [Authoring service contract](../design/chatml-authoring-services.md) defines
context, upload and refresh ownership. The
[trace contract](../design/chatml-execution-tracing.md) defines bounded readback;
its transport and revocation qualifications are separate from source editing.
