# Develop and run a captured ChatML revision

An Authoring context identifies an exact source revision and environment.
Developer Check, Fixture, Live and REPL operations borrow that revision under
separate installed grants. A successful Check does not grant execution.

## Start the terminal REPL

With Ochat installed, `chatml-repl` uses an explicit local policy and the
portable REPL client. This complete offline example grants only the selected
local source and the two method scopes needed to submit and explicitly run a
Task; it does not enable model inference or Shell approvals:

```sh
project=$(mktemp -d)
mkdir -p "$project/workspace"
printf '%s\n' 'let main input = Task.pure(input)' > "$project/check.chatml"
printf '%s\n' '<developer>Local deterministic REPL.</developer>' > "$project/agent.chatmd"
cat > "$project/policy.sexp" <<EOF
(project_sources ((external_roots ($project))))
(authoring_project_access
 ((read_grants (((root $project) (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root $project) (project check.chatml)
                (request (entry ((name main)))) (target one_off_v1))))))
(developer
 ((selections
   (((source (Local_source (root $project) (member check.chatml)))
     (target One_off_v1)
     (principals (((principal_id Embedded_operator)
                   (authentication_kind embedded.local)))))))
  (capacity
   ((max_runs 128) (max_active_commands 4) (max_active_agent_calls 8)
    (max_agent_sources 6) (max_native_bindings 32) (max_retained_tasks 8)
    (max_watches_per_connection 4) (max_pending_approvals 4)
    (max_repl_instances 8) (max_repl_keys_per_instance 128)
    (max_repl_submissions_per_epoch 64) (max_repl_source_bytes 65536)))
  (model Disabled) (approvals Disabled) (repl Standard_v1) (portable true)))
(developer_scopes (developer.repl developer.live.run))
EOF
chatml-repl --local --developer-policy "$project/policy.sexp" \
  --prompt-file "$project/agent.chatmd" --workspace "$project/workspace" \
  --transient --source check.chatml
```

For a configured Project,
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

## Edit, Check and choose Fixture or Live

An editor uses the typed Authoring and Developer APIs for the fuller workflow.
An immutable bundle revision identifies exact uploaded bytes, while every
execution still needs its own installed authority:

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

## Read attributable trace pages

An attributable trace names the admitted execution and exact captured source;
it is a bounded observation, not a substitute for the authoritative receipt.
The initialized client supports read-only pages on embedded, HTTP, Unix and
stdio connections. That transport coverage is separate from the joined authored
workflow above, which runs its complete phases over HTTP and local stdio.

The [Authoring service contract](../design/chatml-authoring-services.md) defines
context, upload and refresh ownership. The
[trace contract](../design/chatml-execution-tracing.md) defines bounded readback;
its transport and revocation qualifications are separate from source editing.
