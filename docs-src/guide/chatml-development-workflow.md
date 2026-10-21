# Develop ChatML with the terminal REPL

Use `chatml-repl` to try ChatML expressions, keep bindings between submissions,
inspect inferred types, and run a Task when you choose. The walkthrough below
runs locally without a provider account or model request. It starts a dedicated
Developer host; it does not create an agent conversation.

You will first calculate a value, then retain and run a Task, recover from an
error, and reset the session. After that, use a configured Project or connect
to an existing host. For editor integration, continue with
[authoring and execution services](chatml-development-services.md); for language
syntax, see [ChatML](../chatml/README.md).

## Before you start

You need a Unix-style shell, an interactive terminal, and Ochat built from a
revision that includes `chatml-repl` and `ochat-agent-stdio`. Follow the **Build**
section of [Build and configure Ochat](../agent-server/quickstart.md#build),
including `dune build @install` and `dune install`. Keep that opam environment
active. Provider configuration is unnecessary for this offline example.

Check both installed commands:

```sh
chatml-repl -help
ochat-agent-stdio -help
```

If a command is missing, load the opam environment for the switch where you
installed Ochat. When working directly in that checkout, you can instead run
`dune exec bin/chatml_repl.exe -- ...` and pass
`--stdio-executable "$PWD/_build/default/bin/ochat_agent_stdio.exe"` after
building the two executables. An older installed stdio host may lack the
Developer service even when the REPL executable is current.

The REPL is a full-screen terminal application. Enter inserts a newline;
**Ctrl+S submits the current buffer**. You can also submit by putting `:submit`
on its own final line and pressing Enter. Use that form if your terminal
intercepts Ctrl+S for flow control. Do not pipe a script into this frontend.

## Start the terminal REPL

### 1. Create the offline workspace

Run these shell commands in the same terminal. The temporary directory is kept
under `/tmp` with a predictable, space-free path shape for the policy below;
all shell path arguments remain quoted.

```sh
project=$(mktemp -d /tmp/ochat-repl.XXXXXX)
mkdir -p "$project/workspace"
printf '%s\n' 'let main input = Task.pure(input)' > "$project/check.chatml"
printf '%s\n' '<developer>Local deterministic REPL.</developer>' > "$project/agent.chatmd"
printf 'REPL files: %s\n' "$project"
```

`check.chatml` supplies the selected source and compiler contract. Creating the
REPL captures it; its `main` function is not automatically inserted into your
interactive bindings. `agent.chatmd` and `workspace/` supply the local host's
explicit prompt and workspace selections.

### 2. Grant the local source and REPL operations

The host requires a policy even for local use. Save this complete fragment. It
grants this source to the local operator, enables the Standard REPL, and permits
explicit Task execution. `model Disabled` and `approvals Disabled` keep this
example independent of inference and Shell approvals.

<details>
<summary>Copy the complete offline policy</summary>

```sh
cat > "$project/policy.sexp" <<EOF_POLICY
(project_sources ((external_roots ("$project"))))
(authoring_project_access
 ((read_grants (((root "$project") (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root "$project") (project check.chatml)
                (request (entry ((name main)))) (target one_off_v1))))))
(developer
 ((selections
   (((source (Local_source (root "$project") (member check.chatml)))
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
EOF_POLICY
```

</details>

Source-read permission, permission to use the REPL, and permission to run a Task
are separate. Listing a path or scope in a client request does not install a
host grant. This fragment is for `--local`; it is not a replacement daemon
configuration.

### 3. Launch

```sh
chatml-repl --local --developer-policy "$project/policy.sexp" \
  --prompt-file "$project/agent.chatmd" --workspace "$project/workspace" \
  --transient --source check.chatml
```

The header identifies `One_off_v1`, root index `0`, `source=check.chatml`,
`embedded.prompt`, and `embedded.workspace`. Wait for `[ready]` before typing.
`--transient` keeps the owned host's storage process-bound. To retain its durable
command storage instead, replace `--transient` with `--data-root ABSOLUTE_DIR`;
choose exactly one. REPL bindings themselves remain process-local.

## Submit values and inspect their types

The following blocks are **REPL input**, not shell commands. Type or paste one
block, then press Enter after its final `:submit` line:

```text
let saved = 8
:submit
```

The submission reports `finished` with a successful statement row. Now enter:

```text
saved + 1
:submit
```

Expect a successful value preview of `9`. The first binding persisted between
submissions. To inspect the expression type of this most recent submission,
enter `:type 1` on its own line and press Enter; the rendered type is `int`.
Statement numbers start at 1 within the displayed submission. A binding's type
uses `:type N binding M`, where `M` is its 1-based binding index. For example,
after submitting `let saved = 8`, `:type 1 binding 1` inspects `saved`.
Binding indexes select only that statement's declared values, in declaration
order; built-ins and earlier bindings are excluded. Expressions, type
declarations and `open` do not introduce inspectable declaration bindings.

The screen shows bounded receipts, not an unbounded output transcript. Large
values or types can be clipped or unavailable after their separate retention
limit is reached. Type inspection reads frozen type information; it does not
evaluate the expression again.

## Retain a Task, then run it

Submit:

```text
Task.pure(saved)
:submit
```

You should see `finished` and a `Task 1:` reference with an opaque identifier.
The Task is inert at this point. To drive the displayed reference, enter:

```text
:run 1
```

After Enter, the terminal reports `Task succeeded` with the value preview `8`.
The reference number selects an item in the displayed Task-reference page;
it is not a statement number. `:refs` reads the first bounded page of references for the displayed
submission; repeat it to follow that page's next cursor. Submit, type inspection and reference paging never implicitly run
a Task. Task execution still requires the host's current execution permission.

## Incomplete input, errors and committed state

You can enter several lines before submitting. If you submit an accepted but
unfinished fragment such as `let doubled =`, the result is `need more input`.
The buffer stays available: complete the expression with `saved * 2` and submit
again. The parser must accept the whole fragment before any statement runs. It then
typechecks and evaluates statements in order, committing each successful one.
A later type or runtime error can leave that completed prefix committed. A
malformed prefix can be rejected rather than classified as incomplete; the
REPL does not insert missing delimiters.

Try a rebinding that fails typechecking:

```text
let saved = 1 + true
:submit
```

The type error leaves the old `saved` binding intact. Submit `saved` to observe
`8`. The service's wire revision still advances for a serialized attempt;
that revision is not a count of successful bindings.

Runtime failure has a different boundary. Submit these two statements together:

```text
let before_failure = 42
1 / 0
:submit
```

The result is `stopped`: the first statement completed and its binding remains.
Submitting `before_failure` afterwards yields `42`. A failed statement does not
publish its new binding, but prior mutations and external effects cannot be
rolled back. Inspect the completed statement rows before retrying anything
that may have caused effects. Rebinding a name later also leaves existing
closures attached to the binding they originally captured.

## Reset, close and keyboard commands

When the terminal is idle, enter `:reset` to discard interactive bindings and
old Task handles and begin a fresh epoch on the **same captured source**. If a
submission or Task is still active, cancel it with Ctrl+C or `:cancel` and wait
for cleanup first. The terminal refuses reset while that work is active.
After reset, submitting `saved` fails because that interactive binding is gone.
Changing a source file on disk does not update an existing instance. To capture
saved edits or another Project/environment, quit and launch a new REPL.

| Key or command | Action |
| --- | --- |
| Enter | Insert a newline; execute a recognized command on its own final line. |
| Ctrl+S or final-line `:submit` | Submit the current ChatML buffer. |
| Left/Right, Home/End, Backspace/Delete | Move and edit at text boundaries. |
| Up/Down | Recall bounded local submission history and return to the draft. |
| `:type N` / `:type N binding M` | Inspect a retained expression/binding type from the displayed submission. |
| `:refs` | Page Task references from the displayed submission. |
| `:run N` | Explicitly execute a Task from the displayed page. |
| Ctrl+C or `:cancel` | Cancel the exact active submission or Task and wait for cleanup. |
| `:observe` | Read the current tracked submission or Task receipt. |
| `:reset` | When idle, clear interactive state on the original capture. |
| `:close` | When idle, close the current instance and release its evaluator. |
| `:retry` | Explicitly resend an uncertain keyed operation with its original payload. |
| `:abandon` | Relinquish local retry custody; effects may already have occurred. |
| `:help` | Show supported commands. |
| Ctrl+Q or `:quit` | Exit the frontend and clean up its owned connection/local host. |

A disconnect or missing reply can leave an operation **uncertain**. Observe its
original key and use an explicit same-key retry when appropriate; a missing
observation does not prove nonexecution. Do not resubmit with a fresh key to
recover a lost reply. `:abandon` does not cancel or undo work; after abandoning
custody, reset or create a new instance before submitting more source. Client
reconnection is restricted to the same host, principal and authentication kind.
The terminal does not offer a command for switching hosts or adopting a different
capture in place.

## Use a configured Project and its modules

A Project makes an admitted module environment available to interactive input.
The entrypoint selects the graph and contract; it does not execute the entry
file into top-level REPL bindings. For example, a Project exposing `Util` can
be probed with `Task.pure(Util.value)`, followed by an explicit `:run`.

After quitting the previous REPL, create this small Project in the same shell
where `$project` still names the offline workspace:

```sh
mkdir -p "$project/src"
printf '%s\n' 'let value = 8' > "$project/src/util.chatml"
printf '%s\n' 'let dependency_value = Util.value' \
  'let main input = Task.pure(input)' > "$project/src/main.chatml"
cat > "$project/chatml.project.json" <<'EOF_PROJECT'
{
  "schema_version": 1,
  "namespace": "repl-example",
  "source_dirs": ["src"],
  "default_environment": "default",
  "environments": {"default": {"modules": {"Util": "self.Util"}}},
  "entries": {"main": {"source": "src/main.chatml", "target": "one_off_v1"}}
}
EOF_PROJECT
```

The matching policy changes both the Authoring selection and Developer source
selector. The former uses its configuration's `entry` record; the latter uses
the typed `Local_project`/`Entry` representation.

<details>
<summary>Copy the complete Project policy</summary>

```sh
cat > "$project/project-policy.sexp" <<EOF_POLICY
(project_sources ((external_roots ("$project"))))
(authoring_project_access
 ((read_grants (((root "$project") (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root "$project") (project chatml.project.json)
                (request (entry ((name main) (environment default))))
                (target one_off_v1))))))
(developer
 ((selections
   (((source
      (Local_project (root "$project") (project chatml.project.json)
       (request (Entry (name main) (environment (default))))))
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
EOF_POLICY
```

</details>

Launch with the Project selection:

```sh
chatml-repl --local --developer-policy "$project/project-policy.sexp" \
  --prompt-file "$project/agent.chatmd" --workspace "$project/workspace" \
  --transient --project chatml.project.json --entry main --environment default
```

Submit `Task.pure(Util.value)` and explicitly run `:run 1`; the Task result is
`8`. This probes the configured `Util` module. The entry file's
`dependency_value` is not an interactive top-level binding.

For another Project, use its real manifest, entry and environment in **both**
the host policy and launch arguments. Omit `--environment` to use its declared
selection, with a matching policy request. Choose source roots with
`--root-index N`; repeat `--dependency-root-index N` for additional already
installed dependency roots. These flags select roots authorized by the host and
do not add directories to its policy. Dependency roots require a Project route,
not `--source`.

Configured value dependencies initialize once when the REPL first needs their
checked graph, with shared module identity preserved. Successful later
submissions do not rerun those initializers. The instance fixes the graph,
module implementations and environment for its lifetime. Reset starts fresh
module/runtime state from that same capture. A new capture requires a new
instance. See [configured ChatML definitions](chatmd-authoring-definitions.md) for
Project-backed scripts, module exports and environment selection.

## Connect to an existing host

Remote mode connects to an already configured Developer REPL service. Its
operator must install the source roots and exact source/target/principal
selection, enable `repl Standard_v1` and portable access, and grant the needed
method scopes. The prompt and workspace below are **catalog definition names**
on that host, not local filesystem paths.

```sh
chatml-repl --connect https://HOST/BASE \
  --bearer-token-file /absolute/private/token \
  --expected-server SERVER_ID \
  --prompt-definition PROMPT_NAME --workspace-definition WORKSPACE_NAME \
  --root-index 0 --source check.chatml
```

Replace the uppercase placeholders with the host's endpoint, server ID and
admitted definitions. Use the operator-provided endpoint URI; for a Unix socket,
use `--connect unix:///absolute/agent.sock` with that socket's authenticated
profile. A Project uses `--project ... --entry ... [--environment ...]` instead
of `--source`. `--bearer-token-file` supplies an HTTP credential; it does not
configure Unix identity. Do not combine `--local` and `--connect`.

If connection succeeds but the source is denied, have the operator compare the
selected principal/authentication kind, root index, member or Project entry,
compiler target, prompt/workspace definitions and installed method grants.
Successful Initialize alone does not authorize a capture or Task execution.
See [host configuration](../agent-server/configuration.md) and the
[protocol reference](../agent-server/protocol.md) for authentication and
capability negotiation.

## Continue from the REPL to an editor workflow

An editor can capture its unsaved revision, Analyze it without running code,
explicitly save or upload it, then choose Developer Check, Fixture or Live.
These are reusable services; the terminal above does not provide editor
completion, a project browser or trace controls. Follow
[authoring and execution services](chatml-development-services.md) for the
end-to-end API sequence, diagnostics, navigation, highlighting, immutable
uploads, recovery and execution-mode distinctions.

For exact parser and state guarantees, see
[REPL submission and captured state](../development/chatml-repl-submission.md).
For optional read-only trace pages, see
[bounded execution tracing](../design/chatml-execution-tracing.md).
Debugger breakpoints, stepping, variable inspection and time travel are not
shipped features of this terminal or trace protocol.
