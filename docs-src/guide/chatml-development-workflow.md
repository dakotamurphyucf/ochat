# Develop ChatML with the terminal REPL

Start `chatml-repl`, enter an expression, and press Enter. The terminal colors
ChatML as you type and displays the inferred type beside the result. Bindings
persist between submissions, suggestions come from the current session, and
Tasks run when you explicitly choose to run them.

This walkthrough starts with a temporary local session: no Project, policy file,
provider account, or model request is needed. You will calculate a value, keep a
binding, inspect structured results, complete a module member, search your
history, and run a Task. The [configured local](#start-with-an-explicit-local-configuration),
[Project](#use-a-configured-project-and-its-modules), and
[remote](#connect-to-an-existing-host) workflows remain available below.

## Before you start

You need an interactive Unix-style terminal and an Ochat installation that includes
`chatml-repl` and its companion `ochat-agent-stdio`. Follow the **Build** section
of [Build and configure Ochat](../agent-server/quickstart.md#build), including
`dune build @install` and `dune install`, and keep that opam environment active.

```sh
chatml-repl -help
ochat-agent-stdio -help
```

If a command is missing, load the opam environment where Ochat was installed.
Both commands should come from the same build. From a checkout, build both
executables and run the frontend with its matching host:

```sh
dune build bin/chatml_repl.exe bin/ochat_agent_stdio.exe
dune exec bin/chatml_repl.exe -- \
  --stdio-executable "$PWD/_build/default/bin/ochat_agent_stdio.exe"
```

The frontend uses a full-screen terminal. Launch it interactively rather than
piping a script into it.

<a id="start-the-terminal-repl"></a>

## Start without configuration

```sh
chatml-repl
```

Wait for `[ready]`. The header identifies `Standalone ChatML (temporary)` and
states that the Standard API is available without model or shell access. This
session uses its own temporary source and local host. It does not discover a
Project in your working directory. Its private startup files are cleaned up
when that host closes; bindings and history last for this invocation.

<a id="submit-values-and-inspect-their-types"></a>

### Your first calculation

Copy only the **REPL input** below into the terminal, then press Enter. Input
panels contain exact ChatML source; **Display only** panels show excerpts of the
terminal's response. Prompts, window chrome, and omitted lines are not source to
paste.

<div class="repl-example">

```chatml-repl title="REPL input · press Enter"
40 + 2
```

```text title="Display only · inferred type and value" frame="terminal"
- : int = 42
```

</div>

Read this as “the expression has type `int`, and its value is `42`.” There is no
separate run or type-inspection step for an ordinary expression. The terminal
also reports `finished` and returns to an empty `#` prompt.

### Keep a binding between submissions

<div class="repl-example">

```chatml-repl title="REPL input · first submission"
let saved = 8
```

```text title="Display only · accepted binding" frame="terminal"
val saved : int
```

</div>

Now submit a second expression:

<div class="repl-example">

```chatml-repl title="REPL input · next submission"
saved + 1
```

```text title="Display only · the binding persists" frame="terminal"
- : int = 9
```

</div>

A declaration displays its authoritative binding signature. The frontend does
not invent a binding's value from the declaration statement's result. To inspect
it explicitly, enter `:type 1 binding 1` immediately after the declaration.
After `saved + 1`, `:type 1` inspects that expression instead. Statement and
binding indexes start at 1 within the displayed submission; earlier submissions
and built-ins do not become extra binding indexes.

The screen shows the current bounded receipt, rather than an unlimited transcript.
Use history to recall source you submitted earlier.

### Infer a function signature

You can define a function without writing its type:

<div class="repl-example">

```chatml-repl title="REPL input · define a function"
let add_one x = x + 1
```

```text title="Display only · inferred function signature" frame="terminal"
val add_one : fun(int) -> int
```

```chatml-repl title="REPL input · call the accepted function"
add_one(41)
```

```text title="Display only · function result" frame="terminal"
- : int = 42
```

</div>

The `+ 1` operation constrains the argument and result to `int`; `fun(int) -> int`
is the inferred function type. The binding stays available to later submissions
until reset. Reading a retained type with `:type` does not call the function
again.

## Read strings, arrays, records and tuples

The result formatter follows the value's runtime structure. Strings retain
quotes and escapes, so a value containing a newline is distinguishable from two
terminal output lines:

<div class="repl-example">

```chatml-repl title="REPL input · an escaped newline"
"line one\nline two"
```

```text title="Display only · quoted string value" frame="terminal"
- : string = "line one\nline two"
```

</div>

Try an array and a record as separate submissions:

<div class="repl-example">

```chatml-repl title="REPL input · array"
[1, 2, 3]
```

```text title="Display only · array type and elements" frame="terminal"
- : int array = [1, 2, 3]
```

```chatml-repl title="REPL input · record"
{ name = "Ada"; scores = [1, 2, 3] }
```

```text title="Display only · record fields and their types" frame="terminal"
- : {name: string; scores: int array} = { name = "Ada"; scores = [1, 2, 3] }
```

</div>

Tuples use ChatML's `(| ... |)` syntax:

<div class="repl-example">

```chatml-repl title="REPL input · tuple"
(|8, "eight"|)
```

```text title="Display only · tuple type and value" frame="terminal"
- : (int * string) = (|8, "eight"|)
```

</div>

Larger nested values can expand across lines. Preview byte, node and depth
budgets keep large or cyclic values bounded; omitted content has an explicit
truncation marker. Functions and modules remain opaque. Formatting never calls
a user printer or executes a Task. Type display has separate bounds, so a
truncated or unavailable type is a presentation limit, not a different inferred
type. See [ChatML](../chatml/README.md) for the language itself.

## Compose several lines

**Enter submits. Shift+Enter inserts a newline when your terminal reports that
key combination; Alt+Enter is also supported.** Some terminals send an ordinary
Enter for Shift+Enter. Use Alt+Enter or bracketed paste if that happens.

For a deliberate multiline submission, type the first line below, insert a
newline with one of those shortcuts, type the second line, then press Enter:

```chatml-repl title="REPL input · one multiline submission"
let shift_one = 3
shift_one + 1
```

```text title="Display only · two completed statements" frame="terminal"
val shift_one : int
- : int = 4
```

Bracketed paste inserts a whole fragment literally, including newlines and
leading colons. It does not submit. Review the buffer, then press Enter.
Ctrl+S and a final-line `:submit` are additional explicit submission controls.

An unfinished fragment such as `let doubled =` reports `need more input`.
The acknowledged prefix remains visible and read-only; enter the missing
continuation in the editable suffix and submit that suffix. The service appends
it to the pending fragment. Completion pauses until the fragment finishes or
is discarded with `:reset`. Reset also clears earlier interactive bindings;
see [reset and close](#reset-close-and-keyboard-commands). If you want to change
the already acknowledged prefix, reset the pending session and start again.

The parser must accept the whole fragment before any statement runs. The REPL
does not guess missing delimiters or turn every malformed prefix into a request
for more input.

## Complete a name without running it

Type `Task.p` and wait for the session suggestion. The following is a cropped
screen excerpt, **not** a source block:

```text title="Display only · completion menu" frame="terminal"
# Task.p
session:  pure
```

Press Tab to insert the selected member. The draft becomes `Task.pure`; it has
not been submitted. Finish the arguments, for example `Task.pure(saved)`, then
press Enter when ready. Up/Down select another suggestion, Shift+Tab moves back,
and Escape dismisses the menu. Tab can reopen suggestions after dismissal.
Enter submits the draft as written; it does not silently accept a suggestion.

Menu labels explain where candidates came from: `session:` for names read from
the committed environment, `local:` for commands, keywords and retained binding
hints, or `session + local:` when the visible menu combines them. Own-line
commands such as `:help` and help topics can also be completed.

Completion is read-only. It does not typecheck or execute an unfinished draft,
initialize a module, or turn a suggestion into a permission grant. Standard
modules and committed module values can expose members; an unchecked configured
module can report them as unavailable. The terminal reports limited or unavailable
session reads and ignores answers for an older draft or session revision.

## Recall work and find help

With a completion menu closed, Up recalls previous submissions; Down moves
forward and restores your draft after the newest history entry. This history is
local to the current frontend invocation.

For a search, press Ctrl+R and type `shift_one`. The search bar previews a match.
Ctrl+R cycles older matches; Backspace edits the literal search query. Press Enter
once to load a match as an **unsubmitted draft**. Edit it or press Enter a second
time to submit. Escape or Ctrl+C cancels search and restores the exact draft you
had before entering it. History recall and search are available when the session
is idle, its current revision is known, and no acknowledged incomplete fragment
is pending.

Enter `:help` to see the available topics. For more detail, enter an individual
command such as `:help shortcuts`, `:help completion`, `:help types`,
`:help history`, `:help tasks`, or `:help recovery`. `:help commands` explains
command arguments and indexes. These are terminal commands, not ChatML
expressions or shell commands.

## Retain a Task, then run it

An expression returning a Task produces a value first. It does not execute the
Task's work:

<div class="repl-example">

```chatml-repl title="REPL input · retain a Task value"
Task.pure(saved)
```

```text title="Display only · the Task is still inert" frame="terminal"
- : task<int> = <Task pure>
```

</div>

The same receipt shows a `Task 1: :run 1 to execute` hint followed by an opaque
reference. The reference bytes vary between sessions and are omitted here. To
run the displayed Task, enter this command on its own line and press Enter:

```text title="REPL command · explicitly execute the displayed Task"
:run 1
```

```text title="Display only · completed Task" frame="terminal"
Task succeeded 8
```

`Task.pure(saved)` produces this result without a model or shell capability.
A Task using native or model operations still needs the selected host's grants.
The default standalone session has neither of those capabilities.

The number in `:run N` selects a Task in the displayed reference page, not a
statement. `:refs` pages through retained references for the displayed submission.
Submitting code, completing names, inspecting a type and paging references never
implicitly run a Task. While one is active, cancel or await its terminal result
before starting another submission or Task.

## Incomplete input, errors and committed state

A type error shows its message and a source-positioned excerpt when the frontend
has the exact matching source bytes:

<div class="repl-example">

```chatml-repl title="REPL input · a type mismatch"
1 + true
```

```text title="Display only · cropped error excerpt" frame="terminal"
stopped: Cannot unify bool with int
1 | 1 + true
    ^^^^^^^^
```

</div>

The full screen includes the source identity and byte range between the message
and excerpt. That dynamic identity is omitted above. The terminal marks the
range in color as well as with carets, and accounts for Unicode and tab display
widths. A diagnostic from a different captured source retains its source/byte
metadata; the frontend does not guess a local excerpt from a filename or stale
buffer.

Try `let saved = 1 + true`. The failed typecheck does not replace the earlier
`saved`; submitting `saved` still yields `8`.

Runtime failure has a different boundary. Submit these statements together:

```chatml-repl title="REPL input · a successful prefix followed by a runtime error"
let before_failure = 42
1 / 0
```

The result is `stopped`: the first statement completed and its binding remains.
A subsequent `before_failure` evaluates to `42`. The failed statement publishes
no new binding, but mutations or external effects that already happened cannot
be rolled back. Inspect the completed statements before retrying anything with
effects. Rebinding later also leaves existing closures attached to the binding
they originally captured. The wire revision advances for a serialized attempt;
it is not a count of successful bindings.

## Reset, close and keyboard commands

Enter `:reset` while idle to discard interactive bindings, unfinished input and
old Task handles and start a fresh epoch on the **same captured source**. After
reset, `saved` is no longer bound. Reset does not reread changed files from disk
or switch the Project/environment. Quit and launch a new instance for a new
capture. If work is active, cancel it and wait for cleanup before resetting.

| Key or command | Action |
| --- | --- |
| Enter | Submit source, execute an own-line command, or load a history-search match without submitting it. |
| Shift+Enter / Alt+Enter | Insert a newline; Shift+Enter requires terminal modifier support. |
| Ctrl+S / final-line `:submit` | Submit the editable ChatML fragment. |
| Tab | Accept a suggestion, or open completion when no menu is displayed. |
| Up/Down | Select a suggestion; otherwise move through local submission history. |
| Shift+Tab / Escape | Select the previous suggestion / dismiss the menu. |
| Ctrl+R | Start reverse history search or cycle older matches. |
| Left/Right, Home/End, Backspace/Delete | Move and edit at text boundaries. |
| `:type N` / `:type N binding M` | Inspect a retained expression/binding type from the displayed submission. |
| `:refs` / `:run N` | Page Task references / explicitly execute a displayed Task. |
| Ctrl+C / `:cancel` | Cancel exact active work; Ctrl+C in history search restores the prior draft. |
| `:observe` | Read the tracked submission or Task receipt. |
| `:reset` / `:close` | Reset the captured instance / close its evaluator while idle. |
| `:retry` / `:abandon` | Retry the exact uncertain request / relinquish its local retry custody. |
| `:help [topic]` | Read command, shortcut, completion, type, Task, history or recovery help. |
| Ctrl+Q / `:quit` | Exit and clean up the owned connection and local host. |

A missing reply can leave an operation **uncertain**. Observe its original key
and use an explicit same-key retry when appropriate; a missing observation does
not prove nonexecution. Do not send a fresh-key copy to recover a lost reply.
`:abandon` does not cancel or undo work. After relinquishing custody, reset or
create a new instance before submitting more source. Reconnection is restricted
to the same host, principal and authentication kind.

If you edit a draft while its submission is uncertain, `:retry` still sends the
original request. After an incomplete acknowledgement, edits appended to the
original text become the new unsent suffix; the acknowledged bytes are not
repeated. If you rewrote the draft instead, it is preserved as an **unsent
suffix** with a review notice. Reconcile it with the read-only accepted prefix
before pressing Enter; use `:reset` to start over.

## Start with an explicit local configuration

Enter `:quit` to return to your shell before running the commands below.

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

For ordinary experiments, the temporary standalone session is enough. Use a
configured Project when interactive source needs your admitted modules, and an
existing host when its operator owns the source and execution permissions.
Providing only part of a configured launch is an error; the frontend does not
silently substitute the standalone environment.

An editor can also capture an unsaved revision, Analyze it without running code,
explicitly save or upload it, then choose Developer Check, Fixture or Live. Follow
[authoring and execution services](chatml-development-services.md) for those
reusable APIs, diagnostics, navigation, highlighting, immutable uploads and
execution-mode distinctions. Terminal completion is not a project browser or a
debugger.

For exact parser and state guarantees, see
[REPL submission and captured state](../development/chatml-repl-submission.md).
For optional read-only trace pages, see
[bounded execution tracing](../design/chatml-execution-tracing.md).
