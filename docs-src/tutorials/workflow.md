# Stop after three completed turns with ChatML

Add an event handler that asks the host to end a session after three completed
turns. Finish [your first local agent](../agent-server/tutorials/local-tui.md)
and [provider setup](../agent-server/quickstart.md) first. ChatML is experimental;
this example targets the native local TUI and uses no UI-only effects, shell,
background jobs, or approval suspension. Model work is billable. Read the current
[provider transport boundary](../agent-server/permissions-and-security.md).

## 1. Prepare the prompt and its script

From the repository root with the installed command available:

```sh
OCHAT_WORKFLOW=$(mktemp -d /tmp/ochat-three-turns.XXXXXX)
cp -R docs-src/examples/learning/three-turns/. "$OCHAT_WORKFLOW/"
```

Or extract the **Three-turn workflow** catalog bundle and set `OCHAT_WORKFLOW`
to its absolute `three-turns/` directory. Both source files are required. The
[ChatMD prompt](../examples/learning/three-turns/three-turns.chatmd) is:

```xml
<config model="gpt-5.6-sol"/>
<developer>Answer the user's request briefly. You have no tools.</developer>
<script language="chatml" kind="moderator" id="three-turns" src="three-turns.chatml"/>
```

Its external [three-turns.chatml](../examples/learning/three-turns/three-turns.chatml) script is:

```chatml
type state = int
type event = [ `Session_start | `Turn_end ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Turn_end ->
      let completed = st + 1 in
      if completed >= 3 then
        Task.bind(Runtime.end_session("Three-turn session finished"),
          fun ignored -> Task.pure(completed))
      else Task.pure(completed)
    | _ -> Task.pure(st)
```

The script starts at zero and increments on `Turn_end`; other delivered events
leave its state unchanged. The third completed turn requests `Runtime.end_session`.
This is executable host workflow logic, not a reminder to the model. A completed
turn can contain multiple model/tool calls: this is **not a dollar spending cap**.

## 2. Complete three turns

Use the installed command from the prepared directory:

```sh
(cd "$OCHAT_WORKFLOW" && chat-tui --no-config --local -file three-turns.chatmd)
```

Submit `Say one.`, wait until the turn finishes, then submit `Say two.` and finally
`Say three.`, waiting each time. Use Insert mode and Meta+Enter as in T01; Esc, `:w`, Enter is the alternate submit path.
The first two completed turns should leave the session available. After the third,
inspect the stopped session state and end reason `Three-turn session finished`.
The TUI need not close automatically; quit it normally. Exact responses vary.
An interrupted or failed request is not proof that a completed-turn event occurred.

## Troubleshooting

A missing-script error usually means only the ChatMD file was copied. Preserve
`three-turns.chatml` beside it and reopen after changing either file. A parse or
runtime error needs its diagnostics; do not assume the model implements the counter.
If the expected stop does not appear, confirm three turns completed, inspect the
session state, and compare the script to the source above. See
[moderator events](../guide/chatml-moderator-runtime.md) for event delivery and state.

## Finish and continue

Press Esc, type `:q`, and press Enter after work stops. Native local state is
process-bound and is not resumed on the next launch; a new session resets this
counter. Archive or remove only the recorded temporary directory after exit.

For tools with explicit command authority, continue to
[the narrow shell tutorial](../agent-server/tutorials/shell-agent.md). For a
session that survives the client, use [the Unix daemon tutorial](../agent-server/tutorials/unix-daemon.md)
and then [background timers](../agent-server/tutorials/background-agent.md).
The [example catalog](../examples/README.md) includes both source files and the license.
