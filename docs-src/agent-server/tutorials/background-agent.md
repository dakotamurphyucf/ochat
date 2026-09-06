# Background work without a connected client

This first example uses a timer, not a paid model request. Prepare the
[private tutorial directory](../../examples/agent-server/README.md).
Edit the private `unix.sexp` prompt entry's `path` to the absolute path of the
tracked [timer.chatmd](../../examples/agent-server/prompts/timer.chatmd).
Keep its configured ID `hello` and workspace `project`; only the source path changes.

Validate and start the daemon as in [the Unix tutorial](unix-daemon.md). In another
terminal create a detached session:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --new-daemon-session --prompt hello --workspace project --detached
```

The moderator schedules `Tick` after ten seconds at session start. Quit the TUI
before ten seconds elapse. The daemon, not the client, delivers the event; the
handler requests session end with `scheduled tutorial stop`. List/reconnect after
the timer fires and inspect session/schedule state. Do not submit a user message:
that would add an unrelated model turn to an intentionally offline example.

With the stdio client, `schedule.list` takes the returned `session_id` and positive
`limit`; `session.get` shows session state. Initialize a new connection before
reattachment. If using a TUI to inspect a stopped session, attachment is not a
request to restart its script.

## Extend to asynchronous model work

The [orchestration guide](../chatml-orchestration.md) describes `Model.spawn`,
completion events, budgets, jobs and cancellation. Use the complete tested
[background recipe example](../../../test/agent_server_e2e/scenarios/background_scenario.ml)
as an embedding/script reference: `agent_prompt_v1` takes a JSON object containing
the prompt, input and `is_local`, and completion handlers dispatch by job ID.
Replace fake-provider setup only after choosing real credentials/model/budget.

For an unattended production script, configure narrow tools, explicit permission
fallback, job limits and misfire policy. Keep detached liveness if no client must
own it. Check `job.list/get` and `schedule.list/get` after reconnect; cancel with a
writable authorized attachment. Stop the daemon normally when done. On restart,
durable scheduling intent and delivery records recover, but in-flight external
effects are not automatically safe to rerun.
