# Private Unix daemon and multiple TUI clients

Create a durable session, disconnect its client, and reattach to the same history.

## Prerequisites and command context

Complete [installation](../quickstart.md) and [private example setup](../../examples/agent-server/README.md). All commands use `dune exec` from the repository root with the active opam environment. Terminal A owns the daemon and provider environment; clients run in separate terminals with the same absolute `OCHAT_DEMO`. Discovery and attachment are offline; submitting messages invokes a billable provider.

Read the current [provider TLS and permission boundaries](../permissions-and-security.md)
before model work or deployment. [Build troubleshooting](../troubleshooting.md)
includes the Apple Silicon/OpenBLAS setup path.


## 1. Prepare

Complete [example setup](../../examples/agent-server/README.md), retaining the
absolute `OCHAT_DEMO` directory. Provider credentials are needed only for model
work, and belong in the daemon environment, not just the TUI environment.

```sh
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/unix.sexp" -validate-only
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/unix.sexp" -print-config
```

Validation must exit successfully. The printed normalized config contains paths
and identity configuration; treat it as operator data when sharing diagnostics.

## 2. Start Terminal A

```sh
export API_URL=api.openai.com
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/unix.sexp"
```

Leave this foreground process running. The socket parent created by `mktemp` is
private. Do not run the HTTP config concurrently: both configs own the same store.

## 3. Create in Terminal B

Set `OCHAT_DEMO` in this terminal to the path from step 1, then:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --new-daemon-session --prompt hello --workspace project --detached
```

The CLI resolves configured names `hello` and `project` to protocol catalog IDs.
Submitting a message now calls the provider. Quit with the normal TUI `:q`
workflow: only this client disconnects.

## 4. List and reattach

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" --list-sessions
```

Copy the returned opaque session ID into `OCHAT_SESSION`, then:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --session "$OCHAT_SESSION"
```

Open another terminal with the same directory/session variables and repeat with
`--read-only`. It receives updates but cannot send messages or approve tools.
Unix clients authenticate as the same effective user; read-only attachment is
not a separate low-scope credential. For transcript-only credentials use HTTP.

## 5. Owner-bound variation

Create another session with `--owner-bound --disconnect-grace-ms 30000` instead
of `--detached`. It has an exclusive owner lease; loss starts a grace interval,
not an instantaneous stop at every network hiccup. Reconnect within permitted
lease/reclaim rules. Do not use `--read-only` with owner-bound mode.

## 6. Stop and shut down

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --stop-session "$OCHAT_SESSION"
```

This stops session work; it does not delete the durable transcript. Quit attached
clients. Use Ctrl+C in Terminal A for graceful daemon shutdown. Starting the same
config again reopens the store. Recovery does not resume an interrupted tool's
process; see [session recovery](../sessions-and-workspaces.md).

Archive the private directory if you want to retain this tutorial's state, or
remove only that exact directory after shutdown. No normal Ochat state was used.

## Checkpoint, troubleshooting, and next step

Success means a detached session remains listed after the first client quits, a new client attaches to the same opaque ID, and the store survives a normal daemon restart. If connection fails, check the foreground daemon, socket path, private-directory permissions, and matching shell variables. A missing session ID is not the configured prompt name. Use [troubleshooting](../troubleshooting.md); do not run two daemons against the same store. Follow step 6 for shutdown and cleanup. Next, [schedule background work](background-agent.md) or [connect a stdio client](stdio-client.md).
