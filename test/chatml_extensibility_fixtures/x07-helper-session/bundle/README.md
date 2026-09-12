# Persisted specialists with a scripted response watcher

`agent.chatmd` manages persisted child sessions through a confined shell helper.
Its moderator implements `manage_agent`, `notify_when_agent_responds` and
`cancel_response_watch`. The model receives pending acknowledgements immediately
and correlated notifications when background work finishes.

`native-watch-agent.chatmd` uses the same agent and moderator, replacing the
watcher's read/status/wait adapter with native session tools. Session creation
and other management in this bundle still use the helper. Neither variant needs
a native child-response push subscription. The helper is an optional extension
demonstration, not a dependency of native Ochat session tools.

## Build and copy the complete bundle

From the repository root:

```sh
opam exec -- dune build @test/chatml_extensibility_fixtures/x07-helper-session/bundle/bundle bin/ochat_agent_helper.exe
```

The complete bundle is in
`_build/default/test/chatml_extensibility_fixtures/x07-helper-session/bundle/`.
Copy that directory to a new private working directory outside the repository's
build tree. Keep its ChatMD, ChatML and JSON files together. Dune assembles
`helper-moderator.chatml` from the maintained helper, watcher and coordinator
entrypoint sources; the integration tests load this same output.

In the copied directory, create `public/` and copy
`_build/default/bin/ochat_agent_helper.exe` from the repository into `public/helper`.
Make that copy executable. Put only the reports/data the specialists may read in
`public/`. Keep the agent definitions, server configuration, store, socket and
provider credentials outside `public/`.

## Configure and start a daemon

Copy `server.template.sexp` to `server.sexp`. Replace both
`REPLACE_WITH_HELPER_SHA256` values with the SHA-256 of `public/helper`:

```sh
shasum -a 256 public/helper
```

On systems providing `sha256sum`, that command can compute the same digest.
The template resolves relative paths against the configuration directory. It
grants `session_bridge` the eight listed operations and gives `session_view`
read/status/wait access only. Host grants do not bypass the prompt's shell
manifest or normal tool approvals. Add any additional private paths to both
grants; never put credentials in the helper's public directory or environment.

Using the normal installed Ochat executables, run these from the copied directory:

```sh
ochat-agent-server -config "$PWD/server.sexp" -validate-only
ochat-agent-server -config "$PWD/server.sexp"
```

In another terminal in that directory:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" --new-daemon-session --prompt coordinator --workspace examples
```

Use the interactive permission flow to approve the selected tools and manifest.
Live conversations use your normal provider configuration and may incur model
costs. Building the bundle and running its offline checks make no provider calls.
To compare native polling, change the configured prompt path to
`./native-watch-agent.chatmd`, restart the daemon, and create a new session.

## Try a workflow

Ask the coordinator to create a specialist that reads one of your public reports,
send it a question, and notify you when its response is ready. Before creating a
definition, it can retrieve the installed child-agent documentation through the
helper's `reference` operation and validate its captured sources. The root prompt
contains a complete first reference request, so manual guidance does not depend
on the model already knowing ChatML or the helper protocol.

Retain creation keys for retries, session IDs for subsequent operations and
submission receipts for individual answers. A watch takes a session ID plus
either a receipt or a caught-up output cursor. Its result is one output page;
follow a returned cursor to read more. Cancelling the watch leaves the child
running. Stopping the child preserves its persisted data. Watch timeout, backoff
and retry policy are authored in the watcher source, not special host features.

The native `run_chatml` tool remains available for deterministic composition.
This example deliberately selects manual authoring policy: use the helper's
reference/validation operations before writing code instead of expecting native
documentation tools to be installed automatically.

## Offline qualification

```sh
opam exec -- dune build @test/runtest-agent_server_helper_test
```

The test imports these exact agent definitions, adds separate negative-test tools
and an authored-persistence specialist, and runs both watcher adapters. It checks
real confined helper processes, inherited authority, asynchronous notifications,
concurrent cancellation, pinned moderator state and daemon restart with a fake
provider and controlled host clock. It does not claim real-model authoring quality
or a wall-clock response-time guarantee.
