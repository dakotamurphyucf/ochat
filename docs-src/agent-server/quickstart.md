# Quickstart

## Build

Use the repository's opam environment. The package currently requires OCaml
5.1 or newer and Dune 3.21 or newer; dependencies and pins are declared in
[dune-project](../../dune-project) and the generated `ochat.opam`.

```sh
opam install . --deps-only
eval "$(opam env)"
dune build bin/chat_tui.exe bin/ochat_agent_server.exe bin/ochat_agent_stdio.exe
```

After installing the package, use `chat-tui`, `ochat-agent-server`, and
`ochat-agent-stdio`. Before installation, use the explicit `dune exec bin/NAME.exe
-- ...` commands below. See [build troubleshooting](../guide/build-troubleshooting.md)
for native dependencies, including platform-specific numerical libraries.

## Provider environment

Set `OPENAI_API_KEY` privately in the launching process environment. Do not put it
in a tracked prompt, config, command transcript, or screenshot. For the current
Responses client, direct OpenAI access uses this hostname setting:

```sh
export API_URL=api.openai.com
```

This overrides an ambient proxy host. Do not append `/v1`: the client appends
`/v1/responses` itself. The underlying POST helper also accepts an explicit origin
such as `http://127.0.0.1:PORT` for a controlled local proxy/fixture. Use the bare
hostname above for ordinary direct access. This describes `lib/openai/responses.ml`, not a
universal provider setting: embedding requests use `EMBEDDINGS_HOST`, for example.
Model availability depends on your account. Change the model in the example
prompt if necessary. Sending messages calls the model and incurs charges;
configuration validation and catalog discovery do not.

## Local TUI: no daemon

From the repository root:

```sh
dune exec bin/chat_tui.exe -- --no-config --local \
  -file docs-src/examples/agent-server/prompts/hello.chatmd
```

The workspace is the current directory. This native local mode is process-bound
and transient. It does not offer the legacy `--session` persistence options.
See the [local walkthrough](tutorials/local-tui.md) for keys and compatibility.

## Durable daemon

Prepare a private directory using the [tracked setup example](../examples/agent-server/README.md),
then leave this running in Terminal A:

```sh
dune exec bin/ochat_agent_server.exe -- -config "$OCHAT_DEMO/unix.sexp"
```

In Terminal B, set `OCHAT_DEMO` to the same printed path, then:

```sh
dune exec bin/chat_tui.exe -- --no-config --connect "unix://$OCHAT_DEMO/agent.sock" \
  --new-daemon-session --prompt hello --workspace project --detached
```

Quitting this TUI detaches it; the daemon session remains. Follow the
[Unix daemon tutorial](tutorials/unix-daemon.md) to list/reattach and stop it.
For integrations use [stdio](tutorials/stdio-client.md) or
[HTTP](tutorials/http-client.md). Shell tools require a separate
[authorization workflow](tutorials/shell-agent.md).
