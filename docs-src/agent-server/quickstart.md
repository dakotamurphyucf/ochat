# Quickstart

Start here to build Ochat and configure the model provider. You do not need
to write OCaml to define an agent, but building Ochat currently requires the
OCaml toolchain. A daemon is optional; the first tutorial runs in your terminal.

## Before you start

You need Git, an OCaml/opam installation, and a model-provider account with an
API key and access to the model you select. Model requests incur provider
charges. These instructions use a Unix-style shell such as bash or zsh.

Install OCaml and opam using the [OCaml installation guide](https://ocaml.org/install#linux_mac_bsd)
if they are not already available. Ochat requires OCaml 5.1+ and Dune 3.21+;
see [package requirements](../../dune-project) and
[build troubleshooting](../guide/build-troubleshooting.md) for native libraries
and Apple Silicon/OpenBLAS setup.

## Build

If you do not already have an Ochat checkout:

```sh
git clone https://github.com/dakotamurphyucf/ochat.git
cd ochat
```

Run the following setup **inside the Ochat checkout**, not the project you
will eventually ask your agent to work on. If there is no suitable opam switch
for this checkout, create one once:

```sh
opam switch create .
```

Use that switch's environment, install the declared dependencies, and build
and install the command-line tools:

```sh
eval "$(opam env)"
opam install . --deps-only
dune build @install
dune install
```

Check that the tools are available:

```sh
chat-tui -help
```

The help command does not contact a model. Keep this terminal open so its
opam environment remains available. In a new terminal, select the same switch
and load its environment before running Ochat. If `chat-tui` is not found,
check that environment and the installation steps above.

The installed commands include `chat-tui`, `ochat-agent-server`, and
`ochat-agent-stdio`. The tutorials also show `dune exec bin/NAME.exe -- ...`
for running directly from the Ochat checkout.

## Provider environment

If `OPENAI_API_KEY` is already set in this terminal, keep it. Otherwise, use
hidden input in bash or zsh, paste the key, and press Enter:

```sh
read -r -s OPENAI_API_KEY
export OPENAI_API_KEY
```

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

Review [permissions and current transport limitations](permissions-and-security.md)
when choosing a runtime environment. In particular, provider transport behavior
is a property of the Ochat runtime; the documentation website's HTTPS does not
change it.

## Local TUI: no daemon

From the repository root:

```sh
dune exec bin/chat_tui.exe -- --no-config --local \
  -file docs-src/examples/agent-server/prompts/hello.chatmd
```

The workspace is the current directory. This native local mode is process-bound
and transient. It does not offer the legacy `--session` persistence options.
See the [local walkthrough](tutorials/local-tui.md) for keys and compatibility.

Continue with the [first-agent walkthrough](tutorials/local-tui.md) to understand
the prompt, submit a request, and quit cleanly.

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
