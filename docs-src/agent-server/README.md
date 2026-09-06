# Agent server

An Ochat agent is defined in a [ChatMD text file](../chatmd/README.md), including
its instructions, tools, and optional [ChatML workflow](../chatml/README.md).
The agent server is an optional way to host that agent: keep it running without
an open terminal, share a session between clients, or integrate your own app.

For everyday work in a local repository, start with the
[local TUI](tutorials/local-tui.md). It uses the same agent core without requiring
a daemon. You can introduce server hosting when you need independent lifetimes
or a client protocol.

## What the server adds

The daemon owns sessions and their runtime independently of any one client.
Multiple clients can attach, see live updates, and—with appropriate write
permissions—send messages. Read-only connections can observe without submitting
messages. The server's catalog selects which prompts and workspaces are
available, while permission policies support both interactive and automated use.

A workspace supplies the logical working location used by prompt and tool
configuration. It can be a configured folder or a managed virtual workspace;
it does not itself grant or confine tool access.

Session lifetime and storage are separate choices. Detached agents can outlive
clients; owner-bound agents depend on a client's renewable ownership and grace
period; process-bound local agents end with their host. Durable state supports
restart recovery, but does not preserve a running OS process or resume an
arbitrary in-flight effect exactly where it stopped. Read
[sessions and workspaces](sessions-and-workspaces.md) before choosing a mode.

## Pick an entry point

- **Interactive local work:** [run the TUI over a local prompt](tutorials/local-tui.md).
- **Persistent local agents:** [start a Unix daemon and connect the TUI](tutorials/unix-daemon.md).
- **A subprocess integration:** [use stdio](tutorials/stdio-client.md), either
  hosting locally or forwarding to a daemon.
- **An HTTP integration:** [use authenticated requests and SSE updates](tutorials/http-client.md).

Unix, stdio, and HTTP expose the Ochat agent protocol, not MCP. Maintained MCP
tools can still be part of an agent's ChatMD definition; the deprecated MCP
prompt-serving host is a different feature.

## Guides and references

| Task | Guide |
|---|---|
| Choose an execution mode | [Concepts](concepts.md), [quickstart](quickstart.md) |
| Run without a daemon | [Local TUI](tutorials/local-tui.md), [local stdio](tutorials/stdio-client.md) |
| Run a durable daemon | [Unix daemon tutorial](tutorials/unix-daemon.md), [configuration](configuration.md) |
| Configure the host environment | [Environment variables](environment.md) |
| Integrate a client | [Protocol](protocol.md), [Unix](transports/unix.md), [stdio](transports/stdio.md), [HTTP/SSE](transports/http.md) |
| Manage agents and workspaces | [Sessions and workspaces](sessions-and-workspaces.md) |
| Configure authority and automation | [Permissions](permissions-and-security.md), [shell host integration](../guide/chatmd-shell-host-integration.md) |
| Run background scripts | [ChatML orchestration](chatml-orchestration.md), [tutorial](tutorials/background-agent.md) |
| Deploy, recover, or diagnose | [Operations](operations.md), [troubleshooting](troubleshooting.md) |
| Embed the libraries | [OCaml integration](embedding.md) |
| Verify the system | [Testing](testing.md) |

Tutorial inputs and clients live in [tracked examples](../examples/agent-server/README.md).
The [architecture](../design/ochat-agent-server-spec.md) and
[implementation](../design/ochat-agent-server-implementation-spec.md) specifications
provide deeper design rationale; current executable commands are in these guides.
