# Ctx — host execution context

The [public interface](../../../lib/chat_response/ctx.mli) describes the immutable
environment, filesystem directory, launch tool directory and cache record used by
shared chat-response components.

`Ctx.create ~env ~dir ~tool_dir ~cache` supplies these explicitly.
`Ctx.of_env ~env ~cache` uses the environment filesystem and cwd. Keep the
referenced Eio capabilities alive for the runtime lifetime. Record access is not
a new authorization grant.

## Paths and execution

`tool_dir` captures the host launch directory unless the embedder overrides it.
It is not necessarily the selected daemon workspace or a shell command's cwd.
Shell commands use their compiled runtime/host path context. Relative default
`read_file` roots follow `tool_dir`; imports/nested sources use retained source
provenance. A connected client's cwd cannot change daemon tool roots.

See [all seven path variables](../../agent-server/sessions-and-workspaces.md#workspaces-and-paths)
and [shell host integration](../../guide/chatmd-shell-host-integration.md).
Do not copy older examples invoking a nonexistent public `Tool.run` helper;
tool invocation belongs to the configured runtime and permission boundary.

## Cache and ownership

The context cache supports shared runtime operations. Maintained MCP discovery
has its own per-connected-declaration/client cache rather than a global URI
entry; see [discovery lifetime](tool.doc.md#cache-invalidation-strategy).
Use [agent embedding](../../agent-server/embedding.md) for new daemon/local hosts.
