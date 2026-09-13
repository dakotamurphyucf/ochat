# Chat_response.Tool — declarations to runtime tools

`of_declaration ?shell_registry ?host ~sw ~ctx ~run_agent decl` is the public
conversion boundary; the [interface](../../../lib/chat_response/tool.mli)
specifies its complete signature. A declaration can expand into multiple tools.

## Declaration families

| Family | Runtime behavior |
|---|---|
| Built-in | Resolve maintained built-in definitions and configured file roots. |
| Shell / legacy command lowering | Use the compiled shell runtime/registry and host policy; legacy syntax does not bypass manifest authority. |
| Nested agent | Use the supplied runner with source context; daemon static-relative sources are pinned transitively. |
| MCP | Connect to the declared server and wrap its remote tools under this runtime's identity. |

See [tool syntax](../../overview/tools.md), [shell declarations](../../overview/chatmd-shell-tools.md),
and [agent source pinning](../../agent-server/sessions-and-workspaces.md).
The older helper names shown in past internal documentation are not all public
APIs; use the current interface rather than copying obsolete `custom_fn` calls.

## Cache invalidation strategy

MCP discovery has a five-minute cache owned by one connected declaration/client,
not a process-global URI-keyed TTL-LRU cache. Equal endpoints do not merge
authenticated identities. An Eio clock controls expiry and a mutex serializes
loading. Failed/cancelled loads release the mutex, allowing later retries.
Closing the runtime closes its discovery lifetime.

One declaration-owned listener invalidates on `notifications/tools/list_changed`;
individual wrappers do not consume that shared notification queue. Before silent
or progress-enabled execution, the wrapper reads the cache and verifies that its
captured name has exactly one matching descriptor, including description and input
schema. A changed or removed descriptor raises `mcp.catalog_changed` before
`tools/call`; discovery failure also prevents the call. This applies to inherited
wrappers, which retain the original client and cache.

The advertised tool list remains pinned until runtime recreation. Invalidation
and five-minute expiry trigger lookup refresh, not hot replacement of provider
schemas. This detects changes reported by discovery; it cannot prove that a remote
server's implementation remains unchanged between listing and execution. See the
[remaining refresh work](../../development/code-documentation-audit.md#mcp-discovery-and-notifications).

MCP tool integration is maintained. Only the old MCP server exposing ChatMD
prompts is deprecated; this outbound tool client is not legacy functionality.

## Ownership and security

Keep `sw`, filesystem context and the shell registry alive through invocation.
The host's tool permission gate and shell runtime enforce their distinct
decisions; declaration conversion does not confer authorization. Use configured
read roots, source provenance and principal-scoped outputs. Runtime exceptions/
cancellation must propagate through owned workers, not be mistaken for successful
tool output. See [embedding](../../agent-server/embedding.md).
