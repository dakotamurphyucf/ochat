# mcp_discovery_cache

Five-minute discovery state owned by one connected MCP tool declaration/client. Eio clock/mutex serialize expiry/loading; invalidation and cancelled-load recovery do not share another identity's data.

This cache primitive does not guarantee notification delivery or rebuild active
tool schemas. Its current host wiring has competing notification consumers and
no live catalog refresh; see the [tracked gaps](../../development/code-documentation-audit.md#mcp-discovery-and-notifications).

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_response/mcp_discovery_cache.mli) · [implementation](../../../lib/chat_response/mcp_discovery_cache.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
(** One discovery cache per connected MCP declaration, never process-global.
    Its loader closes over exactly one authenticated client and tool filter.
    Different declarations, credentials, transports or runtimes cannot share
    entries or invalidation. The owning runtime switch bounds client/listener
    lifetime. Expiry uses the host's Eio clock; failed/cancelled loads are not
    cached, and their exceptions propagate only after the mutex is unlocked so
    subsequent discovery and invalidation remain usable. *)
type 'a t

val create : now:(unit -> float) -> load:(unit -> 'a) -> 'a t
val get : 'a t -> 'a
val invalidate : 'a t -> unit
```
