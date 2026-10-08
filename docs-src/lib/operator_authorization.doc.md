# Original operator authorization

`ochat.operator_authorization` holds the host-only authorization proof for the
original authenticated actor. The opaque value pairs a validated protocol
principal with a trusted, non-yielding currentness guard. It has no wire codec,
token bytes, token digest, or reconstruction from client attributes.

## Authentication ownership

Static bearer authentication captures the expiry of the exact matched immutable
record. A later credential with the same principal and scopes does not extend
that record's authority. The guard reads the trusted host clock and rejects the
original record once its expiry is reached. A static record explicitly configured
without expiry uses `nonexpiring_static`; a trusted local process context uses
`trusted_local`. These constructors express separate host policies.

Custom bearer and reverse-proxy authentication must supply an explicit currentness
policy for autonomous provider operations. A reverse-proxy policy must preserve
the authenticated principal's identity, authentication kind, scopes and
attributes. A legacy remote principal-only validator can still authorize ordinary
RPC methods, but its provider authorization proof fails closed. The host must not
recover currentness by finding another credential with a matching principal.
Unexpected policy exceptions propagate rather than becoming authorization success.

## Request and flow lifetime

A reused HTTP logical connection retains its initial principal authority. Each
HTTP request carries its own authenticated proof through dispatch without changing
the shared connection's proof. The request principal must match the connection
principal, including scopes and attributes. A batch shares the proof authenticated
for that request. Concurrent requests cannot replace the proof captured by an
older autonomous flow.

The provider operator port checks currentness before calling the trusted provider
service. The service retains that original actor proof across asynchronous login
and checks it again at the registry’s final locked commit admission, after staging
and metadata reload. Authentication at flow start does not authorize a new
commit after the original grant expires. Filesystem publication already admitted
by that check can finish after expiry; the guard cannot yield.
Callback guards must not yield or perform authentication lookup or I/O; the proof
is not a new credential or a lower credential-registry authorization mechanism.

## Integration

Use `Connection_context.create_authenticated` when the host has authenticated an
actor. HTTP dispatch supplies the proof for the current request; socket and stdio
connections retain their admitted proof. The compatibility principal-only
constructor cannot authorize provider operations.

See [provider administration](provider_operator.doc.md) for the runtime ownership
and credential lifecycle built on this proof.
