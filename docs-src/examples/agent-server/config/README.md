# Generated configuration fixtures

Run the [setup helper](../README.md) to create `unix.sexp`, `http.sexp`,
`tokens.sexp`, and private client credential files under an empty disposable root.
The checked-in generator is the executable configuration fixture: paths are
absolute, token hashes come from fresh credentials, and both daemon configs pass
the real validator. Static copies containing machine paths or reusable secrets
are deliberately not checked in.

See the [annotated complete config](../../../agent-server/configuration.md) for
every optional section and the [source appendix](../../../agent-server/operator-contracts.md)
for the exact config record and wire/header contracts.
