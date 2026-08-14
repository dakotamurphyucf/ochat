# `Chat_tui.Shell_security_snapshot`

`Shell_security_snapshot.create` projects live `Agent_runtime` inspection and
typed session shell state into immutable, redacted UI data. It includes
requested/live manifest identity, administrative/signature/audit status,
effective runtime summaries, command/manifest grants, and interrupted requests.

The snapshot never contains raw secret values or mutable runtime callbacks.
This makes it safe to replace atomically on the UI domain after startup or a
generation-matching management refresh.
