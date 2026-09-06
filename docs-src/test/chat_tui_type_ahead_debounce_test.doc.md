# Typeahead coordinator and UI tests

Fast offline tests inject a completion provider and an Eio clock into the shared
`Type_ahead_ui` / `Type_ahead_controller`. They replace the retired legacy
operation-ID tests with attachment/context/generation/draft/cursor admission
and completion checks, off/manual/auto behavior, cancellation serialization,
privacy bounds, editor-only acceptance, and redacted errors.
An exact-prompt regression pins the original partial-word completion example,
explicit context/draft delimiters, insertion-point placement with trailing text,
and default exclusion of visible history.

See [the verification record](../development/typeahead-verification.md) for the
separate opt-in real-PTY checks in each host. These pure/fake-provider tests run
under normal `dune runtest`; no paid APIs or ordinary user stores are accessed.
