# Explicit, offline specification-audit probe

Run manually with:

```sh
dune exec test/agent_server_spec_audit/read_only_probe.exe
```

This began as an observational diagnostic. During the September 6 remediation it
was converted to an **enforcing offline regression**: failure of writer-mode
authorization, unchanged state/event positions, or pinned child-source loading
now fails the process. It remains explicit, outside `runtest` and E2E aliases;
normal regression tests in `agent_server_gap_test.ml` and `agent_session_test.ml`
cover these branches too. See `ochat-agent-server-spec-code-audit.md` for history.

The probe uses a private Eio-managed `/tmp/ochat-readonly-audit-<random ID>` root,
an in-process embedded server with the production dispatcher/actor/store/runtime,
and an authenticated principal with full scopes attached in **Read_only** mode.
It exercises a rejected start and attempts deletion of **only its own generated
session**, then cleans the private store and fixture. It never uses an existing
data root, starts a provider request, or calls a network service.

It also creates a prompt-relative nested-tool file and checks whether the
materialized revision plus the production fetch helper can resolve it. This
isolates source loading before any provider/tool execution; it is not a live
nested-agent completion test.

September 6 audit observations:

```text
nested_source_exists=true nested_artifact_exists=false nested_fetch=failed
mode=read_only start=permission_denied revision_before=4 revision_after=6 delete=accepted
```

These historical values demonstrate the original defects, not current expected
behavior. Current assertions require successful pinned source loading, denied
read-only start/deletion, and unchanged revision/event positions. Additional
normal regressions exercise all 14 mutation methods, actor job cancellation and
expired owner leases, and nested import-relative/cyclic source capture after the
original source tree disappears.
