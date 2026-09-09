# X04: reusable standalone report comparison

This is the synchronous internal qualification fixture. The asynchronous Pending
variant remains an E05 obligation; public feature exposure remains gated on A01.

`agent.chatmd` loads `compare.chatml` as a standalone tool with explicit input
and output schemas and one selected dependency, `read_file`. No conversation
moderator is declared. The input has `left` and `right` report filenames, for
example `{"left":"report-a.json","right":"report-b.json"}`. The script rejects
the same filename before effects, reads each report, and returns both disclosed
file results plus an invocation counter.

The mutable counter deliberately demonstrates fresh program initialization:
each invocation must return `invocation_count: 1`. The daemon test submits two
valid calls in the same provider batch with reversed filenames, alongside an
invalid input and a same-file request. Only the valid calls perform reads. A
separate test supplies an incompatible output schema and requires an explicit
failure after the two reads, rather than publishing the invalid result.

Run `dune build @test/chatml_composition/runtest` from the repository root. The
test uses the X01 report files and an offline provider, checks canonical outputs
and nested ownership, and verifies one live/durable session with no extra model
request or moderator.
