# Bounded local document conversion

`Webpage_markdown.Bounded_conversion.convert` transforms already acquired bytes
without opening files, fetching URLs, decompressing data or starting a browser.
The capture owner remains responsible for source permission, acquisition limits
and durable storage. Conversion returns immutable contents and an accounting
receipt; it does not retain its checkpoint callback or mutable parser state.

Select `Verbatim`, `Strip_html` or `Html_to_markdown` explicitly. Verbatim preserves
bytes. Strip mode trims each text node, drops empty nodes and joins the remainder
with newlines. Markdown mode shares the existing local HTML document conversion
and renderer, including its whitespace conventions. The separate
`Html_to_md.to_markdown_string` convenience function additionally strips its
result; local document conversion does not. Flag precedence belongs to the caller.

Supply nonnegative input-byte, output-byte, work, node and depth ceilings through
`Limits.create`. One invocation shares these limits across parsing, conversion and
rendering. A caller processing several documents must pass its remaining aggregate
allowance to each conversion and debit successful receipts rather than reset its
outer budget. A failure produces no partial output.

Input admission precedes parsing. HTML signals are checked before retaining the
DOM, including recovered body attributes. Conversion and rendering account for
visited structures and scanned bytes, including repeated list copies during DOM
mutation. Rendering uses immutable pieces with cached lengths; it measures the
complete escaped and indented output before allocating the final output buffer.
Working structures have their own node/work/input bounds and need not fit within
the final output-byte limit.

These are conservative representation and work counters, not measurements of OCaml
heap words or CPU time. Checkpoints are cooperative. Individual Markup steps, RE2
queries and admitted string/container primitives are indivisible. This API does
not provide process isolation, preemption or a hard wall-clock limit.

Only the invocation's own admission failures and understood HTML parse errors
become typed errors. Checkpoint exceptions, cancellation and unexpected failures
propagate unchanged, including their original backtraces. Ordinary conversion APIs
retain their existing compatibility behavior through the same private cores.

`test/bounded_document_conversion` contains one teaching example and compact
invariants for independent limits, retained-copy accounting, deep/wide inputs,
checkpoint interruption and nested invocation ownership. It also runs the existing
renderer examples from their original test source.

The [captured document content owner](captured-document-content.md) composes these
receipts with parser-owned occurrences, immutable storage and bounded replay.
