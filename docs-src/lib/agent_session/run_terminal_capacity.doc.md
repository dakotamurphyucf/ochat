# Known Run terminal capacity

An admitted live Run reserves the bounded disposition of its currently known
work. `Run_terminal_capacity` charges every missing owned-work terminal proof,
the canonical terminal receipt, the known pending Finish result and pending
intent disposition. Job-frame delivery, disposition and captured retry credits
remain additive. This applies to ordinary Runs as well as goal-associated Runs.
An intermediate terminal Run without its matching retained terminal receipt keeps
the receipt credit; only the actual retained disposition releases it.

The reservation describes storage and constructibility. It supplies no source,
principal, callback, operation or execution authority. Private JSON witnesses
cover recovery, known completion and consumed-intent shapes in a fixed number of
passes across the complete index. They are never decoded as Runs, proofs or
receipts and never become state. `Run_terminal_document` merges each owned
projection with the actual complete-carrier preservation codec and its original
template. Protected unknown data must survive that eventual disposition or the
new admission fails before effects.

`Capacity_growth` measures encoded bytes, object members and JSON nodes with
checked nonnegative arithmetic. The complete document owner sums Run, job and
Goal positive growth before applying the configured whole-state profile, as well
as the Run subtree's 1 MiB limit and existing Goal child/catalog limits. Unknown
outer data cannot spend a child promise. Receipt counts and Run revisions retain
headroom. Keys in another receipt cannot occupy a known live Run's future
canonical terminal or recovery namespace. This is checked on fresh candidates
and historical decoding; historically poisoned live state fails closed rather
than claiming that recovery remains constructible.

`Session_terminal_capacity` also protects the shared session revision,
transaction and durable-event frontiers. The conservative bound includes a final
step per live Run, its known missing-work settlements, captured retry successors
and pending Wait claim/callback steps. Six events per known step cover the
existing worker terminal payloads and the two possible automatic projections;
current permission, review-job and timer cleanup rows are additional. Every
candidate is rechecked, so an unrelated update cannot consume these credits.
This does not budget unlimited future callbacks or unrelated host workflows.

The state document and journal have separate profiles. `Session_persistence`
checks future recovery envelopes under its actual transaction document limits
and its actual `Commit_writer.max_payload_length` before archive or journal
effects. The capacity projection reuses the ordinary delta and transaction
encoders. It includes the retained Run/Goal children, the existing
`session.updated` recovery event, maximum metadata widths and positive child
growth. A state-only size check would not establish this durable promise.

The guarantee covers known bounded completion facts and an honest recovery
record under the admitted fixed profiles. It does not promise storage for
arbitrary future worker errors, final history or provider output. Those still
undergo ordinary commit admission. If such a commit fails, no successful Run or
goal achievement is invented: the durable Run remains live, and existing
recovery can record Interrupted with truthful work evidence and preserve an Open
goal. Physical IO failure remains a persistence failure; uncertain acknowledgement
requires reopening the journal and reconciling retained evidence. Neither the
reservation nor recovery replays inference or external effects.

Source ownership is split among `Run_state`, `Run_transition`, `Run_retirement`,
the complete session document, and `Session_persistence`. Goal controller
enable/resume admission and the complete authored goal workflow remain separate
integration requirements; terminal capacity does not introduce another scheduler.
