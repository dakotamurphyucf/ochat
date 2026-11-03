# Pure recurrence timing rules

`ochat.recurrence_rules` provides timing policy for a host-owned recurrence
controller. The [durable recurrence design](../design/composable-agent-recurrence.md)
specifies the remaining actor and host integration. It does not create schedules, invoke tools, launch sessions or read
timezone files. The host remains responsible for durable admission, permissions,
misfire/overlap policy and execution receipts.

`Rule.Interval` distinguishes fixed-rate and fixed-delay intervals. Periods are
positive integer milliseconds. For restart wall reconciliation,
`Cursor.Interval.due_through` computes a complete fixed-rate due range
arithmetically, even after years of missed slots. During live execution use
`Clock.due_count` on the elapsed phase, then `Cursor.Interval.consume_count`;
current wall corrections cannot change this count. Each slot has a monotone
integer identity. A fixed-delay cursor advances only when the
caller supplies an actual committed completion time; the helper never treats
timer delivery as completion. Timestamp endpoints and integer arithmetic are
checked rather than wrapping.

`Clock` accepts injected wall and process-elapsed snapshots. Live interval phases
follow elapsed time through wall-clock corrections. Commit a consumed cursor and
the matching `Clock.advance` phase together in the owning actor. `advance` keeps
the original fixed-rate phase instead of starting a delay when a callback completes. Recovery
has no surviving monotonic origin: `recover` derives the remaining duration from
a saved wall deadline and the new process clock. An overdue deadline is due
immediately; the host then applies its selected missed-work policy. A recovered
clock anchor is not evidence that interrupted work may be replayed.

`Rule.Calendar` supports daily, selected weekdays and selected monthly days at an
explicit local time in a named zone. Missing monthly dates are skipped. Defaults
skip missing local times and select the earlier repetition; `At_gap_end`, `Later`
and `Both` are explicit alternatives. These rules do not claim cron or RFC5545
syntax compatibility. Calendar search retains one earliest UTC candidate while
examining bounded civil dates. It emits only when the offset bound proves every
later date's candidate is no earlier, so repetitions interleaved across dates stay
complete and chronological. After emission it restarts at the earliest possibly
relevant date and filters already-emitted timestamps. `Cursor.Calendar.next`
examines at most the supplied number of new civil dates; a retained candidate is
separately revalidated against the actual rule and zone before it can be emitted.
`Continue` retains that single speculative candidate and first unexamined date;
it must be persisted or resumed and does not mean no future occurrence. A zone
provenance change discards speculative lookahead and restarts from the last emitted
instant. Emitted UTC timestamps strictly increase, with equal instants coalescing
to the earlier nominal local label. Cursor decoding admits days that only partly
intersect the timestamp domain, plus one explicit exhausted-date sentinel.

`Zone` is a validated **pure data model**, with an initial UTC offset, strictly
ordered transitions, named provenance and explicit inclusive UTC coverage. A
loader supplies this data through `Zone.create`; this package is not an IANA or
TZif reader. It never infers the host's local zone or substitutes UTC for missing
data. Offsets are bounded to plus/minus24 hours and transitions to4096. Candidate
resolution uses offset inversion and exact forward mapping, rather than assuming
each date has unambiguous midnight or one daylight-saving transition. This covers
non-hour changes, midnight transitions, multiple shifts in a date and skipped
dates within the admitted model. Civil resolution requires a covered24-hour
window on each side of the requested local label; otherwise it reports
`Out_of_coverage`, preserving uncertainty at the data boundary. A timezone loader
must not extend its last offset indefinitely when it has not interpreted future
rules.

Materialized occurrence records contain their selected UTC time, nominal local
label/repetition, adjustment flag and admitted zone provenance. Later planning
may use a newer admitted zone model; the caller must retain already-materialized
occurrences unchanged. Provenance identifies admitted bytes/version and grants no
execution authority.

Rule and cursor JSON codecs are versioned and validate missing, duplicate,
unknown and invalid fields. Abstract domain values have no unchecked generated
sexp reader. A cursor is a frontier within an immutable rule revision, not a
globally unique recurrence identity. Its durable owner must bind it to that
definition revision and reset/replace it when editing the definition; local codec
validation cannot establish this external ownership binding. The public `.mli`
files document units, bounds and exhaustion cases.
Controlled test fixtures exercise gaps/folds, non-hour and midnight changes,
Pacific/Apia's skipped date, leap/month boundaries, clock correction/recovery,
large missed ranges and coverage/arithmetic failures without provider credentials
or mutable operating-system timezone expectations.
