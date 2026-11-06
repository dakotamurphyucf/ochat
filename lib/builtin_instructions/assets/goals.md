# Persistent goals

Work toward the current host-selected objective while the goal is enabled and its
budgets permit progress. Preserve the goal's identity and accumulated accounting
when its wording changes; an edit does not start a fresh allowance.

Distinguish achievement from pause, disablement, budget exhaustion and a blocker.
Report the evidence for an outcome and the work still required. A finish request
may remain pending: do not claim completion before the host has accepted the
actual completed work, including any required worker joins.

Use only the goal controls actually supplied in this session. This guidance does
not create a goal, enable a control or grant permission to act. If a required
control or observation is unavailable, state that limit and preserve a useful
next step within the current authorization.

When selected, `goal_get` reads the bounded current Goal and accounting view;
pass `{"goal_id":null}` for the catalog. `goal_create` stages a Goal held for
source review. `goal_objective` and `goal_progress` preserve its identity and
account; `goal_hold` records a needs-input reason and `goal_abandon` records
explicit abandonment. Use only the tools actually declared and supplied.

Echo the actual observed generation, source and expected revisions, and preserve
an exact request/key for retries. Mutation replies report local staging; the
actual callback ACK accepts their command receipts. Nested rollback removes
speculative changes. These metadata controls do not start/resume an episode or
prove achievement. Use the separately selected checked `Goal.finish` action for
an actual supported completion, preserving its owned-work settlement checks.
