# Persistent review team

Three authored reviewers investigate Lantern's tutorial release, retain distinct
conversations and refine their conclusions when given follow-up evidence. A
standalone ChatML collector observes their receipts and output without flattening
failures or disagreements.

Use the complete website archive. It assembles shared Lantern sample files with
this application's source; this raw source subdirectory alone is not the bundle.
Read every companion inline on the application page before downloading it.

## Start

Install Ochat and configure gpt-6-astra provider access. Specialists use high
reasoning. From the extracted persistent-review-team directory:

```sh
ochat-agent-server -config "$PWD/server.sexp" -validate-only
ochat-agent-server -config "$PWD/server.sexp"
```

In a second terminal in that same directory:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" \
  --new-daemon-session --prompt team --workspace lantern
```

Use absolute built executable paths for a checkout. Configuration paths resolve
relative to server.sexp. The private store is private-data/, while only
sample-project/ is exposed through read_file. All declared tools are allowed by
the reader permission profile. No shell tool, editing tool or network tool is
given to these agents; provider requests still require configured access.
Transient native local mode cannot replace this durable host.

## Investigate and follow up

Ask documentation_review a one-off question first. Then request persistent reviews
from all three roles; select mode persistent only for documentation_review. The
correctness_review and integration_review declarations are fixed persistent and
take no mode argument.

Record role, session_id and receipt_id. Omitting session_id creates another
instance. Supplying it to the same named wrapper continues that conversation.
Use collect_reviews with up to three role/session_id/receipt_id/cursor records;
set cursor to null for a new query. Keep the returned output next_cursor when
continuing the same receipt query. The collector observes status, zero-time wait
and one bounded read per reviewer. It does not wait for completion or stop work.

Read follow-up.txt, then ask the SAME reviewers to refine their earlier diagnosis.
Store the new receipts and start their read queries with null cursors. Preserve
disagreement and missing evidence in the report. expected-report.json is recorded
sample input, not proof of a new check. No tool here applies the proposal or runs
the checker. The guarded engineering application covers those steps separately.

## Failure and cleanup

Receipt success, failure, cancellation and interruption are distinct. Idle and
caught_up do not prove success. Per-call lifecycle errors remain in each collector
observation; cancellation of the collection itself may stop the whole call. A
wait timeout does not cancel a reviewer. Use bounded waits, not a tight poll loop.

Try cancelling a reviewer and inspect its actual terminal state. If it finished
before the cancellation, report that honestly. New sends to stopped children fail;
retained output remains readable subject to authority and retention. Create a
replacement deliberately if useful. Stop all retained children, including extra
instances, when the report is complete. Check stop progress rather than treating
its admission receipt as proof that cleanup has joined.

Quit the TUI with Esc then :q and Enter. Stop the daemon with Ctrl-C. Keep the
directory for retained sessions; removing private-data/ removes this example's
store. Never expose the entire bundle as the workspace to fix a path problem.
