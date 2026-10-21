# Bounded execution tracing

Use a trace to correlate an admitted Developer command, REPL submission or
explicit Task run with the exact source that produced its observations. Traces
are optional, bounded and read-only. The command or Task receipt remains the
authoritative outcome. Start with the [REPL walkthrough](../guide/chatml-development-workflow.md)
or [authoring/execution workflow](../guide/chatml-development-services.md) if
you have not yet selected and run a captured revision.

## Enable recording on the host

The shipped Developer host supports summary or full recording. In an existing,
explicitly authorized `developer` policy record, add one of:

```sexp
(traces Trace_summary)
```

or `(traces Trace_full)`. The default is `No_traces`. Summary omits statement
and call-entry observations; Full includes those within the same finite
retention policy. This is a field inside the Developer configuration, not a
standalone configuration or a `chatml-repl` flag. Restart/recreate the configured
host to use the new policy. The [offline REPL policy](../guide/chatml-development-workflow.md#start-the-terminal-repl)
shows a complete local host setup; [host configuration](../agent-server/configuration.md)
explains daemon policy installation.

Recording requires its host retention owner; portable page access additionally
requires compatible installed wire bounds and selection of the `developer.trace`
feature during Initialize. Discover `developer.trace.capabilities` before
reading pages. Enabling recording grants neither execution nor permission to
read captured source metadata. Capacity can be exhausted while other traces
remain active: the execution can proceed without a recording.

The current terminal has no `:trace` command or trace panel. Integrators read
pages through the initialized first-party client or the protocol below; a
stepping debugger, variable inspection, breakpoints and replay are future work.

## Read one execution's pages

1. Keep the admitted operation's exact locator. An ordinary Command uses its
   Developer run ID and key. A REPL statement uses instance, historical epoch
   and submission key. A REPL Task uses its Task run ID and key. These are three
   distinct locator variants, even when two share run/key syntax.
2. Request `developer.trace.page` with that locator, no initial cursor and a
   positive item limit within the installed capabilities. The
   [type/codec reference](../agent-server/protocol-types.md) defines the exact
   `Developer_trace_wire.Page_request` encoding. Do not reconstruct a locator
   from a current editor filename or invent an execution ID.
3. For a retained page, compare its locator, execution ID and submission/selection
   fingerprints to the admitted operation. Match source sites by captured source
   ID, digest and byte range. A site's original source can predate the current
   REPL submission because a closure retains its original owner.
4. Follow `next` to read remaining retained events. For an `Incomplete` trace,
   retain `resume` as the next polling position, including when `next` is absent.
   An incomplete page without new events is not proof that execution finished.
5. Recheck the authoritative receipt for the outcome. On reconnect, initialize
   to the same server, principal and authentication kind before continuing;
   each new page still needs current disclosure and historical source-read
   permission. A cursor does not carry permission or replay the execution.

The first-party `Agent_client.Developer_trace_connection` validates locator,
cursor and item-count affinity on embedded, HTTP, Unix and stdio connections.
Its `create`, `capabilities`, `page` and `reconnect` operations use the existing
connection and notification owner. The
[protocol reference](../agent-server/protocol.md) and
[client interface](../../lib/agent_client/developer_trace_connection.mli)
provide transport and typed API details.

## Interpret a partial trace honestly

| Observation | Meaning |
| --- | --- |
| `Unavailable` | No readable retained trace for this operation; it does not establish whether execution ran. |
| `Expired` | The bounded recording is no longer retained. |
| `Incomplete` | No trace terminal was published yet; execution or exact terminal recovery may still be pending. |
| `omitted_after_prefix` / drop count | The retained prefix filled or bounded delivery lost observations. Later events are missing. |
| Terminal record | A separately reserved copy of the lifecycle outcome; it is not an invented final event in the prefix sequence. |
| `Effect_host_returned` | The host call returned; this does not establish transaction commit or undo later failure. |
| Entered wait without resume | The wait was entered; cancellation or truncation may explain the absent resume. |

A trace contains coarse operation kinds, captured source coordinates and opaque
correlation IDs. It excludes values, arguments/results, prompt/model text,
credentials, environment contents, source excerpts and raw exception messages.
Read diagnostics or source contents through their separately authorized APIs.
Revoked source access may omit previously attributable sites while preserving
safe event ordering and omission metadata. Editing a file, resetting the REPL
or retrying a read cannot rewrite the old capture or make expired data current.

The remaining sections define recorder accounting, ownership and transport
limits for host implementers. They preserve the same read-only contract.

## Purpose and existing boundaries

A developer needs to see which captured source ran, where a failure arose, and what happened around asynchronous host work without changing the program's behavior or exposing its values. The trace is a bounded observation of one admitted execution. The authoritative run receipt, compiler diagnostics and host job status retain their own meanings; the trace does not replace them.

The compiler and evaluator already preserve useful native sites. [Captured locations](../../lib/chatml_sources/chatml_source.mli) bind a source ID and digest to a canonical half-open byte span. [Prepared programs](../../lib/chatml_sources/chatml_prepared_program.mli) retain the exact graph inventory and fingerprint; [linked programs](../../lib/chatml_linked/chatml_linked_program.mli) retain compile failures, resolved units and runtime order. [Resolution](../../lib/chatml/chatml_resolver.ml) keeps authored spans, including when configured reads are lowered. [Evaluation](../../lib/chatml/chatml_eval.ml) polls at expression boundaries, attaches sites to task calls, and retains closure source ownership; [execution control](../../lib/chatml/chatml_execution.mli) distinguishes expression fuel from diagnostic cancellation/deadline polls. A trace hook must observe detailed structured failures before a public adapter reduces them to rendered text. It must never make a checkpoint, `Task.catch`, a host effect or cancellation behave differently.

The [host interpreter](../../lib/chatml/chatml_host_runtime.ml) controls effect dispatch, task steps, native waits and approval continuations. Its `TSpawn` may start external work before returning, and a later wait is a separate action. The [script job service](../../lib/agent_session/script_job_service.mli) and [job protocol](../../lib/agent_protocol/job.mli) expose bounded status/attempt information when a job exists. They are optional adapters to a generic execution trace: a developer run or REPL action does not need an agent conversation. The existing [conversation-run read service](../../lib/agent_server/run_read_service.ml) has a different session-bound identity and no execution-event stream. The process-global [text debug sink](../../lib/chatml/chatml_debug_log.ml) can contain formatted effect details and has the wrong ownership, payload and callback semantics; it is not a trace transport or recorder.

The accepted [developer run/REPL design](chatml-developer-runtime.md) supplies the execution lifecycle. A check has a check ID and executes no initializer. A submitted run receives a distinct developer run ID even if compilation or host admission fails; the terminal receipt then has diagnostics and zero runtime events. Request ID plus capture digest is idempotent, while an explicit repeat uses a new request and run ID. A job ID only correlates child work. The REPL has its own stable session ID, reset epoch and monotone revision; each complete admitted snippet or explicit task drive has a separate execution ID and immutable source ID/digest. An incomplete multiline fragment has no evaluation trace. Reset cancels and joins the active action, increments epoch/revision, and leaves prior bounded receipts/traces attached to their old epoch. Creation is inert; value dependencies initialize on the first successfully checked action in existing linked order, with initialization distinct from statement evaluation. External effects are not reversed by failure, cancellation or reset. These language and lifecycle decisions are inputs to tracing, not new trace decisions.

## Per-execution contract

Use small typed modules for run identity, source site, event, recorder and page, with receiver-first operations, validated constructors and versioned wire DTOs. The developer run ID or REPL execution ID is the trace's primary identity; it is never substituted with a source, job, conversation run or connection ID. The core header freezes a bounded submitted-capture identity before graph preparation, or a bounded prepared graph/compiler-contract/environment/source inventory after preparation succeeds. It keeps a separate selection fingerprint. The authoritative Developer run receipt binds project and upload provenance, REPL session/epoch/submission/revision, admission and current read authority; a fingerprint alone does not grant any of these. The core accepts an optional host-owned observation scope at compile/evaluator/task/effect boundaries. It has no UI dependency and no public mutable callback registration. With tracing disabled, the evaluator takes a cheap domain-local guard branch without requiring `Eio.run`; enabling it must not add fuel charges or suppress/replace cancellation exceptions. Bounded instrumentation still has measurable wall-clock cost, so performance qualification must establish acceptable overhead under the selected host limits.

The core event carries a monotone per-trace sequence, safe kind, optional native source site and opaque correlation; the portable wire envelope adds schema version and trace ID. Phases describe observations rather than inventing a global clock. The core records phase start/end; statement/call entry in `Full` mode; effect attempted/host-returned/failed; a real host wait or approval suspension entered/resumed; recovered task failure; and a coarse failure category with owned site. `Summary` intentionally omits statement/call entries without counting them as capacity drops. `Job.read_result` is a nonblocking result query and is labelled `Job_read`, never an inferred wait. A trusted host adapter can bracket an actual Eio wait; cancellation leaves entered without inventing resumed. Job admission/status/terminal observations require their host adapter. Compilation runs in a joined worker domain, so its start/result/failure observations are made at the owning caller after the worker returns; interpreter and task observations remain in their execution domain. Linked dependency units and the entry unit use distinct initialization phases. The terminal outcome lives in a separately reserved record described below. A host-returned effect is **not** a transaction commit. A failed REPL submission does not publish its failed new binding, but effects and mutations already completed remain; `Task.catch` itself does not roll them back. Trace wording must preserve that distinction. Source-free infrastructure events have no fabricated site. A timeout or cancellation without a primary diagnostic site may carry the last **retained** site explicitly marked as context, never as failure origin.

The source site is a validated `Chatml_source.Location.t` or an equivalent validated ID/digest/byte-span DTO tied to an exact captured source. Closures that outlive an edit or REPL reset keep the old source owner, even while the current execution has a new trace ID; the historical source owner retains and authorizes that source separately when it is outside the current submission inventory. An optional authored-origin projection is separate and qualified by the exact parent document revision. [ChatMD parser ranges](../../lib/chatmd/chatmd_parser_input.mli) are exact ranges in their effective input, but do not guarantee that a generated script is contiguous in that input. The [parser-owned extraction map](../../lib/chatmd/chatmd_extraction_map.mli) provides an exact projection only where an emitted span has one continuous authored origin; otherwise readers retain the native ChatML site and a declaration-related context. Never add a guessed declaration offset or relabel against current disk bytes.

The event payload is deliberately coarse: a fixed stage/kind/category enum, a stable safe code from an explicit allowlist, a host-allowlisted public operation label if available, source coordinates and opaque correlation IDs. It excludes argument/result values, prompt or model text, tool payloads, environment variables, credentials, source excerpts, raw exception names/messages and rendered diagnostics. Unknown or sensitive operation names collapse to a generic kind. The separately authorized diagnostic/source endpoints may render their own content under their own grants. The recorder must not eagerly stringify a value just to decide whether the trace is enabled.

## Prefix retention, ordering and terminal truth

The initial recorder keeps a **bounded prefix** of normal events, limited by both host-selected event count and canonical byte budget. The v1 accounting representation uses eight-byte words: an event has six fixed words; an optional native site adds two length words, raw namespace/path bytes, a 64-byte hexadecimal digest and six position words; an optional correlation adds one length word and its bounded token bytes. The header has seven fixed words, two digests and the bounded execution token, with three optional graph/contract/environment digests and source entries of two length words, raw namespace/path bytes, digest and byte length. Arbitrary namespace bytes remain lossless in this binary accounting format; a later JSON wire adapter must base64 them and enforce its own encoded bound. The separate terminal/drop snapshot reserve is eight words (64 bytes), with no arbitrary site string. Tests independently construct this representation and check exact byte-limit boundaries. This is the only v1 retention policy; no ring, suffix or sampling mode is implied. The owner reserves terminal/drop space before accepting normal events. Once the next event cannot fit, normal retention saturates permanently. The rejected event and later attempted observations increment a saturating drop counter, with an overflow flag if the exact count is no longer representable. They cannot evict or overwrite the prefix or the terminal reserve. Known trace-only capacity or bounded encoding failures saturate the recorder and report availability/truncation without becoming script failures. Instrumentation must not catch cancellation, out-of-memory failures or unrelated implementation bugs as benign trace drops; those follow the host's existing exception policy. The ordinary append path is non-yielding, bounded and contains no I/O, user callback, rendering or remote subscriber operation. An adapter for work produced on another domain must use bounded nonblocking delivery to the run owner; queue loss also saturates normal retention and is counted. No observer can hold an execution lock or make the run wait for acknowledgement.

Sequence numbers are assigned to retained normal events in recorder admission order, starting at one and contiguous throughout the retained prefix. This is the only ordering they claim: neither a distributed clock nor causal ordering of external jobs. Dropped attempted observations receive **no invented sequence number**. The read model places an explicit `omitted_after_prefix` marker after the last retained event when saturation or queue loss occurred, carrying the exact or lower-bound drop count. Readers must not interpret adjacency of the last event and the terminal record as proof that nothing happened between them. The terminal record is separate, not a pretend final event in a contiguous stream; it states the last retained sequence, truncation/drop metadata, and a copy of the authoritative run disposition with receipt version. The lifecycle owner publishes it once after its cancellation/join/terminal decision. The recorder cannot independently infer success from a lack of later events. A crash before terminal publication leaves the trace `Incomplete`; the run service may later mark the receipt `Interrupted` under the developer-runtime recovery rules, and retrieval reports both truths without fabricating a terminal event or replaying source. Late child-job notifications after a terminal barrier belong to that job's own status, not a retroactive edit of the run trace.

One recorder belongs to one admitted execution and its host owner. Captured graph, trace and source custody have host-selected finite byte/count/TTL bounds. After expiry, lookup returns typed `Expired` or `Unavailable` instead of silently changing the source revision or borrowing a live editor connection. Within retention, a cursor binds the exact Command, REPL statement or Task locator, host-minted execution ID, admitted submission and selection fingerprints, wire version and last emitted sequence. The single retained recording for that authenticated association supplies its identity; there is no separate generation ordinal. The server validates the complete encoded page and JSON-RPC envelope under installed bounds, including repeated Header and cursor fields. A native source ID outside the portable source-ID domain produces a typed resource-limit result; it does not alter execution or silently remove the site. A cursor is only comparison data, never authority. Every page rechecks current principal, run visibility and historical source-read policy. An exhausted incomplete page returns a resume cursor for later polling without replaying its prefix. Pages cannot pause, resume, acknowledge, mutate or replay execution. Source content and diagnostics are separate authorized reads.

The private Developer host attachment keeps only a bounded trace token and exact execution association in lifecycle and REPL state. Its retention owner alone holds the recorder and immutable captured-source provenance. A recording starts only for an admitted execution; incomplete REPL input has no trace. After the exact command terminal is installed and its execution switch has joined, the owner publishes the trace terminal independently of command settlement. An uncertain Finish keeps the recording incomplete until exact terminal reconciliation. Full/summary recording limits, a maximum number of active and retained traces, aggregate bytes and expiry are finite. Active recordings cannot be evicted; when they occupy capacity, another execution proceeds without a trace. Historical reads authenticate the current actor and separately recheck permission to disclose captured source metadata. Editing a file or resetting a REPL does not rewrite a retained source identity. Local project recordings retain the selected root and dependency roots; uploaded projects use their published Store pin. Raw document scripts and host-prepared document builds use the admitted document owner. A document Build supplied independently by a selection policy can include dependencies outside that owner's proof, so execution remains available while its trace is omitted unless an authentic dependency owner can be issued. A site outside the currently authorized source inventory is omitted from the read-only snapshot, with an omission count; the event's kind, sequence, correlation, drop metadata and terminal stay unchanged. The daemon advertises trace pages only when recording, retention and portable wire bounds are installed together. Trace-disabled execution keeps its existing admission path.

## Two observation examples

**Compile failure and old closure.** Submission of captured `main.chatml` allocates run `R` before compiling its pinned graph. A type error in captured dependency `math.chatml` yields preparation/compilation observations with that dependency's ID, digest and byte site, then a terminal `Failed` receipt with zero runtime events. An explicit check may return the same diagnostic under check ID `C` without allocating `R`. If a later REPL submission in the same epoch invokes a closure created by an earlier submission, a runtime call failure uses the current execution ID but the closure's earlier source ID/digest. A fresh editor revision does not reassign its site. Reset discards the old epoch's ordinary REPL state/handles, while its retained historical traces keep their old source identity; only a separately admitted externally escaped closure could survive that reset. If the only failure is budget exhaustion without a primary site, the most recent entered site is contextual only.

**Asynchronous host work.** An owned call site emits `Effect_attempted` before host dispatch. If a spawn is admitted, `Effect_host_returned` may carry an opaque job correlation, followed by `Host_wait_entered` when the script actually waits. A job adapter may emit a coarse status, and `Host_wait_resumed` only when the owner resumes. Cancellation during the native wait leaves those partial observations plus authoritative `Cancelled`, without a synthesized tool result or claim that the external job was undone. [The existing cancellation regression](../../test/agent_session/background_execution_tests.ml) and [budget/effect ordering tests](../../test/chatml_execution_budget_test.ml) establish relevant behavior. If the prefix filled before cancellation, its explicit omission marker and separately reserved terminal record still tell the reader the trace is partial and the run was cancelled.

## Debugger feasibility, outside trace delivery

Source-owned sites could support future breakpoint matching, but the recursive evaluator and tail-call trampoline in [Chatml_eval](../../lib/chatml/chatml_eval.ml) do not expose a stable, suspendable frame stack. The host's current approval continuation is specific to permission flow, not an expression pause primitive. A debugger would need safe-point continuations, tail-call/frame semantics, bounded redacted value snapshots and explicit treatment of async jobs that continue while a script is paused. A pause must not hold an expired Eio or actor borrow; it needs owner-held lifetime bounds, disconnect behavior, current authorization on every step/resume and cancellation that still joins the action. Deterministic replay/time travel additionally needs external-effect recording or sandboxing and is outside this proposal. Research a narrow safe-point/stack spike before estimating a debugger protocol. Trace implementation requires none of these pause or replay capabilities.

## Implementation and verification

The [core recorder](../../lib/chatml_trace/chatml_trace.mli) owns bounded
prefix/drop/terminal truth. The [host retention owner](../../lib/agent_server/developer_trace_retention.mli)
binds the immutable source and execution association; the
[page service](../../lib/agent_server/developer_trace_page.mli) rechecks current
read authority. The [typed trace client](../../lib/agent_client/developer_trace_connection.mli)
validates exact locators/cursors and reconnects only to the same server,
principal and authentication kind. None of these read paths mutates execution.

[Core trace tests](../../test/chatml_trace/trace_tests.ml) compare tracing on/off
and independent prefix accounting. [Client tests](../../test/agent_client_trace/trace_connection_tests.ml)
exercise paging and authority-preserving reconnect. The
[transport scenario](../../test/agent_server_e2e/scenarios/developer_trace_client_scenario.ml)
qualifies actual embedded, HTTP, Unix and stdio command reads; its HTTP REPL
cases additionally cover Task/reset/reconnect and source-root revocation.
These are explicit transport and authority oracles, not a claim that every
REPL operation was repeated over every transport. The
[captured development workflow](../guide/chatml-development-workflow.md)
composes edit/save or immutable upload, Check, Fixture/Live, REPL and exact trace
sites. Final combined documentation and runtime qualification remains separate
from these implementation references. Debugger pause, stepping and replay are
not provided by this trace contract.
