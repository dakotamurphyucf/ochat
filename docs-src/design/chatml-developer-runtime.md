# ChatML developer run/test and REPL design

The hosted Developer service uses three independent method scopes:
`developer.check`, `developer.fixture.run`, and `developer.live.run`.
The shipped daemon and local host install these methods only under an explicit
Developer policy with the corresponding source and method grants. Current
source, selected prompt/workspace, native, provider, result-disclosure and
cancellation authority remains separate; naming a scope does not grant it.

The pure [Developer wire types](../../lib/agent_protocol/developer.mli) now
define closed request/result method pairs for command submission, observation,
terminal waits, cancellation, approval comparison and bounded change hints.
They preserve the existing Store command ID's exact 1–256 byte domain and
separate a receipt's bound from the complete response bound, including method
framing. Unknown or duplicate fields and explicit null for absent options are
rejected. A response encoder checks the installed aggregate limit before
returning JSON; approval snapshots reject overflow instead of truncating.
These types carry selectors, owner coordinates and fingerprints as comparison
data. Defining the DTOs installs no authority. An explicit portable policy now
installs Developer methods only when the Daemon has a physically paired
Developer Host and Authoring owner with compatible output, receipt, approval
and transport bounds. The endpoint decodes envelope and typed local requests
under those installed bounds, borrows the current Authoring source, and lets
the Host resolve a historical key inside its serialized miss path before any
fresh source selection. Repeating that key returns its original command;
changing its request conflicts, and a new key requires current source
authority. The connection lease governs admission until durable Reserve;
admitted work then continues under its original actor and source even if the
connection closes. Responses are rechecked before disclosure.
Terminal Check, Fixture and Live receipts are converted from their persisted
values through typed protocol constructors after that disclosure check. A
diagnostic keeps its exact captured source ID, digest and half-open byte range;
the complete wire receipt is then validated under the installed byte and
structure bound. Portable installation requires checked headroom over the
Host's stored receipt ceiling for the diagnostic source ID's base64 encoding
and one nested object. The configured startup preset derives that wire ceiling
from the Host ceiling; a manually configured policy with insufficient headroom
rejects before it can disclose a terminal row. On a fresh portable command,
the Host checks every captured executable Source ID against the wire codec
before Reserve. Check and Fixture diagnostics can attribute only one of those
exact captured IDs. An unrepresentable source rejects without reserving a
command, while a historical same-key lookup remains independent of any new
source capture. Local nonportable commands retain their source rules. REPL Task
receipts use their separate method family.

Observation and terminal-wait admission share the command lifecycle's existing
per-run mutation gate. A read waits for that lifecycle's in-flight Store write,
then releases the gate before checking disclosure or waiting for execution.
Cancellation detaches only the reader; it does not cancel the command. Current
authority is checked again after the read, and lifecycle shutdown rejects queued
readers. A retired run's authenticated historical receipts remain readable.
For an already-ready run without admission or unresolved cleanup custody,
passive command and REPL Task rereads do not reserve the startup recovery slot.
Cold or uncertain nonterminal rows still recover under that exclusive slot
before the fresh authorizing reread. This keeps cancellation admission separate
from another passive wait without bypassing recovery or control permission.
Pending writes owned outside this lifecycle still report a conflict; they are
never treated as missing commands or retried as new submissions.

A watch belongs to one disclosed run and command key. The Host enqueues
bounded, process-local lifecycle and approval invalidations after authoritative
changes, outside Store and approval locks; a connection-owned pump checks
current disclosure before handing a key-bearing hint to the transport.
Overflow yields a resync hint for each affected watched key. Watch
acknowledgement has no replay cursor: clients Observe the key immediately
after registration and again after a hint or reconnect. Approval_list is a
separate, authorized refresh. Unwatch and connection close cancel and join
blocked publication, and Daemon shutdown joins the hub before closing shared
Host and Authoring owners. The installed HTTP and directly authenticated Unix
server paths share this Dispatcher. The portable Developer client routes the
same closed methods through HTTP, Unix socket and the stdio relay. It obtains
installed capabilities only after Initialize, decodes the initial Developer
capability reply under a fixed 64 KiB bound, and decodes later responses under
those effective bounds and checks the physical host/principal and exact
command identity before presenting a View. A failed Submit after transport
handoff retains the original bounded request for explicit same-key retry;
Observe alone cannot prove a lost response did not execute. An HTTP Developer
response body is streamed under the larger of the negotiated
success-result bound and an independent 64 KiB error-JSON bound, each with
the actual request-ID envelope framing. The decoded branch must then meet
its own bound; socket framing also checks the error JSON independently of its
physical line limit. A long Await_terminal occupies one physical request lane,
so a responsive client
uses Watch followed by Observe, or a separately initialized control
connection for concurrent cancellation. A change hint invites reobservation
of its key; it carries no durable cursor.

The first-party REPL transport has a separate typed request lane through the
client, HTTP, Unix socket and framed stdio adapters: capabilities are cached
for one physical Initialize, selected features and methods are checked, and
REPL params and the full request envelope have distinct installed bounds.
The typed first-party REPL client retains separate, bounded Create, Submit,
Task, Reset and Close requests before transport handoff. Authenticated
reconnect adopts only the same host, principal and authentication kind;
missing Observe never resolves uncertainty, and retry explicitly resends the
original key and request. A fresh Task submission takes its recovery run from a
Create/Reset View obtained on that host and principal, and checks the request's
instance and epoch against that View before handoff; the host still authorizes
the opaque Task reference. Embedded local connections route closed Developer
and REPL calls through their existing authenticated dispatcher/context and
single notification owner. Their private Initialize requests the available
features; each additional physical connection selects its own subset. The
in-memory REPL adapter checks the complete serialized request envelope
against the negotiated ceiling before dispatch, alongside the DTO params
bound. The first-party local stdio process uses the same typed client over its
owned child transport. Closed REPL Cancel_submission and Task_cancel_and_wait
use a bounded control lane on that same initialized logical connection, so
they can overtake a held Submit or Task Await. The stdio and Unix reader admit
at most one such waiting ordinary request and one control request; a further
ordinary request gets a correlated Resource_limit response. Other ordinary
methods retain input order, and EOF cancels and joins admitted request waiters
before draining completed replies. HTTP/1.1 uses one additional owned RPC
transport connection for the control POST under the same logical connection
ID, principal and event reader. The endpoint still performs its usual current
actor, source and lease checks. An early control may report a missing key
before the original submission is admitted; the client retains that exact
lookup and never infers cancellation or starts a new key from that error.
Cancelling an in-flight HTTP control POST retires its physical owner; the
control outcome remains uncertain until the exact lookup is reconciled on a
new authenticated connection.

The shipped daemon has an explicit `server.developer` startup profile. Its
bounded rows compare an admitted source origin, compiled target and authenticated
principal; the rows do not create source or native permission. Startup reuses
the installed Authoring/source grants and one Standard compiler-contract cache,
and rejects a selected origin that has no corresponding physical owner. The
`Provider_default` model choice obtains its target from the same configured
inference host; `Disabled` performs no provider-target capture. The standard
capacity keeps cumulative run documents, concurrent calls, retained REPL Task
handles and REPL submission history as separate bounds. The inline ChatMD
script capture is limited by the existing 256 KiB source route, while REPL
submissions have a 64 KiB source limit and Project build input has a separate
1 MiB ceiling. These values are finite starting choices in an explicit
operator profile, not grants inferred from an opened document or a client
request. With both portable access and REPL enabled, the daemon installs
REPL wire limits derived from the same owner and Initialize bounds. The
ordinary compact statement receipt remains 4 KiB; on-demand type inspection
has a separate 32 KiB output ceiling and frozen type details have an
independent 1 MiB retention pool. HTTP and directly authenticated Unix use
that installed service.

Local stdio can opt in through
`--local --developer-policy FILE`: the bounded policy binds its closed
`Embedded_operator` selector to the one Store-owned `embedded.local` identity,
then reuses the same project, upload, Developer and manifest validators. An
absent policy leaves the prior local scope and source setup unchanged.
`ochat-agent-stdio --local --developer-only --developer-policy FILE` is the
closed offline REPL host path: it installs the configured Developer service
without starting an unrelated ordinary agent session or capturing a provider
target. The flag is invalid for a gateway or without an explicit policy.

For example, this `server` record field admits the selected `s` script from
an already authorized supplied ChatMD document for one configured bearer
principal. The token configuration must actually issue `pri_operator` with
authentication kind `http.static_bearer` and the chosen Developer method
scope; spelling it here grants neither the identity nor that scope.

```sexp
(developer
 ((selections
   (((source (Document_source (origin Supplied_document) (script_id s)))
     (target One_off_v1)
     (principals
      (((principal_id pri_operator)
        (authentication_kind http.static_bearer)))))))
  (capacity
   ((max_runs 128)
    (max_active_commands 4)
    (max_active_agent_calls 8)
    (max_agent_sources 6)
    (max_native_bindings 32)
    (max_retained_tasks 8)
    (max_watches_per_connection 4)
    (max_pending_approvals 4)
    (max_repl_instances 8)
    (max_repl_keys_per_instance 128)
    (max_repl_submissions_per_epoch 64)
    (max_repl_source_bytes 65536)))
  (model Disabled)
  (approvals Disabled)
  (repl Standard_v1)
  (portable true)))
```

`Supplied_document` still requires the current installed Authoring context
owner and exact selected script. `Local_source`, `Local_project`, and host
document/project selections additionally require their existing configured
source roots and explicit Authoring read/selection grants. An uploaded Project
uses its own installed upload and rebind policy. No Developer row substitutes
for any of those source grants. Omit the `developer` field to leave the
Developer service unavailable.

For local stdio, the policy file is a fragment rather than a replacement server
configuration. For example, after creating `/absolute/project/check.chatml`
with raw ChatML source `let main input = Task.pure(input)`, this file selects
only Check for that exact project member and the local operator:

```sexp
(project_sources ((external_roots (/absolute/project))))
(authoring_project_access
 ((read_grants (((root /absolute/project)
                 (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root /absolute/project) (project check.chatml)
                (request (entry ((name main)))) (target one_off_v1))))))
(developer
 ((selections
   (((source (Local_source (root /absolute/project) (member check.chatml)))
     (target One_off_v1)
     (principals (((principal_id Embedded_operator)
                   (authentication_kind embedded.local)))))))
  (capacity
   ((max_runs 128) (max_active_commands 4) (max_active_agent_calls 8)
    (max_agent_sources 6) (max_native_bindings 32) (max_retained_tasks 8)
    (max_watches_per_connection 4) (max_pending_approvals 4)
    (max_repl_instances 8) (max_repl_keys_per_instance 128)
    (max_repl_submissions_per_epoch 64) (max_repl_source_bytes 65536)))
  (model Disabled) (approvals Disabled) (repl Disabled) (portable true)))
(developer_scopes (developer.check))
```

Run `ochat_agent_stdio --local --prompt /absolute/agent.chatmd --workspace
/absolute/workspace --data-root /absolute/store --developer-policy FILE`, or
replace `--data-root` with `--transient`. The prompt and workspace are supplied
by the local CLI; no path or method scope in this fragment creates a source
grant. A manifest grant, if present, must list `Embedded_operator` explicitly
and name the actual local `embedded.prompt` and `embedded.workspace`; an empty
principal list is rejected. The policy accepts only the documented sections,
bounded input and exact local authentication kind. It cannot silently override
a separately configured project-source or Developer policy.

The separate `chatml-repl` executable uses the same local policy and portable
REPL client. The published [terminal setup guide](../guide/chatml-development-workflow.md#start-the-terminal-repl)
contains the complete offline source, prompt, workspace, policy and launch
example. The remaining sections here describe detailed host configuration,
flags and implementation contracts.

The terminal shows the active source or project entry, its requested
environment (or the project's default selection), source root, and prompt and
workspace definitions. This frontend selects `One_off_v1`. For an explicitly
configured project entry, use `--project PATH --entry NAME` and optionally
`--environment NAME`; repeat `--dependency-root-index N` for each admitted
dependency root. The source route does not accept dependency roots. These
indices select existing configured roots and grant no new source access.

Enter multiline ChatML literally and place `:submit` on its own final line
or press Ctrl+S. A successful `Task.pure(8)` submission displays a Task
reference; it does not run the Task. `:run 1` executes the displayed reference.
`:type N`, `:refs`, `:observe`, `:cancel`, `:reset`, `:close`, `:retry`, and
`:abandon` use the same typed service. Up/Down navigate bounded local history;
Ctrl+Q exits. Reset keeps the existing captured source and increments the
epoch; selecting a different project or environment requires a new Create.
After a lost keyed reply, `:retry` uses the original key and payload;
`:abandon` only relinquishes local retry custody and does not undo effects.
After abandon, `:observe` may read that exact historical key, but its receipt
does not prove the current revision; further submissions require reset or a new Create.
Ctrl+C sends exact-key cancellation while Submit or Task execution is still
running; an early missing-key reply leaves custody unresolved for an explicit
same-key retry.
The terminal displays bounded sanitized diagnostics and restores the PTY on
normal exit and handled errors. Remote use requires an installed Developer
REPL service, an authenticated connection profile and the same explicit
source selection; the client requests `developer.run`, `developer.repl` and
`developer.repl.events` in one Initialize and checks the pinned server.

Status: M5 design v1, 2026-10-15. The REPL language decisions in §4.3 are accepted. The owner-neutral native admission, durable developer run checkpoint foundation, selected-document transactions, physical job-claim worker, compiler-side REPL typing transaction and incremental module registry described below are implemented. The developer root staging owner, bounded root/native terminal coordinator, selected native policy and claimed job-parent effect-entry proof are implemented. The owner-neutral lexical script-job kernel and its physical developer adapter are implemented. Actor-bound provider selection, the shared Model.spawn recipe decoder, durable intent, physical call-boundary admission, selected compiled-script adapter and claimed model-job runner are implemented. Hosted command, native, job and Model approval callbacks and the portable Developer transport client are implemented. The hosted library REPL Task service retains exact Task references, starts work durably on explicit request, and joins cancellation and reset. The portable server REPL endpoint, typed first-party REPL client, first-party local stdio process, embedded connector and Eio/Notty terminal client are implemented. OCH-130 owns the research; OCH-137, OCH-138 and their implementation children own delivery.

The [authoring service design](chatml-authoring-services.md) owns source context,
overlay and immutable upload contracts used here. Implementation and validation
must preserve that boundary rather than invent a second source resolver.
The [execution trace design](chatml-execution-tracing.md) defines bounded,
read-only observations of these executions.

## 1. Purpose and existing boundary

Developer checks, test runs, live runs and a stateful REPL need explicit source, project, environment and input selection. Their identities and lifetimes differ from an agent conversation. A check executes no initializer; an admitted run may cause external effects. Neither an import nor a successful static check grants tool, process, network or provider authority.

The existing compiler already accepts exact immutable [captured sources](../../lib/chatml_sources/chatml_source.mli) and [prepared graphs](../../lib/chatml_sources/chatml_prepared_program.mli). The graph fingerprint includes selected compiler contract, environment, entry, provenance, source IDs/digests and scope metadata. [Compilation](../../lib/chatml/chatml_compilation.mli) runs in a joined, cancellable Eio domain without executing initializers. A content digest identifies bytes, not an authorized writer or a capability.

The [developer capture host](../../lib/agent_server/developer_capture_host.mli) borrows the installed source and authoring owners to prepare a bounded immutable `One_off_v1` capture. A selected local Project uses its actual source-host capture; a standalone file uses the same authorized no-follow reader and one-source Prepared graph. Editor Project contexts use their current overlay and explicit developer contract without changing the context's analysis contract. An embedded ChatMD script comes from the retained parsed input/source association and, when selected, its genuine Project Build; source-only scripts require their own current host target selection. The original and request actors, physical context or root, selected target and retained generation are checked after preparation and again before exposure. Local temporary source contexts close before a candidate is returned. Captured bytes remain the chosen program after admission even if a local file later changes; current actor and runtime permissions continue to be checked separately.

The configured Prompt that supplies Live native and managed tools chooses its script compiler ABI explicitly: `tool_contract` is `standard` by default or `developer_model_spawn` for a Prompt whose managed scripts may call `Model.spawn`. This is a per-Prompt publication choice, not a provider grant or a request-time fallback. Catalog publication captures the chosen contract against the original source and retains that exact graph in the immutable revision. A developer Live command plans with that same selected contract, selects only a matching retained binding and contract, and restores it before native resource exposure. Standard Live remains valid under Tool_v1 and cannot call `Model.spawn`; the Developer contract enables that separately authorized operation. An old Standard revision with managed declarations or projects has no matching Developer graph to retarget; republish that Prompt with the explicit developer contract before using `Model.spawn` in those scripts. A script-free legacy revision has no graph to select. New conversation sessions reject a developer-only Prompt at selection before creating effects, while existing Standard Prompt behavior remains unchanged. Restart reads the retained authored bytes and contract from the verified revision; it does not recapture current files.

The [physical Root registry](../../lib/agent_server/developer_root_owner.mli) keeps one candidate-specific final admission callback in its existing provisional slot. After its Store, tracking and Worker waits, it reruns the candidate's full owner check, rechecks the Root's original actor and capture, then performs a bounded nonyielding comparison immediately before the provisional-to-owned swap. Uncertain recovery retains the same candidate but requires a fresh checkpoint and full check; an old final receipt cannot be reused. A failed comparison exposes no Root and retains any unresolved Store custody for exact reconciliation. This is a capture-to-Root seam, not the hosted command/result service.

The [immutable developer capture and deterministic fixture check/test core](../development/chatml-developer-capture.md) is the portable foundation. It does not itself install a hosted service or grant live capabilities; the installed Developer host composes those separate owners.

A developer Live owner may resolve a configured Physical workspace or one
Temporary workspace with `on_session_stop` cleanup through the existing
[workspace resolver](../../lib/agent_session/workspace_resolver.mli) and shared
[conflict-domain lease](../../lib/agent_session/workspace_lease.mli). Physical
roots are never deleted. Developer runs have no session-delete or retained
workspace lifecycle, so Temporary `on_session_delete` and `retain` are rejected
before acquisition. The [Store-issued Live directory](../../lib/agent_store/developer_run_store.mli)
supplies runtime/cache/response paths outside the durable run document and
remains under the daemon's actual Store lock. Temporary cleanup is a finite,
cooperatively cancellable no-follow deletion of the exact owned root; it keeps
the conflict lease until cleanup succeeds, then releases the Live directory.
Partial deletion retains the same-process owner and lease for retry. At cold
open, the Store scans bounded abandoned Live children, removes only those with
its exact ownership marker, and rejects uncertain or unmarked children rather
than assuming they are safe to delete. This lifetime mechanism does not grant
source, native-tool, shell or provider authority; hosted run assembly remains
separate.

The current one-off host prepares selected source and tools, revalidates live bindings after waits and runs under a borrowed native scope ([one-off script](../../lib/chat_response/one_off_script.mli), [execution](../../lib/agent_session/one_off_execution.ml)). The [script job service](../../lib/agent_session/script_job_service.mli) and [job protocol](../../lib/agent_protocol/job.mli) provide bounded staging, selection, status, permission waits and cancellation for actor-owned work. Their current launch owner is an invocation or moderator event. Developer execution needs its own authenticated owner and run record while reusing those underlying controls; it must not mutate an unrelated production conversation or disguise a developer run as `Agent_protocol.Run`.

The [lexical script-job kernel](../../lib/agent_session/script_job_kernel.mli) shares selected-binding checks, provisional start tracking, returned `Job.start` parsing and all-ID cleanup with the existing conversation adapter. [Request preparation](../../lib/agent_session/script_job_request.mli) shares the real tool and script decoder/compiler path. The kernel records IDs only after a concrete owner stages them. Its prepared acknowledgment cannot grant permission or publish a job. The [developer adapter](../../lib/agent_server/developer_script_job_service.mli) borrows it under an authenticated physical root, native or claimed Worker scope, without constructing a conversation `Session` ID. For root actions, the lexical scope stays open through the selected parent save; it acknowledges preparation only after durable success. For native calls, an exact physical scope seals and submits the selected terminal pair. Error cleanup asks the physical owner about each staged ID, retaining selected or uncertain capacity until exact reconciliation. `Job.get` and `Job.cancel` use the current physical job-control path for both background tools and admitted model jobs. The developer `Job.get` view gives status and safe result-kind metadata; its `completion` field is null because result bytes require separate authorization. `Job.read_result` uses [authenticated result disclosure](../../lib/agent_server/developer_result_disclosure.mli), including a fresh actor check after yielding reads. For admitted root/native `Developer_tool_v1` scopes, the adapter installs the canonical Model.spawn grammar in its expiring physical scope, admits its effect through the root or native call-boundary transaction, and enables the closed async runtime mode only for the actual selected developer tool compiler contract and pinned model selection. Hosted run/REPL assembly and the shared scheduler remain separate work.

The [managed developer executor](../../lib/agent_server/developer_managed_tool_executor.mli) binds the prepared standalone definition after Root admission to that same native terminal and lexical Job scope. The native policy still admits the exact selected implementation and dispatched invocation. Before interpretation and after yielding checks, the executor rechecks the physical callback, current selected borrow and binding. It runs the existing compiled program under the inherited execution budget, projects the narrower host/deadline/output limits into the tool ABI, sends nested `Tool.call` through the selected native child path, and seals only the returned Job subset. `Developer_tool_v1` receives Model.spawn only when the Root retains an authenticated provider selection; ordinary `Tool_v1` and a providerless Root cannot obtain it. The prepared definition must come from the same trusted live-resource package used to construct the Root; equal capability fingerprints alone do not prove that provenance.

The Agent source producer prepares ordinary `Agent.run` functions from the
selected Prompt revision before Root construction. Its one-time bridge checks
the same physical function and source preparer in the installed runtime and
Root registry. A saved Tool callback then borrows the exact captured Agent
artifact and entered native invocation before lending the Root's tracked model
context; editing the original file does not replace that artifact. The selected
Prompt tool contract also governs child preparation and restoration. The
[hosted child runner](../../lib/agent_server/developer_child_agent_runner.mli)
installs the selected child's native and managed tools under that physical Root
and callback. Source-dependent child messages use parser-owned document and
image occurrences: captured content keeps its immutable bytes, local reads use
the occurrence's declaring source, and remote reads require explicit bounded
host configuration. Declaration and caller content retain distinct source
origins and share one finite conversion budget. A selected source read checks
the live owner before and after yielding acquisition; the existing Converter
keeps its role and inline-image rendering. Immediate moderator/model
orchestration runs inside the selected child's joined execution scope.

The [child preflight](../../lib/agent_session/developer_child_preflight.mli)
reads the genuine parser projection and bounded caller input before the child
prepares effects. Its literal mode retains the strict lower boundary. Its
selected mode classifies document and image content without reading it, admits
the supported selected moderator script, and rejects unowned nested Agents and
the Driver's special `fork` route. The host must prove the saved target,
physical Root and child policy
before installing any tool or acquiring dynamic content. The scan admits at
most 16,384 nodes and 8 MiB of text and propagates the caller's cancellation
checkpoint. Passing it is classification, not an execution grant.

For a `Job.start` made inside an authenticated selected child, the private
[queued issuer](../../lib/agent_server/developer_queued_issuer.mli) records the
issuing child's installed revision, prompt/workspace definitions, tool
contract, source policy and selected runtime policy as bounded comparison
data. A Tool's optional Agent target artifact stays separate from that issuer;
a Script has no Agent target. The physical native terminal stages both kinds
only while the selected Child, registered invocation, original Root effect
witness and current borrowed native scope still agree, including a source
refresh after yielding job preparation. The immutable v3 payload does not
carry a source lease or execution grant. A Worker with the installed paired
Host service decodes the canonical request from its actual Claim, restores the
captured issuing revision without rereading its original File, and rebuilds
that revision's native and managed resources under the same Root, actor,
source and runtime policy. A copied issuer cannot turn another Claim into
that child. A missing or tampered captured artifact, or a changed installed
source or runtime policy, denies without a Root-registry fallback; editing the
original File does not invalidate the retained revision. A Worker without that
installer leaves v3 rows queued; ordinary
root-issued v1/v2 work retains its existing path. The optional Agent target
artifact remains a distinct Tool target and does not authorize the issuer. If
present, the entered Claim checks that target against the same newly prepared
issuer runtime, never the Root's older native registry.

The claimed Tool call uses only the original request's rebound capability
pins. A selected managed handler validates its own binding in the restored
source registry, while its nested Tool calls and Jobs remain within its
declared dependency ceiling and the current physical native borrow. A bare
Script uses the original pins and may stage children through its same claimed
Worker action. Its outer Job kernel remains live through durable selection
and acknowledgement; issuer restoration, execution and preparation happen
inside the physical Claim. Already expired work settles without restoring
resources. A live Tool or Script restores and executes within its effective
deadline; a live Script uses one bounded execution scope across restoration,
rebind, callback and preparation, with durable settlement outside that budget.
Retained-source compilation and custody use their own work limits while polling
that scope's cancellation and deadline without spending interpreter fuel;
later ChatML interpreter execution still spends fuel.
Restored child resources and their fibers join before the Claim loan closes.
Native and managed Tool targets and Script targets are supported here.
Queued Agent Tool calls with a distinct captured target still require a
separate claimed-target admission route; v3 issuer comparison never grants
that execution by itself. Foreground ordinary Agent Tools use the selected
child admission described below.

A physically claimed developer Worker borrows the same lexical kernel through a
[Worker action](../../lib/agent_server/developer_root_owner.mli). It derives the
saved v1/v2 Script request's selected capability ceiling before effects. A v3
Script rebinds that same immutable request subset only after its issuer is
restored inside the Claim; a wider root registry cannot grant another tool.
Child reservations stage under the live claim, and the returned ID subset is
sealed. One [run document replacement](../../lib/agent_store/developer_run_store.mli)
installs the claimed parent's terminal or `Waiting_completion` state together
with exactly those selected children, bound to the parent ID, attempt and depth.
A callback returning `Queued` with selected children fails before publication.
An old-epoch claim may retain the terminal parent while omitting new children;
lexical preparation is acknowledged only when the Store proves the selected set
was installed. After a result or document save may have been attempted, selected
capacity and the prepared transition remain in custody until exact
reconciliation, without replaying the callback. A bare Worker Script uses the
`One_off_v1` compiler surface and has no `Model.spawn` builtin. Managed
`Developer_tool_v1` code inside a physically admitted native call keeps its
separately authorized model path.

The [background runner](../../lib/agent_server/developer_background_job_runner.mli)
now claims a saved Script through that Worker, decodes and compiles its captured
`One_off_v1` source inside the claimed lifetime and runs it under the saved
execution budget. Its [Script evaluator](../../lib/agent_server/developer_script_job_execution.mli)
uses the same selected native child path and lexical `Job.start` scope. A
`Task.catch` can discard a provisional child; only surviving children join the
parent's selected Store transition. An ordinary native `Fail` remains a
catchable `Tool.Error`, while a physical admission failure or cancellation
stops further effects. The worker refreshes its non-yielding local effect
witness after a selected native dispatch without widening the original tool
selection. Malformed saved async-tool payloads enter the physical claim and
become `Failed` without executing a handler. The shared result publisher keeps
an attempted result in custody through uncertain saves, so retry never reruns
the Script callback. The shared dispatcher scans attached physical roots;
hosted-run assembly attaches each Root and runner before saved jobs are
offered.

The physical Root serializes transient mutation owners and terminal settlement
through one private [mutation gate](../../lib/agent_server/developer_root_mutation_gate.mli).
Native dispatch, external model admission, post-join inactive job cancellation,
and terminal enqueue/drain use that gate around their Store mutation. A new effect rechecks the original
actor, selected binding and any parent claim after waits; exact terminal
bookkeeping retains its frozen outcome and physical scope. A failed dispatch
write reconciles its exact Prepared/Handoff once before another Root mutation
can observe the transient writer; an unknown write retains that pair. A native callback's durable
dispatch and effect entry are admitted under the gate, which is released before
running the handler. An already-entered callback may seal its frozen result
while another terminal is queued; that completion check does not grant a new
effect. Each terminal waits for its own exact installed revision, even when a
sibling is draining. If a write is genuinely uncertain, its result and staged
capacity remain with the Root for reconciliation. Worker cancellation joins
outside the gate; recovery visits retained mutations separately, so neither
path waits on its own terminal drain. The Store still owns the one durable
document writer and job capacity still owns reservations.

The run Store keeps its existing exclusive document lease while decoding and
encoding a bounded run document. Large pure codec work uses the Store's Eio
domain manager; only copied bytes or immutable encode input cross that boundary,
and cancellation joins the computation before the lease is released. Typed
payload validation and candidate projection remain on the owner domain under
one charged work budget with periodic yields. The final authorization check,
write-attempt mark and rename entry stay adjacent without a yield. This improves
scheduler responsiveness under large admitted documents without changing the
durable writer or granting a scheduler candidate execution authority.

Native resource preparation can use the same [shell host](../../lib/chat_response/agent_runtime.mli) without constructing a model inference context. The shared tool dispatcher obtains ordinary filesystem, network, directory and shell resources from that host, while the existing conversation constructor keeps its inference-enabled path. An ordinary Agent or `meta_refine` declaration requires a real inference provider unless the host supplied the exact Agent implementation; a shell runtime whose effective inherited policy requires a model reviewer fails before registry activation. Native preparation does not issue model authority to a managed script: a later `Model.spawn` still needs the selected physical provider and live owner checks at execution.

The [provider operator port](../../lib/agent_server/provider_operator_port.mli) now issues an opaque model selection for the exact authenticated actor token. It asks the existing runtime for the current [credential bridge](../../lib/inference_host/credential_bridge.mli), so a port created before provider setup can select after setup. The bridge resolves the complete host-selected target for that actor's principal; the conversation's configured service principal never substitutes for it. The selection checks the physical port and bridge, actor currentness and same-target nonsecret binding after yielding resolution and again before use. Direct runtime close also invalidates it. The [physical developer root](../../lib/agent_server/developer_root_owner.mli) can pin at most one such selection at admission; without one, Model.spawn remains unavailable. These checks establish current provider eligibility, not a future secret lease. The [shared recipe decoder](../../lib/agent_protocol/model_spawn_request.mli) preserves the existing `agent_prompt_v1` grammar and unknown JSON; the conversation module retains that exact API. [Durable developer intent](../../lib/agent_protocol/developer_model_call.mli) stores the original request and full nonsecret target in one bounded versioned envelope. Developer create, load and recovery reject duplicate JSON keys and unsupported recipe versions. The [run Store](../../lib/agent_store/developer_run_store.mli) appends a fresh queued model job under a run root or exact Dispatching invocation through its existing one-document Prepared/Handoff transaction; ordinary selected-job transactions reject this job kind. The root/native call-boundary actor now re-admits the original request under the actual Store limits, derives owner and ancestry from its physical scope, checks the pinned provider selection and live parent after waits, and uses the same Store Handoff with shared job capacity. A single retained mutation slot excludes competing writes while an attempted outcome is unknown. Installed model jobs survive a later parent callback failure; an exact selected `Job.start` save rereads the advanced document. A copied stored intent grants no authority, and secret acquisition and execution remain separate. Model.spawn remains an immediate external effect at its call boundary, independently of the rollbackable `Job.start` selection.

A live, entered `Model_call` Worker can also admit a nested `Model_call` at its own immediate call boundary. The [Root inference scope](../../lib/agent_server/developer_root_owner.mli) supplies the physical Claim and current selected actor/provider; the same external-model mutation slot, Store transaction and shared capacity handle the child. The [Store](../../lib/agent_store/developer_run_store.mli) binds the Claim to a lookup from the exact observation, requires the parent to remain Running when preparing, and checks that same physical Claim again before rename under its writer lease. The canonical document records a `Run_root` child with the exact parent job/attempt and next depth, then keeps that ancestry valid after the parent becomes terminal; terminal history never grants a fresh child effect. Two deliberate equal-payload calls create separate children, while reconciliation of one Prepared write cannot append its child twice. A bare `One_off_v1` Worker still has no Model.spawn operation.

An entered saved-Agent `Async_tool` can use that same inference tracker only after the [captured runner](../../lib/agent_session/captured_agent_runner.mli) checks its exact stored artifact, registered source and physically selected native implementation. It lends an expiring proof from the active callback. The Root compares that proof with its own registered native terminal, original actor, current run and selected provider before admitting an attempt. It refreshes the full source proof after yields and checks the live selected callback and terminal again at final nonyielding writes. The [shared artifact codec](../../lib/agent_protocol/developer_agent_artifact.mli) lets the Store classify the exact v2 saved-Agent payload as historical inference-row evidence; a copied payload, another `Async_tool`, or a completed source artifact cannot enter that callback. The existing `Model_call` claim path remains strict. Saved-Agent external Model.spawn children use the existing native-parent route and the same Store tracker, capacity and recovery path.

The [canonical model recipe](../../lib/chat_response/model_executor.mli) has one strict `agent_prompt_v1` body for prompt fetch, configuration overrides, selected-target capture, same-target context derivation and nested execution. Its existing conversation wrapper still decodes before call execution, maps expected and unexpected failures to the established `Model_error` result, and owns its local asynchronous registration, reinjection and wakeup. A host can instead carry one physically installed `agent_prompt_v1` recipe and inherited prepared-dispatch hook in an immutable [execution context](../../lib/chat_response/ctx.mli). Driver, Fork and auxiliary inference forward the hook and the composed application-submission guard; a physically installed recipe does not construct a second local Model executor. The context only carries closures; the developer host must issue and recheck the actual Root/Worker/provider authority when assembling that recipe.

The [shared inference runtime](../../lib/inference_runtime/inference_runtime.mli) now carries an opt-in application-submission guard through actual prepared attempts. The selected OpenAI HTTP and WebSocket backends call it after credential resolution, connection or handshake, and transport-selection observation, immediately before the first model-request bytes. A guard first refreshes each inherited owner check, which may yield, then runs all returned attempt-local checks without yielding; nested and auxiliary contexts retain their parent's guard. The backend rechecks the actual credential lease after the guard. This checkpoint does not authorize a provider or persist an attempt by itself. An already admitted request can race later revocation, and a connection or WebSocket upgrade may precede application submission. Ordinary conversation executions carry an empty guard and retain their existing behavior. The developer model worker must supply its physical actor, run, claim, provider and deadline checks when it is installed.

The developer run authority document also retains bounded inference-attempt rows for admitted model jobs. Each row carries a stable run/job source, a monotone attempt ordinal, the safe configuration and observation record, and an explicit Prepared, Running, Terminal or Interrupted state. Its owner, capture, job attempt and nested parent are checked against the same versioned document. A later job retry retains earlier attempt evidence, and a delayed nested call may cite a terminal parent inference while the model job's physical worker remains active. The [run Store](../../lib/agent_store/developer_run_store.mli) admits a new row only through an entered claim for the current Running model job, then settles it with the existing one-document writer and exact pending token. Every document write reserves aggregate and whole-envelope capacity for all active inference rows to take their exact recovery interruption state, so another write cannot consume that recovery space. This reserve covers inference interruption; a later job terminal outcome is admitted against its own document limit. New inference effects wait for durable admission; an uncertain write retains the exact token for reconciliation without rerunning a callback. At startup, the one-shot recovery window interrupts stranded Prepared and Running rows with distinct delivery uncertainty before it retires abandoned jobs. These stored rows are evidence for later physical tracking; they do not independently authorize a provider request or prove that a nested call binding came from an admitted tool candidate.

The physical [Root inference scope](../../lib/agent_server/developer_root_owner.mli) borrows the entered Model-call worker, the Root's exact bridge-issued context and the original actor. It reserves a bounded active-attempt slot before saving Prepared, compares the actual Started callback's scope, accounting ID and complete prepared configuration, and retains immutable observation and terminal plans behind the Store's sole uncertain write. Reconciliation settles that physical pending token first, then refreshes the document observation for each deferred sibling update in issue order. A joined callback may lose new-effect authority while its earlier registration still needs truthful terminal bookkeeping. The Root retains capacity and its run claim through unknown writes, including shutdown; it releases them only after exact inference and worker reconciliation. The real stream emits an admitted tool-call token after the call commits and before its parent inference completes; the Root checks that token against the active physical Attempt and Registration. Only the bounded Scope, entry ID, alias and Registration binding remains until children join, allowing a delayed nested child after the parent inference is terminal without retaining the parent request or execution closure. The binding is provenance, not new effect permission. The model runner supplies the Root's strict ports and composed submission guard when it connects this scope to the provider transport.

The [developer tool compiler contract](../../lib/chatml/chatml_compiler_contract.mli) has a distinct static target with the unchanged `tool_v1` `run(context, input)` ABI plus only the existing `Model.spawn` and `Model.spawn_text` builtins. Planning, captured graph identity, managed preparation and code rebinding retain that exact choice; ordinary `tool_v1` remains unchanged. Both single-file and linked standalone execution filter all external asynchronous operations by default. The selected managed callback receives a typed runtime coordinate derived from its actual admitted implementation, enabling only the canonical Model.spawn operation when the root pins an authenticated provider selection. The installed handler still checks the live lexical, actor, provider and parent scope at each effect. Its completed request remains an external call-boundary effect if a later task is caught, discarded or fails. The compiler surface and runtime filter grant neither provider credentials nor permission; production hosted execution must select the same contract when assembling the developer run callback.

### 1.1 Owner-neutral native admission foundation

OCH-287 gives a developer invocation its own [owner coordinate](../../lib/agent_protocol/execution_owner.mli) and [host invocation record](../../lib/agent_protocol/host_invocation.mli). A conversation record is held whole, preserving its existing routing, provider/history and publication metadata. A developer record uses a dedicated validated `Id.Developer_run`, durable host ID, principal and run epoch, with no synthetic Session ID. The later host bridge creates the developer run ID before capture admission and gives `Chatml_developer_test.Run_id` its string form; the protocol layer does not depend upward on the capture implementation. These coordinates are comparisons, not execution authority. The durable server ID may survive a process restart, so liveness also requires the physical issuer and the host's current authenticated capture/epoch check.

The [developer invocation actor](../../lib/agent_session/developer_invocation_owner.mli) accepts a trusted callback that verifies the actual OCH-265 admitted capture fingerprint, owner and current authority. Its durable compare-and-set ledger records Dispatching before effects, then a single resolved/cancelled outcome. After reads, dispatch saves and permission waits, the actor and [shared native policy](../../lib/agent_session/native_tool_invocation.mli) recheck currentness, deadline, direct-parent proof and exact selected implementation before an effect. A successful currentness callback may yield, so the final parent/deadline check uses a non-yielding clock sample. A child borrow can narrow its parent's selection; each child resolution intersects that admitted ceiling with the current host registry by exact capability ID and fingerprint. Revocation or replacement never falls back to a same-name tool. Root and child paths have different typed entrypoints; an expired or foreign parent proof cannot become a root claim.

This foundation preserves the existing conversation admission path and its Function/Custom routing. Developer native calls accept either an object-input function or a string-input custom registration through the same schema and permission checks; a managed standalone handler still requires a function call. It does not install a developer run host, persist run receipts, schedule jobs or authorize a live provider. A developer permission subject has only `Approve_once` and `Deny` until a separately owned durable grant policy exists. Terminal bookkeeping uses the original ledger authority even after execution authority is revoked; a failed terminal save reports uncertainty and leaves the durable Dispatching evidence for recovery, never replaying a possible partial effect. The shared job foundation first distinguishes conversation and developer records and charges them through one capacity ledger. Its developer run bucket is stable across epochs, so a worker claimed before reset remains charged until its actual cleanup; exact owner and attempt still distinguish reservations. The later job tickets will join the existing scheduler and result custody to this owner. The developer run host wires the admitted capture, authorization and run lifecycle to both foundations.

### 1.2 Durable run checkpoint foundation

OCH-297 adds the [developer run store](../../lib/agent_store/developer_run_store.mli) beneath the real [daemon-owned Session_store](../../lib/agent_store/session_store.mli). A physical root claim and serialized lease use the existing daemon lock; session-store close waits for an in-flight developer operation before releasing it. The additive `developer-runs/` directory has one bounded versioned authority document per run. Its final layout includes invocation and job arrays, initialized empty; OCH-303 adds their typed document transitions. Older roots without this directory start empty; there is no intermediate milestone schema or synthetic conversation handle.

The foundation persists durable host/run/principal identity, exact admitted capture fingerprint and ordered epoch checkpoints. A reset interrupts the prior epoch; an old handle cannot reactivate it. A prepared checkpoint carries exact observed before and candidate after bytes, so failed or cancelled writes are reconciled against the authoritative file before any higher actor decides what happened. The store syncs its owned directory before a retry or reconciliation treats a visible rename as durable; opening also syncs the daemon root so an existing subtree created by an interrupted attempt is not mistaken for a durable entry. A normal epoch/run quota rejection is typed `Limit_exceeded`, while a malformed over-limit document on disk fails recovery. Bounded recovery rejects linked, malformed, foreign-host and unknown authority files. A recognized unfinished atomic-writer temporary is not a committed run. This store does not authenticate a principal or grant execution; the selected-job actor must bind live authorization and capacity to the same document transaction, with no separate job writer.

### 1.3 Selected invocation and job document transactions

OCH-303 extends that one authority document with bounded, validated developer invocation and job records. An absent invocation can become durable `Dispatching` under an exact observed document revision before its native callback runs. A resolved invocation and its selected child jobs, or a root checkpoint and its selected jobs, are prepared as one replacement. The lower store checks immutable owner, epoch, capture, revision and record relationships; it does not select a live capability. A selected transaction requires an opaque, physically bound handoff under the same root lease. `Installed` means file and directory durability, `Absent` means the exact previous bytes remain authoritative, and `Diverged` quarantines the handoff. A settled pair returns its original historical receipt on retry, even after a later valid mutation; the caller observes again for current state. An uncertain pair blocks subsequent mutations of that run until bounded reconciliation settles it. The capacity callback may repeat after uncertain I/O, so it must be idempotent; native effects never repeat because of settlement. A close invalidates that physical handoff but retains the authoritative document. The later actor must retire only the run's unclaimed reservations, reconcile durable rows in the existing daemon-wide capacity ledger before admitting new work, and keep claimed workers charged until real cleanup; the lower store does not perform this capacity recovery.

Recovery checks that recorded invocation and job parents exist in the same owner epoch, that a job's invocation parent is terminal, that a child and its invocation agree on their optional parent job, and that stored ancestry has no cycle. A parent job attempt is historical and may be earlier than its latest attempt after retry. A run-root launch can also have a real parent worker; the direct root-action prepare path accepts only root jobs without one, while claimed-worker native staging derives its exact parent job, attempt and depth from the physical claim. Recorded nesting depth is a nonnegative policy coordinate, not a graph-derived proof. Structural recovery cannot prove the current physical worker claim or authorization; OCH-304 supplies live claim proof and joins actual workers with the shared capacity ledger. OCH-312 owns root-action actor admission; OCH-320 coordinates native terminals; OCH-322 and OCH-323 own selected policy and effect-entry ancestry.

### 1.4 Physical developer job claim

The OCH-304 [Store claim](../../lib/agent_store/developer_run_store.mli) changes one exact queued attempt to `Running` in the existing run document and establishes file and directory durability before issuing a physical claim. The [worker scope](../../lib/agent_session/developer_job_scope.mli) consumes that claim once under a live Eio scope, with child work joined before the attempt's final state is persisted. The [capacity adapter](../../lib/agent_server/developer_job_worker.mli) uses the same daemon-wide ledger as conversation jobs; its stable run bucket remains charged while a claimed old-epoch worker finishes. Reset revokes new effects but permits exact terminal bookkeeping for an already claimed attempt. A returned retry or dependency wait can publish only while the epoch remains current, checked in the same Store observation used to prepare the write.

The worker retains an exact prepared document on uncertain terminal persistence and never reruns its callback. If another authorized write wins before the first replacement attempt, the Store retires only that pending token and reports a distinct prewrite conflict; the worker may reread and reprepare the same claimed attempt within a finite bound. Once replacement has been attempted, even an apparent mismatch remains uncertain and retains the exact prepared bytes for recovery. A callback-proposed end state must preserve the claimed attempt and start time and pass the validated retry policy. An invalid proposal becomes durable failure; unexpected exception or cancellation attempts terminal bookkeeping while preserving the original failure identity. This boundary grants no native effect or result-read authority; OCH-313 and OCH-291 supply those separately.

The shared [scheduler](../../lib/agent_server/job_scheduler.mli) keys conversation workers by owner-qualified job identity and exact attempt. An authenticated cancellation passes the original actor and its returned durable job, so another conversation with the same bare job ID cannot be signalled. One owner chooser rotates from the first owner considered, even when its physical claim fails. The [developer Store](../../lib/agent_store/developer_run_store.mli) projects queued jobs from a bounded latest-Active read; a physical Root and at least one exact Tool/Script or Model runner must be attached before the dispatcher scans that run. Both packages share one attachment, scan, offer path and capacity ledger; a kind without its installed package stays queued. The scheduler retains at most one 64-candidate retry batch after a capacity rejection and one fresh 64-candidate scan batch. A full shared-capacity rejection does not advance the developer owner's turn; subsequent offers recheck the physical Root and Worker. Exact candidate/attempt offers coalesce under the Root's configured worker bound; the existing Worker and shared capacity ledger alone admit execution. The chooser awaits only a nonyielding capacity decision, never a Store claim or handler, so a developer turn competes fairly with conversation work at capacity one. A long Script runs in a joined offer fiber. Malformed saved payloads for installed Tool or Model packages enter the same physical Claim and persist Failed; unsupported kinds stay unclaimed. A scan hint does not suppress periodic fallback. Retirement cancels and joins that Root's scans/offers before its physical owner closes; entered claims retain the Root/Worker's result custody. Hosted-run assembly still owns creation, attachment and shutdown ordering of actual Roots and runners.

For a selected result, the same worker accepts a pure terminal selection only after the callback's child switch has joined and its proposed state has passed validation. It binds the selected terminal state to the physical Claim and retains the publisher's immutable preparation token with that Claim and capacity lease. The publisher must establish its private intent and staged bytes, then enter non-discardable attempted custody before the worker's existing terminal Store transition. An uncertain stage or parent write retains the same selection for bounded reconciliation; a possible parent write is reconciled before any retry. Only an exact Store-issued Installed receipt for the selected job can trigger best-effort intent cleanup. Typed cleanup failure leaves the private marker for later retention. An unexpected exception propagates after the installed claim and capacity are settled; neither failure changes the installed job. The worker does not itself define the result format or grant disclosure; the shared publisher supplies those contracts separately.

The existing conversation [result store](../../lib/agent_store/job_result_store.mli) uses one private preparation phase machine for intent staging, attempted publication, acknowledgement and safe discard. An uncertain callback keeps its exact selected reference and makes discard unavailable; a successful callback retains it before best-effort marker removal. Restored verified preparations resume in the attempted phase. The conversation result reference and version-one private intent bytes are unchanged. The phase machine itself grants no developer Store transition or result disclosure.

On startup, the root owner holds the original physical run claim before exposing a worker. A one-shot Store recovery window lists bounded persisted `Running` and `Waiting_permission` attempts across the run's epochs and admits only an exact terminal transition for the original job, attempt and capture. `Waiting_completion` belongs to the already-ended dependency lifecycle. The window excludes ordinary claim/effect preparation until every abandoned attempt has a truthful terminal disposition and the selected write has settled through the normal Store writer. A default admission rejects abandoned attempts; it never infers success or interruption from status alone. Uncertain writes retain their original prepared bytes, provisional owner slot and claim for exact reconciliation, with no callback replay. A revoked actor cannot resume the callback or replace a frozen result; this API has no fresh-actor rebind. Result-intent proof and any separately authorized cleanup after revocation belong to the publisher/host layer.

The [result storage root](../../lib/agent_store/developer_run_store.mli) comes from an existing authenticated run document under the live daemon Store lease, including retained older epochs. [Blob storage](../../lib/agent_store/blob_store.mli) binds to that same physical Store and filesystem, stages bytes through its existing coordinated writer, and rechecks the root on every verified result read. Developer metadata carries an explicit owner target; an absent conversation session target cannot become developer custody. Conversation collection holds one Blob coordinator and the same daemon-root gate from its complete [cross-owner reference proof](../../lib/agent_store/blob_retention.mli) through deletion. A bound live Store supplies a single reader-charged inventory of all historical run documents, private result intents and Blob pairs; the collector checks their exact owners, bytes and decoded reference strings before deleting an unreferenced conversation result. With no bound Store, the existing session gate excludes a new developer claim while all authentic developer namespaces must be empty. Unknown, linked, corrupt or incomplete evidence, exhausted budgets and a stale binding stop deletion. The custody root alone still grants neither publication nor disclosure.

The live [developer result publisher](../../lib/agent_store/developer_result_publisher.mli) freezes the worker's validated, post-join terminal Completion before writing anything. Automatic selection keeps a small complete result inline only if its entire versioned private intent fits; explicit Artifact always selects a Blob. Neither mode truncates the canonical Completion. The intent's filename includes the original run epoch, job and attempt under the daemon-owned `developer-result-preparations` directory. Conversation and developer intents use the same checksummed, no-follow staged-file writer, while conversation version-one bytes and paths remain unchanged. The developer intent is saved before its Blob stage, and the shared phase becomes non-discardable before the worker's single terminal Store CAS. An exact Store-issued Installed receipt acknowledges and retires the process-local selection; typed marker cleanup failure leaves its file for recovery, while an unexpected cleanup exception propagates only after the installed job and capacity have settled. Collection may later retire an exact retained Inline marker when the real historical terminal agrees, or an Artifact marker after its terminal and complete durable Blob agree; missing or uncertain evidence never creates orphan-deletion authority.

The [claimed Tool and Script runner](../../lib/agent_server/developer_background_job_runner.mli) now accepts only a physical scheduler candidate and reads the saved request again from the entered Claim. It runs a selected Tool Function, Custom or managed target through the existing native policy under the saved execution budget and remaining deadline, then gives the joined worker's exact terminal Completion to that publisher. An owned selected child becomes a `Waiting_completion` dependency with the managed declaration's completion schema; an unowned subscription cannot masquerade as one. A saved Tool request pins only its entry target. When the native kernel admits a managed implementation, its opaque prepared execution supplies the distinct lexical ceiling of declared dependencies before user code runs. The lexical Job scope rechecks those pins after waits, so the managed handler can start an authorized dependency without acquiring an unrelated root tool. The native dispatch still retains the original target fingerprint. A post-rename uncertain result save keeps the selected artifact and charged attempt; offering the old candidate again cannot rerun the handler.

One [Root-bound result owner](../../lib/agent_server/developer_root_owner.mli) binds the existing publisher once for Tool, Script and Model workers, so their pending selections share the same cap and exact reconciliation path. The [claimed Model runner](../../lib/agent_server/developer_model_job_runner.mli) redecodes the saved versioned request and full provider target from the entered `Model_call` Claim. Inside its live inference scope it uses the selected provider context, durable attempt tracking, admitted-call provenance, strict shared recipe and genuine source/approval services. Nested `Model.spawn` admissions use that same Root's external-model transaction, Store writer and capacity; no local model queue is constructed. Its composed Root and provider guards check after yielding work and at HTTP/WS application submission. A completed provider response retains its truthful terminal result even if the actor is revoked after the effect; any subsequent effect or result disclosure still needs fresh authorization. The Worker passes the joined Completion to the shared publisher for Inline or Artifact selection and exact uncertain-write recovery. Hosted assembly must supply the real source and permission services and install the package under the admitted Root.

An ordinary Agent.run tool can attach a saved child artifact only through the selected native registration's source preparer under a live Root action, native invocation or claimed Worker parent. That launch label is comparison metadata; the physical parent and original selected binding are checked before and after preparation. At execution, the entered Worker decodes its own saved request, verifies the current runtime has the exact native function and an installed source preparer, and restores the pinned child artifact before any file-source branch. A changed or deleted original child file therefore cannot silently replace the selected code. The headless child keeps its own source and content revisions: an embedded content-agent occurrence uses its defining captured revision and does not consume the enclosing tool's saved pin. Selected native callbacks repeat Root, Claim and invocation checks after yielding restoration. Without an installed runtime, a valid saved Agent artifact stays queued; a raw artifact reference, file path or correlation label is never an installation grant.

The captured runner can lend the exact restored saved target to a trusted selected-child callback; a missing callback keeps the strict literal child path. A nested selected invocation records its live physical selected-borrow ancestor. Root admission compares the saved parent against its original registered invocation and the nested child against its distinct, durably dispatched invocation object and current child action. The child's action may shadow the parent's fiber key, so the descendant check uses the parent's shared terminal state only after proving the child's entered terminal and exact Root. Direct parent completion still requires its own action key. The shared lexical Job kernel resolves the selected capabilities at each check; a failed current-selection lookup denies the effect, and completion repeats the terminal check after any yielding lookup. This source/terminal proof does not construct child tools or grant a child capability by itself; the hosted child resource owner must separately authorize and install those effects.

The lower captured-source interface prepares parser-owned ordinary Agent Tool registrations for an exact selected child and lends an expiring Tool target only inside that Tool's physical native callback. The prepared registration retains the child's original File/Captured source lease, parser reference and immediate selected parent; it does not install a child runtime, Root frame or provider context. The Live owner verifies the selected revision and prepares those registrations before native resource preparation. Child preflight accepts an ordinary Agent declaration only when that exact preparation supplied registered implementations. The registration's implementation revision includes the effective selected-source policy and validated active Agent-call limit; queued issuer comparison also retains the selected policy. A prepared registration or an equal artifact cannot be invoked as a standalone capability.

For a queued selected child, the physical Worker Claim restores its issuing revision through the same Live source binding and original captured source lease. The bound Agent source runtime supplies its existing Root-checked Context and selected child callbacks as one pair. Inside the Claim's joined switch, the lower producer derives the binding and revision from the opaque claimed source, prepares parser-owned ordinary Agent registrations, and admits those declarations before extension initialization. The constructed runtime is checked against each registered native function, `Source(Some)` preparer and implementation revision before the Claim can run a handler. If that child staged a Tool with a distinct saved Agent target, saved-artifact admission uses this claimed issuer runtime and the exact selected target binding. It does not reopen the issuer's original File, borrow a foreground child target or fall back to the Root native registry. Source or policy revocation, cancellation and an uncertain result retain the original Worker and Store custody rather than replaying a possible effect.

The trusted child runner binds one pre-target Context loan and post-target Tool callback from the same Live, Root and native package. The first checks the entered Root child frame, actual selected native implementation and parent admission before lending the guarded Context to the parser-owned source runner. A valid Agent-call depth is checked before that runner selects a File or Captured member or compiles its child. The lower runner then selects the source and mints the Tool target within its physical native callback. The second callback enters the same child-only native, managed, Job and provider kernel with the new target; denial never falls back to a saved Worker Claim or literal runner. The daemon validates the maximum active synchronous Agent calls in the range 1–64 and defaults to 8. A claimed saved child begins at depth 1; each ordinary Agent Tool descends once, while Content and Model children carry the current depth unchanged. The selected Tool and its descendants share the existing aggregate content budget, and expiry or revocation closes both callbacks.

For an installed hosted child, the [private child runner](../../lib/agent_server/developer_child_agent_runner.mli) uses the restored saved target and the same Live workspace, source, actor and Root owner. It prepares the selected child's own native and managed bindings before exposing function schemas to the provider. The first native entry receives the child's admitted registry; a managed handler keeps its own binding while nested calls use only its declared dependency ceiling. Each effect refreshes source and Root admission after yields, and completion checks the physical terminal before durable publication. Provider tool results use the same disclosed canonical outcome envelope as other native dispatchers. Document and image content uses the parser occurrence's declaring source, while invocation content keeps its separate caller source. Authenticated nested Agent content selects each descendant's own source and parser Port under the same Root and aggregate content budget, then prepares that descendant's source-owned native and managed bindings before provider exposure. Synchronous content recursion may share the entering native borrow; a distinct physical native descendant is proved only when a tool callback actually enters one. A missing or revoked File member cannot be replaced with ancestor or captured content, while a recorded Captured member retains its original bytes. Immediate Model and moderator execution uses a joined scope inside that selected child callback.

The selected `agent_prompt_v1` recipe shares the ordinary decoder, target overrides,
context derivation and result envelope. Its local `prompt` string selects a bounded
member relative to the authenticated declaring source; the string itself grants no
read access. `source_mode` defaults to `file`, which checks the original live source
lease. Explicit `captured` requires a local member in the retained immutable inventory.
Neither mode falls back to the other, and a conversation executor without selected
captured custody rejects that request before fetching. Remote executable selection
is denied before network I/O; a document/image read permission does not grant it.
The selected member receives a distinct Model target, with its exact parent and
expiring source port, rather than a fabricated parser Agent occurrence.

The child restores its own retained programs under the configured script policy and
borrowed startup budget. It cannot substitute its parent's programs. Saved, content
and Model descendants retain the same aggregate content budget: executable source
acquisition and its observed byte/work debit run under one short gate, which closes
before runtime initialization, provider work or nested execution. A proven denial
before acquisition leaves the allowance usable. Failure after acquisition has begun
makes the budget unusable when the actual cost is uncertain; catching that failure
does not refund it to a sibling. Existing separate ChatML project and compiler
limits continue to apply. Source/Root checks run again after yielding preparation,
at inference attempt entry and immediately before application submission.

Saved and Content children select their Config through the Driver preparation.
A Model child arrives with its Config already applied by the Model recipe and its
inference session detached. Its Driver preparation retains that selected Context
without applying the overrides again. The shared preparation still preserves the
parsed document, input, child directory and inherited guards.

The Root admits each selected child's model configuration through a lexical
child scope. It independently reads Config from the authenticated revision, derives the expected
Context from the current authorized parent, and compares the complete target with
the selected preparation. Execution uses the Context returned by the Root. The
original dispatch guard remains installed and checks that exact child target;
ordinary workers outside a child scope retain the original target check. Nested
and concurrent children have separate scopes, which expire on exit. Saved and
Content children preserve the inherited inference session; Model children detach
it, matching the existing Model recipe. Source, native ancestry, actor and provider
checks still apply after waits and before effects, including when a real native
child shadows its parent's callback. A copied target or expired scope grants no
access, and a failed direct check cannot fall back to descendant admission.

Inference tracking prepares and commits each admission under one short Root
mutation lock. Source admission waits for an active writer, then validates a fresh
Root snapshot; an uncertain retained write still denies admission. A child keeps
its source admission and Root witness from the same completed refresh. Final
checks compare the exact owner and selection revision without yielding; a sibling
tracking write may advance the effect revision, but cannot renew an old witness.
Provider calls and source, registry or user callbacks run outside that lock.

The selected moderator uses the existing entry-based Session_start, foreground
stream and bounded completion drain. Each drain has its existing policy limit;
there is no new combined lifetime event counter. Startup can leave a bounded queue
remainder for a later safe point without waiting for jobs that need the foreground
turn. Local Model jobs belong to a nested switch that remains inside the selected
child's lifetime. Early termination cancels and joins them before the source scope
expires, then audits worker outcomes so an intentional stop cannot hide an unrelated
failure. These immediate jobs do not replace the durable developer scheduler, and
saved source bytes never reconstruct an expired mutable moderator.
The immediate runner rejects requests to start another turn or compact mutable
session state, and reports paused or UI-dependent execution instead of claiming
completion. An early foreground stop retains assistant text already accepted by
the in-memory stream; this observation does not imply a durable conversation write.

The shared [Driver streaming entry](../../lib/chat_response/driver.mli) accepts
an already selected inference Context, identity-bearing history and allocator,
tool descriptors and dispatch, and an explicit optional moderator. Ordinary
script-bearing nested Agents reach the same streaming loop after their existing
setup; the nonstream `run_entries` path remains separate. The prepared entry
does no source acquisition, runtime construction, ChatMarkdown conversion,
Fetch or recursive Agent/Model-executor setup. It returns the full history,
including its original prefix, and exposes the same semantic assistant-text
projection as ordinary nested execution. Those supplied values are composition
data, not an effect grant: a hosted child must separately bind its source,
content, tools and optional moderator to the actual physical owner before
entering this loop.

The [developer result disclosure service](../../lib/agent_server/developer_result_disclosure.mli) reads only under a current authenticated request actor and an explicit host policy. Its selection carries the original host, run, epoch, principal, captured source digest, job and attempt as comparison coordinates. The Store reopens that exact historical job through its physical root, even if a newer epoch is active; queued or running jobs stay pending, while a failed terminal without a result is distinct. Inline completions and artifact descriptors must match the persisted terminal. Artifact reads reborrow the existing Blob coordinator and verify durable metadata ownership, canonical use, whole bytes, length and digest before bounded JSON/Completion decoding. After yielding I/O, the service rereads the same Store job and rechecks both the original and current actor plus policy immediately before returning frozen completion data. A descriptor or Blob ID alone never grants disclosure. Disclosure remains separate from the collector's authenticated cross-owner reference graph and cannot mint collection authority.

The [developer command storage design](developer-command-storage.md) separately describes top-level Check, Fixture_run and Live_run receipts, bounded recovery and the distinction between a fresh execution acknowledgement and a stored observation. The installed Developer host composes this storage with its separate source and execution checks.

The [startup result inventory](../../lib/agent_store/developer_result_recovery.mli) holds the existing Blob coordinator while one bounded reader checks every authentic retained run and epoch, the complete sibling intent directory, and temporary and durable result Blob evidence. It joins a selected marker to its exact historical job, capture, attempt and Store recovery candidate. A completed Artifact with a retired marker remains valid only when its persisted terminal job and durable metadata/content agree. Verified empty structural directories prove no selection; missing or partial selected content retains an incomplete selection without inventing its Completion. Corrupt complete bytes, unrelated stage evidence, foreign keys, links and exhausted budgets reject the whole inventory. A verified durable result can still be selected when an extra temporary partial fragment remains for later cleanup. Exact resumption uses the original physical recovery window and its current Pending, re-establishes only the same intent and stage through the existing writers, and acknowledges only the Store's actual Installed receipt after its sole terminal transition. It never runs the handler, chooses a replacement result or grants disclosure. The collector reuses its canonical run/intent/Blob join under a separate long retention borrow, and requires a complete proof where startup recovery may retain an incomplete selection.

### 1.5 Developer root staging and lifetime

The [root owner](../../lib/agent_server/developer_root_owner.mli) binds one original authenticated actor, the exact [immutable capture](../../lib/chatml_developer_capture/chatml_developer_capture.mli), a pinned selected live capability registry, a fixed run epoch, and the existing daemon-wide [capacity ledger](../../lib/agent_server/job_capacity.mli). The [Store](../../lib/agent_store/developer_run_store.mli) owns a nonyielding physical claim for each stable run ID; it excludes duplicate owners and epoch admission across independent registries without closing the shared Store or affecting unrelated runs. An in-flight epoch admission also excludes a new claim. A clean close joins its root action and physical workers, retires only that run's unclaimed reservations, and releases the claim. The [worker close policy](../../lib/agent_server/developer_job_worker.mli) joins physical work without retiring unclaimed capacity while the root owner has an unresolved selected handoff. Unknown persistence retains the claim and finite capacity quarantine, preventing same-process replacement until exact reconciliation. The host closes this owner registry before the shared Store and Session_store; a failed close is reported, not treated as a released lock.

A root action owns a child Eio switch and stages typed [background requests](../../lib/agent_session/developer_root_job_admission.mli) provisionally under the selected host policy. The callback returns its value with an opaque one-shot selection from exactly its own staged jobs. Children join before the selected Store transaction. The durable replacement records a checkpoint phase and those selected job rows together; it does not persist the callback's arbitrary return value. One idempotent handoff publishes selected capacity and aborts unselected reservations only after the exact file and directory barrier. A callback error or cancellation aborts provisional capacity and does not rerun its effects. The developer host owns the user-facing result, action idempotency and terminal receipt; result/artifact custody follows the separately reviewed OCH-291 contract.

Within its live action, an unselected provisional job can be rolled back by its exact issuing scope. The action keeps that entry as an aborted record under the same finite staging limit, so repeated rollback is idempotent without allowing unbounded stage/rollback cycles. An escaped action cannot newly release capacity after its bracket ends, and a sealed or uncertain selected handoff cannot be rolled back lexically. The physical root can look up a current job through its Store and cancel only the same immutable owner, capture and attempt. A queued job, queued retry or already-ended dependency wait uses the shared Store's exact-observation transition to durable `Cancelled`; a retry keeps its prior attempt and absent current start time. A queued retry or dependency wait first joins any still-registered physical worker and is reread before that write. A running or permission-waiting job receives cancellation through its actual worker claim; a returned request or join does not claim that terminal persistence has completed. Store uncertainty retains the exact prepared cancellation and one bounded owner recovery slot, preserving capacity until reconciliation without replaying the callback. A prewrite conflict can retry only that original attempt after a fresh read; an attempted write cannot be replaced blindly. Job lookup and cancellation do not themselves grant result disclosure or a new effect.

The [terminal queue](../../lib/agent_server/developer_terminal_queue.mli) now bounds active plus completed native terminal scopes and retains a separate root slot. The root owner registers callbacks before their first yielding admission read, then freezes each completed root phase/selection or resolved native outcome/selection in FIFO order. One drainer uses the existing Store Prepared/Handoff writer; it never holds a lock across a callback. A proven prewrite conflict may reprepare that same immutable intent a finite number of times. An attempted or unknown write retains its exact prepared pair, intent and capacity while later completed intents wait; neither callback nor external effect is replayed. Completed intents invalidate physical effect witnesses, and known settlement issues a fresh witness so a still-live parent can resume after checking current authority. An Interrupted/Closed root phase rejects before enqueue while any native callback or terminal intent remains, then rechecks at nonyielding Store admission. Native terminal custody requires exact durable Dispatching and a resolved canonical outcome. A submitted terminal cannot discard an earlier frozen outcome/selection. If an interrupted registration exits after durable Dispatching without submission, trusted recovery rereads that exact record and either preserves its already sealed pair or saves a caller-produced Fail/Cancelled terminal; it cannot invent an unsealed success or replay the callback. A transient read failure can retain a scope that never dispatched; a second exact read may release it only if the callback never entered, no selection was sealed, and the durable record is absent or still Admitted. The read and physical slot retirement have no yield between them. Dispatch admission must join inside the registration bracket, so no legitimate dispatch can start after that proof. If initial admission fails before any scope is constructed or exposed, the bracket directly retires its private slot: it could not have dispatched. Terminal-only native bookkeeping with no selected jobs can complete after actor or selection revocation under exact Dispatching custody; selected publication still needs current authority. Close cancels and joins all callbacks and workers before draining already completed intents where authority permits, and retains the run claim if any exact terminal custody remains uncertain.

OCH-322 connects the [selected native executor](../../lib/agent_server/developer_native_executor.mli) to the same Function, Custom and managed [policy kernel](../../lib/agent_session/native_tool_invocation.mli) used by conversation and no-child developer calls. It derives its issuer from the physical root, stores exact Dispatching through that root's one Store writer before entering the callback, and rechecks current binding, authorization and live direct-parent proof at effect entry. A managed callback receives a scoped stage operation; it passes explicit IDs alongside its canonical outcome while provisional work remains live. The physical terminal seal validates that each selected ID was staged in this scope. A late failure cannot replace a sealed Complete or Pending result and its selected jobs. An uncertain Dispatching write retains its exact Prepared/Handoff and an immutable Fail/Cancelled terminal; the same owner's bounded reconciliation pass proves absent dispatch or installs that terminal without rerunning the handler. A terminal save made after callback exit retains its original canonical outcome and selection through the same pass. OCH-323 admits a direct `Job_child` only from the root's actual claimed worker and inherits that same physical proof through nested invocation children. After each yielding authority or permission check, effect entry checks the current selected binding, physical claim, root witness, direct-parent deadline and callback Switch liveness without another yield. A child whose parent callback has failed cannot enter a native or managed handler while its protected fiber joins. Child-job staging derives the exact claimed parent ID, attempt and checked nesting depth and repeats the local proof after preparation before reserving capacity. Stored IDs are not authority. The developer host retains the user-facing run receipt.

Developer Shell approvals are an optional, bounded process-local `Approve_once` or `Deny` service. The [approval owner](../../lib/agent_server/developer_approval_owner.mli) is created after durable command Start, paired with the exact Root before native or scheduler callbacks can run, and retained with that Live generation for later commands and claimed jobs. A selected native callback lends its actual dispatched invocation and rechecks the current Root, physical binding, borrowed parent, actor and source after the operator wait and before Shell effects. A claimed Model job lends its actual inference scope without inventing a native invocation. Selected moderator `Process.run` narrows inherited approval services to its refreshed child admission. When approval services are disabled, it adds that same child check to the original Shell executor without replacing its session, Store or provider; an expired parent scope still denies. Pending IDs and redacted command text are display and response coordinates, never execution authority. Pending reads and responses separately check the current operator, Root epoch and exact command or job attempt without entering a worker-fiber scope; the worker repeats its own full admission when it resumes. Cancellation, timeout, generation retirement and daemon shutdown close outstanding waits. Approvals and pending requests are not replayed after restart, and this service writes no new approval table or Session grant.

Trusted Developer embeddings can opt into the selected Prompt's existing native permission profile through [Developer configuration](../../lib/agent_server/daemon.mli); the existing explicit native authorization callback remains a separate mode. The selected profile is pinned under the same catalog admission as the actual Live Prompt and Workspace, and its revision is rechecked at use. The authorizer resolves each binding in the actual Root, prepared child or restored Claim runtime. Native bindings require the same physical function; managed bindings require the exact selected target and permission identity. Non-Shell tools use the existing allow, deny, policy or ask-fallback decision, with a pinned reviewer called once when configured. After a yielding evaluator or reviewer, the Live, actor and profile must still be current before the existing Root/child/Claim effect gate admits execution. Only an actual native Shell binding in that runtime is exempt from the generic profile; Shell still requires its independent exact manifest grant and bounded once-only approval. This opt-in library constructor does not install Developer service or permissive policies in the shipped executable by default.

Only an empty root checkpoint may retire after a proven absent write. Every native terminal, including an empty failure or cancellation, retains its frozen outcome and reprepares within a finite bound; otherwise its durable invocation could remain Dispatching after the run claim was released. A pre-rename I/O failure exercises this path without replaying the native callback.

The Store rereads under its writer lease, writes and syncs a temporary file, then calls the owner's nonyielding authority guard immediately before entering the rename. The guard checks the exact live owner, current authenticated actor, selected-registry revision and action's pending Prepared. Denial prevents this replacement; with unchanged prior bytes it settles as absent, while another winning write is classified separately. An exception keeps its original identity and backtrace. A completed phase change revokes the local owner conservatively at this point. The guard and rename are adjacent, but external authorization may change during the operating-system rename; this contract does not claim atomicity with an unrelated external actor. After rename entry, uncertainty is reconciled using the same retained Prepared and never authorizes a replay of the root callback. A different never-attempted Prepared cannot adopt identical after-bytes as its own publication right. A proven prewrite conflict retires only that pending token, then the owner may reread and reprepare the same sealed jobs within a finite bound without rerunning the callback. OCH-323 consumes the OCH-304 physical job-parent proof for selected native calls.

## 2. Captured input, check and run

Compilation keeps one elapsed-time budget across metadata preparation and its
joined worker domain. Every compiler checkpoint checks caller cancellation and
the configured deadline. Scheduler yields use a separate bounded checkpoint
cadence, followed by another cancellation/deadline check before work resumes.
This avoids entering the scheduler for every encoded metadata field while
remaining cooperative: work between checkpoints must still finish, and the
caller joins the domain before returning. This is not a hard real-time deadline.

A developer capture is an admitted, immutable snapshot of the selected graph and exact input. Its provenance is either a saved local editor revision or an unsaved editor snapshot with an editor revision and editor connection identity. Those editor coordinates are provenance outside the graph fingerprint: equal bytes can come from different editor revisions. A remote client uploads an exact source closure through the authoring bundle service and receives a principal-bound immutable bundle ID/graph receipt. Admission pins that bundle ID, graph/contract/environment/source digests and provenance under the developer host owner; it does not borrow a live editor buffer, lease or connection resource. The capture therefore outlives closing the editor analysis context. It also fixes the entrypoint, project/environment, compiler contract, input, mode and, for test mode, fixture set. Use a separate capture digest over these selections. A later editor edit, save, graph refresh or capability-registry change cannot rewrite the capture.

An unsaved capture is executable as an explicitly labeled isolated input. An explicit immutable bundle upload gives a valid edit/upload/run path without claiming it saved local files or changed a running agent. Local editor saves are local file operations; the developer host treats its selected source roots as read-only and does not promise atomic compare-and-swap with external filesystem writers. A saved capture must be made from the bytes the source owner actually admitted. The authoring bundle upload has its own principal-bound custody and quota, including HTTP and stdio access; a message-input blob with temporary lifetime cannot stand in for it. Repeating a run uses a new request ID, creates a new run ID and never reuses a prior effectful evaluation. A transport retry with the same request ID and capture digest returns the original receipt; reuse of that ID with different captured input rejects before execution.

A check has its own check ID and bounded `Checking -> Ready | Rejected` receipt. A run gets a run ID at submission, including when compilation or runtime admission later fails. Its receipt moves through submitted/checking/queued/running/waiting states to exactly one terminal disposition: succeeded, failed, cancelled, limited or interrupted. A compile failure is terminal with source diagnostics and zero runtime events. A job ID correlates asynchronous child work but does not replace the run ID. Every receipt reports the captured graph and input digests, provenance, selected environment, actual admitted capability pins or fixture digest, entrypoint, bounded output/errors, source sites and terminal status.

`Deterministic_test` admits a finite, versioned fixture set. Each fixture specifies an expected selected operation, validated input match and bounded result/error in an explicit order; missing or surplus calls fail closed. It proves only behavior in the represented environment. `Live` requests named host capabilities; the host resolves the current registry, permissions, credentials and budgets at execution and again after any authorizing wait. Static support and stub success confer no live authority. Both modes inherit caller resource ceilings and use host-selected compilation, execution, output and result limits. External effects that happened before failure or cancellation remain visible as partial effects; no terminal status promises reversal.

The developer service should expose check, submit, inspect, events, cancel and repeat through one portable contract used by local code and HTTP/stdio adapters. Authenticated reconnect reads the same bounded receipt. A disconnected client does not silently cancel an admitted run. Host crash marks unfinished top-level work interrupted unless a child job is proven safe to resume under its existing scheduler contract; source is never replayed automatically. Stale source/editor submissions reject before admission.

## 3. REPL ownership and lifecycle

The hosted library REPL selects the actual captured Standard compiler ABI for
both statement evaluation and a later explicit Task drive. The retained
`Developer_tool_v1` selection alone exposes the Model operation to Task code;
`One_off_v1` and `Tool_v1` remain unchanged, and a mismatched captured target
is rejected before evaluator construction. A Task effect still crosses the
physical Root's current actor and selected-source checks. The same durable
Task action can wait for a real Shell approval, stage a Job and admit a Model
request; approval, Job and provider permissions remain separate from compiler
surface selection.

The hosted REPL retains the evaluator's complete frozen receipt privately under
a distinct type-detail budget. A separate aggregate cap charges compact
outcomes and inert Task-reference metadata. A source-owned Task reference is
prepared before evaluation and installed without yielding when the evaluator
actually retains its handle, before any optional rendering or receipt
eviction. A compact response reports the true statement/reference counts and
whether text was limited. An incomplete parse stays `Need_more` when its
source location exceeds the response cap; only that public location is
omitted. Exact-key reference pages and on-demand type rendering repeat live
actor, source and epoch checks, and type rendering uses roots from the
original immutable snapshot. These projections neither rerun a statement nor
convey effect authority. The portable server endpoint projects these bounded
results; the typed first-party client adds bounded mutation custody and explicit same-key recovery. The embedded connector retains the existing local Host and connection owner; the shipped [terminal client](../../lib/chatml_repl_terminal/controller.mli) owns its separate input and rendering lifecycle.

An accepted REPL transfers the exact selected Prepared graph and compiler
contract to a run-owned source selection before publishing its instance. The
authoring editor's connection and context can close or change without
reinterpreting that graph. Host project and document selections use a separate
readless source owner that continues to consume the configured Source Host
context capacity until joined REPL close. Uploaded selections keep their
genuine Store pin; local selections keep their genuine physical Root check;
supplied documents retain their original and current policy checks. A full
currentness check may yield for uploaded Store verification and ends in an
immediate nonyielding owner/actor/policy suffix. Reset of the same captured
instance keeps this source owner. Failed Create, joined close and shutdown
release it; an expired actor, revoked root or policy, or expired upload pin
still denies new effects.

A REPL has a separate opaque ID, principal/workspace owner, project/environment, pinned source graph, reset epoch and monotone state revision. It is not an agent conversation or a durable background job. One actor serializes access to its mutable runtime and inference state. Creation carries a unique request ID bound to principal and graph/environment payload, so a lost response cannot create two sessions. Creation records the session and is inert: it does not evaluate module initializers.

The REPL pins the selected project's immutable catalog, compiler contract, environment and configured scope, but **does not use the project's selected main source as an executable REPL entry**. [Linked runtime instantiation](../../lib/chatml_linked_runtime/chatml_linked_runtime.mli) evaluates its selected entry last, so calling it unchanged on the project graph would run that source's top-level body. The [incremental registry](../../lib/chatml_linked_runtime/chatml_repl_modules.mli) instead starts empty and inert, bound to the exact compiler owner/epoch and selected graph. A complete submitted snippet has its own immutable source owner with the selected configured scope. After a statement typechecks, the registry lowers newly checked canonical units with the existing resolver, validates the authentic checked order and direct value-reference closure, then acquires the typing execution reservation before effects. It never evaluates project main. Under the actor's persistent bounded execution runner, missing units initialize once in dependency-before-owner order through the [same source-owned initializer](../../lib/chatml_linked_runtime/chatml_module_initializer.mli) used by batch linked execution; type-only/open branches stay inert. The registry returns only the completed modules selected directly by this statement. The installed [REPL epoch owner](../../lib/agent_server/developer_repl_epoch.mli) composes this registry with statement evaluation and explicit Task drives. The registry alone does not execute a submitted statement or drive a Task. A parse or typecheck failure never initializes a dependency. A failing initializer retains completed predecessors and partial effects, faults that module without replay, and preserves its captured diagnostic when available. A selected closure containing that fault fails again; unrelated work remains available under the registry. The actor's broader continuation/reset policy is separate. An explicit dependency preload, if exposed later, needs its own named selection and admitted action.

The compiler-side [configured REPL scope](../../lib/chatml_graph/chatml_configured_lookup.mli) is implemented by OCH-285. It derives each snippet lookup from an authentic implementation anchor and the same admitted `Prepared` fingerprint and selected bindings. The new snippet owner identifies a compiler use site; it is not inserted into the project graph or treated as a runtime grant. An imported binding keeps the source ID/digest and configured declaration site of its first introduction in snippet A; a use in snippet B records B's own source owner, digest and byte site. Only the current lookup in the same private epoch may resolve or reuse that origin. The REPL require adapter must call `Repl_scope.validate_reference` against the current context before selecting an already checked dependency; this primitive does not perform the dependency check or initialization. Revoking the scope or admitting a newer snippet makes older *compiler lookups* stale. It does not relabel old runtime closures, captured bytes or Task handles. A failed or cancelled admission leaves the previous current lookup and bounded source-ID ledger intact.

OCH-286's [incremental root driver](../../lib/chatml_graph/chatml_dependency_session.mli) checks only canonical modules selected by actual current-snippet references and their transitive dependencies. It shares the batch driver's source/reference validation, cycle and depth rules, but returns a bounded multi-root checked order and inter-module edges without inventing a project-main entry or evaluating it. An unused broken catalog member is not checked. The [OCH-269 typing owner](../../lib/chatml/chatml_repl_typing.mli) copies committed lexical and cached public module value schemes with one bounded graph-copy memo before using this driver for a new candidate. The actual configured checker handles each source-owned statement and newly selected canonical unit with one candidate inference machine. Failed typing discards the detached candidate, leaving committed weak cells unchanged. The immutable alias namespace can be reused: host closed templates reject inference variables, and authored declaration templates reject them before storage; public member metadata contains IDs and source locations, not raw types. The current checker has no call site for its recursive-name generator; a future allocator that uses retained alias binders must extend name seeding before activation. The incremental registry owns subsequent value-dependency selection and evaluation; it retains the exact compiler owner by identity but no stale per-candidate checked unit or raw annotation table after admission finalizes.

`submit` carries an expected revision and unique submission ID. The service binds that ID to the exact action and payload fingerprint: an exact retry returns its previous bounded receipt, while reuse with different source, task handle or action rejects. The same rule applies to task drive, reset and close actions. A stale revision rejects without evaluation. Each admitted submission receives its own immutable source ID/digest and execution ID. A multiline pending buffer is bounded; parsing the entire submission precedes effects. Incomplete syntax returns `Need_more` without typing or evaluating any statement, though accepting an additional buffer fragment advances the session revision. Completed malformed syntax returns a diagnostic without changing typing or runtime state. [The submission parser](../../lib/chatml/chatml_parse.mli) classifies a whole captured unit with the production grammar and a bounded, source-owned result; OCH-269 consumes it before any statement typing. Its EOF classification does not claim that every arbitrary byte prefix of a later valid program is `Need_more`: a cut inside a multi-character token may be terminal under the current lexer.

The instance pins its imported module graph and selected environment. Selecting a changed graph, environment or import revision requires a new Create with a fresh capture; imports cannot silently swap implementations under existing closures. Reset rebuilds lexical/runtime state from the same original selection. Reset cancels and joins the active submission, drops REPL-owned compiler/runtime roots, increments epoch and revision, and preserves old bounded receipts/traces with their prior epoch. It does not reverse effects outside the REPL. Close releases roots and rejects future submissions. Idle and maximum lifetime are host-selected bounds, independent of connection and conversation lifetimes. An authorized client can reconnect after its prior transport connection closes; the host checks current principal access and returns the current epoch/revision before new submissions. No editor connection lease is required for reattachment. A host restart expires the live REPL rather than reconstructing it by executing old inputs.

The core emits bounded frozen display/result DTOs for types and values, diagnostics, output/progress events and terminal dispositions through bounded cursor reads or subscriptions. No raw runtime value, reference cell or closure crosses the service boundary. It has no Notty, terminal or desktop types. A standalone M5 client uses Eio and Notty for multiline input, type/value rendering, events, cancellation, reset, close and reconnect. Future desktop and embedded chat-TUI REPL panels can consume the same service and choose their own rendering; they are not shipped by this service. The separate `chatml-repl` frontend is installed today.

### 3.1 Shared terminal rendering

The installed [terminal-highlighting library](../../lib/terminal_highlighting/dune)
owns the Notty styles, immutable TextMate theme, grammar loader, span engine and
embedded grammars. Each frontend owns its registry and engine; each engine has
its own bounded, collision-free theme resolver cache. The chat-facing
`Highlight_*` modules re-export the shared implementation for compatibility.
[Highlight_registry](../../lib/chat_tui/highlight_registry.mli),
[Renderer_highlight_engine](../../lib/chat_tui/renderer_highlight_engine.ml),
[Markdown_fences](../../lib/chat_tui/markdown_fences.mli), chat layout, render
caches and filesystem grammar discovery remain chat-owned.

The shipped [REPL renderer](../../lib/chatml_repl_terminal/render.ml) registers
the dedicated ChatML grammar in a private registry and highlights sanitized,
bounded display chunks through the shared TextMate engine. This generic regex
path is not interruptible inside one Oniguruma call. The chat TUI's bundled
ChatML/ChatMD code fences instead use the bounded source-lexical adapter;
[language highlighting](../development/language-highlighting.md) describes the
separate paths and their limits. Plain fallback never substitutes an OCaml
grammar for ChatML.

The terminal REPL owns multiline editing, prompt layout, service dispatch,
visible status/diagnostics, cancellation, reset, reconnect and cleanup. It uses
the existing [Notty/Eio adapter](../../lib/notty-eio/notty_eio.mli) and shared
palette/spans. Compiler state, run receipts and protocol DTOs remain
presentation-neutral; no chat message layout is required by the REPL core.

## 4. Stateful compiler and evaluator

The existing public checker starts a fresh inference machine and type environment for each program ([typechecker](../../lib/chatml/chatml_typechecker.ml) around `check_program_with_surface`). A checked span table retains mutable inferred type cells and cannot be treated as a deep immutable snapshot. The [resolver](../../lib/chatml/chatml_resolver.ml) starts a fresh lexical frame stack and lowers prior top-level reads to `REVarGlobal`; persistent static bindings and the matching runtime table must therefore advance together. [Runtime environments](../../lib/chatml/chatml_lang.ml) are mutable tables of cells. `copy_env` copies the table while preserving cell sharing, which already makes old closures retain old bindings after rebinding. Batch [evaluation](../../lib/chatml/chatml_eval.ml) discards top-level expression values and publishes `RSLetRec` placeholders before all right-hand sides complete. The REPL uses the implemented [staged statement entrypoint](../../lib/chatml/chatml_eval.mli), which returns the expression result and holds new bindings privately until publication.

### 4.1 Reuse the existing graph copy

[Chatml_type_probe](../../lib/chatml/chatml_type_probe.mli) already clones *all supplied type roots together*. Its `create`/`types` path preserves free-cell identity, sharing, cycles, levels and equality information by installing a copied cell before traversing a bound edge. It checkpoints during traversal. Its `commit` projects constraints back into original cells for interface matching, which the REPL should **not** call: a REPL candidate must be published as the new state or discarded without changing the old state.

The shared [type-graph copier](../../lib/chatml/chatml_type_graph_copy.mli)
now serves both `Type_probe.create` and REPL candidate preparation. The
[REPL root collector](../../lib/chatml/chatml_repl_type_roots.ml) invokes
`copy_detached` once for lexical and imported roots under one identity memo.
It bounds roots, cells, edges, depth and traversal work and preserves
cancellation identity. The copier uses linear physical-identity searches with
an explicit work budget. The probe keeps its existing stale-graph validation
and short non-cancellable commit protocol. `Chatml_type_scheme.transform` is
not a detached copy because it shares free cells; `Chatml_type_snapshot.freeze`
is a display projection and omits the inference levels required for checking.

The candidate clone includes every scheme reachable from the committed `Value_scope` and configured/imported public module exports, using one identity memo across all roots. Clone inference-machine counters and reserve names already used by admitted configured schemes. The existing instantiator groups generics by string name, so a fresh counter starting at zero could alias unrelated binders even when mutable cells were copied correctly. Preserve grouped quantification and both ordinary and recursive binder namespaces across submissions. Keep the existing `Type_alias` namespace by value: it is a functional map, source alias conversion has no inference `Var` construction path, and host closed templates reject mutable inference cells. A candidate is actor-owned and tied to one REPL ID, epoch and base revision. No candidate cell may point back to a committed mutable cell. Per-submission span/visibility/configured-annotation tables and callbacks remain invocation-local. Freeze displayed types and checked facts before a later submission can mutate candidate cells.

### 4.2 Statement transaction

For each parsed top-level statement, clone the committed typing roots to `before`, then infer into a candidate `after` namespace. On type diagnostic or cancellation, discard the candidate; committed typing and runtime state are untouched by that statement. OCH-276 must fully lower newly checked dependency artifacts before the typing owner reserves execution; failed lowering discards the candidate without publishing runtime or compiler roots. A successful reservation blocks new submissions while the host installs the prepared runtime registry and executes under a fresh [persistent runner](../../lib/chatml/chatml_execution.mli) scope. After the runtime outcome, the owner finalizes without yielding: success publishes `after` values/types; initialization or runtime failure/cancellation publishes the constrained `before` values/types and leaves failed new lexical names unpublished. Both outcomes retain the candidate public module cache and its compatible weak constraints, while OCH-276 separately marks any failed initializer so its effects cannot replay. The two receipts freeze distinct before/after namespace displays before execution, each with a bounded immutable binding-name to snapshot-root map from the same lexical capture. This lets clients render imports and shadowed names without reconstructing compiler scope; a failure receipt cannot display a failed new binding. Earlier successful statements in the same submission stay committed.

The constrained `before` namespace can retain a structural type inferred while checking a statement that later fails. This may conservatively reject a future submission even if the relevant effect never happened, but it prevents unsound reuse when the failed statement *did* mutate an old weakly typed reference. A retained raw `typ` has no alias-name/namespace constructor; candidate-only alias maps are discarded. Validate that no retained value scope, type namespace or source owner references an unpublished alias/module identity. New aliases from a completed `type` statement publish immediately because that statement has no runtime initializer. An imported module declaration uses the same staged rule as a value binding.

Runtime evaluation uses a provisional top-level binding table that shares existing cells but holds new cells privately. `RSLetRec` recursive closures capture those provisional placeholder cells. [The checker](../../lib/chatml/chatml_typechecker.ml) accepts only lambdas (possibly annotated) as recursive right-hand sides, so evaluating a group cannot execute user code that publishes its own incomplete cells. On success, publish those *same* cells; on failure, remove the provisional names from the REPL namespace. `RSOpen`, `RSModule` and `RSLet` names also publish only on completion. A completed local binding inside a subsequently failed module body can still escape through a write to an older reference or an authorized host callback. Its initialized private cells and captured source remain alive if reachable; removing the failed module name cannot erase that effect. The checker folds module statements in order and keeps a new module name out of scope until its body is checked; the resolver uses the checked program's lexical order. Add a regression for an escaped closure after a later initializer fails. Writes to old references/arrays/cells, host effects and output are not rolled back. Old closures and module values keep their captured source owner; each current statement resolves only against its own checked source ID/digest and annotation table, never a foreign offset-equivalent table.

### 4.3 Accepted language behavior

The user accepted these M5 REPL conventions:

1. Parse a whole multiline submission before evaluating any statement. Statements then typecheck and evaluate sequentially, committing each success. A later failure reports the last committed statement and leaves earlier successes intact.
2. A failed typecheck leaves prior typing unchanged. A statement that typechecks then fails at runtime keeps any necessary constraints on prior weak variables and any effects already performed, but does not publish its failed new binding.
3. Rebinding shadows the name. Existing closures retain their old lexical cells, matching the current runtime test.
4. Project/environment/import graph changes require a new Create. Reset clears interactive state on the same original capture and does not undo external effects.
5. A top-level `Task` is a value until a separate explicit run action drives it. There is no implicit task execution merely because an expression was entered. Each explicit drive action receives a distinct execution receipt and current host admission.

For example, four separate submissions `let x = 1`, `let f () = x`, `let x = 2`, then `f()` display `1` for the final expression. The first three submissions are not replayed. Submitting `Task.pure(3)` displays a task value and handle; an explicit task run action produces the `3` result. In deterministic test mode, `main` calling a selected tool succeeds only when an admitted fixture matches that exact call. The same source in live mode needs current host permission and may have a different outcome; the two receipts name their respective environments.

## 5. Typed service shape for implementation review

The following is a historical architecture sketch, not the installed OCaml API. The current [Developer host](../../lib/agent_server/developer_host.mli), [REPL epoch owner](../../lib/agent_server/developer_repl_epoch.mli), and [typed client](../../lib/agent_client/developer_repl_connection.mli) expose the validated source, key, instance and epoch contracts. No raw capability or mutable runtime environment is returned to a client.

```ocaml
module Developer_environment : sig
  type t = Deterministic_test of Fixture_set.t | Live of Capability_selection.t
end

module Developer_capture : sig
  type t
  val create
    :  graph:Chatml_prepared_program.t
    -> provenance:Source_provenance.t
    -> input:Jsonaf.t
    -> environment:Developer_environment.t
    -> (t, Error.t) result
  val graph_fingerprint : t -> Chatml_source.Digest.t
  val fingerprint : t -> Chatml_source.Digest.t
end

module Developer_run_service : sig
  type t
  val check : t -> request:Developer_request_id.t -> capture:Developer_capture.t -> (Check_receipt.t, Error.t) result
  val submit : t -> request:Developer_request_id.t -> capture:Developer_capture.t -> (Run_receipt.t, Error.t) result
  val inspect : t -> id:Developer_run_id.t -> (Run_view.t, Error.t) result
  val events : t -> id:Developer_run_id.t -> after:Event_cursor.t option -> (Run_event.t list * Event_cursor.t option, Error.t) result
  val cancel : t -> id:Developer_run_id.t -> (unit, Error.t) result
end

module Repl_service : sig
  type t
  type submit_result = Need_more | Started of Repl_execution_id.t
  val create
    :  t
    -> request:Creation_id.t
    -> graph:Chatml_prepared_program.t
    -> environment:Developer_environment.t
    -> (Repl_view.t, Error.t) result
  val submit
    :  t
    -> repl:Repl_id.t
    -> expected:Repl_revision.t
    -> submission:Submission_id.t
    -> source:Chatml_source.t
    -> (submit_result, Error.t) result
  val drive_task
    :  t
    -> repl:Repl_id.t
    -> expected:Repl_revision.t
    -> submission:Submission_id.t
    -> task:Task_handle.t
    -> (Repl_execution_id.t, Error.t) result
  val inspect : t -> repl:Repl_id.t -> (Repl_view.t, Error.t) result
  val events : t -> repl:Repl_id.t -> after:Event_cursor.t option -> (Repl_event.t list * Event_cursor.t option, Error.t) result
  val cancel : t -> repl:Repl_id.t -> execution:Repl_execution_id.t -> (unit, Error.t) result
  val reset : t -> repl:Repl_id.t -> expected:Repl_revision.t -> request:Action_id.t -> (Repl_view.t, Error.t) result
  val close : t -> repl:Repl_id.t -> request:Action_id.t -> (unit, Error.t) result
end
```

Current principal authorization is verified by the host service on every connection and operation, rather than accepted from source text. All queries and mutations validate owner, epoch and lifecycle. No second scheduler or separate REPL ownership lease is introduced. A `Task_handle` is bounded and scoped to its REPL epoch; it cannot be forged or used as authority. Its explicit drive operation is a fresh admitted execution, not replay of previous statements. Public `.mli` files should document source byte coordinates, ownership, limits, errors, cancellation and effect behavior. Core typed comparison, receiver-first APIs, Jane Street formatting and expect tests apply.

## 6. Verification and implementation split

The strongest local regression sequence starts with a weakly typed reference. A failed *typecheck* that tentatively constrains it must leave the prior graph unchanged, including cell sharing, cycles, free-variable level/equality and namespace keys. A successfully typed statement that mutates it and then fails at runtime must leave its compatible constraint and value but publish none of its failed names. Add a project main with a visible side effect: creating the REPL, an incomplete parse, and a failed typecheck must leave that effect absent. A successfully checked statement that reads a configured value dependency must initialize only that dependency once, without evaluating project main; a type-only/open edge must remain inert. Add closure rebinding across submissions, failed recursive/module/open publication, source-owner preservation, configured imports, inference cancellation, multiline continuation, explicit Task driving, stub/live admission, permission denial, delayed-binding revocation, output limits, cancellation after effects, stale revisions and reconnect. Compare committed type roots before/after a rejected check with a private canonical graph witness plus behavioral expect tests; do not expose raw inference refs publicly. Test actual local Eio/Notty input, accurate ChatML highlighting, visible type/value, a cancellable wait, reset/close and reconnect. Each change needs focused local tests and updated `docs-src` contracts. Integrated qualification also checks the complete documentation website.

The implementation has separate compiler, execution and presentation boundaries. Shared type-graph copying preserves cycles, aliases and free-cell constraints; persistent statement typing builds on that foundation to remap `Value_scope` roots and commit inferred state. Incremental module initialization then provides the dependency registry used by the evaluator without running the project entrypoint. Each boundary needs its own regression evidence, while higher-level checks should exercise their composition without duplicating the lower-level test matrices.

Live execution combines the captured source and selected compiler contract with the host's native invocation admission, shared job capacity, durable ownership and result disclosure. The installed portable run and REPL services expose that lifecycle to clients. The separate terminal REPL uses shared Eio/Notty rendering components. Its [actual PTY qualification](../../test/chatml_repl_terminal/terminal_pty.py) checks interactive input, cancellation, reset, reconnect and terminal cleanup; the [joined workflow](../guide/chatml-development-workflow.md) composes authored source, checks, runs and trace readback over HTTP and local stdio. These executable checks complement the component invariants; this design text alone is not a qualification result.
