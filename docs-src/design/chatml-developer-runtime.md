# ChatML developer run/test and REPL design

Status: M5 design v1, 2026-10-13. The REPL language decisions in §4.3 are accepted; service and implementation interfaces below are proposed contracts for review. This document is not evidence that the services exist yet. OCH-130 owns the research; OCH-137, OCH-138 and their implementation children own delivery.

The [authoring service design](chatml-authoring-services.md) owns source context,
overlay and immutable upload contracts used here. Implementation and validation
must preserve that boundary rather than invent a second source resolver.
The [execution trace design](chatml-execution-tracing.md) defines bounded,
read-only observations of these executions.

## 1. Purpose and existing boundary

Developer checks, test runs, live runs and a stateful REPL need explicit source, project, environment and input selection. Their identities and lifetimes differ from an agent conversation. A check executes no initializer; an admitted run may cause external effects. Neither an import nor a successful static check grants tool, process, network or provider authority.

The existing compiler already accepts exact immutable [captured sources](../../lib/chatml_sources/chatml_source.mli) and [prepared graphs](../../lib/chatml_sources/chatml_prepared_program.mli). The graph fingerprint includes selected compiler contract, environment, entry, provenance, source IDs/digests and scope metadata. [Compilation](../../lib/chatml/chatml_compilation.mli) runs in a joined, cancellable Eio domain without executing initializers. A content digest identifies bytes, not an authorized writer or a capability.

The current one-off host prepares selected source and tools, revalidates live bindings after waits and runs under a borrowed native scope ([one-off script](../../lib/chat_response/one_off_script.mli), [execution](../../lib/agent_session/one_off_execution.ml)). The [script job service](../../lib/agent_session/script_job_service.mli) and [job protocol](../../lib/agent_protocol/job.mli) provide bounded staging, selection, status, permission waits and cancellation for actor-owned work. Their current launch owner is an invocation or moderator event. Developer execution needs its own authenticated owner and run record while reusing those underlying controls; it must not mutate an unrelated production conversation or disguise a developer run as `Agent_protocol.Run`.

## 2. Captured input, check and run

A developer capture is an admitted, immutable snapshot of the selected graph and exact input. Its provenance is either a saved local editor revision or an unsaved editor snapshot with an editor revision and editor connection identity. Those editor coordinates are provenance outside the graph fingerprint: equal bytes can come from different editor revisions. A remote client uploads an exact source closure through the authoring bundle service and receives a principal-bound immutable bundle ID/graph receipt. Admission pins that bundle ID, graph/contract/environment/source digests and provenance under the developer host owner; it does not borrow a live editor buffer, lease or connection resource. The capture therefore outlives closing the editor analysis context. It also fixes the entrypoint, project/environment, compiler contract, input, mode and, for test mode, fixture set. Use a separate capture digest over these selections. A later editor edit, save, graph refresh or capability-registry change cannot rewrite the capture.

An unsaved capture is executable as an explicitly labeled isolated input. An explicit immutable bundle upload gives a valid edit/upload/run path without claiming it saved local files or changed a running agent. Local editor saves are local file operations; the developer host treats its selected source roots as read-only and does not promise atomic compare-and-swap with external filesystem writers. A saved capture must be made from the bytes the source owner actually admitted. The authoring bundle upload has its own principal-bound custody and quota, including HTTP and stdio access; a message-input blob with temporary lifetime cannot stand in for it. Repeating a run uses a new request ID, creates a new run ID and never reuses a prior effectful evaluation. A transport retry with the same request ID and capture digest returns the original receipt; reuse of that ID with different captured input rejects before execution.

A check has its own check ID and bounded `Checking -> Ready | Rejected` receipt. A run gets a run ID at submission, including when compilation or runtime admission later fails. Its receipt moves through submitted/checking/queued/running/waiting states to exactly one terminal disposition: succeeded, failed, cancelled, limited or interrupted. A compile failure is terminal with source diagnostics and zero runtime events. A job ID correlates asynchronous child work but does not replace the run ID. Every receipt reports the captured graph and input digests, provenance, selected environment, actual admitted capability pins or fixture digest, entrypoint, bounded output/errors, source sites and terminal status.

`Deterministic_test` admits a finite, versioned fixture set. Each fixture specifies an expected selected operation, validated input match and bounded result/error in an explicit order; missing or surplus calls fail closed. It proves only behavior in the represented environment. `Live` requests named host capabilities; the host resolves the current registry, permissions, credentials and budgets at execution and again after any authorizing wait. Static support and stub success confer no live authority. Both modes inherit caller resource ceilings and use host-selected compilation, execution, output and result limits. External effects that happened before failure or cancellation remain visible as partial effects; no terminal status promises reversal.

The developer service should expose check, submit, inspect, events, cancel and repeat through one portable contract used by local code and HTTP/stdio adapters. Authenticated reconnect reads the same bounded receipt. A disconnected client does not silently cancel an admitted run. Host crash marks unfinished top-level work interrupted unless a child job is proven safe to resume under its existing scheduler contract; source is never replayed automatically. Stale source/editor submissions reject before admission.

## 3. REPL ownership and lifecycle

A REPL has a separate opaque ID, principal/workspace owner, project/environment, pinned source graph, reset epoch and monotone state revision. It is not an agent conversation or a durable background job. One actor serializes access to its mutable runtime and inference state. Creation carries a unique request ID bound to principal and graph/environment payload, so a lost response cannot create two sessions. Creation records the session and is inert: it does not evaluate module initializers.

The REPL pins the selected project's immutable catalog, compiler contract, environment and configured scope, but **does not use the project's selected main source as an executable REPL entry**. [Linked runtime instantiation](../../lib/chatml_linked_runtime/chatml_linked_runtime.mli) evaluates its selected entry last, so calling it unchanged on the project graph would run that source's top-level body. The REPL instead has a distinct empty bootstrap entry and gives each complete submitted snippet its own immutable entry/source owner with the selected configured scope. Only after one statement typechecks successfully may its checked direct value-dependency closure initialize under that statement's admitted execution. Dependencies initialize once per epoch in the existing linked runtime order; type-only/open edges remain inert. The submitted statement then evaluates once in the persistent REPL environment. OCH-276 owns an incremental, session-owned dependency registry using the existing checked graph and initializer evaluator; `Chatml_linked_runtime.instantiate` cannot itself be reused unchanged because it always evaluates an entry and starts a fresh registry. An explicit dependency preload, if exposed later, needs its own named selection and admitted action. A failed parse or typecheck never initializes a dependency or runs the project entry. If initialization fails after typechecking, the epoch becomes faulted and requires an explicit reset; partial effects remain and no automatic replay occurs. Initialization events are separate from statement events.

The compiler-side [configured REPL scope](../../lib/chatml_graph/chatml_configured_lookup.mli) is implemented by OCH-285. It derives each snippet lookup from an authentic implementation anchor and the same admitted `Prepared` fingerprint and selected bindings. The new snippet owner identifies a compiler use site; it is not inserted into the project graph or treated as a runtime grant. An imported binding keeps the source ID/digest and configured declaration site of its first introduction in snippet A; a use in snippet B records B's own source owner, digest and byte site. Only the current lookup in the same private epoch may resolve or reuse that origin. The REPL require adapter must call `Repl_scope.validate_reference` against the current context before selecting an already checked dependency; this primitive does not perform the dependency check or initialization. Revoking the scope or admitting a newer snippet makes older *compiler lookups* stale. It does not relabel old runtime closures, captured bytes or Task handles. A failed or cancelled admission leaves the previous current lookup and bounded source-ID ledger intact.

`submit` carries an expected revision and unique submission ID. The service binds that ID to the exact action and payload fingerprint: an exact retry returns its previous bounded receipt, while reuse with different source, task handle or action rejects. The same rule applies to task drive, reset and close actions. A stale revision rejects without evaluation. Each admitted submission receives its own immutable source ID/digest and execution ID. A multiline pending buffer is bounded; parsing the entire submission precedes effects. Incomplete syntax returns `Need_more` without typing or evaluating any statement, though accepting an additional buffer fragment advances the session revision. Completed malformed syntax returns a diagnostic without changing typing or runtime state. [The submission parser](../../lib/chatml/chatml_parse.mli) classifies a whole captured unit with the production grammar and a bounded, source-owned result; OCH-269 will consume it for transactional typing. Its EOF classification does not claim that every arbitrary byte prefix of a later valid program is `Need_more`: a cut inside a multi-character token may be terminal under the current lexer.

The instance pins its imported module graph and selected environment. Refreshing either requires an explicit reset/new instance; imports cannot silently swap implementations under existing closures. Reset cancels and joins the active submission, drops REPL-owned compiler/runtime roots, increments epoch and revision, and preserves old bounded receipts/traces with their prior epoch. It does not reverse effects outside the REPL. Close releases roots and rejects future submissions. Idle and maximum lifetime are host-selected bounds, independent of connection and conversation lifetimes. An authorized client can reconnect after its prior transport connection closes; the host checks current principal access and returns the current epoch/revision before new submissions. No editor connection lease is required for reattachment. A host restart expires the live REPL rather than reconstructing it by executing old inputs.

The core emits bounded frozen display/result DTOs for types and values, diagnostics, output/progress events and terminal dispositions through bounded cursor reads or subscriptions. No raw runtime value, reference cell or closure crosses the service boundary. It has no Notty, terminal or desktop types. A standalone M5 client uses Eio and Notty for multiline input, type/value rendering, events, cancellation, reset, close and reconnect. Desktop and embedded chat TUI clients consume the same service and choose their own rendering. The M5 terminal client is required; later command unification cannot defer it.

### 3.1 Terminal rendering reuse for OCH-255

The current [chat TUI library](../../lib/chat_tui/dune) bundles a reusable highlighting pipeline with its entire chat/agent UI. [Highlight_styles](../../lib/chat_tui/highlight_styles.mli) owns Notty attributes, including validated RGB channels and `hex_to_rgb`/`fg_hex`/`bg_hex`; [Highlight_theme](../../lib/chat_tui/highlight_theme.mli) maps TextMate scope rules to the GitHub-dark palette; [Highlight_tm_loader](../../lib/chat_tui/highlight_tm_loader.mli) and [Highlight_tm_engine](../../lib/chat_tui/highlight_tm_engine.mli) turn a private grammar registry into per-line Notty spans with plain fallback and cancellation; [Highlight_grammars](../../lib/chat_tui/highlight_grammars.mli) embeds curated OCaml, Markdown, JSON and other grammars. These implementations do not depend on chat messages. The separate [Notty/Eio terminal adapter](../../lib/notty-eio/notty_eio.mli) already owns event input, synchronized frames and cleanup, while [Notty_scroll_box](../../lib/notty_scroll_box.ml) provides viewport scrolling.

OCH-275 (5 points after discovery of a shared theme-cache correctness and ownership defect) extracts the styles, theme, TextMate loader/engine and bundled grammar registration into one shared UI-side terminal-highlighting library with `Core`, `Notty`, `textmate-language`, `Jsonaf` and embedded grammar assets; it preserves existing chat-facing module names through aliases or a compatible re-export. The theme rules stay immutable, while each engine owns a bounded, collision-free resolver cache safe for concurrent reads. OCH-255 constructs its own private registry and engine from that library. [Highlight_registry](../../lib/chat_tui/highlight_registry.mli) currently mixes a useful private `create` with a process-wide mutable `get`/`replace`; the latter and [Renderer_highlight_engine](../../lib/chat_tui/renderer_highlight_engine.ml) remain chat-owned. [Markdown_fences](../../lib/chat_tui/markdown_fences.mli), message layout, render caches, custom-grammar filesystem discovery and path-to-language choices remain chat-owned as well. They are not required to display REPL source or structured result DTOs. OCH-135 delivers accurate ChatML/ChatMD highlighting assets and shared metadata; OCH-255 uses those assets for final interactive source rendering. Plain spans are an intermediate/unavailable-asset fallback, never a substitute OCaml grammar for ChatML.

The terminal REPL owns prompt layout, multiline editing, event-to-service dispatch, visible status/diagnostics, and local interactive validation. It uses the shared Notty palette and spans, while the REPL engine, run receipts and protocol DTOs remain rendering-neutral. OCH-255 depends on OCH-135 and OCH-275 for final delivery.

## 4. Stateful compiler and evaluator

The existing public checker starts a fresh inference machine and type environment for each program ([typechecker](../../lib/chatml/chatml_typechecker.ml) around `check_program_with_surface`). A checked span table retains mutable inferred type cells and cannot be treated as a deep immutable snapshot. The [resolver](../../lib/chatml/chatml_resolver.ml) starts a fresh lexical frame stack and lowers prior top-level reads to `REVarGlobal`; persistent static bindings and the matching runtime table must therefore advance together. [Runtime environments](../../lib/chatml/chatml_lang.ml) are mutable tables of cells. `copy_env` copies the table while preserving cell sharing, which already makes old closures retain old bindings after rebinding. [Evaluation](../../lib/chatml/chatml_eval.ml) discards top-level expression values and publishes `RSLetRec` placeholders before all right-hand sides complete, so a new staged, value-returning statement entrypoint is needed.

### 4.1 Reuse the existing graph copy

[Chatml_type_probe](../../lib/chatml/chatml_type_probe.mli) already clones *all supplied type roots together*. Its `create`/`types` path preserves free-cell identity, sharing, cycles, levels and equality information by installing a copied cell before traversing a bound edge. It checkpoints during traversal. Its `commit` projects constraints back into original cells for interface matching, which the REPL should **not** call: a REPL candidate must be published as the new state or discarded without changing the old state.

Extract the graph-copy traversal into a small internal shared helper used by both `Type_probe.create` and REPL candidate preparation, or initially use `Type_probe.create`/`types` and drop the probe after remapping roots. This is one implementation of graph copying, not a second clone algorithm. Budget the total retained roots, cells, edges and traversal work; the current probe uses linear physical-identity searches and its recursive shape traversal needs qualification under REPL-sized bounds. Its `commit` stale-graph and non-cancellable publication behavior must remain unchanged if refactored. `Chatml_type_scheme.transform` is not a replacement because it deliberately shares free cells; `Chatml_type_snapshot.freeze` is an immutable display projection and omits inference levels needed to resume checking.

The candidate clone includes every scheme reachable from the committed `Value_scope` and configured/imported public module exports, using one identity memo across all roots. Clone inference-machine counters and reserve names already used by admitted configured schemes. The existing instantiator groups generics by string name, so a fresh counter starting at zero could alias unrelated binders even when mutable cells were copied correctly. Preserve grouped quantification and both ordinary and recursive binder namespaces across submissions. Keep the existing `Type_alias` namespace by value: it is a functional map, source alias conversion has no inference `Var` construction path, and host closed templates reject mutable inference cells. A candidate is actor-owned and tied to one REPL ID, epoch and base revision. No candidate cell may point back to a committed mutable cell. Per-submission span/visibility/configured-annotation tables and callbacks remain invocation-local. Freeze displayed types and checked facts before a later submission can mutate candidate cells.

### 4.2 Statement transaction

For each parsed top-level statement, clone the committed typing roots to `before`, then infer into a candidate `after` namespace. On type diagnostic or cancellation, discard the candidate; committed typing and runtime state are untouched by that statement. Only after successful typing does the host admit and initialize any newly required value dependencies, then evaluate the resolved statement once under a fresh [persistent runner](../../lib/chatml/chatml_execution.mli) scope and current authority. Dependency initialization and statement evaluation share that action's bounded execution; neither precedes its typecheck. On runtime success, publish `after` values/types and its provisional runtime bindings as one actor transition. On initialization or runtime failure/cancellation, publish the constrained candidate `before` value/type namespaces, while discarding every new name, alias, module binding and placeholder of the failed statement. An initializer failure also faults the epoch, preventing a later action from silently replaying partial effects. Earlier successful statements in the same submission stay committed.

The constrained `before` namespace can retain a structural type inferred while checking a statement that later fails. This may conservatively reject a future submission even if the relevant effect never happened, but it prevents unsound reuse when the failed statement *did* mutate an old weakly typed reference. A retained raw `typ` has no alias-name/namespace constructor; candidate-only alias maps are discarded. Validate that no retained value scope, type namespace or source owner references an unpublished alias/module identity. New aliases from a completed `type` statement publish immediately because that statement has no runtime initializer. An imported module declaration uses the same staged rule as a value binding.

Runtime evaluation uses a provisional top-level binding table that shares existing cells but holds new cells privately. `RSLetRec` recursive closures capture those provisional placeholder cells. [The checker](../../lib/chatml/chatml_typechecker.ml) accepts only lambdas (possibly annotated) as recursive right-hand sides, so evaluating a group cannot execute user code that publishes its own incomplete cells. On success, publish those *same* cells; on failure, remove the provisional names from the REPL namespace. `RSOpen`, `RSModule` and `RSLet` names also publish only on completion. A completed local binding inside a subsequently failed module body can still escape through a write to an older reference or an authorized host callback. Its initialized private cells and captured source remain alive if reachable; removing the failed module name cannot erase that effect. The checker folds module statements in order and keeps a new module name out of scope until its body is checked; the resolver uses the checked program's lexical order. Add a regression for an escaped closure after a later initializer fails. Writes to old references/arrays/cells, host effects and output are not rolled back. Old closures and module values keep their captured source owner; each current statement resolves only against its own checked source ID/digest and annotation table, never a foreign offset-equivalent table.

### 4.3 Accepted language behavior

The user accepted these M5 REPL conventions:

1. Parse a whole multiline submission before evaluating any statement. Statements then typecheck and evaluate sequentially, committing each success. A later failure reports the last committed statement and leaves earlier successes intact.
2. A failed typecheck leaves prior typing unchanged. A statement that typechecks then fails at runtime keeps any necessary constraints on prior weak variables and any effects already performed, but does not publish its failed new binding.
3. Rebinding shadows the name. Existing closures retain their old lexical cells, matching the current runtime test.
4. Project/environment/import graph changes require explicit reset. Reset does not undo external effects.
5. A top-level `Task` is a value until a separate explicit run action drives it. There is no implicit task execution merely because an expression was entered. Each explicit drive action receives a distinct execution receipt and current host admission.

For example, four separate submissions `let x = 1`, `let f () = x`, `let x = 2`, then `f()` display `1` for the final expression. The first three submissions are not replayed. Submitting `Task.pure(3)` displays a task value and handle; an explicit task run action produces the `3` result. In deterministic test mode, `main` calling a selected tool succeeds only when an admitted fixture matches that exact call. The same source in live mode needs current host permission and may have a different outcome; the two receipts name their respective environments.

## 5. Typed service shape for implementation review

The following is a contract sketch. Modules such as `Developer_run_id` and `Repl_revision` need validated opaque constructors in their owning implementation tickets; no raw ID, capability or mutable environment is returned to a client.

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

The strongest local regression sequence starts with a weakly typed reference. A failed *typecheck* that tentatively constrains it must leave the prior graph unchanged, including cell sharing, cycles, free-variable level/equality and namespace keys. A successfully typed statement that mutates it and then fails at runtime must leave its compatible constraint and value but publish none of its failed names. Add a project main with a visible side effect: creating the REPL, an incomplete parse, and a failed typecheck must leave that effect absent. A successfully checked statement that reads a configured value dependency must initialize only that dependency once, without evaluating project main; a type-only/open edge must remain inert. Add closure rebinding across submissions, failed recursive/module/open publication, source-owner preservation, configured imports, inference cancellation, multiline continuation, explicit Task driving, stub/live admission, permission denial, delayed-binding revocation, output limits, cancellation after effects, stale revisions and reconnect. Compare committed type roots before/after a rejected check with a private canonical graph witness plus behavioral expect tests; do not expose raw inference refs publicly. Test actual local Eio/Notty input, accurate OCH-135 ChatML highlighting, visible type/value, a cancellable wait, reset/close and reconnect. Each implementation ticket runs focused local tests and updates affected `docs-src` contracts; the comprehensive website checks belong to the final milestone ticket.

The implementation tickets are these nonoverlapping, independently verifiable outcomes; OCH-137 and OCH-138 are tracking parents with no duplicate estimate:

| Ticket | Parent | Outcome | Points | Primary dependency |
| --- | --- | --- | ---: | --- |
| OCH-265 | OCH-137 | Immutable capture/check and deterministic fixture runner | 5 | OCH-129 identity and OCH-130 decisions |
| OCH-266 | OCH-137 | Developer execution owner, live admission and shared job/cancel integration | 8 | OCH-85, OCH-126, OCH-265 |
| OCH-267 | OCH-137 | Portable HTTP/stdio run service, bounded inspection and authoring bundle upload/run integration | 5 | OCH-266 and OCH-129 bundle custody |
| OCH-268 | OCH-138 | Extract/bound shared type-graph copy from `Type_probe`, preserving probe behavior | 3 | OCH-127 correctness audit |
| OCH-283 | OCH-138 | Bounded production-grammar whole-submission Complete/Need_more/Rejected admission | 3 | OCH-268 |
| OCH-285 | OCH-138 | Pinned configured REPL scope, distinct import/use provenance and current reference validation | 5 | OCH-283 |
| OCH-269 | OCH-138 | Persistent statement typing and candidate namespace commit rules | 8 | OCH-268, OCH-283 and OCH-285 |
| OCH-276 | OCH-138 | Session-owned checked value-dependency initialization and inert REPL bootstrap | 5 | OCH-269 |
| OCH-270 | OCH-138 | Incremental evaluator, staged bindings, explicit Task drive and partial-effect semantics | 8 | OCH-276 and developer host admission |
| OCH-271 | OCH-138 | REPL actor, revision/epoch/idempotency lifecycle and portable HTTP/stdio service | 5 | OCH-270 and OCH-267 |
| OCH-275 | OCH-255 prerequisite | Shared Notty/TextMate renderer extraction, bounded safe theme resolver and chat compatibility | 5 | Existing chat TUI pipeline |
| OCH-255 | M5 terminal client | Usable standalone Eio/Notty terminal REPL client with accurate ChatML/ChatMD highlighting | 5 | OCH-271, OCH-275 and OCH-135 |

OCH-268 has its own meaningful validation: probe cycle/share/free-cell/commit regressions and a bounded clone API test. OCH-269 owns `Value_scope` root remapping, inferred-state machine snapshots, source-owned annotations and actual statement commit behavior. This avoids implementing graph copying twice or counting it in both tickets. OCH-276 owns the incremental initializer boundary because the existing linked runtime entry evaluation cannot safely serve as REPL bootstrap; OCH-270 consumes its registry and does not reimplement it. OCH-275 owns renderer extraction; OCH-255 retains the broader interactive terminal scope in its existing 5-point estimate. Delivery still requires working test runs and a working REPL; this design alone does not close those outcomes.
