# OCH-49: provider and storage boundaries

Status: versioned design delivered by M1-T01, at source revision `58e9d2c5567d596fbe52ba1041e37d12dbde102b`. These are implementation recommendations and invariants for downstream work. The `.mli` files are compile-checked design declarations without implementations, installation or a production Dune library. They do not claim the existing runtime is provider-neutral. Refining placement or spelling requires keeping the ownership and observable contracts below.

The change is an extraction from existing history, turn driver, actor and storage implementations. OChat keeps its conversation, tool admission/execution, jobs and recovery. OpenAI is the first concrete adapter; a synthetic protocol is enough to exercise independence. No second executor, allocator, registry, scheduler or session store is introduced. See [source-backed scenarios](scenarios.md), [storage/presence contract](storage.md), [proposed downstream amendments](downstream-updates.json) and [validation](validation/README.md).

## Dependency and placement decisions

The review file collects related declarations to make the design readable; it is not the proposed production dependency unit.

| Proposed placement | Dependencies | Existing implementation to adapt |
|---|---|---|
| Shared identity/payload/content/tool/config/request/event/usage modules | Core and generic Jsonaf only | Extract host IDs from History_entry and Agent_protocol.Id; replace OpenAI-shaped payloads in History_entry/Ochat_function |
| Generic document/envelope/conversion modules | Core/Jsonaf; no runtime, OpenAI or Eio | Agent_store record decoding and existing bounded JSON patterns |
| Host history/actor/turn integration | Shared semantic contracts, existing runtime and Eio | History_entry allocator, Operation_worker, In_memory_stream, Session_actor |
| OpenAI adapter codecs/lowering | Shared semantic contracts plus OpenAI wire modules | Openai.Responses; no actor commits or tool execution |
| Host auth resolver | Target references plus existing host auth/storage and Eio | M2 resolver/credential lifecycle; separate from MCP/daemon identities |
| SSE/WS request transport | Prepared requests/events, temporary auth lease and explicit Eio scope | Existing HTTP/stream plumbing; host-scoped optional channel cache |
| Client projection | Neutral semantic views plus current protocol | Agent_protocol.History, History_codec and TUI projection; no provider decoder |

Do not link Eio into the pure semantic library because Auth/Driver appear in the same design file. Auth/Driver become separate effectful interfaces. Existing host ID representation/reservation stays stable; extracting it avoids the current History_entry→OpenAI dependency without replacing identity allocation.

## Item, replay and tool contract

Keep host entry ID, invocation ID, operation ID, source and attempt distinct from provider item/response/call strings. A result binds to the host call occurrence; repeated provider call strings in later turns cannot select an earlier invocation. Existing invocation records remain execution identity. If an adapter splits richer provider content into multiple logical calls, allocate distinct host entries and retain source grouping in replay metadata; do not dedupe by provider string alone.

Ordered content includes text and admitted immutable image/document assets. Existing outcomes count as covered; declaring Document does not automatically adopt a new binary-file workflow. Mutable local paths and provider-only file IDs do not suffice for local recovery. Asset transfer/admission remains host-owned.

A captured item owns one immutable raw provider JSON envelope and a derived semantic view. The raw envelope retains unknown fields, explicit key presence, opaque reasoning and origin/replay version. This preserves JSON values, not original network JSON whitespace or numeric spelling. Function arguments/custom input retain exact string bytes separately. Semantic edits produce an authored item and invalidate stale opaque replay. Unknown items remain inspectable but cannot execute tools; replay to a different profile/adapter requires a typed compatibility result, never silent dropping or forwarding.

Tool_spec references the existing native binding and captured schema revision. Namespace/discovery/async remain conditional research, not a new registry or hosted tool adoption. Moderator rewrites preserve original evidence separately from final execution input. The current host validates the final target/schema and rechecks authority after waits. Provider completion is not permission to publish a fabricated completed job result.

## Configuration and preparation

Capture nonsecret target/profile/account intent, model string, settings and provenance. Retain arbitrary validated nonempty model IDs. Resolution order is explicit execution/session override, captured ChatMD, selected profile defaults, documented adapter defaults. A provider-owned unresolved default remains absent. Restore uses captured values rather than rereading a profile file and silently changing a saved session. Explicit profile/model updates apply to later preparation; they do not alter an already captured request.

Capability resolution combines adapter encoding, endpoint/profile restrictions and model information. Explicit prohibition at a required layer wins. Unknown optional support produces a preparation error for an explicit request unless a typed compatible-profile declaration supplies support; a declaration cannot create a missing encoder. Baseline text inference can admit arbitrary IDs under declared protocol support. No network probing, silent parameter stripping, different-account fallback or global capability mutation on one rejection.

Output format uses Absent/Null/Value: absent is not explicit Text. Null is accepted only where the selected field/profile contract permits it, rather than universally meaning reset. The [field matrix](storage.md#field-presence) defines codec policies independently of ergonomic options.

Preparation captures effective moderated history, source/attempt, immutable asset versions, selected tool schemas and resolved options before dispatch. Preserve existing admission receipts: pure preparation may fail before any network call; the actor's final before-dispatch admission must still precede submission. M1-T05 must reconcile ordering of current `prepare_model_input` additions with admission so the admitted operation and final fingerprint describe the actual request. No provider event, admission callback or tool execution occurs in pure Adapter.prepare.

## Stream and outcome contract

Events identify attempt/source, item descriptor/provider metadata, part index and semantic delta kind: text, refusal, reasoning summary, function arguments, custom input or opaque data. Consumers never infer delta meaning from bytes. The existing fold owns assembly, host ID allocation and finalization reconciliation; complete validated call input is required before existing tool admission. Unknown metadata stays opaque; duplicate/conflicting terminal item data cannot start the same call twice. Partial text remains visible on failure.

Driver.run delivers a bounded synchronous callback stream under the caller's Eio switch. Only Ok return emits exactly one terminal event and returns the same terminal disposition; Error Auth emits no events. Consumer failures and Eio cancellation propagate into existing host cleanup. Never catch them as provider errors or return a lazy sequence outside its owning switch. The driver does not commit history, schedule tools or own retries. Binding to this callback seam must preserve current incremental first-event/backpressure behavior; transport tests remain downstream.

Completed, incomplete with typed reason, provider failure and transport loss remain distinct. Refusal is content metadata and can accompany a completed inference. A typed Steered reason permits accurate decoding if encountered; it does not adopt an in-flight steering control API. Terminal Completed does not complete an operation, waiting job, goal or CLI run. Usage is attempt-scoped actual/estimated/unknown, with stable observation identity and nonnegative revision. A newer authoritative snapshot replaces the previous one; duplicate identity/revision adds nothing, conflicting same-revision data rejects, and older revisions are stale. Reasoning/cache components must not be added twice. Pricing, context limits and goal policy are consumers, not driver responsibilities.

Temporary credentials resolve on the inference host for captured profile/account intent at dispatch. Auth leases are private and unserialized; opaque owner/generation invalidates compatible WS channels. No credentials in history, CLI capture, diagnostics or fingerprints. SSE is baseline; optional WS must reconstruct from full local history/tool definitions with store=false. Cache/continuation IDs are disposable. Transport loss after possible submission/publication cannot automatically resend on another transport. Retry decisions stay with the existing turn/publication owner using actual delivery and commit evidence.

## Research completion boundary

This ticket supplies reviewed interfaces, record ownership, source traces and targeted design validation. It does not implement neutral runtime, wire codecs, schema converters, auth, WS, goals or run completion. Existing implementation tickets receive the concrete scope/acceptance amendments in downstream-updates.json; no duplicate ticket system is needed. Package patch-version differences and prototype limits are explicit in validation/README.md. Runtime release acceptance still requires working downstream integrations.
