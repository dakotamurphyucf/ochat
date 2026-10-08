# Neutral inference contracts

`ochat.inference` defines pure values shared by provider adapters, runtime owners and usage readers. It does not select a provider, resolve credentials, dispatch a request, commit history or execute tools. Runtime adoption of these contracts is a separate part of the inference-routing work; adding this library alone does not migrate any existing inference route.

## Private request and captured selection

`Inference.Request` captures the final ordered canonical history, selected target, tool descriptions and immutable asset bytes. Preparation follows moderator changes and final authoring guidance. Changing those inputs requires preparing and admitting a new request. Public visible or redacted transcript rows cannot substitute for canonical history.

A target records the actual adapter, profile, optional profile revision and account, endpoint, model and effective settings. Missing historical information stays unavailable. Settings distinguish omission, explicit null and a value, and retain their actual selection provenance. The neutral constructor validates bounded JSON; the selected adapter must still validate supported settings, endpoint semantics, capabilities and replay eligibility. Credentials are supplied through the host's separate runtime capability, never through these settings.

Target and setting codecs preserve their complete admitted private JSON, including unknown fields and numeric spelling. They are storage values, not safe public configuration reports. Named object member order does not change semantic equality. Exact stored bytes and digests remain the persistence owner's responsibility.

`Inference.Selection` distinguishes an unresolved stored selection from a captured target. Capturing an unresolved value preserves unknown wrapper fields. Repeating the same capture is harmless; choosing a different target requires a separate host-authorized change. An unresolved selection does not authorize execution or imply a default provider. The session owner must durably record an explicit selection before dispatch.

Assets contain already resolved immutable bytes and their exact host references. They contain no filesystem or network callbacks. Tool descriptions carry schemas, not execution grants; the existing host registry and admission owner remain responsible for bindings, authorization and execution. Request limits account for the complete neutral representation and base64 body size without encoding asset bodies merely to measure them. Provider wire limits are checked separately by the adapter.

## Provider evidence and host commits

`Inference.Event` admits provisional transcript observations, complete candidate payloads and inference terminals. A provider cannot use it to publish a committed `Item_finalized` or finish the host's local output admission. A candidate retains its actual source, attempt and item identities, but does not allocate a history ID or authorize a tool call. The host publishes finalization only after its existing admission and commit boundary succeeds.

Every candidate explicitly states whether it is eligible for local tool admission. A retained call-shaped payload alone is insufficient. The adapter attests its provider-specific caller semantics, and the neutral constructor rejects unsupported namespace, asynchronous-call and status combinations for tool candidates. Ineligible candidates still retain their original payload. Eligible candidates must pass the host registry, schema, moderation and permission checks before execution; eligibility is evidence, not a grant.

Terminals preserve whether a request was definitely not submitted, possibly submitted, or had a response started. Safe failure categories are closed values; arbitrary provider messages, response bodies and exception text do not enter them. Authentication failure cannot claim that inference was submitted. A provider terminal does not complete an agent workflow, durable job, script, goal or host turn. Cancellation and unexpected exceptions retain their normal propagation semantics.

## Usage and safe observations

`Inference.Observation` separates consumption, context estimates, captured safe configuration and bounded diagnostics. Token components independently distinguish actual, estimated and unknown values. Unknown reasons preserve absent fields and explicit nulls; actual zero remains actual zero. Cached input and reasoning output can be declared subsets of other components and must not be counted again as additional consumption. A provider-reported total is retained separately, without inventing an arithmetic equality that the provider did not promise.

Each observation has a real attempt scope, a host observation identity and a nonnegative authoritative revision. A higher revision replaces the complete previous observation, even when counts decrease or become unknown. An exact duplicate has no additional accounting effect. Conflicting equal revisions reject. Older revisions are stale, but cannot change the scope's parent relation or the observation family. Captured configuration and the identity of an estimated prepared input remain immutable.

Context estimates identify their actual prepared input with an opaque host identity. They are estimates, not consumption, billing, capacity or a hard upper bound. Unknown capacity stays unknown. Safe configuration uses a closed setting vocabulary and withholds private values such as instructions, schemas and cache keys. Diagnostics accept enumerated reasons and bounded numeric facts, not arbitrary strings to be scrubbed later. Detailed configuration and diagnostics still require runtime authorization; a pure constructor grants none.

An attempt read row has an explicit lifecycle, a designated accounting observation identity and bounded observations. Prepared, running, provider-terminal and host-interrupted states remain distinct. A missing usage report cannot imply that the attempt is still running or consumed zero tokens. Neither the row nor the observation reducer claims a committed host turn.

The immutable latest-observation reducer bounds its working set and never silently evicts accounting identities. It is not a lifetime ledger or an archive policy. Runtime persistence must retain or explicitly retire identities, report incomplete coverage honestly, and consult retained records before treating a replay as new consumption. Durable document carriers own future storage extensions; safe public observation codecs do not expose arbitrary retained JSON.

## Boundaries that remain with runtime owners

The effectful adapter binds a prepared request to its selected provider and host credential resolver. Session owners capture and restore independent selections for parent sessions, children and admitted model jobs. Actual persistence acknowledgements precede dispatch; a changed parent selection cannot rewrite already admitted work.

Runtime owners also determine successful host-turn commits, accounting retention, query pagination and visibility. These effects are deliberately absent from the pure contracts. Offline contract tests establish their validation and reconciliation behavior; they do not establish live endpoint compatibility or complete route migration.
