# Findings and classification

All source citations refer to the audited base. The test named cases in `test/och70_chatml_audit/audit_tests.ml` preserve executable evidence. Suggested priority is audit severity, not a Linear priority mutation.

## F1 — Module values pass record checks that the evaluator cannot honor

**Confirmed defect; high correctness impact.**

```chatml
module M = struct let x = 1 end
let result = {M with x=2}.x
```

The full pipeline accepts this and reports `Record extension base is not a record` at runtime. Likewise `match M with | {x=n} -> n` is accepted as exhaustive and then reports `Non-exhaustive pattern match`. Paired literal-record operations succeed; `M.x` succeeds. An explicit `{x:int}` annotation does not separate the module from a record.

The checker constructs `Record (Row (exports_map, Empty_row))` for modules at `lib/chatml/chatml_typechecker.ml:2436`. Field access deliberately supports both runtime representations (`lib/chatml/chatml_eval.ml:257`), but record extension accepts only VRecord (`:332`), and record patterns accept only VRecord (`lib/chatml/chatml_lang.ml:376`). The resolver does not introduce a representation conversion.

The fix must choose and consistently enforce the module/value contract: statically distinguish restricted module operations, or support the promised record operations deliberately. It must not casually merge mutable module environments into immutable records. Cover aliases, annotated/higher-order arguments, branch joins, builtin record consumers, patterns, exported mutable values, lexical closure capture and `open`. This blocks acceptance of relevant module extensions and semantic services, not unrelated repository discovery.

## F2 — Exact record-pattern coverage is invalidated by row widening

**Confirmed defect; high correctness impact. Two independently reproduced paths.**

```chatml
let r = if true then {x=1; y=2} else {x=3}
let result = match r with | {x=n} -> n
```

The checker retains only common field x and considers the pattern exhaustive; the actual selected record still has y, so the evaluator rejects the sole exact-size pattern. Replacing it with `{x=n; _}` succeeds. Accessing the common x succeeds and accessing noncommon y is correctly rejected.

```chatml
let f r = match r with | {x=n} -> n
let result = f({x=1; y=2})
```

The lambda's closed row is reopened, so the wider argument is admitted even though its exact pattern then fails. Calling f with `{x=1}` succeeds.

Common-field joins return a closed Empty_row when appropriate in `lib/chatml/chatml_typechecker.ml:2109`; lambda reopening introduces a fresh row tail at `:2006`. Coverage treats exact known labels plus a closed row as total at `:1658`, while runtime exact patterns require the actual record size to match at `lib/chatml/chatml_lang.ml:389`. An apparent static closed row cannot establish physical field equality after width abstraction.

The fix must retain sufficient width/exactness information or revise exact-pattern admission consistently. Preserve useful safe common-field joins and open patterns. Check both branch directions, if/match joins, nested records/variants, higher-order helpers, annotations and mutable containers. Do not resolve the issue by globally disabling useful row polymorphism.

## F3 — Unit payloads and nullary variants share one static encoding

**Confirmed defect; high correctness impact, both false acceptance and false rejection.**

```chatml
let result = match `A(()) with | `A -> 1
```

The checker accepts an exhaustive nullary pattern against the unary-unit value; execution reports `Non-exhaustive pattern match`. Conversely the valid unary patterns `` `A(x) `` and `` `A(()) `` are rejected as missing nullary `` `A ``. A true nullary value/pattern and a boolean payload with a binding pattern work.

Expression and pattern inference encode zero payloads as Unit and a single payload as its type (`lib/chatml/chatml_typechecker.ml:2308`, `:1978`). `payload_component_types` decodes Unit as no components (`:1645`). Runtime variants retain a list of values and require equal list lengths (`lib/chatml/chatml_lang.ml:363`). This is lost arity, not a dynamic input-schema failure.

The fix must represent/preserve constructor payload arity in inference, declarations, builtin conversion, unification, display, coverage and runtime signatures. Cover zero/one-unit/multiple arguments, recursive aliases, Option/builtin variants, entrypoint contracts and immutable compiled representations. Before proposing a wire change, inventory actual persisted value/type surfaces rather than assuming types are durable.

## F4 — Equality checks do not retain obligations on unknown types

**Confirmed static contract bypass; medium impact. No unsafe memory or authority consequence demonstrated.**

```chatml
let eq x y = x == y
let a = [1]
let result = eq(a, a)
```

Direct `a == a` is rejected as unsupported array equality; the helper accepts arrays and functions and returns true for the same object. Equal ordinary records continue to work. Reproduction distinguishes direct and abstracted forms, so it does not assume physical-identity equality itself is memory unsafe.

`ensure_equality_type` accepts free/generic variables immediately (`lib/chatml/chatml_typechecker.ml:1011`) without recording a deferred restriction; binding/generalization/instantiation (`:279`, `:315`) later admits a prohibited instantiation. Runtime `equal_value` supports physical identity for arrays, refs, closures, modules, builtins and tasks (`lib/chatml/chatml_lang.ml:342`), making the documented static restriction bypass observable.

Choose explicit equality-qualified type schemes/obligation validation or a coherent alternate language policy. Check monomorphic late binding, generalized aliases, module exports, higher-order functions and open row tails. Decide how hidden fields after width abstraction affect equality. Direct and abstracted forms must obey the same chosen rules, with callable diagnostics. No host security priority is inferred from this finding.

## F5 — Eager self-reference reads an empty module export environment

**Confirmed initialization contract mismatch; medium impact.**

```chatml
module M = struct let x = 1 let y = M.x end
let result = M.y
```

This typechecks but reports `No field 'x' in module`. The paired `let f () = M.x` executes successfully after module construction, and an ordinary reference `let y = x` uses the local definition normally.

The checker permits module self-reference through a placeholder and later unifies explicit exports (`lib/chatml/chatml_typechecker.ml:2409`). Runtime publishes the export environment only after evaluating all statements (`lib/chatml/chatml_eval.ml:393`–`:414`). The existing deferred-self-reference test does not cover eager initialization.

Choose a precise static initialization restriction or an incremental publication contract with explicit forward-reference/rebinding behavior. Include eager/deferred reads, later/missing exports, nested modules, duplicate names and initializer failures. Do not add generic recursive-module semantics accidentally. Keep this separate from F1 because even consistent record operations would not solve availability during initialization.

## Intended semantics and bounded observations

- Read-only evidence and executable cases support level-based generalization and value restriction: polymorphic identity succeeds; captured lambda aliases, arrays of functions and module-exported mutable arrays reject later int/bool disagreement. Fresh refs may remain polymorphic through eta expansion, as exercised by the existing checker suite. These cases do not prove every escape path safe.
- Guarded recursive data and explicit recursive aliases are supported; direct self-application and an unguarded alias are rejected. Explicit/inferred recursive comparison coverage exists in the existing suite. Full recursive-type equivalence, alias interactions and rollback after partial mutation are not proven.
- Wrong annotations and call arities reject before execution; out-of-range indexing, zero division and invalid JSON are expected runtime failures. Json.parse_opt handles malformed supplied data without raising. Dynamic JSON conversion is an explicit boundary, not evidence that arbitrary host-returned values obey static schemes.
- Explicit module exports, outer lexical use without implicit export, rebinding and deferred closure capture are covered by baseline and new paired cases.

## Usability/verification gaps, not promoted to confirmed soundness defects

Duplicate lambda parameter names are accepted and select the later binding. Whether they should reject is a language policy question; pattern-binder duplication already rejects. The parser reports existing conflicts (33 shift/reduce states and two reduce/reduce states); warning counts alone do not demonstrate a parse defect. No parser changes were made.

Diagnostics correctly locate the tested missing expression on line2/column17 after UTF8 text and CRLF. An unknown annotated type is located at the RHS expression rather than the type-name token; entrypoint-contract errors lack a source span. These are bounded diagnostic-precision limitations for M5, not full source-map qualification. Columns follow existing source byte positions; editor unit conversion remains a separate contract.

The builtin/host inventory was not exhaustively independently verified. Array.map, Option/Json parsing, core primitives, entrypoint/related-state contracts and existing host-budget tests were sampled. Large builtin atomic operations, hostile recursive graphs, concurrent compile stress, allocation estimates and every host operation/result adapter remain explicit coverage gaps. No decrypt/search access to provider reasoning, provider feature parity or security escape is claimed.
