# Bounded coverage matrix

Executed evidence means a finished successful check reported in verification.md; source-only entries do not imply tests ran. All observations use the audited base plus ticket-owned reproducer tests. Passing cases do not establish soundness.

| Ticket area | Mechanism/source anchor | Evidence and outcome | Remaining limit |
| --- | --- | --- | --- |
| Unification/occurs | typechecker.ml:468,605 | Baseline checker tests; new self-application negative and guarded-record positive | No complete graph equivalence/property proof; no rollback guarantee after failed unification |
| Recursive aliases/guardedness | typechecker.ml:358,1187,1250 | Unguarded alias rejects; baseline explicit/inferred recursive data, binder-scope/payload mismatch cases | No exhaustive alias nesting or mutually recursive type-declaration support assumed |
| Variable levels | typechecker.ml:148,167,315,468 | Captured lambda alias rejects int/bool mixed calls, polymorphic identity accepts independent calls | No exhaustive closure/module escape analysis |
| Generalization/instantiation | typechecker.ml:279,315,1350 | Baseline const/compose/primitive alias tests; new identity versus captured alias | Equality obligations missing (F4); recursive-containing bindings deliberately conservative |
| Value restriction | typechecker.ml:1400,1446,1472 | Weak array and module-exported weak array reject incompatible update/use; baseline ref/array/alias cases | No full variance or mutation-path proof |
| Mutable values | eval.ml:289,298,311; builtin_spec.ml:1848 | Baseline ref/array update/copy; new weak-array negative and indexed-error outcome | All Hashtbl/callback combinations not independently sampled |
| Record rows/joins | typechecker.ml:1522,2109,2132 | Common-field read works; noncommon read rejects; paired exact/open patterns expose F2 | Nested mutable/higher-order width cases require follow-up |
| Variant rows/arity | typechecker.ml:1645,1978,2308 | Bool-payload/constructor coverage cases; unit/nullary mismatch confirms F3 | Recursive/builtin constructor arity interactions require follow-up |
| Match coverage/redundancy | typechecker.ml:1748,1813,1888 | Baseline duplicate/catch-all/boolean/variant checks; new missing payload case rejects | Record width and unit arity break accepted coverage; no full pattern-matrix algorithm proof |
| Equality constraints | typechecker.ml:1007,2078; lang.ml:330 | Direct array reject versus generalized array/function accept confirms F4 | Open rows, tasks, refs, higher-order exports need constrained-policy work |
| Annotations | typechecker.ml:1187,2137 | Bool-as-int and lambda arity reject; related moderator state types checked together | Explicit scheme annotation/skolem escape completeness not proven |
| Function arity | parser.mly:228; eval.ml:201 | Baseline closure arity; new zero-arg good/bad, annotated arity, entrypoint arity | Duplicate binder policy unresolved; no currying assumed |
| Module scope/exports | typechecker.ml:2393; eval.ml:393 | Baseline exports/open/closure capture; new outer-not-exported and rebound export | F1 representation and F5 initialization mismatch; qualified type exports not selected as already working |
| Parse/check/resolve/evaluate agreement | resolver.ml:55; eval.ml:94 | All new source cases use complete default production pipeline; baseline suites pass | Manually supplied resolved AST invariants not exercised; parser conflict counts not proof of defects |
| Builtin scheme vs implementation | builtin_spec.ml:1648,1202,1821; builtin_modules.ml:83 | Array.map positive/type-negative, Json parse/parse_opt, Option.is_none, core operations sampled | Inventory-wide host/builtin type/result/error oracle still missing |
| Host dynamic JSON boundary | host_runtime.ml:600,780; compilation.ml:55 | Invalid raw bool One_off output rejects; poisoning initializer compiles without running; required state bindings linked | No malformed actual host-return stubs or full native adapter audit; compile success grants no tools |
| Compiler invocation isolation | compilation.ml:109; typechecker.ml:148 | Private value/type unavailable in next compile; valid compile succeeds after rejection | Not concurrent stress; no shared state reuse approval |
| Entrypoint validation | typechecker.ml:2499; extension_surface.ml | One_off arity/output negative; Moderator initial-state/event-result disagreement rejects | Every supported surface/alias not independently tested here |
| Cancellation/resource controls | compilation.mli:43; execution.mli:124 | Existing standalone, compiler traversal/diagnostic and execution-budget suites executed successfully; own source-limit case passes | Cooperative budgets, not hard heap/time sandbox; see final results and limitations |
| Diagnostic source accuracy | parse.ml; typechecker.ml:213; host_runtime.ml:790 | New UTF8+CRLF source case gives line2/column17; invalid type RHS and entrypoint span limitations documented | Not a complete embedded ChatMD/import/editor source map; grapheme/UTF16 conversion not tested |

Paths in shortened anchors are under lib/chatml. Tests: test/chatml_{parse,typechecker,runtime,standalone,execution_budget,diagnostic_budget,type_traversal}_test.ml and test/och70_chatml_audit/audit_tests.ml. Interface contracts, especially compilation.mli/execution.mli, define cooperative cleanup/authority boundaries. Native tool/session authority adapters and persisted provider/history schemas are outside this language audit.

Investigations were stopped after useful confirmed categories and representative control qualification. Deferred work is not an assertion of correctness: see structured follow-ups for missing verification and affected consumers. OCH-73 can use the coverage/design findings before fixes finish; the applicable module/type/service acceptance needs the corresponding confirmed defect resolved or explicitly restricted.
