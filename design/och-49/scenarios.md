# Source-backed scenario walkthroughs

These trace current implementation seams and the proposed substitutions. They are design evidence, not proof that the future runtime behaviors have been implemented or tested. Source pin: 58e9d2c5567d596fbe52ba1041e37d12dbde102b. The compile/probe checks are separately recorded in validation/README.md.

## Root turn with one local tool

Current In_memory_stream prepares moderated effective inputs, invokes before_model_call admission, appends prepare_model_input additions and obtains the stream ([2011](../../lib/chat_response/in_memory_stream.ml#L2011)). Operation_worker owns notification/moderator admission ([171](../../lib/agent_session/operation_worker.mli#L171)). Replace only immutable request/lowering/transport inputs; preserve the final admission-before-dispatch rule, reconcile any additional guidance with the fingerprint, and avoid mutable config reads during dispatch.

An output function call is validated/moderated, assigned a host entry, committed with invocation intent and only then dispatched ([1550](../../lib/chat_response/in_memory_stream.ml#L1550), [Tool_dispatch](../../lib/chat_response/in_memory_stream.mli#L82)). Existing commit_invocation_call and publish_invocation_output persist intent/output receipts before observation ([44](../../lib/agent_session/operation_worker.mli#L44)). The adapter merely emits typed call finalization. Tool input bytes/native binding identity travel through the same host service. A denial may record a rejected occurrence but cannot call a native implementation. Duplicate item completion reconciles within the attempt and cannot execute the same occurrence twice.

Owner check: adapter wire decoding, driver resources, existing fold assembly, actor canonical commit and native invocation admission each have one owner. No provider event callback directly appends a tool result or grants permission.

## Nested/authored/generated child request

Runtime_builder captures distinct Root/Generated/Authored_child sources ([1021](../../lib/agent_session/runtime_builder.ml#L1021)); generated configuration currently uses an OpenAI model parser ([573](../../lib/agent_session/runtime_builder.ml#L573), [1198](../../lib/agent_session/runtime_builder.ml#L1198)). Legacy fork is separately scoped: child history/input/moderation stay isolated and only its completed tool output enters parent history ([12](../../lib/chat_response/in_memory_stream.mli#L12), [102](../../lib/chat_response/in_memory_stream.mli#L102)).

Thread target/profile/nonsecret settings and adapter through every child executor rather than ambient OpenAI fallback. Preserve omitted inheritance versus explicit child override with provenance; child attempt/source identity and any independently active WS channel stay distinct. Existing parent authority and child binding revalidation still precede effects. A parent response ID or shared wire call string is not the child's history namespace or run identity.

Acceptance handoff: M1-T05 exercises root/child/grandchild restore/override with a synthetic non-OpenAI-shaped adapter, plus ChatML Model/private helper, compactor, meta-prompting, typeahead and headless paths. Existing Completions routes require an explicit supported/migrate/retire disposition; embeddings are a separate API family. This design does not install a second provider merely to test independence.

## Save, restart and replay

Session_persistence currently writes Session_state/Session_delta/Durable event sexps and decodes current state before upgrade ([41](../../lib/agent_session/session_persistence.ml#L41), [119](../../lib/agent_session/session_persistence.ml#L119)). Transaction.hash re-encodes its typed representation ([167](../../lib/agent_store/transaction.ml#L167)); recovery consumes it for chain anchors ([69](../../lib/agent_store/recovery.ml#L69)). Standalone session_store reads frozen versioned runtime types ([66](../../lib/session_store.ml#L66)) and writes Session.bin_writer_t ([200](../../lib/session_store.ml#L200)).

New path: bounded frame/generic envelope/stored-version anchor verification → pure per-kind conversion → validated current domain → existing actor/recovery. The original journal bytes/digest survive conversion. Canonical IDs/high-water reservation, provider replay origin/opaque data, call occurrence binding and captured profile intent survive. Restore obtains fresh host credentials for the same identity; no stored response/cache is required. Unsupported beta formats leave bytes and pointers unchanged. All archive/moderator/retention paths must use the same boundary, not a new snapshot-only reader.

Acceptance handoff: independently constructed new-version documents cover rename/missing/default/null/extensions and atomic failed installation; meaningful existing persistence/identity suites cover actual integration later. No old binary fixture family is required.

## Refusal, incomplete and transport loss

Responses already distinguishes response.completed/incomplete/failed wire variants ([1483](../../lib/openai/responses.ml#L1483)), but stream_compat raises one terminal error on incomplete/failed ([1639](../../lib/openai/responses.ml#L1639)). Neutral events carry semantic delta kind and final item metadata so refusal/reasoning/call bytes are never inferred as ordinary text.

A refusal can be part of a completed inference. Incomplete preserves its reason, usage uncertainty and partial content; provider failure and transport loss remain separate. Unknown reasons remain inspectable. Steered is a decoding distinction, not adoption of provider steering. Eio cancellation/consumer failure propagates to existing cleanup. Definitive no-submission can support the owner's qualified retry policy; ambiguous submission or observed publication cannot automatically restart or switch transport.

Acceptance handoff: M1-T02/T04 provide independent completed/refused/incomplete/failed/cancelled/partial-loss events and exact field-presence encoding. Live endpoint/auth eligibility is separate qualification. Terminal outcome does not complete a goal, pending tool or CLI run.

## Pending local job and delayed result

Current runtime has model job recipes and completion events ([841](../../lib/agent_session/runtime_builder.ml#L841), [901](../../lib/agent_session/runtime_builder.ml#L901)); Operation_worker separates durable invocation admission, output publication and moderator observation receipts ([65](../../lib/agent_session/operation_worker.mli#L65), [105](../../lib/agent_session/operation_worker.mli#L105)). In_memory_stream awaits scheduled call promises after the response fold ([2063](../../lib/chat_response/in_memory_stream.ml#L2063)).

Keep existing jobs/intent/receipt owners during provider extraction. A submitted/running job is not a fake completed tool result, and provider terminal completion does not delete its pending client state. If optional provider-native async mapping is later justified, launch all independent work before a dependent wait and deliver the actual eventual output to its original host occurrence after interleaved input. A source/operation change cannot rebind it to a reused provider call string. Restart follows existing interrupted-work reconciliation, not blind external-effect replay.

Acceptance handoff: M1-T06a may find existing outcomes adequate and generate no mapping work. Any selected mapping must prove fresh-history reconstruction and occurrence-bound delayed delivery using existing jobs/receipts; otherwise retain eager/current local behavior. CAP controls policy; CLI run completion remains its orchestrator's receipt, while provider outcomes remain inference-only.
