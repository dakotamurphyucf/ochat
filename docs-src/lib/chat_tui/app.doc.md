# Chat_tui.App — terminal hosts and event-loop ownership

<a id="chat_tuiapp"></a>
<a id="chat_tuiapp--start-and-run-the-ochat-terminal-ui"></a>

## Overview

Two public runners share the controller, editor and renderer:

- `run_chat` owns the legacy prompt runtime, local canonical history, export
  and optional session-snapshot flow.
- `run_agent_session` presents an already attached `Agent_session_client.t`.
  Native local, Unix-daemon and HTTP-daemon modes use this runner. Execution,
  permissions, canonical history and persistence belong to the session authority.

Both runners accept `?typeahead_config` and use the shared
[typeahead lifecycle](type_ahead_provider.doc.md). Editor drafts and viewport
state remain client-local. Agent-mode quit does not export over the prompt.
See [host setup](../../agent-server/embedding.md) and the
[TUI guide](../../guide/chat_tui.md).

## Public API

<a id="high-level-entry-point"></a>
<a id="boot-sequence--run_chat"></a>

<a id="run_chat"></a>

### run_chat

```ocaml
val run_chat
  :  ?typeahead_config:Type_ahead_config.t
  -> env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?session:Session.t
  -> ?export_file:string
  -> ?persist_mode:persist_mode
  -> ?parallel_tool_calls:bool
  -> ?textmate_grammar_files:string list
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> unit
  -> unit
```

Boot the legacy TUI and block until quit. The prompt declares tools, model
configuration and initial history. A supplied session's nonempty history takes
precedence over prompt history. Its runtime tasks and key/value state are
restored as well. Explicit grammar files are loaded before terminal startup;
automatic grammar discovery runs after the first frame.

### run_agent_session

```ocaml
val run_agent_session
  :  env:Eio_unix.Stdenv.base
  -> client:Agent_session_client.t
  -> ?textmate_grammar_files:string list
  -> ?typeahead_config:Type_ahead_config.t
  -> unit
  -> unit
```

The caller supplies an attached client and owns its lifetime. History deletion,
submission, cancellation and compaction go through authorized client methods,
not optimistic local history changes. Reconnect replaces the projection while
preserving the private draft; reconnect itself never submits that draft.

[Agent_history_layout](agent_history_layout.doc.md) coalesces initial/width-change
preparation in an aggregate worker. Warm-width updates render dirty chunks.
This is distinct from the legacy progressive corridor pipeline.

### type prompt_context

```ocaml
type prompt_context =
  { cfg : Chat_response.Config.t
  ; tools : Openai.Responses.Request.Tool.t list
  ; tool_tbl : (string, Ochat_function.runner) Base.Hashtbl.t
  ; moderator : Chat_response.In_memory_stream.moderator option
  }
```

Prompt setup derives this runtime configuration. Tool implementations are
`Ochat_function.runner` values, not plain `string -> string` callbacks.

### type persist_mode

```ocaml
type persist_mode = [ `Always | `Never | `Ask ]
```

This controls legacy binary snapshot saving on exit, independently of transcript
export. It has no effect without a supplied session.

<a id="shell-runtime-integration"></a>

## Boot and shell runtime integration

Legacy startup loads the prompt and cache, constructs the model and moderator,
and initializes the shell registry, exact-manifest authorization and approval
broker before normal input is enabled. The startup layout barrier keeps
ordinary input disabled until exact history is published.

Shell management workers return immutable generation-tagged results. The UI
owner alone applies current results. Shell Security has independent page state;
approval/moderator dialogs are overlays. Their input takes precedence over Chat
editor keys. The host switch owns approval, process, audit and worker resources.

<a id="submitting-prompts"></a>
<a id="apply_local_submit_effects"></a>
<a id="handle_submit"></a>
<a id="how-streaming-and-events-fit-together"></a>

<a id="architecture"></a>

## Event loop and streaming architecture

Input, internal worker results and redraw requests use separate streams.
The UI reducer owns model mutation; workers report operation-tagged events.

1. `Controller.handle_key` translates an input event into a typed reaction.
   Bare Normal Escape with a Visual selection clears it and remains Normal;
   it does not request cancellation or quit.
2. `App_submit.capture_request` snapshots text and Plain/Raw mode.
   `clear_editor` is a separate operation. The reducer starts or defers the
   captured request according to active work and moderation.
3. `App_submit.start` validates/converts the request, appends a canonical
   user entry on success, and starts the turn through its `start_streaming`
   callback. Invalid raw input restores the rejected draft when no newer draft
   exists and displays a local notice without starting a turn.
4. `App_streaming.start ctx ~history ~op_id` creates a streaming switch and
   emits `Streaming_started (op_id, sw)`. `App_runtime.op` owns that switch;
   `Model.fetch_sw` is not the active streaming cancellation mechanism.
5. The identity-bearing in-memory Responses driver emits sourced deltas,
   canonical history events, tool lifecycle/output and moderator requests.
   Transport batching produces `Sourced_stream[_batch]`,
   `History_stream[_batch]`, `Tool_execution`, `Tool_output` and
   `Moderator_runtime_request` events.
6. Completion flushes accepted transport events before
   `Streaming_done (op_id, history)`. The reducer installs current-operation
   results and rebuilds the effective projection.

`OCHAT_STREAM_BATCH_MS` defaults to 12 ms and is clamped to 1–50 ms.
Redraw requests are coalesced independently; `OCHAT_TUI_FPS` defaults to 30.
See [App_streaming](app_streaming.doc.md), [App_events](app_events.doc.md) and
[App_reducer](app_reducer.doc.md).

<a id="error-placeholders"></a>
<a id="add_placeholder_stream_error"></a>

### Cancellation and error placeholders

Legacy cancel-or-quit fails an active streaming/compaction switch, or records
cancellation until a starting operation supplies its switch. When idle it
requests shutdown. Selection clearing happens before this host action.

Streaming exceptions, including cancellation, become `Streaming_error`.
For the current operation, the reducer clears activity/tool progress, removes
trailing reasoning and repairs incomplete function/custom-tool pairs with
synthetic outputs. It then refreshes projection and appends a local error
placeholder. This is transcript consistency repair, not rollback of tool effects.
Stale operation events cannot apply these changes to a newer turn.

Agent-mode cancellation instead calls the session client's cancellation method;
do not infer the legacy placeholder mechanism for that host.

### Context compaction

`App_compaction.start` snapshots legacy history, sets the Compacting activity
indicator, optionally saves the current session, and forks the compactor.
Only matching `Compaction_done` results replace history. Failure/cancellation
clears activity and adds a local notice. Saving that snapshot is not an archive
or reset operation. Native/daemon compaction belongs to the session actor.
See [App_compaction](app_compaction.doc.md) and
[compaction policy](../../context_compaction/compactor.doc.md).

<a id="snapshot-persistence"></a>
<a id="persist_snapshot"></a>

<a id="shutdown"></a>

## Sessions, export, and persistence

After releasing the terminal, legacy quit via idle Escape asks whether to export
and can ask for a destination. Explicit quit via `:q`/Ctrl-C exports to
`export_file`, or the original prompt path when omitted. Bare `q` is not
the ordinary Insert-mode quit shortcut. `:wq` quits without submitting.

Snapshot policy is independent: `Always` saves, `Never` skips, and `Ask`
prompts `Save session snapshot? [Y/n]` when a session exists.
The private `Session_persist.persist_snapshot` helper consumes runtime state,
including canonical history, the next unused history sequence, tasks, key/value
state and moderator snapshot; it is not a public `App` API.

[Persistence](persistence.doc.md) exports semantic ChatMarkdown with stable IDs.
Binary session snapshots remain authoritative for full legacy runtime state.
Export does not universally sanitize, bound or redact arbitrary tool output;
inspect sensitive transcripts before sharing.

## Examples

### Minimal CLI using run_chat

Start a legacy prompt host:

```ocaml
let () =
  Eio_main.run (fun env ->
    Chat_tui.App.run_chat ~env ~prompt_file:"prompt.chatmd" ())
```

### Custom persistence policy

Resume or create a named legacy session and save its snapshot on exit:

```ocaml
let () =
  Eio_main.run (fun env ->
    let prompt_file = "prompt.chatmd" in
    let session =
      Session_store.load_or_create ~env ~prompt_file ~id:"work" ()
    in
    Chat_tui.App.run_chat
      ~env ~prompt_file ~session ~export_file:"work-export.chatmd"
      ~persist_mode:`Always ())
```

For native/daemon construction, use the
[embedding guide](../../agent-server/embedding.md).

## Known issues and limitations

Legacy cancellation still produces an error placeholder; transcript repair
does not undo tool effects. Compaction is lossy and may involve provider calls.
Terminal glyph widths can differ from measured editor geometry. Export is not
a universal redaction boundary. These behaviors are described above and in the
[TUI guide](../../guide/chat_tui.md).

<a id="related-modules"></a>

<a id="internal-modules"></a>

## Internal modules

- [App_runtime](app_runtime.doc.md): operation IDs, switches and pending work.
- [App_submit](app_submit.doc.md): admission and turn-start effects.
- [App_streaming](app_streaming.doc.md): driver and batched worker events.
- [App_stream_apply](app_stream_apply.doc.md): UI-owned event application.
- [App_reducer](app_reducer.doc.md): legacy scheduling and cleanup.
- [App_compaction](app_compaction.doc.md): legacy compaction worker.

These are separate modules, not additional functions exported by `App`.
Use the [interface](../../../lib/chat_tui/app.mli) for callable signatures.
