(** High-level orchestration helpers for ChatMarkdown conversations.

    The [Driver] module bridges a user-editable {.chatmd} document and the
    OpenAI Responses API. It bundles prompt parsing, caching,
    tool discovery and the recursive response loop into a few convenience
    wrappers that can be reused by the CLI, the TUI and nested agents.

    {1 Entry points}

    • {!val:run_completion} – blocking, single-turn helper that operates on a
      document on disk.

    • {!val:run_completion_stream} – streaming flavour that invokes a callback
      for every incremental chunk coming from the model so that UIs can render
      the conversation in real time.

    • {!val:run_agent} – evaluate a standalone *agent* prompt from within an
      existing conversation.  Used internally by the [fork] tool but also
      available to power-users.

    Identity-bearing in-memory callers use
    {!In_memory_stream.run_completion_stream_in_memory_entries} with a
    caller-owned allocator and canonical history.
*)

open! Core

(** Observer for transient nested-agent activity.

    [on_event] receives nested Responses stream events in consumption order.
    [on_tool_execution] receives nested tool lifecycle and progress events.
    Observer failures are suppressed and cannot alter the final result.
    Callbacks shared by independent invocations must be concurrency-safe and
    return promptly. *)
type agent_observer = Agent_response_loop.observer =
  { on_event : Openai.Responses.Response_stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

(** [run_agent ~ctx prompt_xml items] evaluates a *nested agent* inside the current conversation.

    [run_agent ~ctx prompt_xml items] treats [prompt_xml] as an independent
    ChatMarkdown document (typically starting with a [&lt;system&gt;] block and
    optional configuration) and appends the additional [items] – usually a
    user message constructed at runtime – before forwarding everything to the
    OpenAI endpoint.

    The function blocks until the agent has produced its final assistant
    answer and returns that answer as plain text (concatenated if multiple
    messages were emitted).

    It is the caller’s responsibility to ensure that [prompt_xml] contains any
    tool declarations required by the agent.

    Complexity: proportional to the number of turns triggered by tool calls
    inside the agent prompt.

    @param history_compaction When [true] the helper collapses repeated
           reads of the same file so that only the *latest* version is
           forwarded to the nested agent.  This helps keep token usage
           under control when the agent spawns long chains of tool calls.

    @param observer Optional sink for transient nested model and tool activity.
           Ordinary prompts use the streaming response loop only in observed
           mode; omitting it preserves the legacy blocking path. Observation
           does not change the single returned final text.

    @param response_dir Directory for raw Responses artifacts. TUI callers
           should pass their session data directory. It defaults to the
           current context directory for compatibility.
*)
val run_agent
  :  ?history_compaction:bool
  -> ?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?session_id:string
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?observer:agent_observer
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?source:string
  -> ?parent_call_id:string
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> string (** ^ XML source of the agent prompt *)
  -> Prompt.Chat_markdown.content_item list (** ^ Inline user items        *)
  -> string

(** [run_entries ~ctx ~allocator ~model ~tool_tbl history] runs blocking or
    observed response execution over canonical entries. The caller owns
    [allocator]; raw payloads exist only at provider request boundaries. *)
val run_entries
  :  ctx:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Openai.Responses.Request.Tool.t list
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?observer:agent_observer
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?source:string
  -> ?parent_call_id:string
  -> ?post:Response_loop.post
  -> ?post_stream:Agent_response_loop.post_stream
  -> model:Openai.Responses.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> History_entry.t list
  -> History_entry.t list

(** [run_completion ~env ?prompt_file ?parallel_tool_calls ~output_file ()] performs a synchronous, file-based completion.

    [run_completion ~env ?prompt_file ~output_file ()] processes a full
    ChatMarkdown turn in *blocking* mode.  The evolving conversation is read
    from [output_file] (created if needed); the final assistant answer is
    appended back to the same file together with any reasoning blocks and
    tool-call artefacts.  If [prompt_file] is provided its contents are
    prepended once at the beginning of the session – handy for templates.

    No value is returned; callers should read [output_file] afterwards if they
    need the assistant answer in memory.

    @param parallel_tool_calls If [true] (default) tool invocations are
           executed concurrently (bounded by an internal semaphore). Their
           outputs are flushed back to the document in the original call
           order so that the conversation remains deterministic.  Set to
           [false] for fully sequential execution (useful in tests).

    @param meta_refine Enable the *meta-refine* experimental feature that
           lets the model self-critique and issue follow-up requests.  The
           flag can also be toggled via the [OCHAT_META_REFINE] environment
           variable.  Defaults to [false].
*)
val run_completion
  :  env:Eio_unix.Stdenv.base
  -> ?prompt_file:string
  -> ?parallel_tool_calls:bool
  -> ?meta_refine:bool (** ^ Enable Recursive Meta-Prompting? *)
  -> output_file:string
  -> unit
  -> unit

(** [run_completion_stream ~env ?prompt_file ?on_event ?history_compaction ~output_file ()]
    streams assistant deltas and high-level events **as they arrive**.

    Compared to {!run_completion} this variant:

    • Uses the streaming OpenAI API to obtain partial tokens.
    • Invokes [?on_event] for every chunk, letting callers update a TUI
      or web UI in real time.  The default callback ignores events so
      existing scripts remain unchanged.
    • Executes tool calls as soon as they are fully parsed, then
      continues streaming the response.

    Side-effects mirror {!run_completion}: partial messages and
    reasoning summaries are appended to [output_file] immediately so
    the buffer is crash-resistant.

    @param history_compaction When [true] the driver collapses redundant
           file-read entries in the history before forwarding it to the
           model, saving tokens on long conversations that repeatedly look
           at the same documents.

    @param parallel_tool_calls  Behaviour identical to the flag of the same
           name in {!val:run_completion}.

    @param meta_refine          Behaviour identical to the flag of the same
           name in {!val:run_completion}.

    Example – live rendering in the terminal:
    {[
      let on_event = function
        | Responses.Response_stream.Output_text_delta d ->
            Out_channel.output_string stdout d.delta
        | _ -> ()

      Eio_main.run @@ fun env ->
        Driver.run_completion_stream
          ~env
          ~output_file:"conversation.chatmd"
          ~on_event
          ()
    ]} *)
val run_completion_stream
  :  env:Eio_unix.Stdenv.base
  -> ?prompt_file:string
  -> ?on_event:(Openai.Responses.Response_stream.t -> unit)
  -> ?on_history_event:(History_stream_event.t -> unit)
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?on_history_tool_out:(History_entry.t -> unit)
  -> ?post_stream:In_memory_stream.post_stream
  -> ?on_final_history:(History_entry.t list -> unit)
  -> ?parallel_tool_calls:bool
  -> ?meta_refine:bool
  -> ?history_compaction:bool
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> output_file:string
  -> unit
  -> unit

(** [run_completion_stream_in_memory_v1 ~env ~history ~tools ()] streams a
    ChatMarkdown conversation **held entirely in memory**.

    Compared to {!run_completion_stream} this helper:

    • Accepts an explicit [history] (list of {!Openai.Responses.Item.t})
      instead of reading a `.chatmd` file from disk.
    • Returns the *complete* history after all assistant turns and tool
      calls have been resolved.
    • Never touches the filesystem except for the persistent cache under
      `[~/.chatmd]`, making it suitable for unit-tests or server back-ends
      where direct file IO is undesirable.

    Optional callbacks mirror the streaming variant:

    • [?on_event] – invoked for each streaming event received from the
      OpenAI API (token deltas, item completions, …). Defaults to a no-op.
    • [?on_fn_out] – executed after each tool call completes, allowing the
      caller to react to side-effects without waiting for the final
      assistant answer.
    • [?on_sourced_event] – receives stream events with outer/fork source
      attribution.
    • [?on_tool_out] – receives canonical tool-output history items.
    • [?on_tool_execution] – receives transient start, progress, and terminal
      events. These values are never canonical history.

    @param env      Standard Eio runtime environment.
    @param history  Initial conversation state.
    @param tools    Compile-time list of tool definitions visible to the
                    model.  Pass [[]] for none.
    @param tool_tbl Optional lookup table generated from [tools].  The
                    default builds a fresh table via
                    {!Ochat_function.functions} when omitted.
    @param temperature Temperature override forwarded the OpenAI request.
    @param max_output_tokens Hard cap on the number of tokens generated by
           the model per request.
    @param reasoning Optional reasoning settings forwarded to the API.
    @param moderator Optional session-scoped moderator runtime. When present,
           the driver applies [turn_start] before each model request and
           [turn_end] after each streamed turn, using the moderator overlay to
           compute the effective request history.
    @param on_runtime_request Optional callback invoked for surfaced moderator
           runtime requests such as compaction or end-session notifications.

    @param history_compaction If [true], the function will compact the
           history so that multiple calls to the same file are replaced with a
           single call that points to the latest file content. Outputs for older calls are replaced with a
           place holder that points to the latest call output (stale) file content removed — see newer read_file output later

    @return The updated [history], i.e. the concatenation of the original
            [history] and every item produced during the streaming loop.

    @raise Any exception bubbled-up by the OpenAI client or user-supplied
           tool functions.  The function does **not** swallow errors. *)
