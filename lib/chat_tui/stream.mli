(** Translate raw OpenAI streaming events into declarative patch commands.

    The {!Chat_tui.Stream} module converts the incremental events emitted by
    the ChatCompletions *stream* endpoint of the OpenAI API – represented by
    {!Openai.Responses.Response_stream.t} – into the declarative
    {!Types.patch} language that the rest of the terminal UI understands.

    {1 Design goals}

    - *Side-effect minimisation* – the primary result of every helper is a
      list of {!Types.patch} values. Tool and reasoning metadata updates are
      confined to model metadata and never alter canonical history.

    - *Single responsibility* – this module is the _only_ place that knows
      how to interpret the many concrete variants of
      {!Openai.Responses.Response_stream.t}.  The remainder of the code base
      deals solely with patches and therefore remains stable when OpenAI
      adds new streaming event kinds.

    - *Forward compatibility* – once the planned migration to an immutable
      model representation lands, even the remaining book-keeping updates
      will move into patches so that the module regains full referential
      transparency.

    {1 Pipeline change 2024-07}

    • **Raw text deltas** – the stream handler now emits *unsanitised* raw
      text via {!Types.Append_text} patches.  No UTF-8 validation or
      word-wrapping happens at this stage.

    • **Batching & coalescing** – the app event loop batches contiguous
      stream events and merges adjacent [Append_text] patches that target the
      same buffer.  This keeps the patch volume small without compromising
      incremental updates.

    • **Renderer-side sanitisation** – invalid byte sequences are removed
      exactly once during rendering, together with word-wrapping.  The
      change reduces cache invalidations and re-wraps per frame while
      leaving the visual output unchanged.
*)

module Res = Openai.Responses
module Res_stream = Openai.Responses.Response_stream

(** [handle_fn_out ~model out] converts a completed tool-call into a patch
    stream.

    When the assistant finishes executing a function call and returns its
    textual result, the OpenAI API emits a
    {!Openai.Responses.Function_call_output.t}.  The helper translates that
    record into the following patches:

    • {!Types.Set_function_output} – stores [out.output] under the final
      message id so that the renderer can display the tool response.

    @param model State snapshot used for tool metadata.
    @param out   Completed function-call record received from the OpenAI API.
*)
val handle_fn_out
  :  model:Model.t
  -> ?entry_id:History_entry.Id.t
  -> Res.Function_call_output.t
  -> Types.patch list

(** [handle_tool_out ~model item] is like {!handle_fn_out} but accepts the
    full history item.

    The function currently handles:

    - {!Openai.Responses.Item.Function_call_output}
    - {!Openai.Responses.Item.Custom_tool_call_output}

    All other items yield [\[\]].

    @param model Mutable UI state used for tool metadata.
    @param item  History item that may carry tool output.
*)
val handle_tool_out
  :  model:Model.t
  -> ?entry_id:History_entry.Id.t
  -> Res.Item.t
  -> Types.patch list

(** [handle_event ~model ?parent_call_id ev] converts a single incremental streaming event
    into a list of patches.

    The implementation understands (and therefore potentially produces
    patches for) the following event classes:

    • [Output_text_delta] – append assistant text chunks
    • [Output_item_added] – announce new items (messages, reasoning blocks
      or function calls) and initialise their buffers / metadata
    • [Output_message]     – full assistant message delivered in a single
      event (handled as a sub-variant of [Output_item_added])
    • [Reasoning_summary_text_delta] – update tool reasoning sections
    • [Function_call_arguments_delta] / [Function_call_arguments_done] –
      stream the argument list of a tool invocation

    For [read_file], [read_directory], and [apply_patch] calls, the
    implementation records tool metadata (function name and, where applicable,
    the referenced path). When tool calls run in parallel, the OpenAI stream may
    deliver tool output before the final arguments event; in that case, the
    stream handler updates the already-rendered tool output metadata and
    invalidates the per-message render cache so syntax-highlighting can be
    applied immediately (without waiting for a full history rebuild at turn
    end).

    All other variants are ignored for now and yield [\[\]].

    The returned list can be empty, contain a single patch, or multiple
    patches when a more complex update – e.g. buffer initialisation *and*
    delta append – is required.

    @param model Mutable UI state used for ancillary tool metadata and
                 reasoning indices.
    @param parent_call_id Fork source attribution. Fork events use role
           ["fork"]; outer events preserve their ordinary roles.
    @param ev    Single streaming event decoded from JSON.
*)
val handle_event
  :  model:Model.t
  -> ?parent_call_id:string option
  -> ?entry_id:History_entry.Id.t
  -> Res_stream.t
  -> Types.patch list

(** [handle_events ~model evs] folds {!handle_event} over [evs] and
    concatenates the resulting patch lists.  It exists purely for
    convenience when a client already has a list of streaming events.  The
    function behaves like:
    {[
      List.concat_map evs ~f:(handle_event ~model)
    ]}

    @param model UI state passed through to {!handle_event}.
    @param evs   List of streaming events to translate.

    It does not introduce additional side-effects beyond those already
    performed by {!handle_event}. *)
val handle_events
  :  model:Model.t
  -> ?parent_call_id:string option
  -> Res_stream.t list
  -> Types.patch list
