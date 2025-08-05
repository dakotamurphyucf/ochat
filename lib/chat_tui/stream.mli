(** Translate raw OpenAI streaming events into declarative patch commands.

    The {!Chat_tui.Stream} module converts the incremental events emitted by
    the ChatCompletions *stream* endpoint of the OpenAI API – represented by
    {!Openai.Responses.Response_stream.t} – into the declarative
    {!Types.patch} language that the rest of the terminal UI understands.

    {1 Design goals}

    - *Side-effect minimisation* – the primary result of every helper is a
      list of {!Types.patch} values.  A small amount of internal
      book-keeping on the supplied {!Model.t} (e.g. toggling
      {!Model.active_fork}) is still required.  These mutations are confined
      to metadata fields and never alter the immutable message history that
      is expressed via patches.

    - *Single responsibility* – this module is the _only_ place that knows
      how to interpret the many concrete variants of
      {!Openai.Responses.Response_stream.t}.  The remainder of the code base
      deals solely with patches and therefore remains stable when OpenAI
      adds new streaming event kinds.

    - *Forward compatibility* – once the planned migration to an immutable
      model representation lands, even the remaining book-keeping updates
      will move into patches so that the module regains full referential
      transparency.
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

    Additionally, if the call belonged to a *fork* (the assistant’s internal
    concurrency primitive) the helper clears
    {!Model.active_fork}/{!Model.fork_start_index} so that new messages are
    rendered using the default appearance again.

    @param model State snapshot that may receive fork-book-keeping updates.
    @param out   Completed function-call record received from the OpenAI API.

    @side_effect Mutates [model.active_fork] and [model.fork_start_index] to
                 clear the special *fork* colour-coding once the call
                 finishes. *)
val handle_fn_out : model:Model.t -> Res.Function_call_output.t -> Types.patch list

(** [handle_event ~model ev] converts a single incremental streaming event
    into a list of patches.

    The implementation understands (and therefore potentially produces
    patches for) the following event classes:

    • [Output_text_delta] – append assistant text chunks
    • [Output_item_added] – initialise message buffers and metadata
    • [Reasoning_summary_text_delta] – update tool reasoning sections
    • [Function_call_arguments_delta] / [Function_call_arguments_done] –
      stream the argument list of a tool invocation

    All other variants are ignored for now and yield [\[\]].

    The returned list can be empty, contain a single patch, or multiple
    patches when a more complex update – e.g. buffer initialisation *and*
    delta append – is required.

    @param model Mutable UI state used for ancillary book-keeping (forks and
                 reasoning indices).
    @param ev    Single streaming event decoded from JSON.

    @side_effect May update {!Model.active_fork} and
                 {!Model.fork_start_index}.  All other state changes are
                 delivered as patches. *)
val handle_event : model:Model.t -> Res_stream.t -> Types.patch list

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
val handle_events : model:Model.t -> Res_stream.t list -> Types.patch list
