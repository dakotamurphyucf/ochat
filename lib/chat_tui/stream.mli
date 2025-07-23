(** Translate OpenAI streaming events into patch commands.

    The {!Chat_tui.Stream} module converts the incremental events emitted by
    the ChatCompletions *stream* endpoint of the OpenAI API – represented by
    {!Openai.Responses.Response_stream.t} – into the declarative
    {!Types.patch} language that the rest of the terminal UI understands.

    {1 Design goals}

    • *Purity* – none of the functions in this interface mutates the received
      {!Model.t}.  All side-effects are captured in the returned patch list so
      that callers can decide {i when} and {i if} they want to apply the
      changes via {!Model.apply_patches}.

    • *Single responsibility* – this module is the {e only} place that knows
      how to interpret the many concrete variants of
      {!Openai.Responses.Response_stream.t}.  The rest of the code base deals
      solely with patches and therefore remains stable when OpenAI adds new
      streaming event kinds.

    • *Forward compatibility* – while the current implementation still peeks
      into some mutable fields of the supplied model (e.g. to detect whether
      a message buffer already exists), that dependency is purely {i
      observational}.  Once the migration to an immutable model completes
      those look-ups will be replaced by equivalent reads on the structural
      snapshot.
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

    The function never mutates [model] directly. *)
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

    The function itself performs {b no} mutations; callers are expected to
    pipe the result through {!Model.apply_patches}. *)
val handle_event : model:Model.t -> Res_stream.t -> Types.patch list

(** [handle_events ~model evs] folds {!handle_event} over [evs] and
    concatenates the resulting patch lists.  It exists purely for
    convenience when a client already has a list of streaming events.  The
    function behaves like:
    {[
      List.concat_map evs ~f:(handle_event ~model)
    ]}
    and never mutates [model] itself. *)
val handle_events : model:Model.t -> Res_stream.t list -> Types.patch list
