(** High-level orchestration helpers for ChatMarkdown conversations.

    The [Driver] module bridges a user-editable {.chatmd} document and the
    OpenAI *chat/completions* API.  It bundles prompt parsing, caching,
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

    • {!val:run_completion_stream_in_memory_v1} – like
      {!val:run_completion_stream} but works on an in-memory history instead
      of a file.  Mainly used by the TUI component.
*)

open! Core

(** Run a *nested agent* inside the current conversation.

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
    inside the agent prompt. *)
val run_agent
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> string (** ^ XML source of the agent prompt *)
  -> Prompt.Chat_markdown.content_item list (** ^ Inline user items        *)
  -> string
(** Concatenated assistant answer      *)

(** Synchronous, file-based completion.

    [run_completion ~env ?prompt_file ~output_file ()] processes a full
    ChatMarkdown turn in *blocking* mode.  The evolving conversation is read
    from [output_file] (created if needed); the final assistant answer is
    appended back to the same file together with any reasoning blocks and
    tool-call artefacts.  If [prompt_file] is provided its contents are
    prepended once at the beginning of the session – handy for templates.

    No value is returned; callers should read [output_file] afterwards if they
    need the assistant answer in memory. *)
val run_completion
  :  env:Eio_unix.Stdenv.base
  -> ?prompt_file:string
  -> output_file:string
  -> unit
  -> unit

(** Streaming completion with real-time callbacks.

    Similar to {!val:run_completion} but uses the streaming OpenAI API.  The
    [on_event] callback is invoked for each {!Openai.Responses.Response_stream.t}
    event so that the caller can update a UI.  The default callback ignores
    events, making the argument optional.

    Side-effects (document updates, cache writes) mirror the blocking helper.
*)
val run_completion_stream
  :  env:Eio_unix.Stdenv.base
  -> ?prompt_file:string
  -> ?on_event:(Openai.Responses.Response_stream.t -> unit)
  -> output_file:string
  -> unit
  -> unit

(** In-memory variant used by the TUI.

    [run_completion_stream_in_memory_v1] carries the entire conversation
    history as an [Openai.Responses.Item.t list].  After the model (and any
    triggered tools) have finished responding, the *extended* history is
    returned.

    • [tools] – list of tool descriptions to advertise to the model.  When
      omitted, no tool is available.
    • [tool_tbl] – dispatch table mapping function names to OCaml
      implementations as created by {!Ochat_function.functions}.  When absent, a
      dummy empty table is used which effectively disables tool calls.
    • [on_event] / [on_fn_out] – streaming callbacks.

    The helper may recurse if the assistant issues tool calls; cancellation
    support and error propagation follow the semantics of {!Eio.Switch.run}.
*)
val run_completion_stream_in_memory_v1
  :  env:Eio_unix.Stdenv.base
  -> history:Openai.Responses.Item.t list
  -> ?on_event:(Openai.Responses.Response_stream.t -> unit)
  -> ?on_fn_out:(Openai.Responses.Function_call_output.t -> unit)
  -> tools:Openai.Responses.Request.Tool.t list option
  -> ?tool_tbl:(string, string -> string) Hashtbl.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?model:Openai.Responses.Request.model
  -> unit
  -> Openai.Responses.Item.t list
