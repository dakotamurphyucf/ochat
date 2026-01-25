(** OpenAI streaming worker for {!Chat_tui.App}.

    This module runs the OpenAI Responses streaming request and forwards
    incremental events back to the UI event loop via
    {!Chat_tui.App_events.internal_event} messages. *)

(** Raised to cancel an in-flight streaming request.

    {!Chat_tui.App_reducer} cancels streaming by failing the streaming switch
    with this exception. *)
exception Cancelled

(** [start ~env ~history ~internal_stream ~system_event ~cfg ~tools ~tool_tbl ...]
    runs a single OpenAI streaming request and reports progress to
    [internal_stream].

    The worker emits:
    {ul
    {- [`Streaming_started] once a dedicated streaming switch exists;}
    {- [`Stream] and [`Stream_batch] events for incremental deltas;}
    {- [`Tool_output] items for tool call outputs;}
    {- [`Streaming_done] with the final item list; or}
    {- [`Streaming_error] on failure or cancellation.}}

    The function catches all exceptions and converts them into a
    [`Streaming_error] event.

    @param env Provides network, filesystem, and clock resources.
    @param history OpenAI item history that seeds the request.
    @param internal_stream Receives streaming lifecycle and delta events.
    @param system_event Queue of out-of-band notes that should be included in
           the assistant context but must not be rendered in the transcript.
    @param cfg Model settings (temperature, model name, token limits, ...).
    @param tools Tool declaration list exposed to the assistant.
    @param tool_tbl Maps tool names to implementations that produce tool outputs.
    @param datadir Directory used for response cache and tool artefacts.
    @param parallel_tool_calls Controls whether tool calls may run concurrently.
    @param history_compaction Forwards to the driver’s lightweight history
           compaction.
    @param op_id Tags events so the reducer can ignore stale messages.

    Example (the app typically passes a partially-applied [start] into the reducer):
    {[
      let handle_submit =
        Chat_tui.App_streaming.start
          ~cfg
          ~tools
          ~tool_tbl
      in
      ignore handle_submit
    ]}
*)
val start
  :  env:Eio_unix.Stdenv.base
  -> history:Openai.Responses.Item.t list
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> cfg:Chat_response.Config.t
  -> tools:Openai.Responses.Request.Tool.t list
  -> tool_tbl:(string, string -> Openai.Responses.Tool_output.Output.t) Core.Hashtbl.t
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> parallel_tool_calls:bool
  -> history_compaction:bool
  -> op_id:int
  -> unit
