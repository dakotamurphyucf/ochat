(** OpenAI streaming worker for {!Chat_tui.App}.

    This module runs the OpenAI Responses streaming request and forwards
    incremental events back to the UI event loop via
    {!Chat_tui.App_events.internal_event} messages. *)

(** Raised to cancel an in-flight streaming request.

    {!Chat_tui.App_reducer} cancels streaming by failing the streaming switch
    with this exception. *)
exception Cancelled

(** [start ctx ~history ~op_id] runs a single OpenAI streaming request and
    reports progress to the internal event stream.

    The worker emits:
    {ul
    {- [`Streaming_started] once a dedicated streaming switch exists;}
    {- [`Sourced_stream] and [`Sourced_stream_batch] events for incremental deltas;}
    {- [`Tool_execution] events for transient tool lifecycle and progress;}
    {- [`Tool_output] items for tool call outputs;}
    {- [`Streaming_done] with the final item list; or}
    {- [`Streaming_error] on failure or cancellation.}}

    Normal completion flushes every accepted sourced stream, tool execution,
    tool output, and moderator request before [`Streaming_done]. The function
    catches all exceptions and converts them into a [`Streaming_error] event.
    Provider stream reads enforce a separate idle timeout in the response
    drivers. The Chat-TUI operation itself has no aggregate deadline, so
    responsive multi-turn agent and tool workflows may continue.

    All inputs other than [history] and [op_id] are bundled in {!Context.t}.

    @param history OpenAI item history that seeds the request.
    @param op_id Tags events so the reducer can ignore stale messages.

    Example:
    {[
      let streams : Chat_tui.App_context.Streams.t =
        { input; internal; redraw }
      in
      let services : Chat_tui.App_context.Services.t =
        { env; ui_sw; cwd; cache; datadir; session }
      in
      let resources : Chat_tui.App_context.Resources.t = { services; streams; ui } in
      let ctx : Chat_tui.App_streaming.Context.t =
        { shared = resources
        ; cfg
        ; tools
        ; tool_tbl
        ; moderator = None
        ; safe_point_input = None
        ; parallel_tool_calls = true
        ; history_compaction = true
        }
      in
      Chat_tui.App_streaming.start ctx ~history ~op_id:0
    ]}
*)
module Context : sig
  type t =
    { shared : App_context.Resources.t
    ; allocator : History_entry.Allocator.t
    ; cfg : Chat_response.Config.t
    ; tools : Openai.Responses.Request.Tool.t list
    ; tool_tbl : (string, Ochat_function.runner) Core.Hashtbl.t
    ; moderator : Chat_response.In_memory_stream.moderator option
    ; safe_point_input : Chat_response.In_memory_stream.Safe_point_input.t option
    ; parallel_tool_calls : bool
    ; history_compaction : bool
    }
end

val start : Context.t -> history:History_entry.t list -> op_id:int -> unit

module For_testing : sig
  type event =
    | Sourced_stream of Chat_response.Sourced_response_event.t
    | History_stream of Chat_response.History_stream_event.t
    | Tool_execution of Chat_response.Tool_execution_event.t
    | Tool_output of History_entry.t
    | Runtime_request of Chat_response.Moderation.Runtime_request.t

  (** [finish ~internal_stream ~op_id ~events ~items] synchronously emits
      chronological [events], followed by [`Streaming_done (op_id, items)].
      The destination stream must have enough capacity or a concurrent
      consumer. *)
  val finish
    :  internal_stream:App_events.internal_event Eio.Stream.t
    -> op_id:int
    -> events:event list
    -> items:History_entry.t list
    -> unit

  (** [run_with_terminal_event ~on_done ~on_error f] reports exactly one
      terminal outcome from [f]. *)
  val run_with_terminal_event
    :  on_done:('a -> unit)
    -> on_error:(exn -> unit)
    -> (unit -> 'a)
    -> unit
end
