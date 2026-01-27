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
    {- [`Stream] and [`Stream_batch] events for incremental deltas;}
    {- [`Tool_output] items for tool call outputs;}
    {- [`Streaming_done] with the final item list; or}
    {- [`Streaming_error] on failure or cancellation.}}

    The function catches all exceptions and converts them into a
    [`Streaming_error] event.

    All inputs other than [history] and [op_id] are bundled in {!Context.t}.

    @param history OpenAI item history that seeds the request.
    @param op_id Tags events so the reducer can ignore stale messages.

    Example:
    {[
      let streams : Chat_tui.App_context.Streams.t =
        { input; internal; system }
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
    ; cfg : Chat_response.Config.t
    ; tools : Openai.Responses.Request.Tool.t list
    ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Core.Hashtbl.t
    ; parallel_tool_calls : bool
    ; history_compaction : bool
    }
end

val start : Context.t -> history:Openai.Responses.Item.t list -> op_id:int -> unit
