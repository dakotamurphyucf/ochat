module Res = Openai.Responses

type observer =
  { on_event : Res.Response_stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

type post_stream =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Res.Item.t list
  -> Res.Response_stream.t Seq.t

(** Raised when an observed OpenAI stream emits no next event before its idle
    deadline. Each received event resets the deadline. *)
exception Openai_stream_idle_timeout of float

(** [run_entries ~ctx ~allocator ~model ~tool_tbl ~observer history] extends
    canonical [history] while forwarding observed activity. Existing IDs
    survive every recursive turn; new provider and tool-output occurrences
    use the caller-owned [allocator]. *)
val run_entries
  :  ctx:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Res.Request.Tool.t list
  -> ?reasoning:Res.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?source:string
  -> ?parent_call_id:string
  -> model:Res.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> observer:observer
  -> ?post_stream:post_stream
  -> History_entry.t list
  -> History_entry.t list

(** [run ~ctx ~model ~tool_tbl ~observer history] streams nested-agent model
    activity while preserving sequential tool execution and canonical history
    order.

    [response_dir] receives raw Responses artifacts and defaults to [ctx]'s
    prompt directory for compatibility. [post_stream] is an injectable
    transport intended for deterministic tests. Parsing failures use the same
    bounded retry policy as {!Response_loop.run_entries}. Each wait for the next event
    has an idle deadline configured by [OCHAT_OPENAI_IDLE_TIMEOUT_SECONDS],
    defaulting to 600 seconds and capped at one hour. Responsive streams and
    subsequent agent turns may continue without an aggregate deadline. *)
val run
  :  ctx:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Res.Request.Tool.t list
  -> ?reasoning:Res.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?source:string
  -> ?parent_call_id:string
  -> model:Res.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> observer:observer
  -> ?post_stream:post_stream
  -> Res.Item.t list
  -> Res.Item.t list
