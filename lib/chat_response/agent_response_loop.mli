module Res = Openai.Responses

type observer =
  { on_event : Transcript.Stream.t -> unit
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

(** Single selected executor with strict scoped transcript/tool observations.
    Existing canonical IDs and captures stay intact. Generation arguments are
    explicit overrides; omission inherits. Supplied legacy transport rejects
    before effects, with no fallback or automatic retry. *)
val run_entries
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Res.Request.Tool.t list
  -> ?reasoning:Res.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?source:string
  -> ?parent_call_id:string
  -> ?model:Res.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> observer:observer
  -> ?post_stream:post_stream
  -> History_entry.t list
  -> History_entry.t list

(** Retired provider DTO history result surface. Always rejects before effects.
    Use [run_entries] and semantic/canonical output instead. *)
val run
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Res.Request.Tool.t list
  -> ?reasoning:Res.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?source:string
  -> ?parent_call_id:string
  -> model:Res.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> observer:observer
  -> ?post_stream:post_stream
  -> Res.Item.t list
  -> Res.Item.t list
