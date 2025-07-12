(** The [Fork] module provides the runtime implementation of the built-in
    "fork" GPT tool.  A call to [execute] clones the conversation history,
    runs a nested completion loop with identical tools, and returns the
    assistant's final reply.  The current commit only provides a stub so
    that the code-base continues to compile while the full logic is
    implemented over the next iterations. *)

(** [execute] clones the current conversation [history], runs a nested
    streaming completion with the same tools and tool table, forwarding
    every streaming event to the supplied [on_event] / [on_fn_out]
    callbacks.  It blocks until the nested agent finishes and returns the
    assistant’s final textual reply (concatenation of new assistant
    messages). *)
val execute
  :  env:Eio_unix.Stdenv.base
  -> history:Openai.Responses.Item.t list
  -> call_id:string
  -> arguments:string
  -> tools:Openai.Responses.Request.Tool.t list
  -> tool_tbl:(string, string -> string) Base.Hashtbl.t
  -> on_event:(Openai.Responses.Response_stream.t -> unit)
  -> on_fn_out:(Openai.Responses.Function_call_output.t -> unit)
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> unit
  -> string

val history
  :  history:Openai.Responses.Item.t list
  -> arguments:string
  -> string
  -> Openai.Responses.Item.t list
