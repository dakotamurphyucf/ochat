(** Synchronous *response loop* for ChatMarkdown conversations.

     [Response_loop] is the **blocking** counterpart to {!Fork.run_stream}.
     It keeps forwarding the current conversation [history] to the OpenAI
     Responses API, resolves every tool invocation requested by the model
     through the user-supplied [tool_tbl], and stops only when the model’s
     last turn contains **no** tool-call entries (neither
     {!Openai.Responses.Item.Function_call} nor
     {!Openai.Responses.Item.Custom_tool_call}).

     The helper is used internally by {!Driver} (CLI & tests) and by nested
     agents spawned through the [fork] tool, but it can be called directly
     when your program does **not** need incremental streaming updates.

     {1 High-level algorithm}

     1. Push [history] to the backend via {!Openai.Responses.post_response}.
     2. Append the resulting [output] items to [history].
     3. Collect every tool-call item; if the list is empty, return.
     4. For each call, look up the OCaml implementation in [tool_tbl],
        execute it, wrap its textual result in a
        [`Function_call_output`] or [`Custom_tool_call_output`] placeholder,
        and append it to [history].
     5. Repeat from step 1.

     The function is pure except for the side-effects performed by the tools
     it invokes. Errors raised by tools or by the underlying HTTP client are
     propagated unchanged, except response parsing failures, which are retried
     up to five times with delays of one through five seconds.
 *)

open! Core

type post =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Openai.Responses.Item.t list
  -> Openai.Responses.Response.t

(** [run_entries ~ctx ~allocator ~model ~tool_tbl history] extends canonical
    [history]. Existing entries retain their IDs, and provider and tool output
    occurrences receive IDs from the caller-owned [allocator].

    Provider inputs are unwrapped only at the request boundary. [post] is an
    injectable blocking transport intended for deterministic tests. *)
val run_entries
  :  ctx:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Openai.Responses.Request.Tool.t list
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?post:post
  -> model:Openai.Responses.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Hashtbl.t
  -> History_entry.t list
  -> History_entry.t list

module For_testing : sig
  (** [retry_request ~sleep ~f] runs [f] and retries response parsing failures
      at most five times. [sleep] receives delays of [1.], [2.], through [5.]
      seconds before the corresponding retry. *)
  val retry_request : sleep:(float -> unit) -> f:(unit -> 'a) -> 'a
end
