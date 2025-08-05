(** Synchronous *response loop* for ChatMarkdown conversations.

     [Response_loop] is the **blocking** counterpart to {!Fork.run_stream}.
     It keeps forwarding the current conversation [history] to the OpenAI
     *chat/completions* endpoint, resolves every tool invocation requested
     by the model through the user-supplied [tool_tbl], and stops only when
     the model’s last turn contains **no** {!Openai.Responses.Item.Function_call}
     entries.

     The helper is used internally by {!Driver} (CLI & tests) and by nested
     agents spawned through the [fork] tool, but it can be called directly
     when your program does **not** need incremental streaming updates.

     {1 High-level algorithm}

     1. Push [history] to the backend via {!Openai.Responses.post_response}.
     2. Append the resulting [output] items to [history].
     3. Collect every [`Function_call`] item; if the list is empty, return.
     4. For each call, look up the OCaml implementation in [tool_tbl],
        execute it, wrap its textual result in a
        [`Function_call_output`] placeholder, and append it to [history].
     5. Repeat from step 1.

     The function is pure except for the side-effects performed by the tools
     it invokes.  Errors raised by those tools or by the underlying HTTP
     client are propagated to the caller unchanged.
 *)

open! Core

(** [run ~ctx ?temperature ?max_output_tokens ?tools ?reasoning
        ?fork_depth ?history_compaction ~model ~tool_tbl history]

    Expands [history] until the assistant’s most recent reply contains
    **no** {!Openai.Responses.Item.Function_call} items and returns the
    concatenated list of conversation items.

    {1 Parameters}

    • [ctx] – immutable execution context providing a network handle
      ([net]), a base directory ([dir]) and a shared cache.

    • [temperature] – optional sampling temperature passed verbatim to
      the model.

    • [max_output_tokens] – per-request upper bound on generated
      tokens.

    • [tools] – list of tool definitions forwarded unchanged so the
      model can invoke them.

    • [reasoning] – request that the model emits [`Reasoning`] blocks.

    • [fork_depth] – internal recursion counter used when the loop is
      entered via the built-in [fork] tool (default = 0).  External
      callers should leave the default.

    • [history_compaction] – when [true], redundant
      [read_file]/[`Function_call_output`] pairs are collapsed so only
      the most recent version of each file is re-sent (see
      {!Compact_history.collapse_read_file_history}).

    • [model] – OpenAI model used for **every** iteration.

    • [tool_tbl] – mapping *tool-name ↦ implementation*.  The table
      **must** contain ["fork"] ↦ {!Fork.execute} so nested agents can
      run.

    • [history] – full conversation so far (user messages, assistant
      replies, previous tool outputs, …).

    {1 Return value}

    Extended conversation that includes every assistant reply and
    [`Function_call_output`] produced while the loop was active.

    @raise Not_found if the model produces a tool name that is not
      present in [tool_tbl].
*)
val run
  :  ctx:< net : _ Eio.Net.t ; .. > Ctx.t
  -> ?temperature:float (** Sampling temperature forwarded to the model. *)
  -> ?max_output_tokens:int (** Hard cap on tokens generated *per request*. *)
  -> ?tools:Openai.Responses.Request.Tool.t list (** Tools visible to the model. *)
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
       (** Should the model emit reasoning blocks? *)
  -> ?fork_depth:int
       (** Internal recursion guard for the [fork] tool.  End-users should
           leave the default. *)
  -> ?history_compaction:bool
       (** When [true] collapses redundant file-read entries so that only
                                    the latest version of each document is sent to the model.
                                    Defaults to [false]. *)
  -> model:Openai.Responses.Request.model (** OpenAI model used for **all** iterations. *)
  -> tool_tbl:(string, string -> string) Hashtbl.t
       (** Mapping *tool name ↦ implementation*.
                                                       Must contain a ["fork"] entry pointing at
                                                       {!Fork.execute}. *)
  -> Openai.Responses.Item.t list (** Full conversation so far. *)
  -> Openai.Responses.Item.t list
