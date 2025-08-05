(** Tool – convert ChatMarkdown [`<tool …/>`] declarations into
    runtime {!Ochat_function.t} values that can be exposed to the
    *OpenAI* chat-completions API.

    At the ChatMarkdown level the user can declare tools using one of
    four independent back-ends:

    {ol 0}
    {li  Built-ins – OCaml helpers hard-coded in {!module:Functions}
          such as ["apply_patch"], ["fork"], … }
    {li  Custom shell wrappers – e.g. [{<tool command="grep" name="grep"/>}]
          which execute an external binary inside the {!Eio} sandbox.}
    {li  Agent prompts – nested ChatMarkdown documents executed through
          the same driver stack.  Allows hierarchical compositions of
          specialised prompts. }
    {li  Remote MCP tools – functions discovered dynamically from a
          Model-Context-Protocol server. }
    {/ol}

    The public surface is intentionally tiny.  Client code should rely
    exclusively on the two helpers documented below; everything else
    is private glue. *)

(** Convert the minimal tool descriptors returned by
    {!Openai.Completions.post_completion} into the richer
    [Openai.Responses.Request.Tool.t] records expected by the
    *chat/completions* endpoint.

    The conversion is purely structural – field-by-field copy – and
    therefore in O(n) where n = length of the input list. *)
val convert_tools : Openai.Completions.tool list -> Openai.Responses.Request.Tool.t list

(** [of_declaration ~sw ~ctx ~run_agent decl] maps a single ChatMarkdown
    [`<tool …/>`] declaration to its runtime implementation.  The
    function inspects the variant constructor of [decl] and returns the
    corresponding {!Ochat_function.t} values.

    A declaration may expand into several functions – for instance an
    [`<tool mcp_server="…"/>`] element yields one function per remote
    tool exposed by the server.  Hence the result is a list.

    Parameters:
    • [sw] — parent {!Eio.Switch.t}.  Any background fibres (e.g. MCP
      cache invalidation listeners) are attached to this switch so that
      they terminate when the caller’s scope ends.
    • [ctx] — shared execution context giving access to filesystem
      roots, network handles, caches, …
    • [run_agent] — higher-order helper used to spawn a nested agent
      when handling the [CM.Agent] variant; it corresponds to
      {!Chat_response.Driver.run_agent} but is passed explicitly to
      avoid a circular dependency.

    @raise Failure if the declaration references an unknown built-in
           tool name. *)
val of_declaration
  :  sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> run_agent:
       (ctx:Eio_unix.Stdenv.base Ctx.t
        -> string (* prompt XML *)
        -> Prompt.Chat_markdown.content_item list
        -> string (* assistant answer *))
  -> Prompt.Chat_markdown.tool
  -> Ochat_function.t list
