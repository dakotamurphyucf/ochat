(** Tool – convert ChatMarkdown [`<tool …/>`] declarations into
    runtime {!Ochat_function.t} values that can be exposed to the
    *OpenAI* chat-completions API.

    At the ChatMarkdown level the user can declare tools using one of
    four independent back-ends:

    {ol 0}
    {li  Built-ins – OCaml helpers hard-coded in {!module:Functions}
          such as ["apply_patch"], ["fork"], … }
    {li  Custom command wrappers – e.g. [{<tool command="grep" name="grep"/>}]
          which execute an external binary inside the {!Eio} sandbox. Stdout
          and stderr are combined, with live output available to observed
          invocations. Commands have a 180-second timeout, a 1,000,000-byte
          read limit, and a 10,000-byte final-result prefix limit.}
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

(** [agent_page_classification decl] returns the Agent-page classification and
    runtime name for subagent and custom shell-script declarations, including
    the built-in [fork] subagent. Other built-ins and MCP declarations return [None]. *)
val agent_page_classification
  :  Prompt.Chat_markdown.tool
  -> (string * Tool_execution_event.agent_page_kind) option

module Agent_source_runner : sig
  (** Host-owned child source selection before any legacy Fetch operation.
      [revision] identifies the host's source-selection implementation and policy;
      the native tool owner includes it in the agent tool's capability identity.
      The callback retains live authority only for the owning runtime lifetime.
      It must use an admitted source owner, never infer grants from [ctx] paths
      or [source], which is only invocation correlation. It owns preparation and
      calls the existing driver with the child's own immutable inputs.
      [run_content] receives the actual emitted/projected content descriptor. Its
      host validates exact parser/projection ownership, including nested invocation
      items. Owned content dispatch bypasses the legacy URL/items response cache;
      callback rejection/exception never falls back to Fetch. *)
  type t =
    { revision : Chatml_source.Digest.t
    ; prepare :
        (declaration:Prompt.Chat_markdown.agent_tool
         -> launch:Source_launch.t
         -> checkpoint:(unit -> unit)
         -> (Background_request.Agent_artifact.t, Agent_protocol.Error.t) Result.t)
          option
      (** Genuine installed-artifact owner for new queued admission. None leaves
          invocation behavior unchanged but rejects queued source admission. It
          must install/verify before returning, never invoke the agent here. *)
    ; run :
        implementation:Ochat_function.t option
        -> declaration:Prompt.Chat_markdown.agent_tool
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> source:string
        -> observer:Agent_response_loop.observer option
        -> Prompt.Chat_markdown.content_item list
        -> string
      (** Actual agent_fn supplies its exact constructed function. Direct None
          cannot consume a saved queued target. Nested Driver tool functions are
          distinct native identities even when names/declarations agree. *)
    ; run_content :
        declaration:Prompt.Chat_markdown.agent_content
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> string
    }
end

module Deferred_agent : sig
  (** One genuine Agent declaration's ordinary native function, constructed
      before a developer Root or inference Ctx exists. The supplied runner is
      the already bound captured-source port for the SAME actual parser
      declaration. This function never uses Fetch or manufactures a Session.

      [with_context] is trusted host code installed while constructing the
      function. It must reject until the exact physical Root/model owner has
      been bound and after that owner closes. At invocation it must enter the
      selected saved-Agent source/native/Worker proof, borrow that Root's
      Model_inference scope, and lend its genuine Ctx only to the callback.
      The callback forwards the same function identity, declaration, source,
      observer and parsed input items to [Agent_source_runner.run]. The normal
      Ochat_function invocation parent is applied to the borrowed Ctx, never
      inferred from the source string. Trusted host code must not retain that
      Ctx; physical scope and effect guards reject use after expiry.
      Failure/cancellation cannot fall back to
      the legacy file runner or turn a denied effect into a successful output.

      This low-level constructor grants neither source nor provider authority;
      the owner must register [implementation] with an exact
      Native_source_registration and its selected Root binding. *)
  type t

  val create
    :  declaration:Prompt.Chat_markdown.agent_tool
    -> runner:Agent_source_runner.t
    -> with_context:
         (implementation:Ochat_function.t
          -> invocation:Ochat_function.Invocation.t
          -> (Eio_unix.Stdenv.base Ctx.t -> string)
          -> string)
    -> t

  val implementation : t -> Ochat_function.t
end

(** Validate configured read-file roots through the same source-relative host
    resolver used during tool construction. Performs filesystem inspection only;
    other declaration variants are not constructed, discovered or executed.
    This is not a complete runtime preflight or an execution permission grant. *)
val validate_read_roots
  :  Shell_runtime.Host.t
  -> Prompt.Chat_markdown.tool
  -> (unit, Shell_runtime.Host.error) result

(** [of_declaration ~sw ~ctx ~run_agent decl] maps a single ChatMarkdown
    [`<tool …/>`] declaration to its runtime implementation.  The
    function inspects the variant constructor of [decl] and returns the
    corresponding {!Ochat_function.t} values.

    A declaration may expand into several functions – for instance an
    [`<tool mcp_server="…"/>`] element yields one function per remote
    tool exposed by the server.  Hence the result is a list.

    MCP discovery metadata is cached only within this connected declaration.
    Endpoint matches never share discovery across authenticated clients or
    runtimes. Cache expiry uses the context's Eio clock; notification listeners
    belong to [sw]. This does not change the legacy MCP server protocol.

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
  :  ?shell_registry:Shell_runtime.Registry.t
  -> ?host:Shell_runtime.Host.t
  -> ?agent_source_runner:Agent_source_runner.t
  -> sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ?observer:Agent_response_loop.observer
        -> source:string
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> string (* prompt XML *)
        -> Prompt.Chat_markdown.content_item list
        -> string (* assistant answer *))
  -> Prompt.Chat_markdown.tool
  -> Ochat_function.t list

module Native_error : sig
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp, compare, equal]
end

(** Construct a provider-independent declaration from the ordinary Eio and
    filesystem resources of this exact shell host. This path has no Ctx,
    inference Adapter, Target or nested-agent callback. An ordinary Agent or
    [meta_refine] declaration therefore returns a typed inference-required
    error; the pool may instead select an already admitted exact native
    registration before calling this function. Existing restrictions on
    Persistent_agent, Inherited and Extension remain in force. Configured
    Read_file still resolves roots through [host]. MCP clients and listeners
    remain owned by [sw]. Unexpected exceptions and cancellation propagate;
    this constructor returns typed errors only for expected missing inference. *)
val of_declaration_without_inference
  :  ?shell_registry:Shell_runtime.Registry.t
  -> ?check_preparation_now:(unit -> (unit, Native_error.t) result)
  -> sw:Eio.Switch.t
  -> host:Shell_runtime.Host.t
  -> Prompt.Chat_markdown.tool
  -> (Ochat_function.t list, Native_error.t) result
