(** Agent wrapper for static {.chatmd} prompt files.

    This module turns a single *ChatMD* document (typically created with
    [chatmd] tooling) into the trio of values expected by an MCP-compatible
    server:

    {ol
    {- a {!Mcp_types.Tool.t} record that is advertised in the
       ["tools/list"] registry,}
    {- a {!Mcp_server_core.tool_handler} ready to be installed in the
       server’s dispatcher so that large-language models can trigger the
       prompt via ["tools/call"],}
    {- a {!Mcp_server_core.prompt} value that allows human users to select
       the template directly through the ["prompts/*"] API.}}

    The generated tool is deliberately simple: it exposes a single string
    parameter called [input] whose JSON schema is

    {["""json
      { "type": "object",
        "properties": { "input": { "type": "string" } },
        "required"  : [ "input" ] }
    """]}

    The corresponding handler executes the agent with
    {!Chat_response.Driver.run_agent}, streaming progress updates back to
    clients via {!Mcp_server_core.notify_progress}.  Execution happens in a
    fresh {!Eio.Switch} so that all resources (HTTP calls, files, etc.) are
    scoped and released properly.

    @raise Failure if the file at [path] cannot be read or contains invalid
           XML/ChatMD.  Most operational errors are, however, caught and
           returned as [Error] results by the handler itself.
*)

(** [of_chatmd_file ~env ~core path] loads the ChatMD document located at
    [path] and returns:  
    – the tool metadata,  
    – a handler able to run the prompt,  
    – the raw prompt (for [prompts/list]).

    The function does *not* register the artefacts; it is the caller’s
    responsibility to add the tool and handler to the relevant registries.

    Example – register a “hello_world.chatmd” agent:
    {[
      let module Prompt_agent = Mcp_prompt_agent in
      let env  = Eio.Stdenv.cwd stdenv in
      let core = Mcp_server_core.create () in
      let tool, handler, prompt =
        Prompt_agent.of_chatmd_file ~env ~core ~path:(env / "hello_world.chatmd")
      in
      Mcp_server_core.register_tool core tool ~handler;
      Mcp_server_core.register_prompt core tool.name prompt;
    ]}
*)
val of_chatmd_file
  :  env:Eio_unix.Stdenv.base
  -> core:Mcp_server_core.t
  -> path:_ Eio.Path.t
  -> Mcp_types.Tool.t * Mcp_server_core.tool_handler * Mcp_server_core.prompt
