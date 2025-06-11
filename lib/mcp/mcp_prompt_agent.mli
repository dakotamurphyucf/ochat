(** [of_chatmd_file ~env path] loads a *.chatmd* document from disk and
    produces three artefacts ready to be registered in the MCP server:

    1.  [tool] metadata – the file is exposed as an *agent tool* so that
        language-models can invoke it via [tools/call].  The tool expects a
        single string argument called [input].

    2.  [handler] – OCaml implementation that runs the chatmd document via
        [Chat_response.Driver.run_agent] and returns the assistant’s answer
        (or an error string).

    3.  [prompt] – a user-selectable prompt template that is returned by
        [prompts/list] & [prompts/get].  For the moment we simply embed the
        raw XML string under the [messages] field so that clients can render
        or post-process it as they wish. *)

val of_chatmd_file
  :  env:Eio_unix.Stdenv.base
  -> core:Mcp_server_core.t
  -> path:_ Eio.Path.t
  -> Mcp_types.Tool.t * Mcp_server_core.tool_handler * Mcp_server_core.prompt

