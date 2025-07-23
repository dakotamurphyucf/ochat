(** Model Context Protocol – stdio transport implementation

    [`Mcp_transport_stdio`] is the reference implementation of the
    {!module-type:Mcp_transport_interface.TRANSPORT} signature that talks to
    an MCP server over the server process’ *standard input* and *standard
    output* streams.

    A fresh child process is started with {!Eio.Process.spawn}.  Two
    uni-directional pipes are attached:

    • *stdin*  – parent *writes* JSON-RPC **requests** → child *reads*
    • *stdout* – child  *writes* JSON-RPC **responses / notifications** →
      parent *reads*  
      stderr is merged into *stdout* so that diagnostics end up in a
      single stream.

    The wire format is **newline-delimited UTF-8 JSON** – each line contains
    exactly one JSON value and the terminating `\n` acts as framing marker.

    See the accompanying
    {{!file:lib/mcp/mcp_transport_stdio.doc.md}guide} for usage examples and
    behavioural notes.
*)

include Mcp_transport_interface.TRANSPORT
