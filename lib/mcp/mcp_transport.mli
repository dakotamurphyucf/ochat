(** Transport pack that re-exports the common {!module-type:Mcp_transport_interface.TRANSPORT}
    signature and provides the two concrete implementations shipped with this
    repository:

    • {!module:Mcp_transport_stdio} – spawns the server binary and exchanges
      newline-delimited JSON messages over the child process’ *stdin*/*stdout*
      pipes.  This is the reference implementation used by the Phase-1 CLI
      client.

    • {!module:Mcp_transport_http} – talks to an MCP server over plain
      HTTP/S.  The module understands both classic JSON bodies and streaming
      Server-Sent Events (SSE) and supports optional OAuth 2 bearer-token
      authentication.

    Developers typically write their code against the
    {!module-type:Mcp_transport_interface.TRANSPORT} signature and decide at
    runtime which concrete transport to instantiate:

    {[
      let (module T : Mcp_transport_interface.TRANSPORT) =
        match Uri.scheme uri with
        | Some "stdio" | None -> (module Mcp_transport_stdio)
        | Some ("http" | "https" | "mcp+http" | "mcp+https") ->
          (module Mcp_transport_http)
        | Some scheme ->
          invalid_arg
            (Printf.sprintf "Unsupported MCP transport scheme: %s" scheme)
      in
      let conn = T.connect ~sw ~env uri_str in
      (* ... *)
    ]}

    The indirection keeps the public API stable while new transports are
    added.
*)

include module type of Mcp_transport_interface
