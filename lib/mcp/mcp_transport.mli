(** Minimal transport abstraction for talking to an MCP server.  The
    design is intentionally tiny – just enough for the Phase-1 stdio
    client implementation.  Additional helpers (streaming, batching,
    HTTP, …) will be added in later milestones. *)

include module type of Mcp_transport_interface
