(** Non-blocking MCP client – stdio Phase-1

    The client maintains a single receiver fibre that *demultiplexes*
    JSON-RPC messages and resolves per-request promises.  All helpers
    therefore come in two flavours:

    • `*_async` – return an [`'a result Eio.Promise.t`] immediately
      without blocking the calling fibre.
    • blocking wrapper (`list_tools`, `call_tool`, …) that simply
      [await]s the promise and converts it to a conventional
      [('a, string) result].

    The implementation currently supports only the stdio transport
    (URI scheme "stdio:<cmd>").  The API is written to allow adding an
    HTTP transport later without breaking callers.
*)

type t


(** Establish a new client connection and start the internal receiver
    fibre.  The MCP *initialize*/*initialized* handshake is performed
    automatically. *)
val connect
  :  sw:Eio.Switch.t
  -> env:< process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ; .. >
  -> uri:string
  -> t

val close : t -> unit
val is_closed : t -> bool

(** Low-level JSON-RPC helper: send an arbitrary request and obtain a
    promise that will resolve once the *matching* response arrives. *)
val rpc_async
  :  t
  -> Mcp_types.Jsonrpc.request
  -> (Jsonaf.t, string) result Eio.Promise.t

(** Synchronous wrapper around [rpc_async]. *)
val rpc
  :  t
  -> Mcp_types.Jsonrpc.request
  -> (Jsonaf.t, string) result

(** Tool discovery – async + blocking versions *)
val list_tools_async
  :  t -> (Mcp_types.Tool.t list, string) result Eio.Promise.t

val list_tools
  :  t -> (Mcp_types.Tool.t list, string) result

(** Tool invocation – async + blocking versions *)
val call_tool_async
  :  t
  -> name:string
  -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result Eio.Promise.t

val call_tool
  :  t
  -> name:string
  -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result

(** Access to a stream of *raw* server notifications (if needed by
    callers).  Each element is the decoded Jsonaf value. *)
val notifications : t -> Jsonaf.t Eio.Stream.t

