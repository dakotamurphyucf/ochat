(** JSON-RPC message router for an MCP server.

    {1 Overview}

    [`Mcp_server_router`] is a {b pure} dispatcher that translates raw
    JSON-RPC 2.0 envelopes into high-level operations on a shared
    {!Mcp_server_core.t} registry.  The module contains {i no} blocking I/O;
    all side-effects (tool execution, structured logging, progress
    streaming …) are delegated to the injected registry or performed by the
    tool handlers themselves.

    The only entry-point {!val:handle} accepts either a single request or a
    batch (JSON array).  For each valid request the function returns exactly
    one response – unless the message is a {i notification}, in which case
    no response is produced.  Errors are {b never} raised to the caller;
    they are converted into well-formed JSON-RPC error objects so that the
    transport layer can forward them verbatim.

    Supported methods (Phase 1):
    {ul
    {- ["initialize"] – protocol and capability negotiation}
    {- ["tools/list"]  – list registered tools}
    {- ["tools/call"]  – invoke a tool by name}
    {- ["prompts/list"] – list registered prompts}
    {- ["prompts/get"] – fetch a single prompt}
    {- ["roots/list"] – enumerate project roots}
    {- ["resources/list"], ["resources/read"] – minimal resource API}
    {- ["ping"] – liveness probe}}
    Additional methods can be added transparently; unknown ones yield a
    “Method not found” JSON-RPC error.
*)

open! Core
module JT = Mcp_types

(** [handle ~core ~env json] routes the RPC envelope [json] and returns the
    list of responses that must be sent back to the client in the same
    order.

    Invariants:
    {ul
    {- Never raises – every exception is wrapped in a JSON-RPC error.}
    {- Notifications yield [[ ]] (empty list).}
    {- Batches preserve order and {e at most} one response per element.}}

    @param core  shared registry used for tools, prompts, logging, …
    @param env   Eio standard environment, required for filesystem access
                 inside the resource handlers.
    @param json  one JSON-RPC 2.0 request/notification or an array thereof.

    {2 Example – answering a ping}

    {[{
      open Core

      let () =
        (* 1 – bootstrap the registry *)
        let registry = Mcp_server_core.create () in

        (* 2 – build a JSON-RPC request  *)
        let open Mcp_types.Jsonrpc in
        let req =
          make_request ~id:(Id.of_int 1) ~method_:"ping" ()
          |> jsonaf_of_request
        in

        (* 3 – route it (we run in the main domain; use a dummy env) *)
        Eio_main.run @@ fun env ->
        let replies = Mcp_server_router.handle ~core:registry ~env req in
        List.iter replies ~f:(fun r -> Format.printf "%a@." Jsonaf.pp r)
    }]}
*)
val handle
  :  core:Mcp_server_core.t
  -> env:Eio_unix.Stdenv.base
  -> Jsonaf.t
  -> Jsonaf.t list
