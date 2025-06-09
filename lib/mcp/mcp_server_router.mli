open! Core

module JT = Mcp_types

(** [handle ~core json] consumes one incoming JSON-RPC payload [json] and
    returns a list of JSON values that should be emitted back to the client.
    The function supports single requests or batches (JSON arrays).  JSON-RPC
    notifications do not yield a response and therefore produce an empty
    list.  All errors are converted into proper JSON-RPC error responses so
    that the caller never needs to catch exceptions. *)
val handle : core:Mcp_server_core.t -> Jsonaf.t -> Jsonaf.t list

